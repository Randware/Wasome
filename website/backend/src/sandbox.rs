use std::collections::HashMap;
use std::io;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;

use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::Command;
use tokio::sync::{Semaphore, mpsc};
use tokio::task::JoinHandle;

const COMPILE_TIMEOUT: Duration = Duration::from_secs(20);
const CLEANUP_TIMEOUT: Duration = Duration::from_secs(5);
const MAX_LOG_BYTES: usize = 128 * 1024;
const MAX_WASM_BYTES: usize = 4 * 1024 * 1024;
const WASM_MODULE_HEADER: &[u8] = b"\0asm\x01\0\0\0";

pub struct CompileJob {
    pub logs: mpsc::UnboundedReceiver<String>,
    pub result: JoinHandle<Result<Option<Vec<u8>>, CompileError>>,
}

pub enum CompileError {
    Busy,
    TimedOut,
    Failed,
}

impl CompileError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::Busy => "Compiler is busy. Try again later.",
            Self::TimedOut => "Compilation timed out",
            Self::Failed => "Compiler sandbox failed",
        }
    }
}

pub fn start(
    slots: Arc<Semaphore>,
    files: &HashMap<String, String>,
    toolchain: String,
    runtime: String,
) -> Result<CompileJob, CompileError> {
    let permit = slots.try_acquire_owned().map_err(|_| CompileError::Busy)?;
    let input = archive(files).map_err(|_| CompileError::Failed)?;
    let (send_logs, logs) = mpsc::unbounded_channel();

    // Keep the job and its cleanup alive if the browser disconnects.
    let result = tokio::spawn(async move {
        let _permit = permit;
        let name = format!("wasome-compile-{}", uuid::Uuid::new_v4());
        let outcome = tokio::time::timeout(
            COMPILE_TIMEOUT,
            run(&name, &toolchain, &runtime, &input, send_logs),
        )
        .await;

        // Killing the Docker CLI process alone doesn't stop a container.
        let _ = tokio::time::timeout(CLEANUP_TIMEOUT, docker().args(["rm", "-f", &name]).status())
            .await;

        match outcome {
            Ok(Ok(wasm)) => Ok(wasm),
            Ok(Err(_)) => Err(CompileError::Failed),
            Err(_) => Err(CompileError::TimedOut),
        }
    });

    Ok(CompileJob { logs, result })
}

fn archive(files: &HashMap<String, String>) -> io::Result<Vec<u8>> {
    let mut archive = tar::Builder::new(Vec::new());
    for (path, contents) in files {
        let mut header = tar::Header::new_gnu();
        header.set_size(contents.len() as u64);
        header.set_mode(0o600);
        header.set_entry_type(tar::EntryType::Regular);
        archive.append_data(&mut header, path, contents.as_bytes())?;
    }
    archive.into_inner()
}

fn docker() -> Command {
    let mut command = Command::new("docker");
    command
        .kill_on_drop(true)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    command
}

async fn run(
    name: &str,
    toolchain: &str,
    runtime: &str,
    input: &[u8],
    logs: mpsc::UnboundedSender<String>,
) -> io::Result<Option<Vec<u8>>> {
    let status = docker()
        .args([
            "create",
            "--rm",
            "--name",
            name,
            "--interactive",
            "--pull=never",
            &format!("--runtime={runtime}"),
            "--network=none",
            "--read-only",
            "--user=1000:1000",
            "--cap-drop=ALL",
            "--security-opt=no-new-privileges",
            "--cpus=0.50",
            "--memory=256M",
            "--pids-limit=64",
            "--log-driver=none",
            "--tmpfs=/workspace:rw,nosuid,nodev,noexec,size=64m,uid=1000,gid=1000,mode=0700",
            "--tmpfs=/tmp:rw,nosuid,nodev,noexec,size=50m,mode=1777",
            &format!("--volume={toolchain}:/opt/wasome:ro"),
            "wasome-compiler-sandbox:bounded",
        ])
        .status()
        .await?;
    if !status.success() {
        return Err(io::Error::other(
            "Docker could not create the compiler sandbox",
        ));
    }

    let mut child = docker()
        .args(["start", "--attach", "--interactive", name])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| io::Error::other("Missing stdin"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| io::Error::other("Missing stdout"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| io::Error::other("Missing stderr"))?;

    let (_, wasm, _, status) = tokio::try_join!(
        async move {
            stdin.write_all(input).await?;
            stdin.shutdown().await
        },
        read_wasm(stdout),
        forward_logs(stderr, logs),
        child.wait(),
    )?;

    Ok(wasm.filter(|_| status.success()))
}

async fn read_wasm(stdout: impl tokio::io::AsyncRead + Unpin) -> io::Result<Option<Vec<u8>>> {
    let mut wasm = Vec::new();
    stdout
        .take(MAX_WASM_BYTES as u64 + 1)
        .read_to_end(&mut wasm)
        .await?;
    if wasm.len() > MAX_WASM_BYTES {
        return Err(io::Error::other(
            "Compiler output exceeded the artifact limit",
        ));
    }
    Ok(wasm.starts_with(WASM_MODULE_HEADER).then_some(wasm))
}

async fn forward_logs(
    mut stderr: impl tokio::io::AsyncRead + Unpin,
    logs: mpsc::UnboundedSender<String>,
) -> io::Result<()> {
    {
        let mut reader = BufReader::new((&mut stderr).take(MAX_LOG_BYTES as u64));
        let mut line = Vec::new();
        while reader.read_until(b'\n', &mut line).await? != 0 {
            while matches!(line.last(), Some(b'\n' | b'\r')) {
                line.pop();
            }
            if logs
                .send(String::from_utf8_lossy(&line).into_owned())
                .is_err()
            {
                break;
            }
            line.clear();
        }
    }
    drop(logs);
    tokio::io::copy(&mut stderr, &mut tokio::io::sink()).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn archive_contains_only_regular_input_files() {
        let files = HashMap::from([("src/main.waso".to_owned(), "example".to_owned())]);
        let input = archive(&files).unwrap();
        let mut decoded = tar::Archive::new(input.as_slice());
        let mut entries = decoded.entries().unwrap();
        let entry = entries.next().unwrap().unwrap();
        assert_eq!(
            entry.path().unwrap().as_ref(),
            std::path::Path::new("src/main.waso")
        );
        assert!(entry.header().entry_type().is_file());
        assert!(entries.next().is_none());
    }

    #[tokio::test]
    async fn accepts_only_bounded_webassembly_output() {
        assert!(read_wasm(WASM_MODULE_HEADER).await.unwrap().is_some());
        assert!(read_wasm(b"plain text".as_slice()).await.unwrap().is_none());
        let oversized = vec![0; MAX_WASM_BYTES + 1];
        assert!(read_wasm(oversized.as_slice()).await.is_err());
    }
}
