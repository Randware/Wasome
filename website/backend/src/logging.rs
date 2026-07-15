use std::collections::HashMap;
use std::fs;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;
use chrono::Local;
use flate2::write::GzEncoder;
use flate2::Compression;

const FLUSH_INTERVAL: Duration = Duration::from_secs(5);
const LOG_DIR: &str = "logs";

pub struct Logger {
    main_writer: Mutex<BufWriter<fs::File>>,
    code_writer: Mutex<BufWriter<fs::File>>,
    log_dir: PathBuf,
}

impl Logger {
    pub fn init() -> Arc<Self> {
        let log_dir = PathBuf::from(LOG_DIR);
        fs::create_dir_all(&log_dir).expect("failed to create logs directory");

        let latest_path = log_dir.join("latest.log");
        let latest_code_path = log_dir.join("latest-code.log");

        if latest_path.exists() {
            compress_and_remove(&latest_path, &log_dir, false);
        }
        if latest_code_path.exists() {
            compress_and_remove(&latest_code_path, &log_dir, true);
        }

        let main_file = fs::File::create(&latest_path).expect("failed to create latest.log");
        let code_file =
            fs::File::create(&latest_code_path).expect("failed to create latest-code.log");

        let logger = Arc::new(Self {
            main_writer: Mutex::new(BufWriter::new(main_file)),
            code_writer: Mutex::new(BufWriter::new(code_file)),
            log_dir,
        });

        let flusher = Arc::clone(&logger);
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(FLUSH_INTERVAL).await;
                flusher.flush().await;
            }
        });

        logger
    }

    async fn write_line(&self, level: &str, module: &str, message: &str) {
        let ts = Local::now().format("%Y-%m-%d %H:%M:%S");
        let line = format!("[{}] [{}] [{}] {}\n", ts, level, module, message);
        let mut w = self.main_writer.lock().await;
        let _ = w.write_all(line.as_bytes());
    }

    pub async fn info(&self, module: &str, message: &str) {
        self.write_line("INFO", module, message).await;
    }

    pub async fn warn(&self, module: &str, message: &str) {
        self.write_line("WARN", module, message).await;
    }

    pub async fn error(&self, module: &str, message: &str) {
        self.write_line("ERROR", module, message).await;
    }

    pub async fn security(&self, module: &str, message: &str) {
        self.write_line("SECURITY", module, message).await;
    }

    pub async fn log_compilation(
        &self,
        ip_hash: &str,
        file_count: usize,
        total_size: usize,
        file_paths: &[&str],
        result: &str,
        duration: Duration,
    ) {
        let msg = format!(
            "IP={} files={} size={}B paths=[{}] result={} duration={:.1}s",
            ip_hash,
            file_count,
            total_size,
            file_paths.join(", "),
            result,
            duration.as_secs_f64()
        );
        self.write_line("INFO", "compile", &msg).await;
    }

    pub async fn log_code(
        &self,
        ip_hash: &str,
        files: &HashMap<String, String>,
        timestamp: &str,
    ) {
        let mut w = self.code_writer.lock().await;
        let separator = "=".repeat(80);
        let header = format!(
            "\n{}\n[{}] IP={} file_count={}\n{}\n",
            separator,
            timestamp,
            ip_hash,
            files.len(),
            separator
        );
        let _ = w.write_all(header.as_bytes());
        for (path, content) in files {
            let file_header = format!("\n--- {} ({} bytes) ---\n", path, content.len());
            let _ = w.write_all(file_header.as_bytes());
            let _ = w.write_all(content.as_bytes());
            let _ = w.write_all(b"\n");
        }
    }

    pub async fn flush(&self) {
        {
            let mut w = self.main_writer.lock().await;
            let _ = w.flush();
        }
        {
            let mut w = self.code_writer.lock().await;
            let _ = w.flush();
        }
    }

    pub async fn shutdown(&self) {
        self.flush().await;
        let latest = self.log_dir.join("latest.log");
        let latest_code = self.log_dir.join("latest-code.log");
        if latest.exists() {
            compress_and_remove(&latest, &self.log_dir, false);
        }
        if latest_code.exists() {
            compress_and_remove(&latest_code, &self.log_dir, true);
        }
    }
}

fn compress_and_remove(source: &Path, log_dir: &Path, is_code: bool) {
    let date = Local::now().format("%Y-%m-%d").to_string();
    let suffix = if is_code { "-code" } else { "" };

    let mut n = 1u32;
    let gz_path = loop {
        let name = format!("{}-{}{}.log.gz", date, n, suffix);
        let path = log_dir.join(&name);
        if !path.exists() {
            break path;
        }
        n += 1;
    };

    if let Ok(data) = fs::read(source) {
        if let Ok(out_file) = fs::File::create(&gz_path) {
            let mut encoder = GzEncoder::new(out_file, Compression::default());
            if encoder.write_all(&data).is_ok() {
                let _ = encoder.finish();
            }
        }
    }
    let _ = fs::remove_file(source);
}
