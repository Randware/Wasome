use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::{Duration, Instant};

use axum::extract::ws::{Message, WebSocket};
use axum::extract::{ConnectInfo, Json, State, WebSocketUpgrade};
use axum::http::{HeaderMap, StatusCode};
use axum::response::IntoResponse;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use tempfile::Builder;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

use crate::security::ThreatEvent;
use crate::session;
use crate::AppState;

const MAX_FILES: usize = 30;
const MAX_TOTAL_SIZE: usize = 50 * 1024;
const MAX_PATH_DEPTH: usize = 5;
const DOCKER_TIMEOUT: Duration = Duration::from_secs(20);

#[derive(Deserialize)]
pub struct CompilePayload {
    files: HashMap<String, String>,
    entry: String,
    #[serde(default)]
    turnstile_token: Option<String>,
}

#[derive(Serialize)]
pub struct CompileResponse {
    logs: String,
    wasm: Option<String>,
}

#[derive(Serialize)]
struct WsMessage {
    #[serde(rename = "type")]
    msg_type: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    success: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    cookie: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    status: Option<u16>,
}

fn hash_ip(ip: IpAddr) -> String {
    let mut hasher = Sha256::new();
    hasher.update(ip.to_string().as_bytes());
    hex::encode(&hasher.finalize()[..8])
}

fn extract_ip(headers: &HeaderMap, addr: Option<&ConnectInfo<std::net::SocketAddr>>) -> IpAddr {
    if let Some(real_ip) = headers.get("x-real-ip") {
        if let Ok(s) = real_ip.to_str() {
            if let Ok(ip) = s.parse::<IpAddr>() {
                return ip;
            }
        }
    }
    addr.map(|ci| ci.0.ip())
        .unwrap_or_else(|| IpAddr::V4(std::net::Ipv4Addr::UNSPECIFIED))
}

fn validate_payload(
    payload: &CompilePayload,
) -> Result<(usize, Vec<String>), (ThreatEvent, &'static str)> {
    if payload.files.len() > MAX_FILES {
        return Err((ThreatEvent::OversizedPayload, "Too many files"));
    }

    let mut total_size = 0usize;
    let mut paths = Vec::new();

    for (path, content) in &payload.files {
        if path.contains('\0') || content.contains('\0') {
            return Err((ThreatEvent::NullBytePayload, "Invalid payload content"));
        }
        if path.contains("..") || path.starts_with('/') || path.starts_with('\\') {
            return Err((ThreatEvent::InvalidPath, "Invalid file path"));
        }
        if path.split('/').count() > MAX_PATH_DEPTH {
            return Err((ThreatEvent::InvalidPath, "Path too deep"));
        }
        total_size += content.len();
        if total_size > MAX_TOTAL_SIZE {
            return Err((ThreatEvent::OversizedPayload, "Payload too large"));
        }
        paths.push(path.clone());
    }

    if !payload.files.contains_key(&payload.entry) {
        return Err((ThreatEvent::InvalidPath, "Entry point not found"));
    }

    Ok((total_size, paths))
}

async fn verify_turnstile(secret: &str, token: &str) -> bool {
    if secret == "1x0000000000000000000000000000000AA" && token == "dummy-bypass" {
        return true;
    }
    let client = reqwest::Client::new();
    let res = client
        .post("https://challenges.cloudflare.com/turnstile/v0/siteverify")
        .form(&[("secret", secret), ("response", token)])
        .send()
        .await;

    #[derive(Deserialize)]
    struct TurnstileResponse {
        success: bool,
    }

    match res {
        Ok(response) => response
            .json::<TurnstileResponse>()
            .await
            .map(|r| r.success)
            .unwrap_or(false),
        Err(_) => false,
    }
}

async fn run_docker(
    temp_path: &std::path::Path,
    wasome_dir: &str,
    runtime: &str,
    ws: &mut WebSocket,
) -> Result<bool, String> {
    let runtime_arg = format!("--runtime={}", runtime);
    let mut child = Command::new("docker")
        .args([
            "run",
            "--rm",
            &runtime_arg,
            "--network=none",
            "--read-only",
            "--user=1000:1000",
            "--cap-drop=ALL",
            "--cpus=0.50",
            "--memory=256M",
            "--tmpfs=/tmp:size=50M,exec,mode=1777",
            &format!("-v={}:/workspace", temp_path.display()),
            &format!("-v={}:/opt/wasome:ro", wasome_dir),
            "wasome-compiler-sandbox:latest",
            "/opt/wasome/bin/waso",
            "build",
            "--target",
            "web",
            "--profile",
            "size",
            "/workspace",
        ])
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .map_err(|e| format!("Failed to start Docker: {}", e))?;

    let stdout = child.stdout.take();
    let stderr = child.stderr.take();

    let (tx, mut rx) = tokio::sync::mpsc::channel::<String>(100);

    let tx_out = tx.clone();
    if let Some(stdout) = stdout {
        tokio::spawn(async move {
            let mut lines = BufReader::new(stdout).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                if tx_out.send(line).await.is_err() {
                    break;
                }
            }
        });
    }

    let tx_err = tx.clone();
    if let Some(stderr) = stderr {
        tokio::spawn(async move {
            let mut lines = BufReader::new(stderr).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                if tx_err.send(line).await.is_err() {
                    break;
                }
            }
        });
    }

    drop(tx);

    let compile_result = tokio::time::timeout(DOCKER_TIMEOUT, async {
        while let Some(line) = rx.recv().await {
            let msg = serde_json::to_string(&WsMessage {
                msg_type: "log",
                data: Some(line),
                message: None,
                success: None,
                cookie: None,
                status: None,
            })
            .unwrap_or_default();
            let _ = ws.send(Message::Text(msg.into())).await;
        }
        child.wait().await
    })
    .await;

    match compile_result {
        Ok(Ok(status)) => Ok(status.success()),
        Ok(Err(e)) => Err(format!("Docker process error: {}", e)),
        Err(_) => Err("Compilation timed out".into()),
    }
}

async fn send_ws_json(ws: &mut WebSocket, msg: &WsMessage) {
    if let Ok(text) = serde_json::to_string(msg) {
        let _ = ws.send(Message::Text(text.into())).await;
    }
}

pub async fn ws_compile_handler(
    ws: WebSocketUpgrade,
    headers: HeaderMap,
    State(state): State<Arc<AppState>>,
    addr: Option<ConnectInfo<std::net::SocketAddr>>,
) -> impl IntoResponse {
    let ip = extract_ip(&headers, addr.as_ref());
    let cookie_header = headers
        .get("cookie")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("")
        .to_owned();

    ws.on_upgrade(move |socket| handle_ws_connection(socket, ip, cookie_header, state))
}

async fn handle_ws_connection(
    mut ws: WebSocket,
    ip: IpAddr,
    cookie_header: String,
    state: Arc<AppState>,
) {
    let ip_hash = hash_ip(ip);

    if state.security.check_ip(ip).await.is_err() {
        send_ws_json(
            &mut ws,
            &WsMessage {
                msg_type: "error",
                message: Some("IP is temporarily banned".into()),
                data: None,
                success: None,
                cookie: None,
                status: Some(403),
            },
        )
        .await;
        let _ = ws.close().await;
        return;
    }

    if state.config.maintenance_mode.load(Ordering::Relaxed) {
        send_ws_json(
            &mut ws,
            &WsMessage {
                msg_type: "error",
                message: Some("Server is in maintenance mode".into()),
                data: None,
                success: None,
                cookie: None,
                status: Some(503),
            },
        )
        .await;
        let _ = ws.close().await;
        return;
    }

    if !state.config.compilation_enabled.load(Ordering::Relaxed) {
        send_ws_json(
            &mut ws,
            &WsMessage {
                msg_type: "error",
                message: Some("Compilation is currently disabled".into()),
                data: None,
                success: None,
                cookie: None,
                status: Some(403),
            },
        )
        .await;
        let _ = ws.close().await;
        return;
    }

    if !state.rate_limiter.check_rate(ip).await {
        send_ws_json(
            &mut ws,
            &WsMessage {
                msg_type: "error",
                message: Some("Rate limit exceeded. Try again later.".into()),
                data: None,
                success: None,
                cookie: None,
                status: Some(429),
            },
        )
        .await;
        let _ = ws.close().await;
        return;
    }

    let raw = match ws.recv().await {
        Some(Ok(Message::Text(text))) => text.to_string(),
        _ => return,
    };

    let payload: CompilePayload = match serde_json::from_str(&raw) {
        Ok(p) => p,
        Err(_) => {
            send_ws_json(
                &mut ws,
                &WsMessage {
                    msg_type: "error",
                    message: Some("Invalid JSON payload".into()),
                    data: None,
                    success: None,
                    cookie: None,
                    status: Some(400),
                },
            )
            .await;
            return;
        }
    };

    let (total_size, file_paths) = match validate_payload(&payload) {
        Ok(v) => v,
        Err((event, msg)) => {
            state.security.record_event(ip, event).await;
            send_ws_json(
                &mut ws,
                &WsMessage {
                    msg_type: "error",
                    message: Some(msg.into()),
                    data: None,
                    success: None,
                    cookie: None,
                    status: Some(400),
                },
            )
            .await;
            return;
        }
    };

    let has_valid_session = if let Some(sid) = session::extract_session_id(&cookie_header) {
        state.sessions.validate_session(sid, ip).await
    } else {
        false
    };

    if !has_valid_session {
        if let Some(ref secret) = state.config.turnstile_secret_key {
            let token = payload.turnstile_token.as_deref().unwrap_or("");
            if token.is_empty() || !verify_turnstile(secret, token).await {
                state
                    .security
                    .record_event(ip, ThreatEvent::InvalidTurnstile)
                    .await;
                send_ws_json(
                    &mut ws,
                    &WsMessage {
                        msg_type: "error",
                        message: Some("Turnstile verification failed".into()),
                        data: None,
                        success: None,
                        cookie: None,
                        status: Some(401),
                    },
                )
                .await;
                return;
            }
        }

        let session_id = state.sessions.create_session(ip).await;
        send_ws_json(
            &mut ws,
            &WsMessage {
                msg_type: "session",
                cookie: Some(session::session_cookie(&session_id)),
                message: None,
                data: None,
                success: None,
                status: None,
            },
        )
        .await;
    }

    let start = Instant::now();

    let work_dir = std::path::Path::new("/tmp/wasome-workspaces");
    let _ = tokio::fs::create_dir_all(work_dir).await;

    let temp_dir = match Builder::new().tempdir_in(work_dir) {
        Ok(d) => d,
        Err(_) => {
            send_ws_json(
                &mut ws,
                &WsMessage {
                    msg_type: "error",
                    message: Some("Failed to create workspace".into()),
                    data: None,
                    success: None,
                    cookie: None,
                    status: Some(500),
                },
            )
            .await;
            return;
        }
    };

    for (path, content) in &payload.files {
        let file_path = temp_dir.path().join(path);
        if let Some(parent) = file_path.parent() {
            let _ = tokio::fs::create_dir_all(parent).await;
        }
        let _ = tokio::fs::write(&file_path, content).await;
    }

    let _ = std::process::Command::new("chmod")
        .args(["-R", "777", temp_dir.path().to_str().unwrap_or("")])
        .status();

    send_ws_json(
        &mut ws,
        &WsMessage {
            msg_type: "status",
            message: Some("Compiling...".into()),
            data: None,
            success: None,
            cookie: None,
            status: None,
        },
    )
    .await;

    let success = match run_docker(temp_dir.path(), &state.config.host_wasome_dir, &state.config.docker_runtime, &mut ws).await {
        Ok(ok) => ok,
        Err(e) => {
            send_ws_json(
                &mut ws,
                &WsMessage {
                    msg_type: "error",
                    message: Some(e),
                    data: None,
                    success: None,
                    cookie: None,
                    status: Some(500),
                },
            )
            .await;
            false
        }
    };

    if success {
        let out_dir = temp_dir.path().join("out");
        if let Ok(mut entries) = tokio::fs::read_dir(&out_dir).await {
            while let Ok(Some(entry)) = entries.next_entry().await {
                if entry.path().extension().and_then(|e| e.to_str()) == Some("wasm") {
                    if let Ok(wasm_bytes) = tokio::fs::read(entry.path()).await {
                        let _ = ws.send(Message::Binary(wasm_bytes.into())).await;
                    }
                    break;
                }
            }
        }
    }

    send_ws_json(
        &mut ws,
        &WsMessage {
            msg_type: "done",
            success: Some(success),
            message: None,
            data: None,
            cookie: None,
            status: None,
        },
    )
    .await;

    let duration = start.elapsed();
    let result_str = if success { "success" } else { "error" };
    let path_refs: Vec<&str> = file_paths.iter().map(|s| s.as_str()).collect();
    state
        .logger
        .log_compilation(
            &ip_hash,
            payload.files.len(),
            total_size,
            &path_refs,
            result_str,
            duration,
        )
        .await;

    let ts = chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
    state
        .logger
        .log_code(&ip_hash, &payload.files, &ts)
        .await;

    state
        .logger
        .info(
            "compile",
            &format!(
                "Client compiled {} files ({}B total)",
                payload.files.len(),
                total_size
            ),
        )
        .await;
}

// --- HTTP fallback handler (kept for backwards compat) ---

pub async fn http_compile_handler(
    headers: HeaderMap,
    State(state): State<Arc<AppState>>,
    addr: Option<ConnectInfo<std::net::SocketAddr>>,
    Json(payload): Json<CompilePayload>,
) -> impl IntoResponse {
    let ip = extract_ip(&headers, addr.as_ref());
    let ip_hash = hash_ip(ip);

    if state.security.check_ip(ip).await.is_err() {
        return (StatusCode::FORBIDDEN, "IP is temporarily banned").into_response();
    }
    if state.config.maintenance_mode.load(Ordering::Relaxed) {
        return (StatusCode::SERVICE_UNAVAILABLE, "Maintenance mode").into_response();
    }
    if !state.config.compilation_enabled.load(Ordering::Relaxed) {
        return (StatusCode::SERVICE_UNAVAILABLE, "Compilation disabled").into_response();
    }
    if !state.rate_limiter.check_rate(ip).await {
        return (StatusCode::TOO_MANY_REQUESTS, "Rate limit exceeded").into_response();
    }

    let (total_size, file_paths) = match validate_payload(&payload) {
        Ok(v) => v,
        Err((event, msg)) => {
            state.security.record_event(ip, event).await;
            return (StatusCode::BAD_REQUEST, msg).into_response();
        }
    };

    if let Some(ref secret) = state.config.turnstile_secret_key {
        let token = payload.turnstile_token.as_deref().unwrap_or("");
        if token.is_empty() || !verify_turnstile(secret, token).await {
            state
                .security
                .record_event(ip, ThreatEvent::InvalidTurnstile)
                .await;
            return (StatusCode::FORBIDDEN, "Turnstile verification failed").into_response();
        }
    }

    let start = Instant::now();

    let work_dir = std::path::Path::new("/tmp/wasome-workspaces");
    let _ = tokio::fs::create_dir_all(work_dir).await;
    let temp_dir = match Builder::new().tempdir_in(work_dir) {
        Ok(d) => d,
        Err(_) => {
            return (StatusCode::INTERNAL_SERVER_ERROR, "Failed to create workspace")
                .into_response()
        }
    };

    for (path, content) in &payload.files {
        let file_path = temp_dir.path().join(path);
        if let Some(parent) = file_path.parent() {
            let _ = tokio::fs::create_dir_all(parent).await;
        }
        let _ = tokio::fs::write(&file_path, content).await;
    }

    let _ = std::process::Command::new("chmod")
        .args(["-R", "777", temp_dir.path().to_str().unwrap_or("")])
        .status();

    let runtime_arg = format!("--runtime={}", state.config.docker_runtime);
    let child = tokio::process::Command::new("docker")
        .args([
            "run",
            "--rm",
            &runtime_arg,
            "--network=none",
            "--read-only",
            "--user=1000:1000",
            "--cap-drop=ALL",
            "--cpus=0.50",
            "--memory=256M",
            "--tmpfs=/tmp:size=50M,exec,mode=1777",
            &format!("-v={}:/workspace", temp_dir.path().display()),
            &format!("-v={}:/opt/wasome:ro", state.config.host_wasome_dir),
            "wasome-compiler-sandbox:latest",
            "/opt/wasome/bin/waso",
            "build",
            "--target",
            "web",
            "--profile",
            "size",
            "/workspace",
        ])
        .output();

    let output = match tokio::time::timeout(Duration::from_secs(20), child).await {
        Ok(Ok(out)) => out,
        Ok(Err(_)) => {
            return (StatusCode::INTERNAL_SERVER_ERROR, "Docker execution failed").into_response()
        }
        Err(_) => return (StatusCode::REQUEST_TIMEOUT, "Execution timed out").into_response(),
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let logs = format!("{}{}", stdout, stderr);

    let mut wasm_base64 = None;
    let out_dir = temp_dir.path().join("out");
    if let Ok(mut entries) = tokio::fs::read_dir(&out_dir).await {
        while let Ok(Some(entry)) = entries.next_entry().await {
            if entry.path().extension().and_then(|e| e.to_str()) == Some("wasm") {
                if let Ok(wasm_bytes) = tokio::fs::read(entry.path()).await {
                    use base64::Engine;
                    wasm_base64 =
                        Some(base64::engine::general_purpose::STANDARD.encode(wasm_bytes));
                }
                break;
            }
        }
    }

    let duration = start.elapsed();
    let result_str = if wasm_base64.is_some() {
        "success"
    } else {
        "error"
    };
    let path_refs: Vec<&str> = file_paths.iter().map(|s| s.as_str()).collect();
    state
        .logger
        .log_compilation(
            &ip_hash,
            payload.files.len(),
            total_size,
            &path_refs,
            result_str,
            duration,
        )
        .await;

    let ts = chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
    state
        .logger
        .log_code(&ip_hash, &payload.files, &ts)
        .await;

    (
        StatusCode::OK,
        Json(CompileResponse {
            logs,
            wasm: wasm_base64,
        }),
    )
        .into_response()
}
