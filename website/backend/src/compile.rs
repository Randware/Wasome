use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};

use axum::extract::ws::{Message, WebSocket};
use axum::extract::{ConnectInfo, Json, State, WebSocketUpgrade};
use axum::http::{HeaderMap, StatusCode};
use axum::response::IntoResponse;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::AppState;
use crate::sandbox::{self, CompileError, CompileJob};
use crate::security::ThreatEvent;
use crate::session;

const MAX_FILES: usize = 30;
const MAX_TOTAL_SIZE: usize = 50 * 1024;
const MAX_PATH_DEPTH: usize = 5;
// JSON escapes can use six bytes per byte of decoded source.
const MAX_WS_PAYLOAD_SIZE: usize = 384 * 1024;
const WS_PAYLOAD_TIMEOUT: Duration = Duration::from_secs(15);
const WS_SEND_TIMEOUT: Duration = Duration::from_secs(5);

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
        if path.contains('\\')
            || path.split('/').next() == Some("out")
            || path
                .split('/')
                .any(|part| part.is_empty() || matches!(part, "." | "..") || part.len() > 255)
        {
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
        .timeout(Duration::from_secs(5))
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

fn start_compile(state: &AppState, payload: &CompilePayload) -> Result<CompileJob, CompileError> {
    sandbox::start(
        Arc::clone(&state.compile_slots),
        &payload.files,
        state.config.host_wasome_dir.clone(),
        state.config.docker_runtime.clone(),
    )
}

fn compile_error_status(error: &CompileError) -> StatusCode {
    match error {
        CompileError::Busy => StatusCode::SERVICE_UNAVAILABLE,
        CompileError::TimedOut => StatusCode::REQUEST_TIMEOUT,
        CompileError::Failed => StatusCode::INTERNAL_SERVER_ERROR,
    }
}

async fn send_ws_json(ws: &mut WebSocket, msg: &WsMessage) -> bool {
    let Ok(text) = serde_json::to_string(msg) else {
        return false;
    };
    send_ws_message(ws, Message::Text(text.into())).await
}

async fn send_ws_message(ws: &mut WebSocket, message: Message) -> bool {
    matches!(
        tokio::time::timeout(WS_SEND_TIMEOUT, ws.send(message)).await,
        Ok(Ok(()))
    )
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

    ws.max_message_size(MAX_WS_PAYLOAD_SIZE)
        .max_frame_size(MAX_WS_PAYLOAD_SIZE)
        .on_upgrade(move |socket| handle_ws_connection(socket, ip, cookie_header, state))
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
        send_ws_message(&mut ws, Message::Close(None)).await;
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
        send_ws_message(&mut ws, Message::Close(None)).await;
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
        send_ws_message(&mut ws, Message::Close(None)).await;
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
        send_ws_message(&mut ws, Message::Close(None)).await;
        return;
    }

    let raw = match tokio::time::timeout(WS_PAYLOAD_TIMEOUT, ws.recv()).await {
        Ok(Some(Ok(Message::Text(text)))) => text,
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

    let result = match start_compile(&state, &payload) {
        Ok(CompileJob { mut logs, result }) => {
            while let Some(line) = logs.recv().await {
                if !send_ws_json(
                    &mut ws,
                    &WsMessage {
                        msg_type: "log",
                        message: None,
                        data: Some(line),
                        success: None,
                        cookie: None,
                        status: None,
                    },
                )
                .await
                {
                    return;
                }
            }
            result.await.unwrap_or(Err(CompileError::Failed))
        }
        Err(error) => Err(error),
    };

    let success = match result {
        Ok(Some(wasm)) => send_ws_message(&mut ws, Message::Binary(wasm.into())).await,
        Ok(None) => false,
        Err(error) => {
            send_ws_json(
                &mut ws,
                &WsMessage {
                    msg_type: "error",
                    message: Some(error.message().into()),
                    data: None,
                    success: None,
                    cookie: None,
                    status: Some(compile_error_status(&error).as_u16()),
                },
            )
            .await;
            false
        }
    };

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

    let ts = jiff::Zoned::now().strftime("%Y-%m-%d %H:%M:%S").to_string();
    state.logger.log_code(&ip_hash, &payload.files, &ts).await;

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

    let CompileJob { mut logs, result } = match start_compile(&state, &payload) {
        Ok(job) => job,
        Err(error) => return (compile_error_status(&error), error.message()).into_response(),
    };
    let mut compiler_logs = String::new();
    while let Some(line) = logs.recv().await {
        compiler_logs.push_str(&line);
        compiler_logs.push('\n');
    }

    let wasm_base64 = match result.await.unwrap_or(Err(CompileError::Failed)) {
        Ok(wasm) => wasm.map(|wasm| {
            use base64::Engine;
            base64::engine::general_purpose::STANDARD.encode(wasm)
        }),
        Err(error) => return (compile_error_status(&error), error.message()).into_response(),
    };

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

    let ts = jiff::Zoned::now().strftime("%Y-%m-%d %H:%M:%S").to_string();
    state.logger.log_code(&ip_hash, &payload.files, &ts).await;

    (
        StatusCode::OK,
        Json(CompileResponse {
            logs: compiler_logs,
            wasm: wasm_base64,
        }),
    )
        .into_response()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_regular_input_paths_and_reserves_compiler_output() {
        assert!(validates_path("src/main.waso"));
        for path in ["out/result.wasm", "", "./src/main.waso", "src//main.waso"] {
            assert!(!validates_path(path));
        }
    }

    fn validates_path(path: &str) -> bool {
        validate_payload(&CompilePayload {
            files: HashMap::from([(path.to_owned(), "source".to_owned())]),
            entry: path.to_owned(),
            turnstile_token: None,
        })
        .is_ok()
    }
}
