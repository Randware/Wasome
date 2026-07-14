mod compile;
mod config;
mod logging;
mod rate_limit;
mod security;
mod session;
mod webhook;

use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use axum::extract::{Json, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::IntoResponse;
use axum::routing::{get, post};
use axum::Router;
use serde::{Deserialize, Serialize};

use config::Config;
use logging::Logger;
use rate_limit::RateLimiter;
use security::SecurityEngine;
use session::SessionStore;
use webhook::WebhookSender;

pub struct AppState {
    pub config: Arc<Config>,
    pub sessions: Arc<SessionStore>,
    pub security: Arc<SecurityEngine>,
    pub rate_limiter: Arc<RateLimiter>,
    pub logger: Arc<Logger>,
}

#[derive(Serialize)]
struct StatusResponse {
    compilation_enabled: bool,
    maintenance_mode: bool,
}

#[derive(Deserialize)]
struct AdminToggle {
    #[serde(default)]
    compilation_enabled: Option<bool>,
    #[serde(default)]
    maintenance_mode: Option<bool>,
}

async fn status_handler(State(state): State<Arc<AppState>>) -> impl IntoResponse {
    Json(StatusResponse {
        compilation_enabled: state.config.compilation_enabled.load(Ordering::Relaxed),
        maintenance_mode: state.config.maintenance_mode.load(Ordering::Relaxed),
    })
}

async fn admin_toggle_handler(
    headers: HeaderMap,
    State(state): State<Arc<AppState>>,
    Json(body): Json<AdminToggle>,
) -> impl IntoResponse {
    let provided_secret = headers
        .get("x-admin-secret")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    match &state.config.admin_secret {
        Some(secret) if secret == provided_secret => {}
        Some(_) => return (StatusCode::FORBIDDEN, "Invalid admin secret").into_response(),
        None => return (StatusCode::FORBIDDEN, "Admin endpoint not configured").into_response(),
    }

    if let Some(val) = body.compilation_enabled {
        state.config.compilation_enabled.store(val, Ordering::Relaxed);
        state
            .logger
            .info("admin", &format!("compilation_enabled set to {}", val))
            .await;
    }
    if let Some(val) = body.maintenance_mode {
        state.config.maintenance_mode.store(val, Ordering::Relaxed);
        state
            .logger
            .info("admin", &format!("maintenance_mode set to {}", val))
            .await;
    }

    Json(StatusResponse {
        compilation_enabled: state.config.compilation_enabled.load(Ordering::Relaxed),
        maintenance_mode: state.config.maintenance_mode.load(Ordering::Relaxed),
    })
    .into_response()
}

async fn update_compiler_task(logger: Arc<Logger>) {
    let install_script_path = "/app/install.sh";
    loop {
        logger.info("updater", "Updating Wasome compiler...").await;
        let status = tokio::process::Command::new("sh")
            .arg(install_script_path)
            .arg("-y")
            .status()
            .await;

        match status {
            Ok(st) if st.success() => {
                logger.info("updater", "Compiler updated successfully").await;
            }
            Ok(_) => {
                logger.error("updater", "Compiler update failed").await;
            }
            Err(e) => {
                logger
                    .error("updater", &format!("Failed to run update script: {}", e))
                    .await;
            }
        }

        tokio::time::sleep(Duration::from_secs(2 * 3600)).await;
    }
}

#[tokio::main]
async fn main() {
    let config = Arc::new(Config::from_env());
    let logger = Logger::init();

    // Run official install script inside the container at startup to install/bootstrap compiler
    let install_script_path = "/app/install.sh";
    if std::path::Path::new(install_script_path).exists() {
        logger.info("main", "Running compiler installation script...").await;
        let status = tokio::process::Command::new("sh")
            .arg(install_script_path)
            .arg("-y")
            .status()
            .await;
        match status {
            Ok(st) if st.success() => {
                logger.info("main", "Compiler installed/updated successfully at startup").await;
            }
            Ok(_) => {
                logger.error("main", "Compiler installation script failed at startup").await;
            }
            Err(e) => {
                logger.error("main", &format!("Failed to execute compiler installation script: {}", e)).await;
            }
        }
    } else {
        logger.warn("main", "Compiler installation script /app/install.sh not found").await;
    }

    let webhook_sender = WebhookSender::new(Arc::clone(&config));
    let security = Arc::new(SecurityEngine::new(Arc::clone(&logger), webhook_sender));
    let sessions = Arc::new(SessionStore::new());
    let rate_limiter = Arc::new(RateLimiter::new(Arc::clone(&config), Arc::clone(&security)));

    sessions.start_prune_task();
    security.start_prune_task();

    let bind_addr = config.bind_addr.clone();

    let state = Arc::new(AppState {
        config,
        sessions,
        security,
        rate_limiter,
        logger: Arc::clone(&logger),
    });

    tokio::spawn(update_compiler_task(Arc::clone(&logger)));

    let app = Router::new()
        .route("/ws/compile", get(compile::ws_compile_handler))
        .route("/api/compile", post(compile::http_compile_handler))
        .route("/api/status", get(status_handler))
        .route("/api/admin/toggle", post(admin_toggle_handler))
        .layer(axum::extract::DefaultBodyLimit::max(100 * 1024))
        .with_state(state);

    logger
        .info("main", &format!("Backend listening on {}", bind_addr))
        .await;

    let listener = tokio::net::TcpListener::bind(&bind_addr).await.unwrap();

    let shutdown_logger = Arc::clone(&logger);
    let shutdown_signal = async move {
        let ctrl_c = tokio::signal::ctrl_c();
        let mut sigterm =
            tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate()).unwrap();

        tokio::select! {
            _ = ctrl_c => {},
            _ = sigterm.recv() => {},
        }

        shutdown_logger
            .info("main", "Shutdown signal received, flushing logs...")
            .await;
        shutdown_logger.shutdown().await;
    };

    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<std::net::SocketAddr>(),
    )
    .with_graceful_shutdown(shutdown_signal)
    .await
    .unwrap();
}
