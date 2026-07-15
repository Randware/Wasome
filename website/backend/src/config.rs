use std::sync::atomic::AtomicBool;

pub struct Config {
    pub turnstile_secret_key: Option<String>,
    pub discord_webhook_alert: Option<String>,
    pub discord_webhook_critical: Option<String>,
    pub discord_webhook_emergency: Option<String>,
    pub compilation_enabled: AtomicBool,
    pub maintenance_mode: AtomicBool,
    pub admin_secret: Option<String>,
    pub host_wasome_dir: String,
    pub bind_addr: String,
    pub docker_runtime: String,
    pub rate_limit_max_tokens: u32,
    pub rate_limit_refill_period: u64,
}

impl Config {
    pub fn from_env() -> Self {
        Self {
            turnstile_secret_key: non_empty_env("TURNSTILE_SECRET_KEY"),
            discord_webhook_alert: non_empty_env("DISCORD_WEBHOOK_ALERT"),
            discord_webhook_critical: non_empty_env("DISCORD_WEBHOOK_CRITICAL"),
            discord_webhook_emergency: non_empty_env("DISCORD_WEBHOOK_EMERGENCY"),
            compilation_enabled: AtomicBool::new(
                std::env::var("COMPILATION_ENABLED")
                    .unwrap_or_else(|_| "true".into())
                    .parse()
                    .unwrap_or(true),
            ),
            maintenance_mode: AtomicBool::new(
                std::env::var("MAINTENANCE_MODE")
                    .unwrap_or_else(|_| "false".into())
                    .parse()
                    .unwrap_or(false),
            ),
            admin_secret: non_empty_env("ADMIN_SECRET"),
            host_wasome_dir: std::env::var("HOST_WASOME_DIR")
                .unwrap_or_else(|_| "/root/.wasome".into()),
            bind_addr: std::env::var("BIND_ADDR")
                .unwrap_or_else(|_| "0.0.0.0:3000".into()),
            docker_runtime: std::env::var("DOCKER_RUNTIME")
                .unwrap_or_else(|_| "runsc".into()),
            rate_limit_max_tokens: std::env::var("RATE_LIMIT_MAX_TOKENS")
                .unwrap_or_else(|_| "10".into())
                .parse()
                .unwrap_or(10),
            rate_limit_refill_period: std::env::var("RATE_LIMIT_REFILL_PERIOD")
                .unwrap_or_else(|_| "900".into())
                .parse()
                .unwrap_or(900),
        }
    }
}

fn non_empty_env(key: &str) -> Option<String> {
    std::env::var(key).ok().filter(|v| !v.is_empty())
}
