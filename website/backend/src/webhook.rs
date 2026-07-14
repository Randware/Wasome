use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

use crate::config::Config;

const WEBHOOK_COOLDOWN: Duration = Duration::from_secs(300);

const COLOR_WARNING: u32 = 0xFFFF00;
const COLOR_CRITICAL: u32 = 0xFF8C00;
const COLOR_EMERGENCY: u32 = 0xFF0000;

#[derive(Clone)]
pub struct WebhookSender {
    config: Arc<Config>,
    last_sent: Arc<Mutex<HashMap<IpAddr, Instant>>>,
    client: reqwest::Client,
}

impl WebhookSender {
    pub fn new(config: Arc<Config>) -> Self {
        Self {
            config,
            last_sent: Arc::new(Mutex::new(HashMap::new())),
            client: reqwest::Client::new(),
        }
    }

    pub fn send_alert(
        &self,
        level: AlertLevel,
        ip: IpAddr,
        score: f64,
        event: &str,
        details: &str,
    ) {
        let url = match level {
            AlertLevel::Warning => self.config.discord_webhook_alert.clone(),
            AlertLevel::Critical => self.config.discord_webhook_critical.clone(),
            AlertLevel::Emergency => self.config.discord_webhook_emergency.clone(),
        };

        let url = match url {
            Some(u) => u,
            None => return,
        };

        let sender = self.clone();
        let event = event.to_owned();
        let details = details.to_owned();

        tokio::spawn(async move {
            {
                let mut map = sender.last_sent.lock().await;
                if let Some(last) = map.get(&ip) {
                    if last.elapsed() < WEBHOOK_COOLDOWN {
                        return;
                    }
                }
                map.insert(ip, Instant::now());
            }

            let masked_ip = mask_ip(ip);
            let color = match level {
                AlertLevel::Warning => COLOR_WARNING,
                AlertLevel::Critical => COLOR_CRITICAL,
                AlertLevel::Emergency => COLOR_EMERGENCY,
            };
            let level_str = match level {
                AlertLevel::Warning => "WARNING",
                AlertLevel::Critical => "CRITICAL",
                AlertLevel::Emergency => "EMERGENCY",
            };
            let title = match level {
                AlertLevel::Warning => "SECURITY ALERT",
                AlertLevel::Critical => "SECURITY CRITICAL",
                AlertLevel::Emergency => "SECURITY EMERGENCY",
            };

            let timestamp = chrono::Utc::now().to_rfc3339();

            let body = serde_json::json!({
                "embeds": [{
                    "title": title,
                    "color": color,
                    "fields": [
                        {"name": "Level", "value": level_str, "inline": true},
                        {"name": "IP", "value": masked_ip, "inline": true},
                        {"name": "Score", "value": format!("{:.0}/200", score), "inline": true},
                        {"name": "Event", "value": event},
                        {"name": "Details", "value": details}
                    ],
                    "timestamp": timestamp
                }]
            });

            let _ = sender.client.post(&url).json(&body).send().await;
        });
    }
}

#[derive(Clone, Copy)]
pub enum AlertLevel {
    Warning,
    Critical,
    Emergency,
}

fn mask_ip(ip: IpAddr) -> String {
    match ip {
        IpAddr::V4(v4) => {
            let octets = v4.octets();
            format!("{}.{}.x.x", octets[0], octets[1])
        }
        IpAddr::V6(v6) => {
            let segments = v6.segments();
            format!("{:x}:{:x}::x", segments[0], segments[1])
        }
    }
}
