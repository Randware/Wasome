use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

use crate::logging::Logger;
use crate::webhook::{AlertLevel, WebhookSender};

const DECAY_RATE: f64 = 1.0;
const PRUNE_INTERVAL: Duration = Duration::from_secs(300);
const BAN_1H: Duration = Duration::from_secs(3600);
const BAN_24H: Duration = Duration::from_secs(86400);

#[derive(Clone, Copy, Debug)]
pub enum ThreatEvent {
    RateLimitHit,
    InvalidPath,
    OversizedPayload,
    InvalidTurnstile,
    RepeatedErrors,
    NullBytePayload,
}

impl ThreatEvent {
    fn points(self) -> f64 {
        match self {
            Self::RateLimitHit => 5.0,
            Self::InvalidPath => 20.0,
            Self::OversizedPayload => 10.0,
            Self::InvalidTurnstile => 15.0,
            Self::RepeatedErrors => 10.0,
            Self::NullBytePayload => 25.0,
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::RateLimitHit => "Rate limit exceeded",
            Self::InvalidPath => "Path traversal attempt",
            Self::OversizedPayload => "Oversized payload",
            Self::InvalidTurnstile => "Invalid Turnstile token",
            Self::RepeatedErrors => "Repeated errors",
            Self::NullBytePayload => "Null byte in payload",
        }
    }
}

#[derive(Debug, Clone)]
pub enum SecurityAction {
    Banned,
}

struct ThreatState {
    score: f64,
    last_decay: Instant,
    events: Vec<(Instant, ThreatEvent)>,
    banned_until: Option<Instant>,
}

impl ThreatState {
    fn new() -> Self {
        Self {
            score: 0.0,
            last_decay: Instant::now(),
            events: Vec::new(),
            banned_until: None,
        }
    }

    fn apply_decay(&mut self) {
        let elapsed = self.last_decay.elapsed().as_secs_f64() / 60.0;
        self.score = (self.score - elapsed * DECAY_RATE).max(0.0);
        self.last_decay = Instant::now();
    }
}

pub struct SecurityEngine {
    states: Mutex<HashMap<IpAddr, ThreatState>>,
    logger: Arc<Logger>,
    webhook: WebhookSender,
}

impl SecurityEngine {
    pub fn new(logger: Arc<Logger>, webhook: WebhookSender) -> Self {
        Self {
            states: Mutex::new(HashMap::new()),
            logger,
            webhook,
        }
    }

    pub async fn check_ip(&self, ip: IpAddr) -> Result<(), SecurityAction> {
        let mut map = self.states.lock().await;
        if let Some(state) = map.get_mut(&ip) {
            if let Some(until) = state.banned_until {
                if Instant::now() < until {
                    return Err(SecurityAction::Banned);
                }
                state.banned_until = None;
            }
            state.apply_decay();
        }
        Ok(())
    }

    pub async fn record_event(&self, ip: IpAddr, event: ThreatEvent) {
        let mut map = self.states.lock().await;
        let state = map.entry(ip).or_insert_with(ThreatState::new);
        state.apply_decay();
        state.score += event.points();
        state.events.push((Instant::now(), event));

        let score = state.score;
        let label = event.label();

        if score >= 200.0 {
            state.banned_until = Some(Instant::now() + BAN_24H);
            drop(map);
            self.logger
                .security(
                    "security",
                    &format!(
                        "EMERGENCY ip={} score={:.0} event={} ban=24h",
                        ip, score, label
                    ),
                )
                .await;
            self.webhook.send_alert(
                AlertLevel::Emergency,
                ip,
                score,
                label,
                &format!("IP banned for 24 hours. Score: {:.0}", score),
            );
        } else if score >= 100.0 {
            state.banned_until = Some(Instant::now() + BAN_1H);
            drop(map);
            self.logger
                .security(
                    "security",
                    &format!(
                        "CRITICAL ip={} score={:.0} event={} ban=1h",
                        ip, score, label
                    ),
                )
                .await;
            self.webhook.send_alert(
                AlertLevel::Critical,
                ip,
                score,
                label,
                &format!("IP banned for 1 hour. Score: {:.0}", score),
            );
        } else if score >= 50.0 {
            drop(map);
            self.logger
                .security(
                    "security",
                    &format!(
                        "WARNING ip={} score={:.0} event={}",
                        ip, score, label
                    ),
                )
                .await;
            self.webhook.send_alert(
                AlertLevel::Warning,
                ip,
                score,
                label,
                &format!("Suspicious activity detected. Score: {:.0}", score),
            );
        }
    }

    pub fn start_prune_task(self: &Arc<Self>) {
        let engine = Arc::clone(self);
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(PRUNE_INTERVAL).await;
                let mut map = engine.states.lock().await;
                map.retain(|_, state| {
                    state.apply_decay();
                    state.score > 0.0 || state.banned_until.is_some()
                });
            }
        });
    }
}
