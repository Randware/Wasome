use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;

const SESSION_TTL: Duration = Duration::from_secs(900);
const PRUNE_INTERVAL: Duration = Duration::from_secs(60);

struct Session {
    created: Instant,
    ip: IpAddr,
}

pub struct SessionStore {
    sessions: RwLock<HashMap<String, Session>>,
}

impl SessionStore {
    pub fn new() -> Self {
        Self {
            sessions: RwLock::new(HashMap::new()),
        }
    }

    pub async fn create_session(&self, ip: IpAddr) -> String {
        let id = uuid::Uuid::new_v4().to_string();
        let mut map = self.sessions.write().await;
        map.insert(
            id.clone(),
            Session {
                created: Instant::now(),
                ip,
            },
        );
        id
    }

    pub async fn validate_session(&self, session_id: &str, ip: IpAddr) -> bool {
        let map = self.sessions.read().await;
        match map.get(session_id) {
            Some(s) => s.ip == ip && s.created.elapsed() < SESSION_TTL,
            None => false,
        }
    }

    async fn prune_expired(&self) {
        let mut map = self.sessions.write().await;
        map.retain(|_, s| s.created.elapsed() < SESSION_TTL);
    }

    pub fn start_prune_task(self: &Arc<Self>) {
        let store = Arc::clone(self);
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(PRUNE_INTERVAL).await;
                store.prune_expired().await;
            }
        });
    }
}

pub fn session_cookie(session_id: &str) -> String {
    format!(
        "wasome_session={}; HttpOnly; SameSite=Strict; Max-Age=900; Path=/",
        session_id
    )
}

pub fn extract_session_id(cookie_header: &str) -> Option<&str> {
    for part in cookie_header.split(';') {
        let trimmed = part.trim();
        if let Some(val) = trimmed.strip_prefix("wasome_session=") {
            let val = val.trim();
            if !val.is_empty() {
                return Some(val);
            }
        }
    }
    None
}
