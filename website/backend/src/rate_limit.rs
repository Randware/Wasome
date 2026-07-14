use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

use crate::security::ThreatEvent;

struct RateBucket {
    tokens: u32,
    last_refill: Instant,
}

pub struct RateLimiter {
    buckets: Mutex<HashMap<IpAddr, RateBucket>>,
    config: Arc<crate::config::Config>,
    security: Arc<crate::security::SecurityEngine>,
}

impl RateLimiter {
    pub fn new(
        config: Arc<crate::config::Config>,
        security: Arc<crate::security::SecurityEngine>,
    ) -> Self {
        Self {
            buckets: Mutex::new(HashMap::new()),
            config,
            security,
        }
    }

    pub async fn check_rate(&self, ip: IpAddr) -> bool {
        let max_tokens = self.config.rate_limit_max_tokens;
        let refill_interval = Duration::from_secs(self.config.rate_limit_refill_period);

        let mut map = self.buckets.lock().await;
        let bucket = map.entry(ip).or_insert(RateBucket {
            tokens: max_tokens,
            last_refill: Instant::now(),
        });

        let elapsed = bucket.last_refill.elapsed();
        if elapsed >= refill_interval {
            bucket.tokens = max_tokens;
            bucket.last_refill = Instant::now();
        }

        if bucket.tokens > 0 {
            bucket.tokens -= 1;
            true
        } else {
            self.security
                .record_event(ip, ThreatEvent::RateLimitHit)
                .await;
            false
        }
    }
}
