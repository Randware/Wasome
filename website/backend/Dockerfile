FROM rust:bookworm AS builder
WORKDIR /app
COPY . .
RUN cargo build --release

FROM debian:bookworm-slim
# Install dependencies for docker, and curl/tar/wget for install.sh
RUN apt-get update && apt-get install -y docker.io curl wget tar ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/target/release/backend /usr/local/bin/backend

# Set the compiler installation home to the named volume path
ENV WASOME_HOME=/opt/wasome

CMD ["backend"]
