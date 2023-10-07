# FROM rust:1.70 as builder
# WORKDIR /usr/src/sisyphus
# COPY . .
# RUN cargo install --path .

# FROM debian:bullseye-slim
# RUN apt-get update && rm -rf /var/lib/apt/lists/*
# COPY --from=builder /usr/local/cargo/bin/sisyphus /usr/local/bin/sisyphus
# CMD ["sisyphus", "/usr/src/sisyphus/config.toml"]

FROM rust:1.70

WORKDIR /usr/src/sisyphus
COPY . .

RUN cargo install --path .

CMD ["sisyphus", "/usr/src/sisyphus/config.toml"]
