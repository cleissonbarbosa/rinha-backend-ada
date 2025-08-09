#!/usr/bin/env bash
set -euo pipefail

# Simple local runner for the Ada backend + Payment Processors
# - Builds Ada binary (with Alire)
# - Builds Docker image for backend
# - Starts Payment Processors (official compose)
# - Starts backend compose (nginx + 2x backends + postgres)
# Usage:
#   ./run-local.sh           # build and start everything
#   ./run-local.sh down      # stop everything
#   ./run-local.sh rebuild   # rebuild image and restart backend

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ADA_DIR="$ROOT_DIR/ada-backend"
PP_COMPOSE="$ROOT_DIR/rinha-de-backend-2025/payment-processor/docker-compose.yml"
BACKEND_COMPOSE="$ADA_DIR/docker-compose.yml"
IMAGE_TAG="rinha-ada-backend:latest"

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "Error: '$1' is required but not installed." >&2; exit 1; }
}

# Pick docker compose command (plugin or legacy)
compose() {
  if docker compose version >/dev/null 2>&1; then
    docker compose "$@"
  else
    docker-compose "$@"
  fi
}

wait_http() {
  local url="$1"; shift
  local max_tries="${1:-60}"; shift || true
  local delay="${1:-1}";
  echo "Waiting for $url ..."
  for ((i=1;i<=max_tries;i++)); do
    if curl -fsS "$url" > /dev/null; then
      echo "Service is up: $url"; return 0
    fi
    sleep "$delay"
  done
  echo "Timeout waiting for $url" >&2
  return 1
}

build_backend() {
  if command -v alr >/dev/null 2>&1; then
    echo "==> Building Ada backend (release) with Alire"
    (cd "$ADA_DIR" && alr build --release)
  else
    echo "==> 'alr' not found; will use existing binary if present"
  fi
  if [ ! -x "$ADA_DIR/bin/rinha" ]; then
    echo "Error: missing binary $ADA_DIR/bin/rinha. Install Alire (alr) or provide a prebuilt binary." >&2
    exit 1
  fi
  echo "==> Building Docker image: $IMAGE_TAG"
  (cd "$ADA_DIR" && docker build -t "$IMAGE_TAG" .)
}

up_processors() {
  echo "==> Starting Payment Processors"
  compose -f "$PP_COMPOSE" up -d
}

up_backend() {
  echo "==> Starting backend stack"
  (cd "$ADA_DIR" && compose up -d)
}

down_all() {
  echo "==> Stopping backend stack"
  (cd "$ADA_DIR" && compose down || true)
  echo "==> Stopping Payment Processors"
  compose -f "$PP_COMPOSE" down || true
}

rebuild_backend() {
  echo "==> Rebuilding backend image and restarting services"
  build_backend
  (cd "$ADA_DIR" && compose up -d)
}

main() {
  require_cmd docker
  require_cmd curl
  # 'alr' optional if binary already exists

  case "${1:-up}" in
    down)
      down_all
      ;;
    rebuild)
      rebuild_backend
      ;;
    up|start)
      build_backend
      up_processors
      up_backend
      # Wait for health endpoint
      wait_http "http://localhost:9999/health" 120 1
      echo "==> All services are up. Try: curl http://localhost:9999/health"
      ;;
    *)
      echo "Usage: $0 [up|down|rebuild]" >&2
      exit 1
      ;;
  esac
}

main "$@"
