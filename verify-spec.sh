#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ADA_DIR="$ROOT_DIR/ada-backend"
COMPOSE_FILE="$ADA_DIR/docker-compose.yml"
RUN_SCRIPT="$ROOT_DIR/run-local.sh"
BASE_URL="http://localhost:9999"

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "Missing required command: $1" >&2; exit 1; }
}

echo_section() { echo; echo "==== $*"; }

# Simple HTTP helpers
http_get() { curl -fsS "$1"; }
http_post_json() { curl -fsS -H 'Content-Type: application/json' -d "$2" "$1"; }

# Parse total CPU and Memory limits from compose (best-effort, matches current file format)
parse_compose_limits() {
  local cpus_sum=0.0
  local mem_sum_mb=0
  # cpus lines like: cpus: "0.50"
  while read -r line; do
    local v
    v=$(echo "$line" | sed -E 's/.*cpus:\s*"?([0-9]+\.[0-9]+|[0-9]+)"?.*/\1/')
    python3 - "$v" <<'PY'
import sys
acc=float(sys.stdin.readline().strip())
val=float(sys.argv[1])
print(f"{acc+val:.2f}")
PY
  done < <(grep -E '^\s*cpus:' "$COMPOSE_FILE" | awk '{print $0}') | tail -n1 || true

  if [ -n "${PIPESTATUS[*]}" ]; then :; fi

  # memory lines like: memory: "100MB"
  mem_sum_mb=$(grep -E '^\s*memory:' "$COMPOSE_FILE" | sed -E 's/.*memory:\s*"?([0-9]+)MB"?.*/\1/' | awk '{s+=$1} END{print s+0}')
  echo "$cpus_sum" "$mem_sum_mb"
}

# Extract totalRequests sum across default/fallback from a summary JSON string
sum_requests_from_json() {
  local json="$1"
  local d=$(echo "$json" | sed -nE 's/.*"default"\s*:\s*\{[^}]*"totalRequests"\s*:\s*([0-9]+).*/\1/p' | head -n1)
  local f=$(echo "$json" | sed -nE 's/.*"fallback"\s*:\s*\{[^}]*"totalRequests"\s*:\s*([0-9]+).*/\1/p' | head -n1)
  d=${d:-0}; f=${f:-0}; echo $((d+f))
}

iso_now() { date -u +"%Y-%m-%dT%H:%M:%S.000Z"; }
iso_shift() { date -u -d "$1" +"%Y-%m-%dT%H:%M:%S.000Z"; }

assert() { if ! eval "$1"; then echo "Assertion failed: $1" >&2; exit 1; fi }

main() {
  require_cmd docker
  require_cmd curl
  require_cmd python3

  echo_section "Starting stack via run-local.sh"
  "$RUN_SCRIPT" up

  echo_section "Health check"
  http_get "$BASE_URL/health" >/dev/null

  echo_section "Compose limits check (CPU<=1.50, Mem<=350MB)"
  read -r cpus_total mem_total_mb < <(parse_compose_limits)
  echo "CPUs total: ${cpus_total:-unknown}; Memory total: ${mem_total_mb:-unknown}MB"
  # Use awk for float compare
  awk -v x="${cpus_total:-2.00}" 'BEGIN{ if (x+0 > 1.5+1e-9) exit 1 }' || { echo "CPU limits exceed 1.5" >&2; exit 1; }
  [ "${mem_total_mb:-1000}" -le 350 ] || { echo "Memory limits exceed 350MB" >&2; exit 1; }

  echo_section "Network check (payment-processor external)"
  docker network inspect payment-processor >/dev/null 2>&1 || { echo "Missing external network 'payment-processor'" >&2; exit 1; }

  echo_section "Services running"
  docker ps --format '{{.Names}}' | grep -E '^(rinha-nginx|rinha-backend1|rinha-backend2|rinha-postgres)$' | sort | uniq | wc -l | grep -q '^4$' || { echo "Expected 4 backend containers running" >&2; exit 1; }

  echo_section "Baseline summary"
  SUM0_JSON=$(http_get "$BASE_URL/payments-summary")
  SUM0=$(sum_requests_from_json "$SUM0_JSON")
  echo "Total requests baseline: $SUM0"

  echo_section "POST /payments (new unique, now)"
  CID_NOW=$(python3 - <<'PY'
import uuid; print(str(uuid.uuid4()))
PY
)
  NOW=$(iso_now)
  http_post_json "$BASE_URL/payments" "{\"correlationId\":\"$CID_NOW\",\"amount\":100.50,\"requestedAt\":\"$NOW\"}" >/dev/null
  sleep 0.5
  SUM1_JSON=$(http_get "$BASE_URL/payments-summary")
  SUM1=$(sum_requests_from_json "$SUM1_JSON")
  echo "Total requests after first payment: $SUM1"
  assert "[ $SUM1 -eq $((SUM0+1)) ]"

  echo_section "Idempotência (mesmo correlationId não muda total)"
  http_post_json "$BASE_URL/payments" "{\"correlationId\":\"$CID_NOW\",\"amount\":100.50,\"requestedAt\":\"$NOW\"}" >/dev/null
  sleep 0.3
  SUM2_JSON=$(http_get "$BASE_URL/payments-summary")
  SUM2=$(sum_requests_from_json "$SUM2_JSON")
  echo "Total requests after idempotent retry: $SUM2"
  assert "[ $SUM2 -eq $SUM1 ]"

  echo_section "Filtro de período (from/to)"
  FROM=$(iso_shift "-1 minute")
  TO=$(iso_shift "+1 minute")
  W0_JSON=$(http_get "$BASE_URL/payments-summary?from=$FROM&to=$TO")
  W0=$(sum_requests_from_json "$W0_JSON")
  echo "Window count baseline: $W0"

  CID_OLD=$(python3 - <<'PY'
import uuid; print(str(uuid.uuid4()))
PY
)
  OLD="2000-01-01T00:00:00.000Z"
  http_post_json "$BASE_URL/payments" "{\"correlationId\":\"$CID_OLD\",\"amount\":11.11,\"requestedAt\":\"$OLD\"}" >/dev/null
  sleep 0.3
  W1_JSON=$(http_get "$BASE_URL/payments-summary?from=$FROM&to=$TO")
  W1=$(sum_requests_from_json "$W1_JSON")
  echo "Window after old payment (should be unchanged): $W1"
  assert "[ $W1 -eq $W0 ]"

  CID_NOW2=$(python3 - <<'PY'
import uuid; print(str(uuid.uuid4()))
PY
)
  NOW2=$(iso_now)
  http_post_json "$BASE_URL/payments" "{\"correlationId\":\"$CID_NOW2\",\"amount\":22.22,\"requestedAt\":\"$NOW2\"}" >/dev/null
  sleep 0.3
  W2_JSON=$(http_get "$BASE_URL/payments-summary?from=$FROM&to=$TO")
  W2=$(sum_requests_from_json "$W2_JSON")
  echo "Window after now payment (should +1): $W2"
  assert "[ $W2 -eq $((W0+1)) ]"

  echo_section "Estrutura do sumário (chaves default e fallback)"
  echo "$SUM2_JSON" | grep -q '"default"' || { echo "Missing 'default' key" >&2; exit 1; }
  echo "$SUM2_JSON" | grep -q '"fallback"' || { echo "Missing 'fallback' key" >&2; exit 1; }

  echo_section "All checks passed ✅"
}

main "$@"
