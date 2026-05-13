#!/usr/bin/env bash
# =============================================================================
# 05-package.sh - Package the offline distribution
# Architecture: Nginx (3000) → static frontend/dist/ files + /api proxy → backend (3002)
#       PostgreSQL (5433)
# Supports macOS / Linux / WSL
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"

init_paths

OS="$(detect_os)"
ARCH="$(detect_arch)"
PLATFORM="$(detect_platform)"

BACKEND_BIN="${DEPS_DIR}/arkham-api"
FRONTEND_SRC="${DEPS_DIR}/frontend"
SETUP_SQL="${PROJECT_ROOT}/setup.sql"
PG_BIN_DIR="${DEPS_DIR}/postgres/bin"
PG_LIB_DIR="${DEPS_DIR}/postgres/lib"
NGINX_BIN="${DEPS_DIR}/nginx/bin/nginx"

PKG_NAME="ArkhamHorror-${PLATFORM}"
PKG_DIR="${_DIST_DIR}/${PKG_NAME}"
PKG_ARCHIVE="${_DIST_DIR}/${PKG_NAME}.tar.gz"

# ═════════════════════════════════════════════════════════════════════════════
# Generate start.sh (launcher script) — supports macOS / Linux / WSL
# ═════════════════════════════════════════════════════════════════════════════

generate_launch_script() {
    substep "Generating start.sh ..."

    cat > "${PKG_DIR}/start.sh" << 'LAUNCHSCRIPT'
#!/usr/bin/env bash
set -euo pipefail

# ── Root check: PostgreSQL must not run as root ───────────────────────────────
# Windows: start.bat already ensures a non-root user through -u arkham
# macOS/Linux: users normally run as a regular user; this is only a safety fallback
if [ "$(id -u)" = "0" ]; then
    echo "[ARKHAM] Error: running as root is not allowed." >&2
    echo "         PostgreSQL refuses to run initdb/pg_ctl as root." >&2
    if grep -qi microsoft /proc/version 2>/dev/null; then
        echo "         Please launch via start.bat (it runs automatically as user arkham)," >&2
        echo "         or specify the user manually: su arkham -c 'bash start.sh'" >&2
    else
        echo "         Please run as a regular user: bash start.sh" >&2
    fi
    die 1001 "Running as root is not allowed"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA_DIR="$SCRIPT_DIR/data"
NGINX_PID="$DATA_DIR/nginx.pid"
NGINX_LOG_DIR="$DATA_DIR"
API_PID_FILE="$DATA_DIR/arkham-api.pid"
PG_LOG="$DATA_DIR/pg.log"

NGINX_PORT="${ARKHAM_PORT:-3000}"
API_PORT="${ARKHAM_API_PORT:-3002}"
PG_PORT="${ARKHAM_PG_PORT:-5433}"
PG_USER="arkham_user"
PG_DB="arkham-horror-backend"

# Runtime pgdata directory: keep it under the OS user data directory to avoid WSL/NTFS permission issues
# macOS: ~/Library/Application Support/ArkhamHorror/pgdata/
# Linux/WSL: ~/.local/share/ArkhamHorror/pgdata/
case "$(uname -s)" in
    Darwin) PGDATA_OS="$HOME/Library/Application Support/ArkhamHorror/pgdata" ;;
    Linux)  PGDATA_OS="$HOME/.local/share/ArkhamHorror/pgdata" ;;
esac
PGDATA_OS_PARENT="$(dirname "$PGDATA_OS")"
PGDATA_LOCAL="$DATA_DIR/pgdata"        # Backed up here on shutdown for easier manual copying
VERSION_FILE="$PGDATA_OS_PARENT/pgdata_version"
VERSION_FILE_LOCAL="$DATA_DIR/pgdata_version"
PG_DATA="$PGDATA_OS"                    # Actual pgdata path used at runtime

# Unix domain socket directory:
# - macOS: distribution paths can be long; a Unix socket path longer than 103 bytes can make PostgreSQL fail to start
# - WSL/NTFS (DrvFS): Unix sockets are not supported, so the socket must live on the native Linux filesystem
# Detection rule: paths starting with /mnt/ are usually Windows drive mounts (DrvFS)
if [ "$(uname -s)" = "Darwin" ]; then
    PG_SOCKET_DIR="/tmp/arkham-pg-${PG_PORT}"
    mkdir -p "$PG_SOCKET_DIR" 2>/dev/null || PG_SOCKET_DIR="/tmp"
elif [[ "$DATA_DIR" == /mnt/* ]] && grep -qi microsoft /proc/version 2>/dev/null; then
    PG_SOCKET_DIR="/tmp/arkham-pg-${PG_PORT}"
    mkdir -p "$PG_SOCKET_DIR" 2>/dev/null || PG_SOCKET_DIR="/tmp"
else
    PG_SOCKET_DIR="$DATA_DIR"
fi

GREEN=$'\033[0;32m'
YELLOW=$'\033[0;33m'
RED=$'\033[0;31m'
CYAN=$'\033[0;36m'
BOLD=$'\033[1m'
RESET=$'\033[0m'

info()  { printf '%s[ARKHAM]%s %s\n' "$GREEN" "$RESET" "$*"; }
warn()  { printf '%s[ARKHAM]%s %s\n' "$YELLOW" "$RESET" "$*" >&2; }
die() {
    local code="$1" msg="$2" log="${3:-}"
    printf '%s[ARKHAM]%s [Error Code %s] %s\n' "$RED" "$RESET" "$code" "$msg" >&2
    if [ -n "$log" ] && [ -f "$log" ] && [ -s "$log" ]; then
        warn "Last 20 log lines:"
        tail -20 "$log" 2>/dev/null | while IFS= read -r line; do
            echo "  $line" >&2
        done
        echo "Full log: $log" >&2
    fi
    exit "$code"
}

# Open a URL in the default browser (cross-platform, silently ignore failures)
open_browser() {
    local url="$1"
    if grep -qi microsoft /proc/version 2>/dev/null; then
        # WSL: use explorer.exe to open the Windows default browser (returns immediately)
        explorer.exe "$url" >/dev/null 2>&1 || true
    elif [ "$(uname -s)" = "Darwin" ]; then
        open "$url" >/dev/null 2>&1 || true
    elif command -v xdg-open >/dev/null 2>&1; then
        xdg-open "$url" >/dev/null 2>&1 || true
    fi
}

# If startup exits abnormally, automatically clean up all started services
_CLEANUP_ON_EXIT=0
_cleanup_on_exit() {
    if [ "$_CLEANUP_ON_EXIT" = "1" ]; then
        _CLEANUP_ON_EXIT=0          # Prevent re-triggering if do_stop itself fails
        warn "Startup did not finish; cleaning up any started services ..."
        do_stop 2>/dev/null || true
    fi
}
trap '_cleanup_on_exit' EXIT

pg_bin()  { echo "$SCRIPT_DIR/pgsql/bin"; }
pg_ctl()  { "$(pg_bin)/pg_ctl" "$@"; }
pg_ready(){ { "$(pg_bin)/pg_isready" -h 127.0.0.1 -p "$PG_PORT" -q; } 2>/dev/null; }
psql_cmd(){ "$(pg_bin)/psql" -h 127.0.0.1 -p "$PG_PORT" -U "$PG_USER" "$@"; }

# On WSL/NTFS (DrvFS), mkdir can fail because of timing issues, so retry
ensure_dir() {
    local target="$1" i=0
    while [ ! -d "$target" ] && [ $i -lt 5 ]; do
        mkdir -p "$target" 2>&1 || true
        [ -d "$target" ] && return 0
        i=$((i + 1))
        warn "Failed to create directory $target (retry $i/5) ..."
        sleep 1
    done
    [ -d "$target" ] || die 1008 "Unable to create directory: $target"
}

configure_runtime_env() {
    export PATH="$SCRIPT_DIR/bin:$(pg_bin):$PATH"
    case "$(uname -s)" in
        Linux)  export LD_LIBRARY_PATH="$SCRIPT_DIR/lib:$SCRIPT_DIR/pgsql/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ;;
        Darwin) export DYLD_LIBRARY_PATH="$SCRIPT_DIR/lib:$SCRIPT_DIR/pgsql/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}" ;;
    esac
}

# ── macOS Gatekeeper: ad-hoc sign all binaries and clear quarantine ───────────
# Order: 1) make writable → 2) clear quarantine with xattr → 3) remove old signatures and re-sign ad-hoc → 4) clear xattr again
# Homebrew-bundled dylib source files may be read-only (0444), so chmod u+w is required first.
ensure_macos_signing() {
    if [ "$(uname -s)" != "Darwin" ]; then
        return 0
    fi
    if ! command -v codesign >/dev/null 2>&1; then
        warn "codesign not found; skipping signing (manual approval may be needed on macOS)"
        return 0
    fi

    # 1) Ensure all files that need signing are writable (Homebrew dylibs may be read-only)
    for dir in bin pgsql/bin pgsql/lib lib; do
        local d="${SCRIPT_DIR}/${dir}"
        [ -d "$d" ] && chmod -R u+w "$d" 2>/dev/null || true
    done

    # 2) Clear all quarantine attributes first
    if ! xattr -rd com.apple.quarantine "$SCRIPT_DIR"; then
        warn "Failed to clear quarantine attributes with xattr (safe to ignore; continuing signing)"
    fi

    info "Applying ad-hoc signatures to all binaries and dynamic libraries ..."
    local cs_ok=0 cs_fail=0
    for dir in bin pgsql/bin pgsql/lib lib; do
        local d="${SCRIPT_DIR}/${dir}"
        [ ! -d "$d" ] && continue
        # lib/ contains Homebrew-bundled dylibs; remove signatures twice to ensure the original signature is fully stripped
        if [ "$dir" = "lib" ]; then
            find "$d" -name '*.dylib' -exec codesign --remove-signature {} \; 2>/dev/null || true
            find "$d" -name '*.dylib' -exec codesign --remove-signature {} \; 2>/dev/null || true
        fi
        while IFS= read -r -d '' f; do
            codesign --remove-signature "$f" 2>/dev/null || true
            if codesign --force --sign - "$f"; then cs_ok=$((cs_ok + 1))
            else cs_fail=$((cs_fail + 1)); warn "Signing failed: $f"
            fi
        done < <(find "$d" -type f \( -perm -a=x -o -name '*.so' -o -name '*.dylib' \) -print0 2>/dev/null)
    done
    info "  Signing complete: ${cs_ok} succeeded"$([ $cs_fail -gt 0 ] && echo ", ${cs_fail} failed")

    # 3) Clear quarantine attributes again after signing (codesign may re-trigger them)
    xattr -rd com.apple.quarantine "$SCRIPT_DIR" 2>/dev/null || true
}

is_pg_running()   { pg_ready; }
is_api_running()  {
    if [ -f "$API_PID_FILE" ]; then
        local p; p="$(cat "$API_PID_FILE" 2>/dev/null)"
        [ -n "$p" ] && [ -d "/proc/$p" ] && return 0
    fi
    # Fallback: probe the port directly (PID files may be inaccessible across users)
    (echo >/dev/tcp/127.0.0.1/$API_PORT) 2>/dev/null
}
is_nginx_running() {
    if [ -f "$NGINX_PID" ]; then
        local p; p="$(cat "$NGINX_PID" 2>/dev/null)"
        [ -n "$p" ] && [ -d "/proc/$p" ] && return 0
    fi
    (echo >/dev/tcp/127.0.0.1/$NGINX_PORT) 2>/dev/null
}

# Check whether all three services are healthy
is_all_services_running() { is_pg_running && is_api_running && is_nginx_running; }

# Get the PID for each running service (used for status output)
get_pg_pid() {
    # Prefer the PID file when the service was started by the same user
    if [ -f "$PG_DATA/postmaster.pid" ]; then
        local pid_val; pid_val="$(head -1 "$PG_DATA/postmaster.pid" 2>/dev/null)"
        [ -n "$pid_val" ] && echo "$pid_val" && return
    fi
    # Cross-user case: read the first line of postmaster.pid via SQL (PostgreSQL can read its own files internally)
    local pid_val; pid_val="$(psql_cmd -d postgres -tAc \
        "SELECT split_part(pg_read_file('postmaster.pid'), E'\n', 1);" 2>/dev/null | tr -d ' ')"
    if [ -n "$pid_val" ] && [ "$pid_val" != "" ]; then
        echo "$pid_val" && return
    fi
    # Final fallback: try lsof / ss
    local p=""
    p="$(lsof -ti ":$PG_PORT" -sTCP:LISTEN 2>/dev/null | head -1)"
    [ -n "$p" ] && echo "$p" && return
    p="$(ss -tlnp 2>/dev/null | grep ":${PG_PORT} " | grep -oP 'pid=\K[0-9]+' | head -1)"
    [ -n "$p" ] && echo "$p" && return
    echo "?"
}
get_api_pid() {
    if [ -f "$API_PID_FILE" ]; then
        cat "$API_PID_FILE" 2>/dev/null && return
    fi
    local p=""
    p="$(lsof -ti ":$API_PORT" -sTCP:LISTEN 2>/dev/null | head -1)"
    [ -n "$p" ] && echo "$p" && return
    p="$(ss -tlnp 2>/dev/null | grep ":${API_PORT} " | grep -oP 'pid=\K[0-9]+' | head -1)"
    [ -n "$p" ] && echo "$p" && return
    echo "?"
}
get_nginx_pid() {
    if [ -f "$NGINX_PID" ]; then
        cat "$NGINX_PID" 2>/dev/null && return
    fi
    local p=""
    p="$(lsof -ti ":$NGINX_PORT" -sTCP:LISTEN 2>/dev/null | head -1)"
    [ -n "$p" ] && echo "$p" && return
    p="$(ss -tlnp 2>/dev/null | grep ":${NGINX_PORT} " | grep -oP 'pid=\K[0-9]+' | head -1)"
    [ -n "$p" ] && echo "$p" && return
    echo "?"
}

# Find and terminate the process holding a port (fallback when PID files are missing)
# Send SIGTERM first → wait 5 seconds → escalate to SIGKILL if needed
kill_port_holder() {
    local port="$1" label="${2:-}"
    local pids=""
    if [ "$(uname -s)" = "Darwin" ]; then
        pids="$(lsof -ti ":$port" -sTCP:LISTEN 2>/dev/null || true)"
    else
        pids="$(ss -tlnp 2>/dev/null | grep ":${port} " | grep -o 'pid=[0-9]*' | sed 's/pid=//' | sort -u || true)"
    fi
    [ -z "$pids" ] && return 0
    local pid
    # First pass: SIGTERM (graceful shutdown)
    for pid in $pids; do
        [ "$pid" = "$$" ] && continue
        kill "$pid" 2>/dev/null && \
            warn "Sent SIGTERM via port ${port} (PID $pid)${label:+ [$label]}"
    done
    # Wait for the port to be released (up to 10 seconds)
    local i=0
    while is_port_in_use "$port" && [ $i -lt 10 ]; do
        i=$((i + 1)); sleep 1
    done
    sleep 2
    # Second pass: SIGKILL (force stop after SIGTERM timeout)
    if is_port_in_use "$port"; then
        if [ "$(uname -s)" = "Darwin" ]; then
            pids="$(lsof -ti ":$port" -sTCP:LISTEN 2>/dev/null || true)"
        else
            pids="$(ss -tlnp 2>/dev/null | grep ":${port} " | grep -o 'pid=[0-9]*' | sed 's/pid=//' | sort -u || true)"
        fi
        for pid in $pids; do
            [ "$pid" = "$$" ] && continue
            kill -9 "$pid" 2>/dev/null && \
                warn "SIGTERM timed out on port ${port}; sent SIGKILL (PID $pid)${label:+ [$label]}"
        done
        # Wait for SIGKILL to take effect (up to 3 seconds)
        i=0
        while is_port_in_use "$port" && [ $i -lt 6 ]; do
            i=$((i + 1)); sleep 1
        done
        sleep 2
    fi
}

# Wait for a PID to exit (up to max_wait seconds), then force-kill with SIGKILL on timeout
wait_pid_exit() {
    local pid="$1" label="${2:-}" max_wait="${3:-5}"
    local i=0
    while kill -0 "$pid" 2>/dev/null && [ $i -lt $((max_wait * 2)) ]; do
        i=$((i + 1)); sleep 0.5
    done
    if kill -0 "$pid" 2>/dev/null; then
        kill -9 "$pid" 2>/dev/null && \
            warn "${label} SIGTERM timed out; sent SIGKILL (PID $pid)"
        i=0
        while kill -0 "$pid" 2>/dev/null && [ $i -lt 6 ]; do
            i=$((i + 1)); sleep 0.5
        done
    fi
}

# Check whether a port is in use (without relying on PID files)
is_port_in_use() {
    if [ "$(uname -s)" = "Darwin" ]; then
        lsof -i ":$1" -sTCP:LISTEN >/dev/null 2>&1
    else
        ss -tln 2>/dev/null | grep -q ":$1 "
    fi
}

generate_nginx_conf() {
    local conf="$SCRIPT_DIR/config/nginx.conf"
    local mime="$SCRIPT_DIR/config/mime.types"
    local frontend_root="$SCRIPT_DIR/frontend/dist"
    cat > "$conf" << NGINX_EOF
worker_processes auto;
pid $NGINX_PID;
error_log $NGINX_LOG_DIR/error.log warn;
events { worker_connections 256; }
http {
  include $mime;
  default_type application/octet-stream;
  access_log $NGINX_LOG_DIR/access.log;
  sendfile on;
  keepalive_timeout 65;
  client_body_temp_path $DATA_DIR/nginx_temp;
  proxy_temp_path       $DATA_DIR/nginx_temp;
  map \$http_upgrade \$connection_upgrade { default upgrade; '' close; }
  server {
    listen $NGINX_PORT;
    server_name localhost;
    client_max_body_size 5M;
    location / {
      root $frontend_root;
      try_files \$uri \$uri/ /index.html;
    }
    # Normalize image path handling: /img/arkham/cards/ → mapped to zh/cards/
    location /img/arkham/cards/ {
      alias $frontend_root/img/arkham/zh/cards/;
      try_files \$uri @img_cdn;
    }
    location /img/ {
      root $frontend_root;
      try_files \$uri @img_cdn;
    }
    location @img_cdn {
      expires 7d;
      add_header Server-Timing "cdn" always;
      proxy_pass http://assets.arkhamhorror.app;
      proxy_set_header Host assets.arkhamhorror.app;
    }
    location /api {
      proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
      proxy_set_header Host \$http_host;
      proxy_redirect off;
      proxy_pass http://127.0.0.1:$API_PORT;
      proxy_set_header X-Real-IP \$remote_addr;
      proxy_http_version 1.1;
      proxy_set_header Upgrade \$http_upgrade;
      proxy_set_header Connection \$connection_upgrade;
    }
    location /health {
      proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
      proxy_set_header Host \$http_host;
      proxy_redirect off;
      proxy_pass http://127.0.0.1:$API_PORT;
      proxy_set_header X-Real-IP \$remote_addr;
      proxy_http_version 1.1;
      proxy_set_header Upgrade \$http_upgrade;
      proxy_set_header Connection \$connection_upgrade;
    }
  }
}
NGINX_EOF
}

# ── Stop ─────────────────────────────────────────────────────────────────────
do_stop() {
    info "Stopping services ..."

    # --- nginx ---
    if is_nginx_running; then
        local nginx_pid
        nginx_pid="$(cat "$NGINX_PID" 2>/dev/null || echo "")"
        "$SCRIPT_DIR/bin/nginx" -e "$NGINX_LOG_DIR/error.log" \
            -c "$SCRIPT_DIR/config/nginx.conf" -s stop 2>/dev/null || true
        [ -n "$nginx_pid" ] && wait_pid_exit "$nginx_pid" "nginx" 5
        info "nginx stopped"
    fi
    # Port-based fallback: clean up leftover nginx when the PID file is missing
    if is_port_in_use "$NGINX_PORT"; then
        kill_port_holder "$NGINX_PORT" "nginx"
    fi
    rm -f "$NGINX_PID"

    # --- arkham-api ---
    if is_api_running; then
        local api_pid
        api_pid="$(cat "$API_PID_FILE" 2>/dev/null || echo "")"
        kill "$api_pid" 2>/dev/null || true
        [ -n "$api_pid" ] && wait_pid_exit "$api_pid" "arkham-api" 5
        info "arkham-api stopped"
    fi
    if is_port_in_use "$API_PORT"; then
        kill_port_holder "$API_PORT" "arkham-api"
    fi
    rm -f "$API_PID_FILE"

    # --- PostgreSQL ---
    if [ -f "$PG_DATA/postmaster.pid" ]; then
        # -m fast: roll back active transactions and disconnect clients; -w: wait for shutdown to complete
        pg_ctl -D "$PG_DATA" -m fast -w stop 2>/dev/null || true
        info "PostgreSQL stopped"
    fi
    if is_port_in_use "$PG_PORT"; then
        kill_port_holder "$PG_PORT" "postgresql"
    fi
    # Clean up leftover PostgreSQL PID and socket files
    rm -f "$PG_DATA/postmaster.pid" 2>/dev/null
    rm -f "$PG_SOCKET_DIR/.s.PGSQL.${PG_PORT}" 2>/dev/null
    rm -f "$PG_SOCKET_DIR/.s.PGSQL.${PG_PORT}.lock" 2>/dev/null

    # After a normal stop, back up pgdata into the distribution directory for easier backup/migration
    if [ -f "$PG_DATA/PG_VERSION" ]; then
        info "Backing up database ..."
        if ! rm -rf "$PGDATA_LOCAL" 2>/dev/null; then
            warn "[4005] pgdata backup failed: unable to remove old backup"
        elif ! cp -a "$PG_DATA" "$PGDATA_LOCAL"; then
            warn "[4005] pgdata backup failed: copy error"
        else
            date +%s > "$VERSION_FILE"
            date +%s > "$VERSION_FILE_LOCAL"
        fi
    fi

    # Clear runtime logs after a normal shutdown (keep them when startup fails for troubleshooting)
    if [ "$_STARTUP_SUCCEEDED" = "1" ]; then
        for f in "$PG_LOG" \
                 "$DATA_DIR/initdb.log" \
                 "$DATA_DIR/psql.log" \
                 "$DATA_DIR/arkham-api.log" \
                 "$NGINX_LOG_DIR/error.log" \
                 "$NGINX_LOG_DIR/access.log"; do
            [ -f "$f" ] && :> "$f"
        done
    fi

    # --- Final verification: ensure all ports have been released ---
    local still_occupied=""
    is_port_in_use "$NGINX_PORT" && still_occupied="${still_occupied} ${NGINX_PORT}(nginx)"
    is_port_in_use "$API_PORT"   && still_occupied="${still_occupied} ${API_PORT}(api)"
    is_port_in_use "$PG_PORT"    && still_occupied="${still_occupied} ${PG_PORT}(pg)"
    if [ -n "$still_occupied" ]; then
        warn "[4004] These ports are still in use:${still_occupied}; attempting forced cleanup with SIGKILL ..."
        is_port_in_use "$NGINX_PORT" && kill_port_holder "$NGINX_PORT" "nginx"
        is_port_in_use "$API_PORT"   && kill_port_holder "$API_PORT" "arkham-api"
        is_port_in_use "$PG_PORT"    && kill_port_holder "$PG_PORT" "postgresql"
    fi

    # WSL/NTFS: file handles may linger briefly after process exit, so wait a bit
    if grep -qi microsoft /proc/version 2>/dev/null; then
        sleep 1
    fi

    info "All services stopped."
}

# ── Status ───────────────────────────────────────────────────────────────────
do_status() {
    echo ""
    printf '%sService Status%s\n\n' "$BOLD" "$RESET"
    if is_pg_running; then
        printf '  %s●%s PostgreSQL   %sRunning%s (Port %s, PID %s)\n' "$GREEN" "$RESET" "$GREEN" "$RESET" "$PG_PORT" "$(get_pg_pid)"
    else
        printf '  %s●%s PostgreSQL   %sStopped%s\n' "$RED" "$RESET" "$RED" "$RESET"
    fi
    if is_api_running; then
        printf '  %s●%s arkham-api   %sRunning%s (Port %s, PID %s)\n' "$GREEN" "$RESET" "$GREEN" "$RESET" "$API_PORT" "$(get_api_pid)"
    else
        printf '  %s●%s arkham-api   %sStopped%s\n' "$RED" "$RESET" "$RED" "$RESET"
    fi
    if is_nginx_running; then
        printf '  %s●%s nginx        %sRunning%s (Port %s, PID %s)\n' "$GREEN" "$RESET" "$GREEN" "$RESET" "$NGINX_PORT" "$(get_nginx_pid)"
    else
        printf '  %s●%s nginx        %sStopped%s\n' "$RED" "$RESET" "$RED" "$RESET"
    fi
    echo ""
}

# ── Initialize database ──────────────────────────────────────────────────────
init_database() {
    info "First run detected, initializing database ..."
    ensure_dir "$DATA_DIR"
    ensure_dir "$PG_DATA"
    "$(pg_bin)/initdb" -D "$PG_DATA" -U "$PG_USER" --no-locale -E UTF8 \
        > "$DATA_DIR/initdb.log" 2>&1 \
        || die 2003 "initdb initialization failed" "$DATA_DIR/initdb.log"

    cat > "$PG_DATA/pg_hba.conf" << HBA_EOF
local   all   all                 trust
host    all   all   127.0.0.1/32  trust
host    all   all   ::1/128       trust
HBA_EOF

    cat >> "$PG_DATA/postgresql.conf" << PG_CONF_EOF

port = $PG_PORT
unix_socket_directories = '$PG_SOCKET_DIR'
listen_addresses = '127.0.0.1'
PG_CONF_EOF

    info "Database schema initialization complete."
}

# ── Start ────────────────────────────────────────────────────────────────────
do_start() {
    [ -x "$SCRIPT_DIR/bin/arkham-api" ] || die 1002 "bin/arkham-api not found"
    [ -x "$SCRIPT_DIR/bin/nginx" ]      || die 1003 "bin/nginx not found"
    [ -d "$SCRIPT_DIR/pgsql/bin" ]      || die 1004 "pgsql/bin/ not found"
    [ -d "$SCRIPT_DIR/frontend/dist" ]  || die 1005 "frontend/dist/ not found"

    if is_nginx_running || is_api_running || is_pg_running \
       || is_port_in_use "$NGINX_PORT" || is_port_in_use "$API_PORT" || is_port_in_use "$PG_PORT"; then
        warn "Detected occupied services or ports; cleaning up first ..."
        do_stop; sleep 1
    fi

    # Enable cleanup protection: from here until startup fully succeeds, any abnormal exit triggers do_stop automatically
    _CLEANUP_ON_EXIT=1

    # macOS: sign all binaries + clear quarantine (only performed at startup)
    ensure_macos_signing

    ensure_dir "$DATA_DIR"
    configure_runtime_env

    # 1. PostgreSQL
    # Store pgdata under the OS user data directory to avoid WSL/NTFS permission issues.
    # On first startup, automatically migrate old data from data/pgdata if present.
    ensure_dir "$PGDATA_OS_PARENT" || die 2001 "Unable to create user data directory"

    local is_fresh=0
    if [ -d "$PG_DATA" ] && [ -f "$PG_DATA/PG_VERSION" ]; then
        # A valid cluster already exists in the OS data directory; use it directly
        :
    elif [ -d "$PGDATA_LOCAL" ] && [ -f "$PGDATA_LOCAL/PG_VERSION" ]; then
        # First-time migration: old data exists inside the distribution, so copy it to the OS data directory
        info "First startup detected; migrating database to the user data directory ..."
        cp -a "$PGDATA_LOCAL" "$PG_DATA" || die 2002 "Old data migration failed"
        date +%s > "$VERSION_FILE"
        date +%s > "$VERSION_FILE_LOCAL"
    else
        # Fresh installation: initdb only (initialize structure without starting PostgreSQL)
        init_database
        is_fresh=1
        date +%s > "$VERSION_FILE"
        date +%s > "$VERSION_FILE_LOCAL"
    fi

    # Check and repair pgdata permissions (PostgreSQL requires 0700 or 0750)
    local pg_perms
    if [ "$(uname -s)" = "Darwin" ]; then
        pg_perms="$(stat -f '%Lp' "$PG_DATA" 2>/dev/null || echo "000")"
    else
        pg_perms="$(stat -c '%a' "$PG_DATA" 2>/dev/null || echo "000")"
    fi
    if [ "$pg_perms" != "700" ] && [ "$pg_perms" != "750" ]; then
        warn "pgdata permissions are invalid ($pg_perms); fixing to 0700 ..."
        chmod 700 "$PG_DATA" 2>/dev/null || true
        find "$PG_DATA" -type d -exec chmod 700 {} \; 2>/dev/null || true
        find "$PG_DATA" -type f -exec chmod 600 {} \; 2>/dev/null || true
    fi

    # Ensure unix_socket_directories points to the current PG_SOCKET_DIR
    if grep -q 'unix_socket_directories' "$PG_DATA/postgresql.conf" 2>/dev/null; then
        if [ "$(uname -s)" = "Darwin" ]; then
            sed -i '' "s|^unix_socket_directories = .*|unix_socket_directories = '$PG_SOCKET_DIR'|" "$PG_DATA/postgresql.conf"
        else
            sed -i "s|^unix_socket_directories = .*|unix_socket_directories = '$PG_SOCKET_DIR'|" "$PG_DATA/postgresql.conf"
        fi
    fi

    # Unified PostgreSQL startup (init_database no longer starts PG; this is the only pg_ctl start entry point)
    if ! pg_ready; then
        info "Starting PostgreSQL ..."
        if ! pg_ctl -D "$PG_DATA" -l "$PG_LOG" -o "-k $PG_SOCKET_DIR" start; then
            die 2004 "PostgreSQL failed to start" "$PG_LOG"
        fi
        local tries=0
        while ! pg_ready; do
            tries=$((tries + 1)); [ "$tries" -gt 30 ] && die 2005 "PostgreSQL startup timed out" "$PG_LOG"
            sleep 1
        done
    fi
    info "PostgreSQL started (port $PG_PORT, PID $(get_pg_pid))"

    # Create the database on the first run of a fresh cluster only
    if [ "$is_fresh" = "1" ]; then
        info "Creating database ..."
        psql_cmd -d postgres -c "CREATE DATABASE \"$PG_DB\";" \
            > "$DATA_DIR/psql.log" 2>&1 \
            || die 2006 "Failed to create database" "$DATA_DIR/psql.log"
        psql_cmd -d "$PG_DB" -f "$SCRIPT_DIR/setup.sql" \
            >> "$DATA_DIR/psql.log" 2>&1 \
            || die 2007 "Schema import failed" "$DATA_DIR/psql.log"
        info "Database initialization complete."
    fi

    # Recovery check: ensure the database exists (prevents retries from skipping DB creation after a first-run failure)
    if ! psql_cmd -d postgres -tAc "SELECT 1 FROM pg_database WHERE datname='$PG_DB'" 2>/dev/null | grep -q 1; then
        info "Database $PG_DB does not exist; recreating it ..."
        psql_cmd -d postgres -c "CREATE DATABASE \"$PG_DB\";" \
            >> "$DATA_DIR/psql.log" 2>&1 \
            || die 2006 "Failed to create database" "$DATA_DIR/psql.log"
        psql_cmd -d "$PG_DB" -f "$SCRIPT_DIR/setup.sql" \
            >> "$DATA_DIR/psql.log" 2>&1 \
            || die 2007 "Schema import failed" "$DATA_DIR/psql.log"
        info "Database recreation complete."
    fi

    # 2. arkham-api (port 3002)
    info "Starting backend API ..."
    export DATABASE_URL="postgres://${PG_USER}@127.0.0.1:${PG_PORT}/${PG_DB}"
    export PORT="$API_PORT"
    export PGHOST="127.0.0.1" PGPORT="$PG_PORT" PGSSLMODE="disable"
    # Auto-detect: use local relative paths when local images exist; fall back to the CDN otherwise (same behavior as web-entrypoint.sh)
    if [ -z "${ASSET_HOST+x}" ]; then
      if [ -n "$(ls -A "$SCRIPT_DIR/frontend/dist/img" 2>/dev/null)" ]; then
        export ASSET_HOST=""
      else
        export ASSET_HOST="https://assets.arkhamhorror.app"
      fi
    fi
    # nohup prevents SIGHUP; redirect stdout/stderr to log files so SIGPIPE cannot kill the process when the pipe closes
    ( cd "$SCRIPT_DIR"; nohup "$SCRIPT_DIR/bin/arkham-api" >> "$DATA_DIR/arkham-api.log" 2>&1 & echo $! > "$API_PID_FILE" )

    local tries=0
    while ! curl -fs "http://127.0.0.1:${API_PORT}/health" > /dev/null 2>&1; do
        tries=$((tries + 1)); [ "$tries" -gt 30 ] && die 3003 "Backend API startup timed out" "$DATA_DIR/arkham-api.log"; sleep 1
    done
    info "Backend API started (port $API_PORT, PID $(get_api_pid))"

    # 3. nginx (port 3000)
    info "Configuring and starting nginx ..."
    ensure_dir "$DATA_DIR/nginx_temp"
    generate_nginx_conf
    touch "$NGINX_LOG_DIR/error.log" "$NGINX_LOG_DIR/access.log" 2>/dev/null || true
    sync 2>/dev/null || true
    if ! "$SCRIPT_DIR/bin/nginx" -e "$NGINX_LOG_DIR/error.log" -c "$SCRIPT_DIR/config/nginx.conf" 2>&1; then
        die 3002 "nginx failed to start" "$NGINX_LOG_DIR/error.log"
    fi
    info "nginx started (port $NGINX_PORT, PID $(get_nginx_pid))"

    # ── Post-start health check: wait 2 seconds, then verify all services are still alive ─
    # Prevent a silent crash (for example, delayed termination by macOS Gatekeeper) from being treated as a successful startup
    sleep 2
    if ! is_pg_running;  then die 3004 "PostgreSQL crashed after startup" "$PG_LOG"; fi
    if ! is_api_running; then die 3005 "arkham-api crashed after startup" "$DATA_DIR/arkham-api.log"; fi
    if ! is_nginx_running; then die 3006 "nginx crashed after startup" "$NGINX_LOG_DIR/error.log"; fi

    # Startup succeeded completely; disable cleanup protection
    _CLEANUP_ON_EXIT=0
    _STARTUP_SUCCEEDED=1

    echo ""
    printf '%s============================================%s\n' "$CYAN" "$RESET"
    printf '%s  Arkham Horror LCG Started!%s\n' "$BOLD" "$RESET"
    printf '%s============================================%s\n' "$CYAN" "$RESET"
    echo ""
    printf '  %-14s %shttp://localhost:%s%s\n' "URL" "$GREEN" "$NGINX_PORT" "$RESET"
    echo ""
    printf '  %-14s PID %-8s Port %s\n' "PostgreSQL" "$(get_pg_pid)" "$PG_PORT"
    printf '  %-14s PID %-8s Port %s\n' "arkham-api" "$(get_api_pid)" "$API_PORT"
    printf '  %-14s PID %-8s Port %s\n' "nginx" "$(get_nginx_pid)" "$NGINX_PORT"
    echo ""
    printf '  Status: %sbash %s/start.sh --status%s\n' "$CYAN" "$SCRIPT_DIR" "$RESET"
    printf '  Stop:   %sbash %s/start.sh --stop%s\n' "$CYAN" "$SCRIPT_DIR" "$RESET"
    echo ""

    # Open the browser automatically (silent; failure does not affect services)
    open_browser "http://localhost:${NGINX_PORT}"
}

# ── Foreground keepalive: keep the terminal window open and stop all services automatically when the window closes ─
run_foreground() {
    # On macOS, delay closing the terminal window until the shell fully exits to avoid a "Terminate" dialog
    _close_terminal_window() {
        ( sleep 0.5; osascript -e "tell application \"Terminal\" to close front window" 2>/dev/null ) &
        disown 2>/dev/null
    }

    # On exit signal: stop services → clear EXIT trap → close window → exit
    trap 'info "Exit signal received, stopping all services ..."; \
          do_stop; \
          trap - EXIT; \
          [ "$(uname -s)" = "Darwin" ] && _close_terminal_window; \
          exit 0' HUP INT TERM
    trap 'do_stop 2>/dev/null || true' EXIT

    echo ""
    printf '%s────────────────────────────────────────────%s\n' "$CYAN" "$RESET"
    printf '  Keep this terminal window open to keep the services running.\n'
    printf '  Press %sCtrl+C%s or %sclose this window%s to stop all services automatically.\n' "$BOLD" "$RESET" "$BOLD" "$RESET"
    printf '%s────────────────────────────────────────────%s\n' "$CYAN" "$RESET"
    echo ""

    # Sleep in the foreground without spawning a background child process
    while true; do
        sleep 86400 || true
    done
}

# ── Argument parsing ──────────────────────────────────────────────────────────
# Ensure the runtime environment (library paths) is ready before any action; signing only happens for actual startup
configure_runtime_env

ACTION="start"
while [ $# -gt 0 ]; do
    case "$1" in
        --stop)   ACTION="stop"; shift ;;
        --status) ACTION="status"; shift ;;
        --help|-h)
            echo "Usage: bash start.sh [--stop|--status|--help]"; exit 0 ;;
        *) die 1099 "Unknown argument: $1" ;;
    esac
done

case "$ACTION" in
    start)
        # First check whether all services are already healthy and running
        if is_all_services_running; then
            info "All services are already running."
            echo ""
            printf '%s============================================%s\n' "$CYAN" "$RESET"
            printf '%s  Arkham Horror LCG (Already Running)%s\n' "$BOLD" "$RESET"
            printf '%s============================================%s\n' "$CYAN" "$RESET"
            echo ""
            printf '  %-14s %shttp://localhost:%s%s\n' "URL" "$GREEN" "$NGINX_PORT" "$RESET"
            echo ""
            printf '  %-14s PID %-8s Port %s\n' "PostgreSQL" "$(get_pg_pid)" "$PG_PORT"
            printf '  %-14s PID %-8s Port %s\n' "arkham-api" "$(get_api_pid)" "$API_PORT"
            printf '  %-14s PID %-8s Port %s\n' "nginx" "$(get_nginx_pid)" "$NGINX_PORT"
            echo ""
            open_browser "http://localhost:${NGINX_PORT}"
            run_foreground
        else
            do_start
            run_foreground
        fi
        ;;
    stop)   do_stop ;;
    status) do_status ;;
esac
LAUNCHSCRIPT

    chmod +x "${PKG_DIR}/start.sh"
    info "  ✓ start.sh generated"
}

# ── Generate stop.sh (shortcut) ───────────────────────────────────────────────

generate_stop_script() {
    cat > "${PKG_DIR}/stop.sh" << 'STOPSCRIPT'
#!/usr/bin/env bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
exec bash "$SCRIPT_DIR/start.sh" --stop
STOPSCRIPT
    chmod +x "${PKG_DIR}/stop.sh"
}

# ── Generate start.bat (Windows launcher) ────────────────────────────────────

generate_start_bat() {
    cat > "${PKG_DIR}/start.bat" << 'BATSCRIPT'
@echo off
chcp 65001 >nul 2>&1
setlocal EnableDelayedExpansion

title Arkham Horror LCG

echo.
echo   ============================================
echo     Arkham Horror LCG - Windows Offline Launcher
echo   ============================================
echo.

REM ---- 1. Check whether WSL is installed ----
where wsl >nul 2>&1
if %ERRORLEVEL% neq 0 goto :NO_WSL
goto :HAS_WSL

:NO_WSL
echo [^!] WSL is not installed. Starting installation...
echo     Administrator privileges are required, and a reboot will be needed after installation.
echo.
powershell -Command "Start-Process cmd -ArgumentList '/c wsl --install -d Ubuntu && pause' -Verb RunAs"
echo.
echo [^!] WSL installation has been launched in an elevated window.
echo     After installation finishes and the computer is restarted, double-click start.bat again.
echo.
pause
exit /b 0

:HAS_WSL
REM ---- 2. Probe Ubuntu distributions one by one ----
echo [*] Detecting Ubuntu distributions...
set "WSL_DISTRO="

wsl -d Ubuntu -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu"& goto :FOUND_DISTRO)

wsl -d Ubuntu-24.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-24.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-22.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-22.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-20.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-20.04"& goto :FOUND_DISTRO)

wsl -d Ubuntu-18.04 -- echo ok >nul 2>&1
if !ERRORLEVEL! equ 0 (set "WSL_DISTRO=Ubuntu-18.04"& goto :FOUND_DISTRO)

REM All probes failed; try installing Ubuntu
goto :NO_UBUNTU

:NO_UBUNTU
echo [^!] No Ubuntu distribution was found. Installing one now...
echo.
wsl --install -d Ubuntu
if !ERRORLEVEL! neq 0 goto :INSTALL_FAILED
echo.
echo [^!] Ubuntu installation completed. Please double-click start.bat again.
echo.
pause
exit /b 0

:INSTALL_FAILED
echo.
echo [^!] Ubuntu installation failed. Please install it manually:
echo     Open PowerShell and run: wsl --install -d Ubuntu
echo.
pause
exit /b 1

:FOUND_DISTRO
echo [*] Using distribution: !WSL_DISTRO!
echo.

REM ---- 2.5. Ensure the arkham user exists (PostgreSQL does not allow root) ----
wsl -d !WSL_DISTRO! -u root -- id arkham >nul 2>&1
if !ERRORLEVEL! equ 0 goto :HASUSER
echo [*] Creating arkham user...
wsl -d !WSL_DISTRO! -u root -- useradd -m -s /bin/bash arkham
wsl -d !WSL_DISTRO! -u root -- id arkham >nul 2>&1
if !ERRORLEVEL! neq 0 goto :USER_FAILED
echo [*] arkham user created
goto :HASUSER

:USER_FAILED
echo.
echo [^!] Failed to create the arkham user. Please run manually:
echo     wsl -d !WSL_DISTRO! -u root -- useradd -m -s /bin/bash arkham
echo.
pause
exit /b 1

:HASUSER
REM ---- 3. Get the WSL path for the current directory ----
set "WIN_DIR=%~dp0"
if "!WIN_DIR:~-1!"=="\" set "WIN_DIR=!WIN_DIR:~0,-1!"

for /f "tokens=*" %%i in ('wsl -d !WSL_DISTRO! wslpath -u "!WIN_DIR!" 2^>nul') do set "WSL_DIR=%%i"
if "!WSL_DIR!"=="" goto :PATH_FAILED
goto :PATH_OK

:PATH_FAILED
echo.
echo [^!] WSL path conversion failed. The path may contain special characters.
echo     Please move the folder to a path without special characters (for example, C:\ArkhamHorror\).
echo.
pause
exit /b 1

:PATH_OK
echo [*] Package path: !WIN_DIR!
echo [*] WSL path:    !WSL_DIR!
echo.

REM ---- 4. Start services ----
echo [*] Starting Arkham Horror LCG ...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!' && bash start.sh"
set START_EXIT=!ERRORLEVEL!

if !START_EXIT! equ 42 goto :FIX_PERMISSIONS
if !START_EXIT! equ 0 goto :START_OK
goto :START_RETRY

REM ---- 5. Automatically fix WSL filesystem permissions (exit code 42) ----
:FIX_PERMISSIONS
echo.
echo [*] Configuring WSL filesystem permissions (no password required)...
echo.
wsl -d !WSL_DISTRO! -u root -- bash -c "if grep -q 'metadata' /etc/wsl.conf 2>/dev/null; then echo '[wsl.conf] already ok.'; else if [ -f /etc/wsl.conf ]; then cp /etc/wsl.conf /etc/wsl.conf.bak; fi; if grep -q '\[automount\]' /etc/wsl.conf 2>/dev/null; then sed -i '/\[automount\]/a options = \"metadata\"' /etc/wsl.conf; else printf '\n[automount]\noptions = \"metadata\"\n' >> /etc/wsl.conf; fi; echo '[wsl.conf] updated.'; fi"
echo.
echo [*] Restarting !WSL_DISTRO!. Please do not close this window. Wait about 8 seconds...
wsl --terminate !WSL_DISTRO! >nul 2>nul
timeout /t 3 /nobreak >nul
echo [*] !WSL_DISTRO! restarted. Starting services again...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!' && bash start.sh"
set START_EXIT=!ERRORLEVEL!
if !START_EXIT! equ 42 goto :PERM_UNSUPPORTED
if !START_EXIT! equ 0 goto :START_OK
goto :START_RETRY

:PERM_UNSUPPORTED
echo.
echo [^!] The current drive filesystem does not support Unix permissions (it may be exFAT or FAT32).
echo     The metadata option only works on NTFS and ReFS.
echo.
echo     Please move the entire folder to an NTFS partition (usually the C: drive), then run start.bat again.
echo.
pause
exit /b 1

REM ---- 6. Startup failed: retry after restart ----
:START_RETRY
echo.
echo [^!] Startup failed (exit code: !START_EXIT!). Restarting !WSL_DISTRO! and retrying...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!' && bash start.sh --stop" 2>nul
wsl --terminate !WSL_DISTRO! >nul 2>nul
timeout /t 2 /nobreak >nul
echo [*] Starting services again...
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!' && bash start.sh"
set START_EXIT=!ERRORLEVEL!
if !START_EXIT! equ 0 goto :START_OK

echo.
echo [^!] It still failed after restart (exit code: !START_EXIT!). Please check the error messages above.
echo.
wsl -d !WSL_DISTRO! -u arkham -- bash -c "cd '!WSL_DIR!' && bash start.sh --stop" 2>nul
goto :END

:START_OK
echo.

:END
pause
BATSCRIPT

    # The bat file must use CRLF line endings (required on Windows)
    if command -v unix2dos >/dev/null 2>&1; then
        unix2dos "${PKG_DIR}/start.bat" 2>/dev/null || true
    elif command -v sed >/dev/null 2>&1; then
        sed -i.bak 's/$/\r/' "${PKG_DIR}/start.bat" 2>/dev/null && rm -f "${PKG_DIR}/start.bat.bak" || true
    fi

    info "  ✓ start.bat generated"
}

# ── Generate mime.types ───────────────────────────────────────────────────────

generate_mime_types() {
    ensure_dir "${PKG_DIR}/config"
    cat > "${PKG_DIR}/config/mime.types" << 'MIME_EOF'
types {
  text/html                             html htm shtml;
  text/css                              css;
  text/xml                              xml;
  application/javascript                js mjs;
  application/json                      json;
  image/png                             png;
  image/jpeg                            jpeg jpg;
  image/gif                             gif;
  image/svg+xml                         svg svgz;
  image/webp                            webp;
  image/x-icon                          ico;
  application/font-woff                 woff;
  application/font-woff2                woff2;
  font/ttf                              ttf;
  font/otf                              otf;
  application/wasm                      wasm;
  application/octet-stream              bin;
  text/plain                            txt;
}
MIME_EOF
    substep "mime.types generated"
}

# ── Generate Start-ArkhamHorror.bat ───────────────────────────────────────────

generate_windows_shortcut() {
    cat > "${PKG_DIR}/Start-ArkhamHorror.bat" << 'WINSHORTCUT'
@echo off
chcp 65001 >nul 2>&1
cd /d "%~dp0"
call start.bat
WINSHORTCUT
    info "  ✓ Start-ArkhamHorror.bat generated"
}

# ── Generate Start-ArkhamHorror.command ───────────────────────────────────────

generate_macos_command() {
    cat > "${PKG_DIR}/Start-ArkhamHorror.command" << 'MACCOMMAND'
#!/bin/bash
cd "$(dirname "$0")" && bash start.sh
MACCOMMAND
    chmod +x "${PKG_DIR}/Start-ArkhamHorror.command"
    info "  ✓ Start-ArkhamHorror.command generated"
}


# ═════════════════════════════════════════════════════════════════════════════

main() {
    step "Packaging distribution: ${PKG_NAME}"
    echo ""

    [ -f "$BACKEND_BIN" ]   || die "Backend binary does not exist: $BACKEND_BIN"
    [ -d "$FRONTEND_SRC" ]  || die "Frontend artifacts do not exist: $FRONTEND_SRC"
    [ -d "$PG_BIN_DIR" ]    || die "PostgreSQL does not exist: $PG_BIN_DIR"
    [ -f "$NGINX_BIN" ]     || die "Nginx does not exist: $NGINX_BIN"

    # If an old distribution exists and has start.sh, stop any possibly running services first (to avoid NTFS file locks)
    if [ -d "$PKG_DIR" ] && [ -f "${PKG_DIR}/start.sh" ]; then
        substep "Old distribution detected; stopping any possibly running services first ..."
        bash "${PKG_DIR}/start.sh" --stop 2>/dev/null || true
    fi

    # Remove old distribution directory
    # Problem: start.bat runs PostgreSQL initdb as the arkham user (UID 1001),
    # so the created pgdata directory is 0700 and owned by arkham; the current dev user may not be allowed to delete it.
    # Solution: try normal rm first, then fall back to sudo rm if needed.
    if [ -d "$PKG_DIR" ]; then
        if ! rm -rf "$PKG_DIR" 2>/dev/null; then
            warn "Normal deletion failed (data/pgdata may have been created by another user); sudo is required ..."
            sudo rm -rf "$PKG_DIR" \
                || die "Unable to delete old distribution directory: $PKG_DIR\n  Manual fix: sudo rm -rf $PKG_DIR"
        fi
    fi
    ensure_dir "${PKG_DIR}/bin"
    ensure_dir "${PKG_DIR}/pgsql/bin"
    ensure_dir "${PKG_DIR}/pgsql/lib"
    ensure_dir "${PKG_DIR}/frontend/dist"
    ensure_dir "${PKG_DIR}/config"
    ensure_dir "${PKG_DIR}/data"

    # Copy backend
    substep "Copy: ${BACKEND_BIN} → ${PKG_DIR}/bin/arkham-api"
    cp "$BACKEND_BIN" "${PKG_DIR}/bin/arkham-api"
    chmod +x "${PKG_DIR}/bin/arkham-api"

    # Copy Nginx
    substep "Copy: ${NGINX_BIN} → ${PKG_DIR}/bin/nginx"
    cp "$NGINX_BIN" "${PKG_DIR}/bin/nginx"
    chmod +x "${PKG_DIR}/bin/nginx"

    # Copy frontend
    substep "Copy: ${FRONTEND_SRC}/ → ${PKG_DIR}/frontend/dist/"
    cp -r "${FRONTEND_SRC}/"* "${PKG_DIR}/frontend/dist/"

    # Copy PostgreSQL
    substep "Copy PostgreSQL binaries ..."
    for b in postgres initdb pg_ctl pg_isready psql; do
        [ -x "${PG_BIN_DIR}/${b}" ] && cp "${PG_BIN_DIR}/${b}" "${PKG_DIR}/pgsql/bin/"
    done
    [ -d "$PG_LIB_DIR" ] && cp -r "${PG_LIB_DIR}/"* "${PKG_DIR}/pgsql/lib/" 2>/dev/null || true
    local pg_share="${DEPS_DIR}/postgres/share"
    [ -d "$pg_share" ] && { ensure_dir "${PKG_DIR}/pgsql/share"; cp -r "${pg_share}/"* "${PKG_DIR}/pgsql/share/" 2>/dev/null || true; }

    # setup.sql is a full production database dump (tables + columns + constraints + indexes + triggers)
    # Docker initializes the database directly from this file instead of composing Sqitch migrations
    if [ -f "$SETUP_SQL" ]; then
        cp "$SETUP_SQL" "${PKG_DIR}/setup.sql"
        substep "setup.sql copied (full production database dump)"
    else
        warn "setup.sql not found"
    fi

    # Config files
    local config_src="${PROJECT_ROOT}/backend/arkham-api/config"
    for f in settings.yml client_session_key.aes favicon.ico robots.txt routes; do
        [ -f "${config_src}/$f" ] && cp "${config_src}/$f" "${PKG_DIR}/config/" 2>/dev/null || true
    done

    # Generate nginx/mime runtime files
    generate_mime_types
    generate_launch_script
    generate_stop_script

    # Generate user-facing launch shortcuts (per target platform)
    if [ "$OS" = "linux" ]; then
        generate_start_bat
        generate_windows_shortcut
    elif [ "$OS" = "macos" ]; then
        generate_macos_command
    fi

    # ── Collect dynamic library dependencies (self-contained distribution; target environment needs no dev packages) ─
    if [ "$OS" = "macos" ]; then
        substep "Bundling macOS dynamic library dependencies ..."
        ensure_dir "${PKG_DIR}/lib"

        # Use otool -L to scan non-system dylib dependencies of arkham-api and nginx
        # System libraries under /usr/lib/ and /System/Library/ are not bundled
        # Homebrew libraries (/opt/homebrew/ or /usr/local/) must be bundled
        local bundled=0
        for bin in "${PKG_DIR}/bin/arkham-api" "${PKG_DIR}/bin/nginx"; do
            while IFS= read -r line; do
                local lib_path
                lib_path="$(echo "$line" | sed 's/^[[:space:]]*//;s/ (compatibility.*//;s/ (.*//')"
                # Skip system libraries
                case "$lib_path" in
                    /usr/lib/*|/System/Library/*) continue ;;
                    @rpath/*|@executable_path/*)  continue ;;  # already using relative paths
                esac
                local lib_name="$(basename "$lib_path")"
                [ -f "${PKG_DIR}/lib/${lib_name}" ] && continue  # already bundled
                if [ -f "$lib_path" ]; then
                    cp "$lib_path" "${PKG_DIR}/lib/"
                    # Homebrew source dylibs may be read-only (0444); chmod is required before signing
                    chmod u+w "${PKG_DIR}/lib/${lib_name}"
                    # Strip the original Homebrew signature (it will be invalid on another machine and can block replacement with a fresh ad-hoc signature)
                    codesign --remove-signature "${PKG_DIR}/lib/${lib_name}" || warn "Failed to remove signature: ${lib_name}"
                    # Rewrite absolute library references to @rpath (paired with -add_rpath)
                    install_name_tool -change "$lib_path" "@rpath/${lib_name}" "$bin" || warn "install_name_tool failed: ${lib_name}"
                    bundled=$((bundled + 1))
                    substep "  + ${lib_name} ← ${lib_path}"
                fi
            done < <(otool -L "$bin" 2>/dev/null | tail -n +2)
        done

        # Fix rpath so binaries prefer dylibs from the bundled lib/ directory
        for bin in "${PKG_DIR}/bin/arkham-api" "${PKG_DIR}/bin/nginx"; do
            install_name_tool -add_rpath "@executable_path/../lib" "$bin" 2>/dev/null || true
        done

        info "  ✓ Bundled ${bundled} macOS dynamic libraries → ${PKG_DIR}/lib/"
    elif [ "$OS" = "linux" ]; then
        substep "Collecting dynamic library dependencies ..."
        ensure_dir "${PKG_DIR}/lib"

        # Collect direct + transitive dependencies for all binaries and already bundled .so files
        # Exclude glibc core libraries (libc/libm/libdl/libpthread/librt/ld-linux/linux-vdso)
        #   because they are present on any Linux distribution and must match the target system kernel
        # Exclude libraries already present under pgsql/lib/ (for example libpq.so.5 from our own build)
        local bins_to_scan=(
            "${PKG_DIR}/bin/arkham-api"
            "${PKG_DIR}/bin/nginx"
        )
        # Also scan .so files under pgsql/lib (for example uuid-ossp.so depends on libuuid.so.1)
        while IFS= read -r extra_so; do
            bins_to_scan+=("$extra_so")
        done < <(find "${PKG_DIR}/pgsql/lib" -name '*.so' -type f 2>/dev/null)

        # First scan the NEEDED entries of binaries (direct dependencies)
        local needed_libs=()
        for bin_file in "${bins_to_scan[@]}"; do
            while IFS= read -r lib_name; do
                needed_libs+=("$lib_name")
            done < <(readelf -d "$bin_file" 2>/dev/null | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/')
        done

        # For each NEEDED library, find the system path and copy it (excluding glibc core libs and already bundled ones)
        local copied_count=0
        for lib_name in "${needed_libs[@]}"; do
            # Skip glibc core libraries
            if echo "$lib_name" | grep -qE "^(libc\.so|libm\.so|libdl\.so|libpthread\.so|librt\.so)"; then
                continue
            fi
            # Skip libraries already under pgsql/lib (our self-built libpq must not be overridden by the system version)
            if [ -f "${PKG_DIR}/pgsql/lib/${lib_name}" ]; then
                continue
            fi
            # Skip libraries already copied
            if [ -f "${PKG_DIR}/lib/${lib_name}" ]; then
                continue
            fi
            # Find the system path
            local lib_path
            lib_path="$(ldconfig -p 2>/dev/null | grep -F "$lib_name" | head -1 | sed 's/.*=> //')"
            if [ -z "$lib_path" ] || [ ! -f "$lib_path" ]; then
                # Also try searching standard paths directly
                lib_path="$(find /lib /usr/lib -name "$lib_name" 2>/dev/null | head -1)"
            fi
            if [ -n "$lib_path" ] && [ -f "$lib_path" ]; then
                cp "$lib_path" "${PKG_DIR}/lib/"
                copied_count=$((copied_count + 1))
                info "    Bundled: ${lib_name} ← ${lib_path}"

                # Recurse into this library's own NEEDED entries (transitive dependencies)
                while IFS= read -r transitive_lib; do
                    if echo "$transitive_lib" | grep -qE "^(libc\.so|libm\.so|libdl\.so|libpthread\.so|librt\.so)"; then
                        continue
                    fi
                    if [ -f "${PKG_DIR}/lib/${transitive_lib}" ] || [ -f "${PKG_DIR}/pgsql/lib/${transitive_lib}" ]; then
                        continue
                    fi
                    local t_path
                    t_path="$(ldconfig -p 2>/dev/null | grep -F "$transitive_lib" | head -1 | sed 's/.*=> //')"
                    if [ -z "$t_path" ] || [ ! -f "$t_path" ]; then
                        t_path="$(find /lib /usr/lib -name "$transitive_lib" 2>/dev/null | head -1)"
                    fi
                    if [ -n "$t_path" ] && [ -f "$t_path" ]; then
                        cp "$t_path" "${PKG_DIR}/lib/"
                        copied_count=$((copied_count + 1))
                        info "    Bundled: ${transitive_lib} ← ${t_path} (transitive dependency)"
                    fi
                done < <(readelf -d "$lib_path" 2>/dev/null | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/')
            else
                warn "    Not found: ${lib_name} (must be installed on the target system)"
            fi
        done

        info "  Bundled ${copied_count} dynamic libraries into lib/"
    fi

    # ── macOS Gatekeeper: ad-hoc signing after library collection ─────────────
    # Key point: Homebrew dylibs carry original signatures that must be fully stripped before re-signing with a clean file.
    # Order: strip lib/ dylibs first (twice to ensure completeness) → sign other binaries → strip lib/ dylibs again
    # → finally clear quarantine. The target Mac's start.sh will re-sign lib/ dylibs.
    if [ "$OS" = "macos" ]; then
        # Step 1: ensure dylibs under lib/ are writable first (Homebrew sources may be read-only)
        if [ -d "${PKG_DIR}/lib" ]; then
            chmod -R u+w "${PKG_DIR}/lib/" 2>/dev/null || true
            # Fully strip the original Homebrew signatures (run twice to be safe)
            find "${PKG_DIR}/lib" -name '*.dylib' -exec codesign --remove-signature {} \; >/dev/null 2>&1 || true
            find "${PKG_DIR}/lib" -name '*.dylib' -exec codesign --remove-signature {} \; >/dev/null 2>&1 || true
            substep "Original signatures stripped from bundled dylibs"
        fi

        # Step 2: ad-hoc sign all binaries (including lib/ dylibs, which will be stripped again in the next step)
        substep "Ad-hoc signing all binaries ..."
        local signed=0
        while IFS= read -r -d '' f; do
            if codesign --force --sign - "$f" >/dev/null 2>&1; then signed=$((signed + 1))
            else warn "Signing failed: $f"
            fi
        done < <(find "${PKG_DIR}" -type f \( -perm -a=x -o -name '*.so' -o -name '*.dylib' \) -print0 2>/dev/null)
        info "  ✓ Signed ${signed} files"

        # Step 3: strip signatures from lib/ dylibs again (they will always be invalid after cross-machine transfer)
        find "${PKG_DIR}/lib" -name '*.dylib' -exec codesign --remove-signature {} \; >/dev/null 2>&1 || true

        # Step 4: clear all quarantine attributes
        xattr -rd com.apple.quarantine "${PKG_DIR}" || true
    fi

    # Verification
    echo ""
    substep "Verifying distribution integrity ..."
    local required=("bin/arkham-api" "bin/nginx" "pgsql/bin/postgres" "pgsql/bin/initdb" "pgsql/bin/pg_ctl" "frontend/dist/index.html" "start.sh" "stop.sh")
    if [ "$OS" = "linux" ]; then
        required+=("start.bat" "Start-ArkhamHorror.bat")
    elif [ "$OS" = "macos" ]; then
        required+=("Start-ArkhamHorror.command")
    fi
    local ok=true
    for f in "${required[@]}"; do
        [ -e "${PKG_DIR}/${f}" ] || { warn "  Missing: $f"; ok=false; }
    done
    [ "$ok" = true ] && info "  ✓ All core files are present" || die "  ✗ Distribution is incomplete"

    # Archive
    echo ""
    step "Creating archive"
    pushd "$_DIST_DIR" > /dev/null
    tar -czf "${PKG_ARCHIVE}" "$PKG_NAME"
    popd > /dev/null

    local sz; sz="$(du -h "$PKG_ARCHIVE" | cut -f1)"
    info "  ✓ ${PKG_ARCHIVE} ($sz)"
    echo ""
    info "Usage: tar -xzf ${PKG_NAME}.tar.gz && cd ${PKG_NAME} && bash start.sh"
}

main "$@"
