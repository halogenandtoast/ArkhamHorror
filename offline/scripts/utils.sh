#!/usr/bin/env bash
# =============================================================================
# utils.sh - Shared helper library
# Provides logging, system detection, cached downloads, extraction, and toolchain PATH management
# Compatible with macOS and Linux (Ubuntu)
# =============================================================================

set -euo pipefail

# ── Colors ────────────────────────────────────────────────────────────────────
readonly CLR_RESET='\033[0m'
readonly CLR_GREEN='\033[0;32m'
readonly CLR_YELLOW='\033[0;33m'
readonly CLR_RED='\033[0;31m'
readonly CLR_BLUE='\033[0;34m'
readonly CLR_CYAN='\033[0;36m'

# ── Global paths (initialized by init_paths()) ────────────────────────────────
PROJECT_ROOT=""
OFFLINE_DIR=""
DEPS_DIR=""
SCRIPTS_DIR=""
_DIST_DIR=""
GHCUP_DIR=""    # GHC + Stack install directory: _deps/ghcup/
TMP_DIR=""      # Download cache + temporary extraction: offline/_tmp/

# ── Logging ───────────────────────────────────────────────────────────────────

info()    { printf "${CLR_GREEN}[INFO]${CLR_RESET} %s\n" "$*"; }
step()    { printf "\n${CLR_CYAN}━━━ %s ━━━${CLR_RESET}\n" "$*"; }
warn()    { printf "${CLR_YELLOW}[WARN]${CLR_RESET} %s\n" "$*" >&2; }
die()     { printf "${CLR_RED}[ERROR]${CLR_RESET} %s\n" "$*" >&2; exit 1; }
substep() { printf "  ${CLR_BLUE}→${CLR_RESET} %s\n" "$*"; }

# ── System detection ──────────────────────────────────────────────────────────

detect_os() {
    case "$(uname -s)" in
        Darwin) echo "macos" ;;
        Linux)  echo "linux" ;;
        *)      die "Unsupported operating system: $(uname -s)" ;;
    esac
}

detect_arch() {
    case "$(uname -m)" in
        arm64|aarch64) echo "arm64" ;;
        x86_64|amd64)  echo "x86_64" ;;
        *)             die "Unsupported CPU architecture: $(uname -m)" ;;
    esac
}

detect_platform() {
    echo "$(detect_os)-$(detect_arch)"
}

has_cmd() {
    command -v "$1" >/dev/null 2>&1
}

# ── Safe directory creation for WSL/NTFS ─────────────────────────────────────
# When WSL accesses NTFS through DrvFS, deleting a directory may leave behind ghost handles,
# causing mkdir to report "File exists" even though the directory does not actually exist.
# This helper includes retry logic and works on WSL/NTFS as well as native Linux/macOS.

ensure_dir() {
    local dir="$1"
    local retries=5
    local i=0
    while [ $i -lt $retries ]; do
        if [ -d "$dir" ]; then
            return 0
        fi
        if mkdir -p "$dir" 2>/dev/null; then
            return 0
        fi
        # mkdir failed, but the directory may already exist (WSL/NTFS race condition)
        if [ -d "$dir" ]; then
            return 0
        fi
        i=$((i + 1))
        if [ $i -lt $retries ]; then
            warn "mkdir -p '$dir' failed (attempt ${i}); retrying in 1 second ..."
            sleep 1
        fi
    done
    # Final attempt; exit with an error if it still fails
    mkdir -p "$dir" || die "Unable to create directory: $dir (retried ${retries} times)"
}

# ── Path initialization ──────────────────────────────────────────────────────

init_paths() {
    SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    OFFLINE_DIR="$(dirname "$SCRIPTS_DIR")"
    PROJECT_ROOT="$(dirname "$OFFLINE_DIR")"
    DEPS_DIR="${OFFLINE_DIR}/_deps"
    _DIST_DIR="${OFFLINE_DIR}/_dist"
    GHCUP_DIR="${DEPS_DIR}/ghcup"
    TMP_DIR="${OFFLINE_DIR}/_tmp"
}

# ── Cached downloads (all archives are stored in _tmp/ and skipped if present) ─

# download_cached URL archive_name
#   Check whether the file already exists under _tmp/ and skip the download if it does
#   Returns: full file path
# Note: all logs go to stderr (>&2), and only the file path is printed to stdout.
# This keeps logs visible when called inside $() command substitution without polluting the return value.
download_cached() {
    local url="$1"
    local filename="$2"
    local cache_path="${TMP_DIR}/${filename}"

    ensure_dir "$TMP_DIR"

    if [ -f "$cache_path" ] && [ -s "$cache_path" ]; then
        printf "${CLR_GREEN}[INFO]${CLR_RESET} Using cache: _tmp/${filename}\n" >&2
        printf "  ${CLR_BLUE}→${CLR_RESET} (already downloaded, skipping)\n" >&2
    else
        # Print the URL and destination before downloading
        printf "${CLR_GREEN}[INFO]${CLR_RESET} Downloading: ${filename}\n" >&2
        printf "  ${CLR_BLUE}→${CLR_RESET} Download URL: $url\n" >&2
        printf "  ${CLR_BLUE}→${CLR_RESET} Save path: offline/_tmp/${filename}\n" >&2
        curl -fSL --connect-timeout 30 --max-time 600 --progress-bar \
            -o "$cache_path" "$url" || die "Download failed: $url"
        printf "${CLR_GREEN}[INFO]${CLR_RESET} ✓ Download complete: ${filename}\n" >&2
    fi

    echo "$cache_path"
}

# ── Extraction (with logging) ────────────────────────────────────────────────

extract_tgz() {
    local archive="$1"; local dest="$2"; local strip="${3:-0}"
    substep "Extract tgz: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    tar -xzf "$archive" -C "$dest" ${strip:+--strip-components="$strip"}
}

extract_txz() {
    local archive="$1"; local dest="$2"; local strip="${3:-0}"
    substep "Extract txz: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    tar -xJf "$archive" -C "$dest" ${strip:+--strip-components="$strip"}
}

extract_tbz() {
    local archive="$1"; local dest="$2"; local strip="${3:-0}"
    substep "Extract tbz: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    tar -xjf "$archive" -C "$dest" ${strip:+--strip-components="$strip"}
}

extract_zip() {
    local archive="$1"; local dest="$2"
    substep "Extract zip: $(basename "$archive") → ${dest}"
    ensure_dir "$dest"
    unzip -qo "$archive" -d "$dest"
}

# Create a dedicated subdirectory for each extraction under _tmp/extract/
prepare_extract_dir() {
    local name="$1"
    local dir="${TMP_DIR}/extract/${name}"
    rm -rf "$dir"
    ensure_dir "$dir"
    echo "$dir"
}

# Clean up the extraction directory used by the current operation
cleanup_extract_dir() {
    local dir="$1"
    [ -d "$dir" ] && rm -rf "$dir" || true
}

# ── Installation logging ─────────────────────────────────────────────────────

# Print the installation target path in a consistent format
install_log() {
    local component="$1"
    local target="$2"
    info "  ✓ ${component} installed to: ${target}"
}

# ── Verification ─────────────────────────────────────────────────────────────

verify_cmd() {
    local name="$1" cmd="$2" flag="${3:---version}"
    substep "Verify $name ..."
    if "$cmd" "$flag" >/dev/null 2>&1; then
        info "  ✓ $name is available: $("$cmd" "$flag" 2>&1 | head -1)"
        return 0
    else
        die "  ✗ $name is not available; please check the installation"
    fi
}

# ── Activate toolchain PATH ──────────────────────────────────────────────────

source_ghcup_env() {
    if [ -f "${GHCUP_DIR}/env" ]; then
        source "${GHCUP_DIR}/env" 2>/dev/null || true
        return 0
    fi
    if [ -d "${GHCUP_DIR}/bin" ]; then
        export PATH="${GHCUP_DIR}/bin:${PATH}"
    fi
    return 0
}

activate_deps_path() {
    source_ghcup_env || true
    if [ -d "${DEPS_DIR}/node/bin" ]; then
        export PATH="${DEPS_DIR}/node/bin:${PATH}"
    fi
    if [ -d "${DEPS_DIR}/postgres/bin" ]; then
        export PATH="${DEPS_DIR}/postgres/bin:${PATH}"
    fi
    return 0
}

# ── Debug mode (triggered by build_all.sh --verbose) ─────────────────────────
# Child scripts inherit this automatically after sourcing utils.sh: if ARKHAM_TRACE=true, enable set -x

if [ "${ARKHAM_TRACE:-}" = "true" ]; then
    set -x
fi
