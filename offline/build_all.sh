#!/usr/bin/env bash
# =============================================================================
# build_all.sh - Arkham Horror LCG offline build entry script
#
# Usage:
#   cd offline && chmod +x build_all.sh scripts/*.sh
#   ./build_all.sh                  # Full build
#   ./build_all.sh --skip-deps      # Skip dependency installation
#   ./build_all.sh --clean          # Force a clean rebuild
#   ./build_all.sh --help           # Show help
#
# Flow:
#   System tool check → Project dependency installation → Verification → Frontend build → Backend build → Packaging
#
# CI invokes the numbered scripts directly; see .github/workflows/build-offline.yml.
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ── Argument parsing ──────────────────────────────────────────────────────────

SKIP_DEPS=false
SKIP_FRONTEND=false
SKIP_BACKEND=false
SKIP_PACKAGE=false
CLEAN_BUILD=false
VERBOSE=false
HELP=false

usage() {
    cat << EOF
Usage: ./build_all.sh [options]

Options:
  --skip-deps       Skip dependency installation (assume dependencies are already installed)
  --skip-frontend   Skip frontend build
  --skip-backend    Skip backend build
  --skip-package    Skip packaging
  --clean           Remove build stamps and force a full rebuild
  --verbose         Print every executed shell command (set -x debug mode)
  --help            Show this help

Examples:
  ./build_all.sh                     # Full build
  ./build_all.sh --skip-deps         # Build and package only
  ./build_all.sh --clean             # Force a clean rebuild
  ./build_all.sh --verbose           # Print all shell commands
EOF
}

for arg in "$@"; do
    case "$arg" in
        --skip-deps)     SKIP_DEPS=true ;;
        --skip-frontend) SKIP_FRONTEND=true ;;
        --skip-backend)  SKIP_BACKEND=true ;;
        --skip-package)  SKIP_PACKAGE=true ;;
        --clean)         CLEAN_BUILD=true ;;
        --verbose)       VERBOSE=true ;;
        --help)          HELP=true ;;
        *)               echo "Unknown option: $arg"; usage; exit 1 ;;
    esac
done

if [ "$HELP" = true ]; then
    usage; exit 0
fi

# ── Debug mode (--verbose) ───────────────────────────────────────────────────
# Export an environment variable so child scripts enable set -x automatically when sourcing utils.sh

if [ "$VERBOSE" = true ]; then
    export ARKHAM_TRACE=true
    set -x
fi

# ── Load shared helpers ───────────────────────────────────────────────────────

source "${SCRIPT_DIR}/scripts/utils.sh"
init_paths

OS="$(detect_os)"
ARCH="$(detect_arch)"
PLATFORM="$(detect_platform)"

# ── Cleanup ───────────────────────────────────────────────────────────────────

do_clean() {
    step "Cleaning build stamps"
    rm -f "${DEPS_DIR}"/stamp_*
    info "Cleanup complete (dependency files kept, only build stamps removed)"
}

# ── System tool check ────────────────────────────────────────────────────────

check_system_tools() {
    step "Checking basic system tools"
    local tools=("curl" "tar" "gzip" "unzip" "make")
    local missing=()

    for tool in "${tools[@]}"; do
        if has_cmd "$tool"; then
            substep "  ✓ $tool"
        else
            substep "  ✗ $tool (missing)"
            missing+=("$tool")
        fi
    done

    if [ ${#missing[@]} -gt 0 ]; then
        echo ""
        warn "Missing system tools: ${missing[*]}"
        case "$OS" in
            macos) warn "Install with Homebrew: brew install ${missing[*]}" ;;
            linux) warn "Install with apt: sudo apt install -y ${missing[*]}" ;;
        esac
        ! has_cmd "curl" && die "curl is required"
        ! has_cmd "tar"  && die "tar is required"
    fi

    info "System tool check complete"
}

# ── Main flow ─────────────────────────────────────────────────────────────────

main() {
    echo ""
    echo "  ╔══════════════════════════════════════════════════╗"
    echo "  ║   Arkham Horror LCG - Offline Build System       ║"
    echo "  ╚══════════════════════════════════════════════════╝"
    echo ""
    info "Build platform: $PLATFORM"
    info "Project root:   $PROJECT_ROOT"
    info "Output dir:     $_DIST_DIR"
    echo ""

    ensure_dir "${DEPS_DIR}"
    ensure_dir "${_DIST_DIR}"

    if [ "$CLEAN_BUILD" = true ]; then
        do_clean
    fi

    # [0] System tools
    check_system_tools
    echo ""

    # [1] Project dependencies
    if [ "$SKIP_DEPS" = false ]; then
        bash "${SCRIPT_DIR}/scripts/01-check-project-deps.sh"
    else
        step "Skipping dependency installation (--skip-deps)"
        activate_deps_path
    fi
    echo ""

    # [2] Verification
    bash "${SCRIPT_DIR}/scripts/02-verify-deps.sh"
    echo ""

    # [3] Frontend
    if [ "$SKIP_FRONTEND" = false ]; then
        bash "${SCRIPT_DIR}/scripts/03-build-frontend.sh"
    else
        step "Skipping frontend build (--skip-frontend)"
    fi
    echo ""

    # [4] Backend
    if [ "$SKIP_BACKEND" = false ]; then
        bash "${SCRIPT_DIR}/scripts/04-build-backend.sh"
    else
        step "Skipping backend build (--skip-backend)"
    fi
    echo ""

    # [5] Packaging
    if [ "$SKIP_PACKAGE" = false ]; then
        bash "${SCRIPT_DIR}/scripts/05-package.sh"
    else
        step "Skipping packaging (--skip-package)"
    fi
    echo ""

    # ── Done ──────────────────────────────────────────────────────────────────

    echo ""
    echo "  ╔══════════════════════════════════════════════════╗"
    echo "  ║   ✅ Build Complete!                             ║"
    echo "  ╚══════════════════════════════════════════════════╝"
    echo ""
    if [ -f "${_DIST_DIR}/ArkhamHorror-${PLATFORM}.tar.gz" ]; then
        info "Distribution archive: ${_DIST_DIR}/ArkhamHorror-${PLATFORM}.tar.gz"
    fi
    echo ""
    info "Usage:"
    echo "  1. Extract the tar.gz archive"
    echo "  2. cd ArkhamHorror-${PLATFORM} && ./start.sh"
    echo "  3. Open http://localhost:3000 in your browser"
    echo "  4. Stop with Ctrl+C or ./stop.sh"
    echo ""
}

main "$@"
