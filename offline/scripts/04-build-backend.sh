#!/usr/bin/env bash
# =============================================================================
# 04-build-backend.sh - Build backend
# Compile the Haskell project with Stack in the backend/ directory
# Artifact: _deps/arkham-api (executable)
# Redirect all stack-work directories into _deps/ through symlinks to avoid polluting the project tree
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"

init_paths
activate_deps_path

OS="$(detect_os)"

BACKEND_DIR="${PROJECT_ROOT}/backend"
BACKEND_BIN_OUTPUT="${DEPS_DIR}/arkham-api"
STACK_WORK_DIR="${DEPS_DIR}/stack-work"         # Root package stack-work under _deps/
STACK_WORK_SUB_DIR="${DEPS_DIR}/stack-work-sub" # Subpackage stack-work under _deps/stack-work-sub/<pkg>/
STACK_WORK_LINK="stack-work-local"              # Symlink name (STACK_WORK requires a pure relative path without / or ..)

# The three subpackage directories defined in stack.yaml
STACK_SUB_PACKAGES=("arkham-api" "cards-discover" "validate")

# ── Build backend ─────────────────────────────────────────────────────────────

build_backend() {
    step "Building backend (Haskell/GHC 9.12.2)"

    if [ ! -d "$BACKEND_DIR" ]; then
        die "Backend directory does not exist: $BACKEND_DIR"
    fi

    pushd "$BACKEND_DIR" > /dev/null
    export STACK_WORK="${STACK_WORK_LINK}"

    # ── Clean up legacy leftovers: backend/.stack-work (created when STACK_WORK was not set) ─
    if [ -d "${BACKEND_DIR}/.stack-work" ] && [ ! -L "${BACKEND_DIR}/.stack-work" ]; then
        substep "Cleaning legacy leftover: backend/.stack-work/ ..."
        rm -rf "${BACKEND_DIR}/.stack-work"
        info "  ✓ Cleaned backend/.stack-work/"
    fi

    # ── Root package stack-work symlink ──────────────────────────────────────
    ensure_dir "${STACK_WORK_DIR}"
    ln -sfn "${STACK_WORK_DIR}" "${BACKEND_DIR}/${STACK_WORK_LINK}"
    info "stack-work → ${STACK_WORK_DIR}"

    # ── Subpackage stack-work symlinks (STACK_WORK creates a same-named directory under each subpackage) ─
    for pkg in "${STACK_SUB_PACKAGES[@]}"; do
        local pkg_dir="${BACKEND_DIR}/${pkg}"
        local pkg_sw_link="${pkg_dir}/${STACK_WORK_LINK}"
        local pkg_sw_real="${STACK_WORK_SUB_DIR}/${pkg}"

        # Skip if the subpackage directory does not exist (for example, validate may not exist yet)
        [ -d "$pkg_dir" ] || continue

        ensure_dir "$pkg_sw_real"

        # Migrate an existing real directory (the first run may already have accumulated many build artifacts)
        if [ -d "$pkg_sw_link" ] && [ ! -L "$pkg_sw_link" ]; then
            substep "Migrating ${pkg}/${STACK_WORK_LINK}/ → ${pkg_sw_real}/ ..."
            cp -r "$pkg_sw_link"/* "$pkg_sw_real"/ 2>/dev/null || true
            rm -rf "$pkg_sw_link"
            info "  ✓ Migrated ${pkg} stack-work (real directory → symlink)"
        fi

        ln -sfn "$pkg_sw_real" "$pkg_sw_link"
        substep "${pkg}/${STACK_WORK_LINK} → ${pkg_sw_real}"
    done

    # Register exit cleanup: keep symlinks for future reuse and do only minimal cleanup
    cleanup_build() {
        # Keep the symlinks; later builds and incremental compilation can reuse them
        # The real data lives under _deps/, so data safety does not depend on the symlinks
        true
    }
    trap cleanup_build EXIT

    # ── Check system libraries required for Haskell package compilation ──────
    echo ""
    substep "Checking required system C libraries ..."

    # pg_config: use the PostgreSQL we built ourselves
    local pg_config_bin="${DEPS_DIR}/postgres/bin/pg_config"
    if [ -x "$pg_config_bin" ]; then
        export PATH="${DEPS_DIR}/postgres/bin:${PATH}"
        info "  ✓ pg_config available ($(pg_config --version 2>/dev/null))"
    else
        die "  ✗ pg_config missing: ${pg_config_bin} (please run 01-check-project-deps.sh first)"
    fi

    # pcre: required by the pcre-light package
    local pcre_ok=false
    if pcre-config --version >/dev/null 2>&1; then
        info "  ✓ pcre available ($(pcre-config --version 2>/dev/null))"
        pcre_ok=true
    elif [ -f /opt/homebrew/opt/pcre/lib/libpcre.dylib ] || [ -f /usr/local/opt/pcre/lib/libpcre.dylib ]; then
        info "  ✓ pcre installed (Homebrew)"
        pcre_ok=true
    elif ldconfig -p 2>/dev/null | grep -q libpcre; then
        info "  ✓ pcre installed (system)"
        pcre_ok=true
    fi
    if [ "$pcre_ok" = false ]; then
        echo ""
        warn "Missing pcre (required by the pcre-light Haskell package)"
        warn "  macOS: brew install pcre"
        warn "  Linux:  sudo apt install -y libpcre3-dev"
        die "Missing pcre"
    fi
    echo ""

    # Stack build (the first run downloads GHC and all Haskell dependencies and may take 10-30 minutes)
    echo ""
    info "Starting Stack build — the first run will download GHC and all Haskell dependencies"
    info "This may take 10-30 minutes; please be patient ..."
    echo ""

    # Build environment: consistently use _deps/postgres
    local stack_extra_flags="--extra-include-dirs=${DEPS_DIR}/postgres/include --extra-lib-dirs=${DEPS_DIR}/postgres/lib"
    export C_INCLUDE_PATH="${DEPS_DIR}/postgres/include:${C_INCLUDE_PATH:-}"
    export LIBRARY_PATH="${DEPS_DIR}/postgres/lib:${LIBRARY_PATH:-}"
    export PKG_CONFIG_PATH="${DEPS_DIR}/postgres/lib/pkgconfig:${PKG_CONFIG_PATH:-}"

    if [ "$OS" = "macos" ]; then
        # macOS: add Homebrew paths (system libraries such as pcre usually come from Homebrew)
        local brew_prefix=""
        if [ -d /opt/homebrew ]; then
            brew_prefix="/opt/homebrew"
        elif [ -d /usr/local ]; then
            brew_prefix="/usr/local"
        fi
        if [ -n "$brew_prefix" ]; then
            export C_INCLUDE_PATH="${brew_prefix}/include:${C_INCLUDE_PATH}"
            export LIBRARY_PATH="${brew_prefix}/lib:${LIBRARY_PATH}"
            stack_extra_flags="${stack_extra_flags} --extra-include-dirs=${brew_prefix}/include --extra-lib-dirs=${brew_prefix}/lib"
        fi
        substep "Build environment: _deps/postgres + Homebrew ${brew_prefix}"
        echo ""
    fi

    # Run Stack build
    local build_cmd="stack build --fast --ghc-options=\"-Wno-missing-home-modules\" ${stack_extra_flags}"
    info "Running: cd ${BACKEND_DIR} && ${build_cmd}"
    info "The first run downloads all Haskell dependencies and may take 10-30 minutes ..."
    echo ""

    if ${build_cmd} 2>&1; then
        info "Stack build succeeded"
    else
        warn "stack build --fast failed; trying a full build instead ..."
        local retry_cmd="stack build --ghc-options=\"-Wno-missing-home-modules\" ${stack_extra_flags}"
        info "Running: ${retry_cmd}"
        ${retry_cmd} 2>&1
    fi

    # Find the built artifact
    substep "Locating build artifact ..."
    local built_bin=""

    # Method 1: locate it through stack exec which
    for name in arkham-horror-web arkham-api arkham; do
        if stack exec -- which "$name" >/dev/null 2>&1; then
            built_bin="$(stack exec -- which "$name")"
            substep "Found: $built_bin"
            break
        fi
    done

    # Method 2: search under _deps/stack-work
    if [ -z "$built_bin" ]; then
        info "  Searching in ${STACK_WORK_DIR} ..."
        built_bin="$(find "${STACK_WORK_DIR}" -type f \( -name 'arkham*' -o -name 'validate' \) ! -name '*.o' ! -name '*.hi' ! -name '*.hs' 2>/dev/null | while read f; do [ -x "$f" ] && echo "$f" && break; done)"
    fi

    # Method 3: look under stack local-bin-path
    if [ -z "$built_bin" ]; then
        local stack_local_bin
        stack_local_bin="$(stack path --local-bin-path 2>/dev/null || echo '')"
        if [ -n "$stack_local_bin" ] && [ -d "$stack_local_bin" ]; then
            built_bin="$(find "$stack_local_bin" -type f -perm -a=x 2>/dev/null | head -1)"
            [ -n "$built_bin" ] && substep "Found: $built_bin"
        fi
    fi

    popd > /dev/null

    if [ -z "$built_bin" ] || [ ! -f "$built_bin" ]; then
        echo ""
        warn "Unable to locate the backend binary automatically. Please find and copy it manually:"
        warn "  1. cd ${BACKEND_DIR}"
        warn "  2. find ${STACK_WORK_DIR} -type f -perm -a=x"
        warn "  3. cp <binary-path> ${BACKEND_BIN_OUTPUT}"
        die "Backend binary not found"
    fi

    info "  ✓ Found backend binary: $(basename "$built_bin")"

    substep "Copying to ${BACKEND_BIN_OUTPUT} ..."
    cp "$built_bin" "$BACKEND_BIN_OUTPUT"
    chmod +x "$BACKEND_BIN_OUTPUT"

    if [ -x "$BACKEND_BIN_OUTPUT" ]; then
        local bin_size
        bin_size="$(du -h "$BACKEND_BIN_OUTPUT" | cut -f1)"
        info "  ✓ Backend binary ready: ${BACKEND_BIN_OUTPUT} (${bin_size})"
    else
        die "  ✗ Failed to copy backend binary"
    fi

    info "Backend build complete"
}

# ── Main flow ─────────────────────────────────────────────────────────────────

main() {
    build_backend
}

main "$@"
