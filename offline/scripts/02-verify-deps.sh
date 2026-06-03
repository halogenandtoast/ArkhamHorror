#!/usr/bin/env bash
# =============================================================================
# 02-verify-deps.sh - Thoroughly verify that all dependencies are usable
# Check one by one whether GHC, Stack, Node, npm, and PostgreSQL work correctly
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"

init_paths
activate_deps_path

OS="$(detect_os)"
PLATFORM="$(detect_platform)"

# ── Verification helpers (with minimal functional tests) ─────────────────────

# Verify GHC: compile and run a simple Haskell program
verify_ghc_functional() {
    substep "GHC functional test: compile Hello World ..."

    local tmpdir
    tmpdir="$(mktemp -d 2>/dev/null || echo "${TMP_DIR}/ghc_test_$$")"
    ensure_dir "$tmpdir"
    local test_file="${tmpdir}/Test.hs"

    cat > "$test_file" << 'EOF'
main :: IO ()
main = putStrLn "ghc-functional-test-ok"
EOF

    if ghc -outputdir "$tmpdir" -o "${tmpdir}/test" "$test_file" >/dev/null 2>&1; then
        local output
        output="$("${tmpdir}/test")"
        if [ "$output" = "ghc-functional-test-ok" ]; then
            info "  ✓ GHC compilation and execution succeeded"
        else
            die "  ✗ GHC produced incorrect output: $output"
        fi
    else
        die "  ✗ GHC compilation failed"
    fi

    rm -rf "$tmpdir"
}

# Verify Stack: confirm it can resolve the project correctly
verify_stack_functional() {
    substep "Stack functional test: resolve project config ..."

    if [ ! -f "${PROJECT_ROOT}/backend/stack.yaml" ]; then
        warn "  ! backend/stack.yaml not found; skipping Stack project verification"
        return 0
    fi

    pushd "${PROJECT_ROOT}/backend" > /dev/null
    if stack --no-terminal path --project-root >/dev/null 2>&1; then
        info "  ✓ Stack resolved the project correctly"
    else
        warn "  ! Stack had trouble resolving the project (system C libraries may be missing, or project files may be incomplete)"
        warn "    If the later build fails, please check for missing system dependencies"
    fi
    popd > /dev/null
}

# Verify Node.js: execute a simple JS snippet
verify_node_functional() {
    substep "Node.js functional test: eval ..."

    if node -e 'console.log("node-functional-test-ok")' 2>&1 | grep -q "node-functional-test-ok"; then
        info "  ✓ Node.js executed successfully"
    else
        die "  ✗ Node.js execution failed"
    fi
}

# Verify npm: confirm the command works normally
verify_npm_functional() {
    substep "npm functional test: version ..."

    if npm --version >/dev/null 2>&1; then
        info "  ✓ npm is available"
    else
        die "  ✗ npm is unavailable"
    fi
}

# Verify PostgreSQL: version and file integrity
verify_postgres_functional() {
    substep "PostgreSQL integrity check ..."

    local pg_bin="${DEPS_DIR}/postgres/bin"

    # Check that the required binaries are present
    local required_bins=("postgres" "initdb" "pg_ctl")
    local all_ok=true

    for bin in "${required_bins[@]}"; do
        if [ -x "${pg_bin}/${bin}" ]; then
            :
        else
            warn "  ✗ Missing: ${pg_bin}/${bin}"
            all_ok=false
        fi
    done

    if [ "$all_ok" = true ]; then
        info "  ✓ PostgreSQL core binaries are complete"
    else
        die "  ✗ PostgreSQL installation is incomplete"
    fi
}

# ── Port check (used by the packaging script as a reference) ─────────────────

# Check whether PostgreSQL default port 5433 is already in use
check_port_available() {
    local port="${1:-5433}"
    substep "Checking port $port ..."

    case "$OS" in
        macos)
            if lsof -i ":$port" -sTCP:LISTEN >/dev/null 2>&1; then
                warn "  ! Port $port is already in use; the packaged start.sh will use a fallback port"
            else
                info "  ✓ Port $port is available"
            fi
            ;;
        linux)
            if ss -tlnp "sport = :$port" 2>/dev/null | grep -q ":$port"; then
                warn "  ! Port $port is already in use; the packaged start.sh will use a fallback port"
            else
                info "  ✓ Port $port is available"
            fi
            ;;
    esac
}

# ── Main flow ─────────────────────────────────────────────────────────────────

main() {
    step "Functional verification for all dependencies"
    echo ""
    info "Platform: $PLATFORM"
    echo ""

    # Version checks
    verify_cmd "GHC"         "ghc"     "--version"
    verify_cmd "Stack"       "stack"   "--version"
    verify_cmd "Node.js"     "node"    "--version"
    verify_cmd "npm"         "npm"     "--version"
    verify_cmd "PostgreSQL"  "postgres" "--version"

    echo ""

    # Functional checks
    verify_ghc_functional
    verify_stack_functional
    verify_node_functional
    verify_npm_functional
    verify_postgres_functional

    echo ""
    check_port_available 5433
    echo ""

    info "All checks passed"
    echo ""
}

main "$@"
