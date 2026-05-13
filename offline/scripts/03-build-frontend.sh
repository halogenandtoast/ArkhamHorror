#!/usr/bin/env bash
# =============================================================================
# 03-build-frontend.sh - Build frontend
# Run npm install and the build in the frontend/ directory
# Output build artifacts to offline/_dist/frontend/ without polluting the main project tree
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/utils.sh"

init_paths
activate_deps_path

FRONTEND_DIR="${PROJECT_ROOT}/frontend"
FRONTEND_OUTPUT="${DEPS_DIR}/frontend"        # Unified output location: offline/_deps/frontend/
FRONTEND_BUILT_MARKER="${DEPS_DIR}/stamp_frontend_built"

# ── Compute a hash of frontend source contents (to decide whether a rebuild is needed) ─
# Includes: all files under src/ + package-lock.json + the index.html template
# Excludes: node_modules/ (only dependency declarations matter, not installed files)
compute_frontend_hash() {
    (
        cd "$FRONTEND_DIR"
        # Use find | sort to keep a stable order, then hash each file
        find src -type f | sort | xargs sha256sum 2>/dev/null
        # A package-lock.json change means dependency declarations may have changed
        sha256sum package-lock.json 2>/dev/null || true
        # Rebuild when the index.html template changes as well
        sha256sum index.html 2>/dev/null || true
    ) | sha256sum | cut -d' ' -f1
}

# ── Build frontend ────────────────────────────────────────────────────────────

build_frontend() {
    step "Building frontend (→ ${FRONTEND_OUTPUT})"

    # ── Decide whether a rebuild is needed based on the content hash ─────────
    local current_hash
    current_hash="$(compute_frontend_hash)"

    if [ -f "$FRONTEND_BUILT_MARKER" ]; then
        local stored_hash
        stored_hash="$(cat "$FRONTEND_BUILT_MARKER" 2>/dev/null || echo '')"
        if [ "$current_hash" = "$stored_hash" ] && [ -d "$FRONTEND_OUTPUT" ] && [ -f "${FRONTEND_OUTPUT}/index.html" ]; then
            info "Frontend source unchanged (hash matches), skipping build"
            return 0
        fi
        info "Frontend source changed; rebuild required"
    fi

    # CI cache hit: if artifacts exist but the stamp does not, we still need to validate the source hash
    # If a hash record exists in the cache and matches, the artifacts are still valid
    local hash_record="${FRONTEND_OUTPUT}/source_hash"
    if [ -d "$FRONTEND_OUTPUT" ] && [ -f "${FRONTEND_OUTPUT}/index.html" ]; then
        if [ -f "$hash_record" ] && [ "$(cat "$hash_record" 2>/dev/null)" = "$current_hash" ]; then
            info "Frontend artifacts already exist and the source is unchanged (CI cache hit), skipping build"
            echo "$current_hash" > "$FRONTEND_BUILT_MARKER"
            return 0
        fi
        # Artifacts exist but source changed, so rebuild is required
        info "Frontend artifacts are stale (source hash mismatch); rebuilding"
        rm -rf "$FRONTEND_OUTPUT"
    fi

    if [ ! -d "$FRONTEND_DIR" ]; then
        die "Frontend directory does not exist: $FRONTEND_DIR"
    fi

    # ── Decision 6: place node_modules under _deps/ and expose it to frontend/ through a symlink ─
    NM_LINK="${FRONTEND_DIR}/node_modules"
    NM_REAL="${DEPS_DIR}/node_modules"
    NM_WAS_REAL_DIR=false

    if [ -L "$NM_LINK" ]; then
        info "node_modules → ${NM_REAL} (symlink already exists)"
    elif [ -d "$NM_LINK" ]; then
        substep "Migrating existing node_modules/ to ${NM_REAL} (Decision 6) ..."
        ensure_dir "$NM_REAL"
        cp -r "$NM_LINK"/* "$NM_REAL"/ 2>/dev/null || true
        rm -rf "$NM_LINK"
        NM_WAS_REAL_DIR=true
        ensure_dir "$NM_REAL"
        ln -sfn "$NM_REAL" "$NM_LINK"
        info "  ✓ node_modules → ${NM_REAL}"
    else
        ensure_dir "$NM_REAL"
        ln -sfn "$NM_REAL" "$NM_LINK"
        info "node_modules → ${NM_REAL} (symlink created)"
    fi

    # Register cleanup on exit: remove the symlink and restore helpers.ts
    _HELPERS_TS="${FRONTEND_DIR}/src/arkham/helpers.ts"
    _HELPERS_BAK="${FRONTEND_DIR}/src/arkham/helpers.ts.bak_$$"
    cleanup_nm_symlink() {
        if [ -L "$NM_LINK" ]; then
            rm -f "$NM_LINK"
            if [ "$NM_WAS_REAL_DIR" = true ]; then
            warn "The original node_modules/ was moved to ${NM_REAL} and will not be restored automatically"
            fi
        fi
        [ -f "$_HELPERS_BAK" ] && mv -f "$_HELPERS_BAK" "$_HELPERS_TS" 2>/dev/null || true
    }
    trap cleanup_nm_symlink EXIT

    pushd "$FRONTEND_DIR" > /dev/null

    # 1. Install dependencies
    if [ -f "package-lock.json" ]; then
        substep "npm ci (the first run may need 2-5 minutes to download dependencies) ..."
        info "Running: npm ci --prefer-offline"
        if npm ci --prefer-offline 2>&1 | while IFS= read -r line; do
            echo "    $line"
        done; then
            info "  ✓ npm ci succeeded"
        else
            warn "  ! npm ci failed; falling back to npm install (keeping node_modules to avoid re-downloading) ..."
            info "Running: npm install"
            npm install 2>&1 | while IFS= read -r line; do
                echo "    $line"
            done
        fi
    else
        substep "npm install (the first run may need 2-5 minutes to download dependencies) ..."
        info "Running: npm install"
        npm install 2>&1 | while IFS= read -r line; do
            echo "    $line"
        done
    fi

    # ── Temporary patch: helpers.ts hard-codes a CDN URL in production ───────
    # The offline package needs relative paths; prefer VITE_ASSET_HOST first and fall back to the CDN when unset
    cp "$_HELPERS_TS" "$_HELPERS_BAK"
    substep "Patching helpers.ts: use VITE_ASSET_HOST in production (fall back to the CDN if unset)"
    sed -i.bak "s|export const baseUrl = import.meta.env.PROD ? \"https://assets.arkhamhorror.app\" : ''|export const baseUrl = import.meta.env.PROD ? (import.meta.env.VITE_ASSET_HOST ?? \"https://assets.arkhamhorror.app\") : ''|" "$_HELPERS_TS" && rm -f "${_HELPERS_TS}.bak"

    # 2. Build and output to offline/_dist/frontend/
    substep "npm run build (output to ${FRONTEND_OUTPUT}) ..."

    # Set VITE_ASSET_HOST="" so both images use relative paths during the frontend build
    export VITE_ASSET_HOST=""

    ensure_dir "$FRONTEND_OUTPUT"

    # Try to write directly to the target directory through Vite CLI --outDir
    info "Running: npm run build -- --outDir ${FRONTEND_OUTPUT}"
    if npm run build -- --outDir "${FRONTEND_OUTPUT}" 2>&1 | while IFS= read -r line; do
        echo "    $line"
    done; then
        info "  ✓ Wrote output directly to ${FRONTEND_OUTPUT}"
    else
        # Fallback: build in place, then copy
        warn "  --outDir did not take effect; building in place and copying instead ..."
        info "Running: npm run build"
        npm run build 2>&1 | while IFS= read -r line; do
            echo "    $line"
        done
        if [ -d "dist" ]; then
            cp -r dist/* "${FRONTEND_OUTPUT}/"
            info "  ✓ Copied to ${FRONTEND_OUTPUT}"
        else
            die "  ✗ Frontend build failed: dist/ directory does not exist"
        fi
    fi

    popd > /dev/null

    # 3. Verify artifacts
    if [ -d "$FRONTEND_OUTPUT" ]; then
        local dist_files
        dist_files="$(find "${FRONTEND_OUTPUT}" -type f | wc -l | tr -d ' ')"
        info "  ✓ Frontend build finished, produced ${dist_files} files → ${FRONTEND_OUTPUT}/"

        if [ -f "${FRONTEND_OUTPUT}/index.html" ]; then
            info "  ✓ index.html generated"
        else
            warn "  ! index.html not found; please check the frontend build configuration"
        fi
    else
        die "  ✗ Frontend build failed: ${FRONTEND_OUTPUT} does not exist"
    fi

    # 4. Inject the CDN image detection script (only into dist/index.html, without touching source files)
    local index_html="${FRONTEND_OUTPUT}/index.html"
    if [ -f "$index_html" ]; then
        substep "Injecting CDN image cloud-badge detection script ..."
        local js_file="${TMP_DIR}/cdn-detector.js"
        cat > "$js_file" << 'CDNJS'
(function(){
  var cloudSVG='<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17.5 19H9a7 7 0 1 1 6.71-9h1.79a4.5 4.5 0 1 1 0 9Z"/></svg>';
  var cdnPaths=new Set();
  var cdnBase="https://assets.arkhamhorror.app";

  // Extract pathname from a full URL so different URL formats share the same matching key
  function getPath(url){
    var a=document.createElement("a");
    a.href=url;
    return a.pathname;
  }

  function addBadge(img,cdnUrl){
    if(img.dataset.cdnBadged)return;
    img.dataset.cdnBadged="1";
    // Wrap the image in a span so the badge is positioned relative to the image instead of the parent container
    var wrap=document.createElement("span");
    wrap.style.position="relative";
    wrap.style.display="inline-block";
    wrap.style.lineHeight="0";
    var cs=getComputedStyle(img);
    if(cs.verticalAlign)wrap.style.verticalAlign=cs.verticalAlign;
    img.parentNode.insertBefore(wrap,img);
    wrap.appendChild(img);
    var b=document.createElement("span");
    b.innerHTML=cloudSVG;
    b.title="CDN resource: click to view original\n"+cdnUrl;
    b.setAttribute("style","position:absolute;top:4px;right:4px;width:22px;height:22px;background:rgba(255,255,255,0.82);border-radius:4px;cursor:pointer;display:flex;align-items:center;justify-content:center;color:#555;font-size:12px;z-index:100;box-shadow:0 1px 3px rgba(0,0,0,0.12)");
    b.onclick=function(e){e.stopPropagation();window.open(cdnUrl,"_blank")};
    wrap.appendChild(b);
  }

  function scanAll(){
    document.querySelectorAll("img").forEach(function(img){
      var path=getPath(img.src);
      if(cdnPaths.has(path))addBadge(img,cdnBase+path);
    });
  }

  try{
    var po=new PerformanceObserver(function(list){
      list.getEntries().forEach(function(e){
        var path=getPath(e.name);
        // Only handle resources under /img/ (exclude CSS/JS/fonts and other non-image resources)
        if(path.indexOf("/img/")===-1)return;
        if(!e.serverTiming)return;
        var hit=false;
        for(var i=0;i<e.serverTiming.length;i++){
          if(e.serverTiming[i].name==="cdn"){hit=true;break}
        }
        if(!hit)return;
        cdnPaths.add(path);
        scanAll();
      });
    });
    po.observe({type:"resource",buffered:true});

    var mo=new MutationObserver(function(mutations){
      mutations.forEach(function(m){
        m.addedNodes.forEach(function(node){
          if(node.tagName==="IMG"){
            var path=getPath(node.src);
            if(cdnPaths.has(path))addBadge(node,cdnBase+path);
          }
          if(node.querySelectorAll){
            node.querySelectorAll("img").forEach(function(img){
              var path=getPath(img.src);
              if(cdnPaths.has(path))addBadge(img,cdnBase+path);
            });
          }
        });
      });
    });
    mo.observe(document.documentElement,{childList:true,subtree:true});
    // Fallback scan: wait 3 seconds so asynchronously loaded images are also processed
    setTimeout(scanAll,3000);
  }catch(e){}
})();
CDNJS
        python3 - "$index_html" "$js_file" << 'PYEOF'
import sys
html = open(sys.argv[1], 'r').read()
js = open(sys.argv[2], 'r').read()
html = html.replace('</body>', '<script>' + js + '</script></body>')
open(sys.argv[1], 'w').write(html)
PYEOF
        rm -f "$js_file"
        info "  ✓ CDN detection script injected"
    fi

    echo "$current_hash" > "$FRONTEND_BUILT_MARKER"
    # Write the hash into the artifact directory so CI cache restores can validate artifact/source consistency
    echo "$current_hash" > "${FRONTEND_OUTPUT}/source_hash"
    info "Frontend build complete"
}

# ── Main flow ─────────────────────────────────────────────────────────────────

main() {
    build_frontend
}

main "$@"
