# Share this checkout's frontend with friends over Radmin VPN.
#
# Serves the production build on 0.0.0.0:$Port and proxies /api to the public
# server (see vite.remote.config.mjs), so friends use accounts on arkhamhorror.app.
#
# Usage (from anywhere):
#   powershell -ExecutionPolicy Bypass -File scripts\share-radmin.ps1 start     # build if needed, then serve
#   powershell -ExecutionPolicy Bypass -File scripts\share-radmin.ps1 rebuild   # force rebuild, then serve
#   powershell -ExecutionPolicy Bypass -File scripts\share-radmin.ps1 stop
#   powershell -ExecutionPolicy Bypass -File scripts\share-radmin.ps1 status
param(
  [ValidateSet('start', 'rebuild', 'stop', 'status')][string]$Action = 'start',
  [int]$Port = 8090
)

$ErrorActionPreference = 'Stop'
$frontend = Split-Path -Parent $PSScriptRoot
Set-Location $frontend

function Get-RadminIp {
  $a = Get-NetIPAddress -AddressFamily IPv4 -ErrorAction SilentlyContinue |
    Where-Object { $_.InterfaceAlias -like '*Radmin*' } | Select-Object -First 1
  if ($a) { $a.IPAddress } else { $null }
}

# Only the process listening on our port is ever touched.
function Get-ServerPid {
  $c = Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue | Select-Object -First 1
  if ($c) { $c.OwningProcess } else { $null }
}

function Stop-Server {
  $p = Get-ServerPid
  if ($p) { Stop-Process -Id $p -Force; Write-Host "Stopped server (PID $p) on port $Port." }
  else { Write-Host "Nothing listening on port $Port." }
}

$ip = Get-RadminIp

switch ($Action) {
  'stop' { Stop-Server; return }
  'status' {
    $p = Get-ServerPid
    if ($p) { Write-Host "RUNNING (PID $p): http://${ip}:$Port" } else { Write-Host 'NOT running.' }
    return
  }
}

if (-not $ip) { throw 'Radmin VPN adapter not found. Start Radmin VPN and join your network first.' }

if (Get-ServerPid) { Write-Host "Port $Port already in use; restarting."; Stop-Server; Start-Sleep -Seconds 1 }

if ($Action -eq 'rebuild' -or -not (Test-Path 'dist/index.html') -or -not (Test-Path 'dist/cards')) {
  Write-Host 'Building frontend...'
  node scripts/slim-cards.cjs | Out-Null
  npx vite build --config vite.remote.config.mjs
  if ($LASTEXITCODE -ne 0) { throw 'Build failed.' }
}

$rule = Get-NetFirewallRule -DisplayName "Arkham Radmin $Port" -ErrorAction SilentlyContinue
if (-not $rule) {
  Write-Host "NOTE: no firewall rule for port $Port. If friends cannot connect, run as admin:"
  Write-Host "  New-NetFirewallRule -DisplayName 'Arkham Radmin $Port' -Direction Inbound -Protocol TCP -LocalPort $Port -InterfaceAlias 'Radmin VPN' -Action Allow"
}

Write-Host ""
Write-Host "Send friends this link: http://${ip}:$Port" -ForegroundColor Green
Write-Host 'Press Ctrl+C (or close this window) to stop sharing.'
Write-Host ""
npx vite preview --config vite.remote.config.mjs --host 0.0.0.0 --port $Port --strictPort
