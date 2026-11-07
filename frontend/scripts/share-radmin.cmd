@echo off
rem Double-click to share over Radmin VPN. Args: start (default) | rebuild | stop | status
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0share-radmin.ps1" %*
pause
