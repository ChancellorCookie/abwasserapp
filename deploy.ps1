# ============================================
# ZE1 Report Generator - Deployment auf 10.89.11.30
# ============================================

# Prüfe SSH-Verbindung
$server = "10.89.11.30"
$user = "administrator"
$password = "water123"

# Daten vorbereiten: Kopiere initiale Daten in ./data/
$dataDir = ".\data\R_Viewer"
New-Item -ItemType Directory -Force -Path "$dataDir\Calibration" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\QC" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\Config" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\Config\Backups" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\ICP-OES" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\Multielement" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\Pt-Spezies" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\Platin Gesamt" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\Bromid Bromat" | Out-Null
New-Item -ItemType Directory -Force -Path "$dataDir\gc-ms-fid" | Out-Null
New-Item -ItemType Directory -Force -Path ".\logs\shiny-server" | Out-Null

Write-Host "==> Kopiere Konfigurationsdaten..."
Copy-Item -Path ".\myShinyApps\R_Viewer\Calibration\*" -Destination "$dataDir\Calibration\" -Recurse -Force
Copy-Item -Path ".\myShinyApps\R_Viewer\QC\*" -Destination "$dataDir\QC\" -Recurse -Force
Copy-Item -Path ".\myShinyApps\R_Viewer\Config\defaults.csv" -Destination "$dataDir\Config\" -Force
Copy-Item -Path ".\myShinyApps\R_Viewer\Config\defaults.sqlite" -Destination "$dataDir\Config\" -Force -ErrorAction SilentlyContinue
Copy-Item -Path ".\myShinyApps\R_Viewer\R_Viewer.usr" -Destination "$dataDir\" -Force
Copy-Item -Path ".\myShinyApps\R_Viewer\R_Viewer.pw" -Destination "$dataDir\" -Force

Write-Host "==> Erstelle leere Log-Dateien..."
"" | Out-File -FilePath "$dataDir\log.usr" -Encoding ascii
"" | Out-File -FilePath "$dataDir\defaults.log" -Encoding ascii

Write-Host ""
Write-Host "============================================"
Write-Host "Bereit für Deployment auf $server"
Write-Host "============================================"
Write-Host ""
Write-Host "Manuell auszufuehren:"
Write-Host "  1. Dateien per SCP kopieren:"
Write-Host "     scp -r .\* administrator@$server:~/abwasserapp/"
Write-Host ""
Write-Host "  2. Auf dem Server per SSH:"
Write-Host "     ssh administrator@$server"
Write-Host "     cd ~/abwasserapp"
Write-Host "     docker-compose up -d --build"
Write-Host ""
Write-Host "  3. Pruefen:"
Write-Host "     http://$server:3838/myShinyApps/R_Viewer/"
Write-Host "     http://$server:3838/myShinyApps/QC_Viewer_org/"
Write-Host "============================================"
