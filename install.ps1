# Wasome Installer for Windows

$ErrorActionPreference = "Stop"

# Configuration
$GithubRepo = "Dari-OS/Wasome"

$WasomeHome = $env:WASOME_HOME
if ([string]::IsNullOrWhiteSpace($WasomeHome)) {
    $WasomeHome = Join-Path $env:USERPROFILE ".wasome"
}
$WasomeBin = Join-Path $WasomeHome "bin"

# Define Target
$Target = "x86_64-pc-windows-msvc"

# Fetch latest version
Write-Host "Fetching latest Wasome release from $GithubRepo..."
try {
    $Release = Invoke-RestMethod -Uri "https://api.github.com/repos/$GithubRepo/releases/latest"
    $Version = $Release.tag_name
} catch {
    Write-Error "Failed to fetch latest release. Please check your internet connection or GitHub API limits."
    exit 1
}

$DownloadUrl = "https://github.com/$GithubRepo/releases/download/$Version/wasome-$Target.zip"
$ZipPath = Join-Path $env:TEMP "wasome.zip"

Write-Host "Downloading Wasome $Version from $DownloadUrl..."
Invoke-WebRequest -Uri $DownloadUrl -OutFile $ZipPath

if (!(Test-Path $WasomeHome)) {
    New-Item -ItemType Directory -Force -Path $WasomeHome | Out-Null
}

if (Test-Path (Join-Path $WasomeHome "bin")) {
    Write-Host "Updating existing installation at $WasomeHome..."
} else {
    Write-Host "Installing to $WasomeHome..."
}

$TempDir = Join-Path $env:TEMP "wasome_update_$([guid]::NewGuid().ToString().Substring(0,8))"
New-Item -ItemType Directory -Force -Path $TempDir | Out-Null
Expand-Archive -Path $ZipPath -DestinationPath $TempDir -Force
Remove-Item $ZipPath

# Safely replace bin
$NewBin = Join-Path $TempDir "bin"
$OldBin = Join-Path $WasomeHome "bin"
if (Test-Path $NewBin) {
    if (Test-Path $OldBin) { Remove-Item -Path $OldBin -Recurse -Force }
    Move-Item -Path $NewBin -Destination $WasomeHome
}

# Dynamically replace std contents without removing user's custom stds
$NewStd = Join-Path $TempDir "std"
$OldStd = Join-Path $WasomeHome "std"
if (Test-Path $NewStd) {
    if (!(Test-Path $OldStd)) { New-Item -ItemType Directory -Force -Path $OldStd | Out-Null }
    Get-ChildItem -Path $NewStd | ForEach-Object {
        $TargetItem = Join-Path $OldStd $_.Name
        if (Test-Path $TargetItem) { Remove-Item -Path $TargetItem -Recurse -Force }
        Move-Item -Path $_.FullName -Destination $OldStd
    }
}

# Safely merge lib (preserving existing user-downloaded LLVM binaries)
$NewLib = Join-Path $TempDir "lib"
$OldLib = Join-Path $WasomeHome "lib"
if (Test-Path $NewLib) {
    if (!(Test-Path $OldLib)) { New-Item -ItemType Directory -Force -Path $OldLib | Out-Null }
    Get-ChildItem -Path $NewLib | ForEach-Object {
        $TargetItem = Join-Path $OldLib $_.Name
        if (Test-Path $TargetItem) { Remove-Item -Path $TargetItem -Recurse -Force }
        Move-Item -Path $_.FullName -Destination $OldLib
    }
}

Remove-Item -Path $TempDir -Recurse -Force

# ---------------------------------------------------------
# TODO: Install precompiled LLVM/LLD binaries
# ---------------------------------------------------------
# The CD pipeline creates an empty `lib` folder.
# Download your Windows LLVM/LLD binaries here 
# and extract them into "$WasomeHome\lib".
# 
# Example:
# Write-Host "Downloading LLVM binaries..."
# $LlvmUrl = "https://your-llvm-source.com/llvm-${Target}.zip"
# $LlvmZip = Join-Path $env:TEMP "llvm.zip"
# Invoke-WebRequest -Uri $LlvmUrl -OutFile $LlvmZip
# Expand-Archive -Path $LlvmZip -DestinationPath "$WasomeHome\lib" -Force
# Remove-Item $LlvmZip
# ---------------------------------------------------------

# Setup Environment Variables
Write-Host "Configuring Environment Variables..."

# Set WASOME_HOME
[Environment]::SetEnvironmentVariable("WASOME_HOME", $WasomeHome, [EnvironmentVariableTarget]::User)
Write-Host "Set WASOME_HOME to $WasomeHome"

# Update PATH
$UserPath = [Environment]::GetEnvironmentVariable("PATH", [EnvironmentVariableTarget]::User)
if ($UserPath -notlike "*$WasomeBin*") {
    $NewPath = "$UserPath;$WasomeBin"
    [Environment]::SetEnvironmentVariable("PATH", $NewPath, [EnvironmentVariableTarget]::User)
    Write-Host "Added $WasomeBin to your user PATH"
} else {
    Write-Host "WASOME_HOME\bin is already in your PATH."
}

Write-Host ""
Write-Host "Wasome installed/updated successfully!"
Write-Host "Please restart your PowerShell or Command Prompt terminal to start using the 'waso' command."