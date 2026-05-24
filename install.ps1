# Wasome Installer for Windows

$ErrorActionPreference = "Stop"

# Determine WASOME_HOME
$WasomeHome = $env:WASOME_HOME
if ([string]::IsNullOrWhiteSpace($WasomeHome)) {
    $WasomeHome = Join-Path $env:USERPROFILE ".wasome"
}
$WasomeBin = Join-Path $WasomeHome "bin"

# Define Target
$Target = "x86_64-pc-windows-msvc"

# Fetch latest version
Write-Host "Fetching latest Wasome release..."
try {
    $Release = Invoke-RestMethod -Uri "https://api.github.com/repos/Dari-OS/Wasome/releases/latest"
    $Version = $Release.tag_name
} catch {
    Write-Error "Failed to fetch latest release. Please check your internet connection or GitHub API limits."
    exit 1
}

$DownloadUrl = "https://github.com/Dari-OS/Wasome/releases/download/$Version/wasome-$Target.zip"
$ZipPath = Join-Path $env:TEMP "wasome.zip"

Write-Host "Downloading Wasome $Version from $DownloadUrl..."
Invoke-WebRequest -Uri $DownloadUrl -OutFile $ZipPath

if (Test-Path (Join-Path $WasomeHome "bin")) {
    Write-Host "Updating existing installation at $WasomeHome..."
    # Clean up old core directories to prevent stale files, but LEAVE lib/ intact!
    $DirsToClean = @("bin", "std")
    foreach ($Dir in $DirsToClean) {
        $DirPath = Join-Path $WasomeHome $Dir
        if (Test-Path $DirPath) {
            Remove-Item -Path $DirPath -Recurse -Force
        }
    }
} else {
    Write-Host "Installing to $WasomeHome..."
    if (!(Test-Path $WasomeHome)) {
        New-Item -ItemType Directory -Force -Path $WasomeHome | Out-Null
    }
}

Expand-Archive -Path $ZipPath -DestinationPath $WasomeHome -Force
Remove-Item $ZipPath

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