$quartoDir = Join-Path $PSScriptRoot "..\\.quarto"

if (-not (Test-Path $quartoDir)) {
  exit 0
}

$cacheDirs = @(
  "project-cache",
  "idx",
  "listing"
)

foreach ($dir in $cacheDirs) {
  $target = Join-Path $quartoDir $dir
  if (Test-Path $target) {
    Remove-Item -Path $target -Recurse -Force -ErrorAction SilentlyContinue
  }
}

$previewLock = Join-Path $quartoDir "preview\\lock"
if (Test-Path $previewLock) {
  Remove-Item -Path $previewLock -Force -ErrorAction SilentlyContinue
}
