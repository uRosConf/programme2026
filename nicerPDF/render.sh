#!/usr/bin/env bash
set -euo pipefail
Rscript programme_pdf.R "${1:-programme.md}" "${2:-programme.pdf}"
