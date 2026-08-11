# uRos programme PDF workflow - R version

This is the R equivalent of the Markdown-to-PDF programme renderer. It is designed for the structure used by the uRos programme: one document title, day headings, schedule tables, session headings, and presentation lists.

## Requirements

- R 4.2 or newer
- Pandoc (RStudio includes Pandoc; otherwise install Quarto or Pandoc separately)
- Chrome or Chromium
- R packages: `rmarkdown`, `knitr`, `pagedown`

Install the R dependencies with:

```r
Rscript install_packages.R
```

## Render

```bash
Rscript programme_pdf.R programme.md programme.pdf
```

or:

```bash
./render.sh programme.md programme.pdf
```

The script expects `programme.css` next to `programme_pdf.R`.

## Markdown convention

```markdown
# uRos 2026 Programme

Paris · 18–20 November 2026

## Thursday, 19 November 2026 — Conference Day 1

| Time | Room 1 | Room 2 | Room 3 |
|---|---|---|---|
| 11:00–12:30 | A1 — AI Assistants | B1 — Pipelines | C1 — Publishing |

### A1 — AI Assistants for Statistical Production

- **Presentation title** — Presenter Name
- **Another presentation title** — Presenter Name
```

Numbered lists are also accepted, which is useful for Lightning Talks. `### EMOS Presentations` is treated as a special session.

## How it works

`programme_pdf.R` deliberately does not use generic Markdown-to-PDF conversion. It first parses the programme semantics so that the output can distinguish:

- the masthead and conference dates,
- tutorial/conference days,
- overview schedule tables,
- A/B/C session tracks,
- presentation title and presenter,
- long Lightning Talk sessions,
- EMOS presentations.

It then generates a temporary `pagedown::html_paged` document and prints that document to PDF with `pagedown::chrome_print()`.

## Styling

Edit `programme.css` to change typography, margins, session colours, spacing, table appearance, columns, or page numbering. The Markdown itself remains content-only.

The current CSS uses A4 pages and two-column session listings. Long Lightning Talk sessions automatically receive slightly smaller typography.

## GitHub Actions

The included workflow renders `programme.pdf` whenever the Markdown, R renderer, or CSS changes. The result is uploaded as a workflow artifact.
