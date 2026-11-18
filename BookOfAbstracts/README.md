# Building the Book of Abstracts

From the repository root:

```sh
Rscript BookOfAbstracts/read_submissions.R
Rscript BookOfAbstracts/book_abstracts_pdf.R abstracts.csv uros2026_book_of_abstracts.pdf
python3 BookOfAbstracts/validate_book.py
```

Preparation requires the private `submissions_uRos.csv` export and the current programme in `README.md`. It regenerates `abstracts.csv`, the existing local email lists, and the editorial audit. The raw export and email lists are excluded from Git. CI renders from the committed `abstracts.csv`; regenerate and commit that file after changing the submission data or programme author lists.

The book includes keynotes, tutorials, regular presentations, lightning talks and the selected EMOS presentations. Existing explicit submission exclusions remain in effect. The preparation step preserves submitted co-authors and their numbered affiliation references while using uniquely matched programme names. It retains programme-only authors and reports any missing affiliation links.

Publication text is in `front_matter.html`, practical information in `practical_information.html`, and book styling in `abstracts.css`. Update the ISSN placeholder in the front matter when the official number is assigned. Editorial names and affiliations were checked against the publication website on 21 October 2026; the verification sources and unresolved items are in [REVIEW.md](REVIEW.md).

The alphabetical index includes all authors and co-authors, grouped by normalized displayed name; accents are preserved. Names are sorted as printed, without guessing family-name boundaries. Every page reference is generated from the final pagination and links to its abstract. Printed page numbers agree with PDF page positions; the cover counts as page 1 but hides its number.

The validator requires Python 3, `qpdf` and Poppler's `pdftotext`. It checks complete abstract text and affiliations, author presence, abstract destinations, hyperlink preservation, index coverage, and printed reference numbers against their PDF destinations. GitHub Actions runs this validation before committing regenerated PDFs. A passing validation establishes consistency with the inputs; it cannot resolve editorial gaps in those inputs.
