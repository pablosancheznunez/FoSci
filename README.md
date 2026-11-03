# forsci

Forensic scientometrics: signals in publication data that may deserve a closer look, kept apart from findings.

Status: **prototype**. Methods are validated on a simulated benchmark only; their error rates on real corpora are not yet known. Part of the [metascience](https://github.com/pablosancheznunez/metascience) ecosystem.

## Principles

- **An anomaly is not misconduct.** Every result carries an evidence level: `signal` → `pattern` → `documentary` → `established`. Computations produce `signal`, or `pattern` when independent signals converge on the same work. `documentary` and `established` need cited documents or the finding of an independent body, added by a person in a case.
- **Alternative explanations travel with every signal.**
- **No rankings of people.** Results are ordered by identifier, never by score; person-level signals need `include_individuals = TRUE`.
- Claims at signal and pattern level cannot be worded as accusations (`fs_check_wording()`).

## Use

```r
# install.packages("remotes")
remotes::install_github("pablosancheznunez/forsci")
library(forsci)

x <- fs_collect_openalex("primary_location.source.id:S123,publication_year:2020-2023")
# or offline, with known outcomes:
x <- fs_simulate(seed = 1)

s <- fs_signals(x)             # retractions, citation stacking, template titles, tortured phrases
s[s$flagged, ]

pm <- fs_pm_screen(x)          # paper mill screen: converging signals become a "pattern"

fs_validate(s, attr(x, "truth"))   # precision and recall against the planted cases

case <- fs_case("Template titles", "Do these works share a common template?")
case <- fs_case_add_signals(case, fs_signal_title_templates(x))
case <- fs_case_add(case, "W9001", "Retracted by the publisher, stated reason: paper mill.",
                    "established", source = "https://doi.org/10.9999/notice.1")
fs_report(case, "case.html", corpus = x)
```

## Signals (method 1.0.0)

| Signal | Unit | Flagged when | Main alternative explanations |
| --- | --- | --- | --- |
| `retractions` | journal or publisher | Wilson lower bound above the corpus rate | active investigation, mass retraction events, field practice |
| `citation_stacking` | journal pair, journal | observed flow ≥ 3 × expected (baseline excludes the tested flow), ≥ 10 references; self-citation robust z ≥ 3 | narrow specialties, partial corpus, special issues |
| `title_templates` | work | char 4-gram Jaccard ≥ 0.5 in groups of ≥ 3 | conference formats, serial works, duplicate records |
| `tortured_phrases` | work | phrase from the dictionary in title or abstract | translation, legitimate use, OCR |
| `productivity` (opt-in) | author-year | robust z ≥ 4 and ≥ 10 works | consortia, name disambiguation, field norms |

The citation stacking threshold was set on the simulated benchmark, where random journal pairs reach at most about 1.6 and planted pairs about 4.5. All thresholds are arguments.

## Data

- OpenAlex (`fs_collect_openalex()`, `fs_normalise_openalex()`); set `OPENALEX_API_KEY` for a larger budget.
- Retraction Watch database, distributed openly by Crossref (`fs_read_retraction_watch()`, `fs_link_retractions()`).
- Any other source mapped to the three corpus tables (`fs_corpus()`).
- Example files in `inst/extdata` are synthetic: all names, journals and DOIs are fictitious.

## License

MIT.
