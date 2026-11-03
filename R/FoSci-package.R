#' FoSci: forensic scientometrics
#'
#' Finds signals in publication data that may deserve a closer look, and keeps
#' them apart from findings. The workflow:
#'
#' 1. Build a corpus: [fs_collect_openalex()], [fs_normalise_openalex()] or
#'    [fs_corpus()] from your own tables; [fs_simulate()] for a benchmark with
#'    known outcomes.
#' 2. Compute signals: [fs_signals()] runs [fs_signal_retractions()],
#'    [fs_signal_citation_stacking()], [fs_signal_title_templates()] and
#'    [fs_signal_tortured_phrases()]. [fs_signal_productivity()] concerns
#'    individuals and only runs when asked explicitly.
#' 3. Screen for paper mills: [fs_pm_screen()] looks for independent signals
#'    that converge on the same works.
#' 4. Validate thresholds: [fs_validate()] against a benchmark.
#' 5. Document: [fs_case()], [fs_case_add()] and [fs_report()].
#'
#' # Principles
#'
#' * **An anomaly is not misconduct.** Every row carries an `evidence_level`
#'   from the shared metascience vocabulary: `signal`, `pattern`,
#'   `documentary` or `established`. Computations produce `signal`, or
#'   `pattern` when independent signals converge; nothing computed is ever
#'   `documentary` or `established`. Those levels require documents or
#'   findings of an independent body, added by a person in a case.
#' * Every signal lists its **alternative explanations**.
#' * **No rankings of people.** Results are ordered by unit identifier, never by
#'   score, and person-level signals need `include_individuals = TRUE`.
#' * Corpus baselines are only as good as the corpus: differences across
#'   fields, languages, regions and database coverage are a limitation of every
#'   signal.
#'
#' @keywords internal
"_PACKAGE"
