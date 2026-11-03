accusatory_terms <- c("fraud", "fraudulent", "misconduct", "fabricat\\w*", "falsif\\w*", "plagiari\\w*",
                      "fake", "cheat\\w*", "dishonest\\w*", "guilty", "manipulat\\w*", "forged?")

#' Check the wording of a claim against its evidence level
#'
#' Claims at the levels `signal` and `pattern` must describe what was
#' observed, not what someone did: words such as "fraud", "fabricated" or
#' "manipulated" are refused at those levels. At `documentary` and
#' `established` they are allowed, because the claim then rests on a cited
#' document or finding.
#'
#' @param text The claim.
#' @param evidence_level One of `"signal"`, `"pattern"`, `"documentary"`,
#'   `"established"`.
#' @return The accusatory terms found (a character vector, empty when the
#'   wording is acceptable for the level).
#' @export
#' @examples
#' fs_check_wording("Twelve titles share one template.", "pattern")
#' fs_check_wording("These papers were fabricated by a paper mill.", "signal")
fs_check_wording <- function(text, evidence_level) {
  evidence_level <- match.arg(evidence_level, evidence_levels)
  if (evidence_level %in% c("documentary", "established")) return(character())
  hits <- vapply(accusatory_terms, function(p) grepl(paste0("\\b", p, "\\b"), text, ignore.case = TRUE, perl = TRUE), TRUE)
  m <- regmatches(text, gregexpr(paste0("\\b(", paste(accusatory_terms[hits], collapse = "|"), ")\\b"),
                                 text, ignore.case = TRUE, perl = TRUE))[[1]]
  if (!any(hits)) character() else unique(tolower(m))
}

#' Document a case
#'
#' A case collects the evidence about one question (for example "Do these
#' twelve articles come from the same template?") with the evidence level and
#' source of every item. `fs_case_add()` adds one item; it refuses
#'
#' * an evidence level outside the shared vocabulary;
#' * `documentary` or `established` items without a citable source (a DOI,
#'   URL or document reference);
#' * `signal` or `pattern` items without alternative explanations, or worded
#'   as an accusation (see [fs_check_wording()]).
#'
#' `fs_case_add_signals()` adds the flagged rows of a signal table, with their
#' alternatives.
#'
#' @param title Short title of the case.
#' @param question The question the case examines, phrased neutrally.
#' @return An object of class `fs_case`.
#' @export
#' @examples
#' x <- fs_simulate(seed = 1)
#' case <- fs_case("Template titles in S5 and S6",
#'                 "Do these works share a common template?")
#' case <- fs_case_add_signals(case, fs_signal_title_templates(x))
#' case <- fs_case_add(case, "W9001", "The publisher retracted this article.",
#'                     "documentary", source = "https://doi.org/10.9999/notice.1")
#' case
fs_case <- function(title, question) {
  if (!is.character(title) || length(title) != 1L || !nzchar(title)) stop("`title` must be a string.", call. = FALSE)
  if (!is.character(question) || length(question) != 1L || !nzchar(question)) stop("`question` must be a string.", call. = FALSE)
  problems <- fs_check_wording(question, "signal")
  if (length(problems)) {
    stop("Phrase the question neutrally; it presupposes: ", paste(problems, collapse = ", "), call. = FALSE)
  }
  structure(list(title = title, question = question, created_at = utc_now(),
                 items = data.frame(unit_type = character(), unit_id = character(), claim = character(),
                                    evidence_level = character(), source = character(),
                                    alternatives = character(), added_at = character(),
                                    stringsAsFactors = FALSE)),
            class = "fs_case")
}

#' @rdname fs_case
#' @param case An `fs_case`.
#' @param unit_id The work, journal or other unit the item is about.
#' @param claim What the item shows, in one sentence.
#' @param evidence_level `"signal"`, `"pattern"`, `"documentary"` or
#'   `"established"`.
#' @param source Where the evidence comes from: required for `documentary`
#'   and `established`.
#' @param alternatives Alternative explanations: required for `signal` and
#'   `pattern`.
#' @param unit_type Type of unit, for example `"work"` or `"source"`.
#' @export
fs_case_add <- function(case, unit_id, claim, evidence_level, source = NA_character_,
                        alternatives = NA_character_, unit_type = "work") {
  if (!inherits(case, "fs_case")) stop("`case` must come from fs_case().", call. = FALSE)
  if (!is.character(evidence_level) || length(evidence_level) != 1L || !evidence_level %in% evidence_levels) {
    stop("`evidence_level` must be one of: ", paste(evidence_levels, collapse = ", "), ".", call. = FALSE)
  }
  blank <- function(x) is.null(x) || all(is.na(x)) || !any(nzchar(trimws(x)))
  if (evidence_level %in% c("documentary", "established") && blank(source)) {
    stop("A ", evidence_level, " item needs a citable `source`.", call. = FALSE)
  }
  if (evidence_level %in% c("signal", "pattern")) {
    if (blank(alternatives)) stop("A ", evidence_level, " item needs `alternatives`.", call. = FALSE)
    problems <- fs_check_wording(claim, evidence_level)
    if (length(problems)) {
      stop("A ", evidence_level, " cannot be worded as an accusation (", paste(problems, collapse = ", "),
           "). Describe what was observed.", call. = FALSE)
    }
  }
  case$items <- rbind(case$items, data.frame(
    unit_type = unit_type, unit_id = unit_id, claim = claim, evidence_level = evidence_level,
    source = paste(source, collapse = "; "), alternatives = paste(alternatives, collapse = " | "),
    added_at = utc_now(), stringsAsFactors = FALSE))
  case
}

#' @rdname fs_case
#' @param signals An `fs_signals` data frame; only flagged rows are added.
#' @export
fs_case_add_signals <- function(case, signals) {
  if (!inherits(case, "fs_case")) stop("`case` must come from fs_case().", call. = FALSE)
  f <- signals[signals$flagged %in% TRUE, , drop = FALSE]
  for (i in seq_len(nrow(f))) {
    claim <- paste0(f$signal[i], " (method ", f$method_version[i], "): ",
                    if (is.na(f$detail[i])) sprintf("observed %.3g, expected %.3g", f$observed[i], f$expected[i]) else f$detail[i])
    case <- fs_case_add(case, f$unit_id[i], claim, f$evidence_level[i],
                        source = "FoSci computation", alternatives = f$alternatives[i],
                        unit_type = f$unit_type[i])
  }
  case
}

#' @export
print.fs_case <- function(x, ...) {
  cat("<FoSci case> ", x$title, "\n  question: ", x$question, "\n", sep = "")
  if (nrow(x$items)) {
    tab <- table(factor(x$items$evidence_level, evidence_levels))
    cat("  items: ", paste(names(tab), tab, sep = " ", collapse = ", "), "\n", sep = "")
  } else {
    cat("  no items yet\n")
  }
  invisible(x)
}

#' Write a reproducible report
#'
#' Writes a self-contained HTML report of a case or of a signal table. Every
#' report states the evidence level of each item next to it, lists the
#' alternative explanations, records data provenance and software versions,
#' and opens with a notice that signals are not findings of misconduct.
#'
#' @param x An `fs_case` or an `fs_signals` data frame (only flagged rows are
#'   shown).
#' @param path File to write (`.html`).
#' @param corpus Optional `fs_corpus` whose provenance is recorded.
#' @param title Report title; by default the case title.
#' @return `path`, invisibly.
#' @export
#' @examples
#' x <- fs_simulate(seed = 1)
#' fs_report(fs_signals(x), tempfile(fileext = ".html"), corpus = x)
fs_report <- function(x, path, corpus = NULL, title = NULL) {
  if (inherits(x, "fs_case")) {
    items <- x$items
    title <- title %||% x$title
    question <- x$question
  } else if (inherits(x, "fs_signals")) {
    f <- x[x$flagged %in% TRUE, , drop = FALSE]
    items <- data.frame(unit_type = f$unit_type, unit_id = f$unit_id,
                        claim = paste0(f$signal, ": ", ifelse(is.na(f$detail), "", f$detail)),
                        evidence_level = f$evidence_level, source = "FoSci computation",
                        alternatives = f$alternatives, stringsAsFactors = FALSE)
    title <- title %||% "Integrity signals"
    question <- NA_character_
  } else {
    stop("`x` must be an fs_case or fs_signals.", call. = FALSE)
  }
  prov <- if (!is.null(corpus)) corpus$provenance else list()
  rows <- if (nrow(items)) paste0(
    "<tr><td>", html_escape(items$unit_type), "</td><td>", html_escape(items$unit_id), "</td><td>",
    html_escape(items$claim), "</td><td><span class=\"lvl ", items$evidence_level, "\">", items$evidence_level,
    "</span></td><td>", html_escape(items$source), "</td><td>",
    html_escape(gsub(" | ", "; ", items$alternatives, fixed = TRUE)), "</td></tr>", collapse = "\n"
  ) else "<tr><td colspan=\"6\">No items.</td></tr>"
  html <- paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>", html_escape(title), "</title>",
    "<style>body{font-family:system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#1d2433}",
    "table{border-collapse:collapse;width:100%;font-size:.9rem}td,th{border:1px solid #d5d9e0;padding:.4rem;vertical-align:top;text-align:left}",
    ".notice{background:#fff7e0;border-left:4px solid #c98a00;padding:.8rem 1rem}.lvl{padding:.1rem .4rem;border-radius:3px;font-weight:600}",
    ".signal{background:#eef1f5}.pattern{background:#e3ecfa}.documentary{background:#e4f4ea}.established{background:#d6efe0}</style></head><body>",
    "<h1>", html_escape(title), "</h1>",
    if (!is.na(question)) paste0("<p><strong>Question.</strong> ", html_escape(question), "</p>"),
    "<p class=\"notice\"><strong>Read this first.</strong> A <em>signal</em> is a statistical or textual departure from what is expected; ",
    "a <em>pattern</em> is the convergence of independent signals. Neither is evidence of misconduct, and both have the alternative ",
    "explanations listed next to them. <em>Documentary</em> items rest on the cited documents; <em>established</em> items on the ",
    "finding of the cited independent body, not on this report.</p>",
    "<h2>Items</h2><table><tr><th>Unit type</th><th>Unit</th><th>Claim</th><th>Evidence level</th><th>Source</th><th>Alternative explanations</th></tr>",
    rows, "</table>",
    "<h2>Provenance</h2><ul>",
    "<li>Generated: ", utc_now(), " with FoSci ", as.character(utils::packageVersion("FoSci")), " on ", html_escape(R.version.string), "</li>",
    if (length(prov)) paste0("<li>Data: ", html_escape(paste(names(prov), unlist(lapply(prov, paste, collapse = ", ")), sep = " = ", collapse = "; ")), "</li>"),
    if (inherits(x, "fs_signals")) paste0("<li>Methods: ", html_escape(paste(unique(paste0(x$signal, "@", x$method_version)), collapse = ", ")), "</li>"),
    "</ul><h2>Limitations</h2><ul>",
    "<li>Baselines come from the corpus analysed, not from the whole field; coverage of the database shapes every result.</li>",
    "<li>Disciplines, languages, regions and publication types differ legitimately in the features measured.</li>",
    "<li>Thresholds are validated on simulated and documented benchmark cases; their error rates in other corpora are unknown.</li>",
    "</ul></body></html>"
  )
  writeLines(html, path, useBytes = TRUE)
  invisible(path)
}
