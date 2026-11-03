#' Paper mill screening
#'
#' Paper mills sell fabricated or manipulated manuscripts, often produced from
#' templates and disguised with paraphrasing software. No single feature
#' identifies them. This screen looks for independent signals that converge on
#' the same work:
#'
#' * membership of a template-title group ([fs_signal_title_templates()]);
#' * tortured phrases ([fs_signal_tortured_phrases()]);
#' * a retraction flag in the corpus.
#'
#' A work with one signal is reported at evidence level `signal`; a work where
#' two or more independent signals converge is reported as a `pattern`. The
#' screen never assigns a higher level: documents and findings belong in a
#' case ([fs_case()]). When Retraction Watch notices are supplied, they are
#' shown in separate columns with their own evidence level (from
#' [fs_link_retractions()]), including whether the notice states a paper mill
#' as a reason.
#'
#' Image duplication, another frequent feature, is checked with the reproimage
#' package on the article images and added to a case.
#'
#' @param corpus An `fs_corpus`.
#' @param retractions Optional result of [fs_link_retractions()].
#' @param ... Passed to [fs_signal_title_templates()].
#' @return A data frame with one row per work that has at least one signal:
#'   `work_id`, `title`, `source_id`, `template_group`, `tortured_phrases`,
#'   `retracted`, `n_signals`, `evidence_level`, and, when `retractions` is
#'   given, `notice`, `notice_reasons`, `notice_states_paper_mill` and
#'   `notice_evidence_level`. Ordered by `work_id`.
#' @export
#' @examples
#' x <- fs_simulate(seed = 1)
#' pm <- fs_pm_screen(x)
#' table(pm$evidence_level)
#' head(pm[pm$evidence_level == "pattern", ])
fs_pm_screen <- function(corpus, retractions = NULL, ...) {
  check_corpus(corpus)
  tt <- fs_signal_title_templates(corpus, ...)
  tp <- fs_signal_tortured_phrases(corpus)
  w <- corpus$works
  template <- sub(" \\(.*$", "", sub("^group ", "", tt$detail[match(w$work_id, tt$unit_id)]))
  tortured <- tp$detail[match(w$work_id, tp$unit_id)]
  retracted <- w$is_retracted %in% TRUE
  n <- (!is.na(template)) + (!is.na(tortured)) + retracted
  keep <- n > 0
  out <- data.frame(work_id = w$work_id, title = w$title, source_id = w$source_id,
                    template_group = template, tortured_phrases = tortured, retracted = retracted,
                    n_signals = n, evidence_level = ifelse(n >= 2, "pattern", "signal"),
                    stringsAsFactors = FALSE)[keep, , drop = FALSE]
  if (!is.null(retractions)) {
    i <- match(out$work_id, retractions$work_id)
    out$notice <- retractions$nature[i]
    out$notice_reasons <- retractions$reasons[i]
    out$notice_states_paper_mill <- grepl("paper mill", retractions$reasons[i], ignore.case = TRUE)
    out$notice_evidence_level <- retractions$evidence_level[i]
  }
  out <- out[order(out$work_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}
