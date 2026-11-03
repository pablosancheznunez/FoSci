#' Retraction rate by journal or publisher
#'
#' Compares the share of retracted works of each unit with the share in the
#' whole corpus. A unit is flagged when the lower bound of its Wilson interval
#' lies above the corpus rate. Works whose retraction status is unknown are
#' left out of both numerator and denominator.
#'
#' @param corpus An `fs_corpus`.
#' @param by Column of `corpus$works` that defines the unit: `"source_id"`
#'   (journal) or `"publisher"`.
#' @param min_works Units with fewer works are reported but never flagged.
#' @param conf Confidence level of the interval.
#' @return An `fs_signals` data frame; see [fs_signals()] for its columns.
#' @family signals
#' @export
#' @examples
#' fs_signal_retractions(fs_simulate(seed = 1))
fs_signal_retractions <- function(corpus, by = c("source_id", "publisher"), min_works = 20, conf = 0.95) {
  check_corpus(corpus)
  by <- match.arg(by)
  w <- corpus$works[!is.na(corpus$works$is_retracted) & !is.na(corpus$works[[by]]), , drop = FALSE]
  p0 <- if (nrow(w)) mean(w$is_retracted) else NA_real_
  units <- sort(unique(w[[by]]))
  n <- vapply(units, function(u) sum(w[[by]] == u), 0)
  k <- vapply(units, function(u) sum(w$is_retracted[w[[by]] == u]), 0)
  ci <- wilson(k, n, conf)
  label <- if (by == "source_id") w$source_name[match(units, w$source_id)] else units
  signal_frame(
    "retractions", "1.0.0", if (by == "source_id") "source" else "publisher", units, label,
    observed = k / n, expected = p0,
    statistic = ifelse(p0 > 0 & p0 < 1, (k - n * p0) / sqrt(n * p0 * (1 - p0)), NA_real_),
    n = n, flagged = n >= min_works & ci$lower > p0,
    detail = sprintf("%d of %d retracted; %d%% interval %.3f-%.3f", k, n, round(conf * 100), ci$lower, ci$upper),
    alternatives = c("Journals that investigate actively retract more, so a high rate can reflect good practice.",
                     "A single mass retraction event can dominate the rate.",
                     "Retraction practices differ across fields and publishers.",
                     "Retraction flags in bibliographic databases are incomplete and delayed.")
  )
}

#' Citation stacking between journals
#'
#' Counts the references from works in journal A to works in journal B within
#' the corpus, and compares each flow with the flow expected if A cited
#' journals in proportion to how much the whole corpus cites them. A pair is
#' flagged when the observed flow is at least `ratio` times the expected flow
#' and at least `min_flow` references. Flagged pairs whose reverse is also
#' flagged are marked `reciprocal`. Journal self-citation shares far above the
#' corpus median (robust z of at least `z`) are reported as `source` rows.
#'
#' Only references to works inside the corpus are counted, so the baseline is
#' the corpus, not the field.
#'
#' @inheritParams fs_signal_retractions
#' @param ratio Minimum observed-to-expected ratio for a pair to be flagged.
#' @param min_flow Minimum number of references for a pair or a journal to be
#'   flagged.
#' @param z Minimum robust z of the self-citation share.
#' @return An `fs_signals` data frame.
#' @family signals
#' @export
#' @examples
#' s <- fs_signal_citation_stacking(fs_simulate(seed = 1))
#' s[s$flagged, ]
fs_signal_citation_stacking <- function(corpus, ratio = 3, min_flow = 10, z = 3) {
  check_corpus(corpus)
  src <- stats::setNames(corpus$works$source_id, corpus$works$work_id)
  refs <- corpus$references
  refs$from <- unname(src[refs$work_id])
  refs$to <- unname(src[refs$cited_id])
  refs <- refs[!is.na(refs$from) & !is.na(refs$to), , drop = FALSE]
  alts <- c("Journals in the same narrow specialty legitimately cite each other heavily.",
            "The corpus may cover only part of the citing field, inflating within-corpus flows.",
            "Special issues, editorial series and review journals concentrate citations.")
  if (!nrow(refs)) return(signal_frame("citation_stacking", "1.0.0", character(), character()))
  out_n <- table(refs$from)
  flows <- as.data.frame(table(from = refs$from, to = refs$to), stringsAsFactors = FALSE)
  self <- flows[flows$from == flows$to, , drop = FALSE]
  flows <- flows[flows$from != flows$to & flows$Freq > 0, , drop = FALSE]
  # Baseline: how much journals other than A cite B, excluding self-citations,
  # so that the flow being tested does not inflate its own expectation.
  cross <- refs[refs$from != refs$to, , drop = FALSE]
  in_n <- table(factor(cross$to, names(out_n)))
  cross_out <- table(factor(cross$from, names(out_n)))
  others_to_b <- as.numeric(in_n[flows$to]) - flows$Freq
  others_total <- nrow(cross) - as.numeric(cross_out[flows$from]) -
    vapply(flows$to, function(b) sum(cross$from == b), 0)
  flows$expected <- as.numeric(cross_out[flows$from]) * pmax(others_to_b, 0.5) / pmax(others_total, 1)
  flows$ratio <- flows$Freq / flows$expected
  flows$flagged <- flows$ratio >= ratio & flows$Freq >= min_flow
  key <- paste(flows$from, flows$to, sep = "->")
  reverse <- paste(flows$to, flows$from, sep = "->")
  recip <- flows$flagged & reverse %in% key[flows$flagged]
  pairs <- signal_frame(
    "citation_stacking", "1.0.0", "source_pair", key, NA_character_,
    observed = flows$Freq, expected = flows$expected, statistic = flows$ratio,
    n = as.numeric(out_n[flows$from]), flagged = flows$flagged,
    detail = ifelse(recip, "reciprocal", ifelse(flows$flagged, "one-way", NA_character_)),
    alternatives = alts
  )
  self$out <- as.numeric(out_n[self$from])
  self$share <- self$Freq / self$out
  zs <- robust_z(self$share)
  selfs <- signal_frame(
    "citation_stacking", "1.0.0", "source", self$from, NA_character_,
    observed = self$share, expected = stats::median(self$share), statistic = zs, n = self$out,
    flagged = zs >= z & self$Freq >= min_flow, detail = "journal self-citation share",
    alternatives = alts
  )
  bind_signals(pairs, selfs)
}

#' Template titles
#'
#' Finds groups of works whose titles are near-duplicates: the same sentence
#' frame with a few words swapped, a known feature of paper mill output.
#' Titles are compared by the Jaccard similarity of their character 4-grams;
#' pairs at or above `threshold` are linked and groups of at least
#' `min_cluster` works are reported, one row per work.
#'
#' @inheritParams fs_signal_retractions
#' @param threshold Minimum Jaccard similarity of two titles.
#' @param min_cluster Minimum number of works in a group.
#' @param max_df 4-grams present in more than this share of titles are
#'   ignored, because they carry no information.
#' @return An `fs_signals` data frame with one row per work in a group; the
#'   group is named in `detail`.
#' @family signals
#' @export
#' @examples
#' fs_signal_title_templates(fs_simulate(seed = 1))
fs_signal_title_templates <- function(corpus, threshold = 0.5, min_cluster = 3, max_df = 0.2) {
  check_corpus(corpus)
  w <- corpus$works[!is.na(corpus$works$title) & nzchar(corpus$works$title), , drop = FALSE]
  alts <- c("Conference series and journals with standard title formats.",
            "Serial works by the same team (parts I, II, III) or translations.",
            "Duplicate records of the same work in the database.")
  empty <- signal_frame("title_templates", "1.0.0", character(), character())
  if (nrow(w) < 2) return(empty)
  norm <- gsub("\\s+", " ", gsub("[^a-z0-9 ]", " ", tolower(w$title)))
  sh <- lapply(norm, function(t) {
    if (nchar(t) < 4) return(character())
    unique(substring(t, 1:(nchar(t) - 3), 4:nchar(t)))
  })
  size <- lengths(sh)
  idx <- data.frame(doc = rep(seq_along(sh), size), g = unlist(sh), stringsAsFactors = FALSE)
  df <- table(idx$g)
  idx <- idx[idx$g %in% names(df)[df >= 2 & df <= max(2, max_df * nrow(w))], , drop = FALSE]
  by_g <- split(idx$doc, idx$g)
  pair_list <- lapply(by_g, function(d) if (length(d) > 1) t(utils::combn(sort(d), 2)) else NULL)
  pairs <- do.call(rbind, pair_list)
  if (is.null(pairs)) return(empty)
  pk <- paste(pairs[, 1], pairs[, 2])
  inter <- table(pk)
  ab <- do.call(rbind, strsplit(names(inter), " ", fixed = TRUE))
  a <- as.integer(ab[, 1])
  b <- as.integer(ab[, 2])
  jac <- as.numeric(inter) / (size[a] + size[b] - as.numeric(inter))
  keep <- jac >= threshold
  if (!any(keep)) return(empty)
  a <- a[keep]
  b <- b[keep]
  jac <- jac[keep]
  nodes <- as.character(sort(unique(c(a, b))))
  comp <- components(nodes, as.character(a), as.character(b))
  sizes <- table(comp)
  big <- names(sizes)[sizes >= min_cluster]
  if (!length(big)) return(empty)
  members <- as.integer(nodes[comp %in% big])
  cl <- comp[comp %in% big]
  cluster_name <- paste0("T", match(cl, sort(unique(cl))))
  mean_sim <- vapply(members, function(m) mean(jac[a == m | b == m]), 0)
  signal_frame(
    "title_templates", "1.0.0", "work", w$work_id[members], w$title[members],
    observed = mean_sim, expected = NA_real_, statistic = mean_sim,
    n = as.integer(sizes[cl]), flagged = TRUE,
    detail = paste0("group ", cluster_name, " (", as.integer(sizes[cl]), " works)"),
    alternatives = alts
  )
}

#' Tortured phrases
#'
#' Looks for "tortured phrases" in titles and abstracts: established terms
#' replaced by awkward synonyms (for example "counterfeit consciousness" for
#' "artificial intelligence"), typically produced by paraphrasing software to
#' disguise plagiarism. Works without title and abstract cannot be screened;
#' their number is stored in the attribute `"not_screened"`.
#'
#' @inheritParams fs_signal_retractions
#' @param phrases A data frame with columns `tortured` and `expected`; by
#'   default [fs_tortured_phrases()].
#' @return An `fs_signals` data frame with one row per work containing at
#'   least one phrase.
#' @family signals
#' @export
#' @examples
#' fs_signal_tortured_phrases(fs_simulate(seed = 1))
fs_signal_tortured_phrases <- function(corpus, phrases = fs_tortured_phrases()) {
  check_corpus(corpus)
  if (!is.data.frame(phrases) || !all(c("tortured", "expected") %in% names(phrases))) {
    stop("`phrases` needs columns `tortured` and `expected`.", call. = FALSE)
  }
  w <- corpus$works
  text <- tolower(paste(ifelse(is.na(w$title), "", w$title), ifelse(is.na(w$abstract), "", w$abstract)))
  screened <- nzchar(trimws(text))
  pats <- paste0("\\b", gsub("([.()+*?^$\\[\\]|\\\\])", "\\\\\\1", tolower(phrases$tortured)), "\\b")
  hits <- lapply(text, function(t) which(vapply(pats, grepl, TRUE, x = t, perl = TRUE)))
  found <- which(lengths(hits) > 0 & screened)
  out <- signal_frame(
    "tortured_phrases", "1.0.0", "work", w$work_id[found], w$title[found],
    observed = lengths(hits[found]), n = lengths(hits[found]), flagged = TRUE,
    detail = vapply(hits[found], function(h) {
      paste0("\"", phrases$tortured[h], "\" (for \"", phrases$expected[h], "\")", collapse = "; ")
    }, ""),
    alternatives = c("Machine translation or writing in a second language.",
                     "Legitimate use of the phrase in its own sense.",
                     "OCR or metadata errors in the title or abstract.")
  )
  attr(out, "not_screened") <- sum(!screened)
  out
}

#' Tortured phrase dictionary
#'
#' A small, documented starting list of tortured phrases from the literature
#' that introduced the concept. It is illustrative, not exhaustive: pass your
#' own list, for example from the Problematic Paper Screener, to
#' [fs_signal_tortured_phrases()].
#'
#' @return A data frame with columns `tortured`, `expected` and `source`.
#' @references Cabanac, G., Labbé, C. and Magazinov, A. (2021). Tortured
#'   phrases: A dubious writing style emerging in science. Evidence of
#'   critical issues affecting established journals. arXiv:2107.06751.
#' @export
#' @examples
#' fs_tortured_phrases()
fs_tortured_phrases <- function() extdata_csv("tortured_phrases.csv")

#' Unusual productivity of individual authors
#'
#' Counts works per author and year and flags author-years far above the
#' corpus distribution (robust z of at least `z` and at least `min_works`
#' works). This signal concerns individuals, so it only runs with
#' `include_individuals = TRUE`, and its results must not be published as a
#' list of names: high productivity has many legitimate causes.
#'
#' @inheritParams fs_signal_retractions
#' @param include_individuals Must be `TRUE`; a deliberate choice to compute
#'   person-level signals.
#' @param min_works Minimum works in a year for an author-year to be flagged.
#' @param z Minimum robust z.
#' @return An `fs_signals` data frame with one row per author-year.
#' @family signals
#' @export
#' @examples
#' s <- fs_signal_productivity(fs_simulate(seed = 1), include_individuals = TRUE)
#' s[s$flagged, ]
fs_signal_productivity <- function(corpus, include_individuals = FALSE, min_works = 10, z = 4) {
  check_corpus(corpus)
  if (!isTRUE(include_individuals)) {
    stop("This signal concerns individual researchers. Set `include_individuals = TRUE` to compute it, ",
         "and do not publish its results as a list of names.", call. = FALSE)
  }
  au <- unique(corpus$authorships[!is.na(corpus$authorships$author_id), c("work_id", "author_id")])
  au$year <- corpus$works$year[match(au$work_id, corpus$works$work_id)]
  au <- au[!is.na(au$year), , drop = FALSE]
  if (!nrow(au)) return(signal_frame("productivity", "1.0.0", character(), character()))
  counts <- stats::aggregate(work_id ~ author_id + year, data = au, FUN = length)
  zs <- robust_z(counts$work_id)
  signal_frame(
    "productivity", "1.0.0", "author_year", paste(counts$author_id, counts$year, sep = "@"),
    NA_character_, observed = counts$work_id, expected = stats::median(counts$work_id),
    statistic = zs, n = counts$work_id, flagged = zs >= z & counts$work_id >= min_works,
    detail = NA_character_,
    alternatives = c("Large consortia and multi-centre studies list many authors on many works.",
                     "Author disambiguation can merge different people with similar names.",
                     "Field norms differ by orders of magnitude (e.g. high-energy physics).",
                     "Editorials, letters and conference abstracts may be counted as works.")
  )
}

#' Compute all signals for a corpus
#'
#' Runs every signal that does not concern individual people:
#' [fs_signal_retractions()], [fs_signal_citation_stacking()],
#' [fs_signal_title_templates()] and [fs_signal_tortured_phrases()].
#'
#' Every signal function returns the same columns:
#'
#' * `signal`, `method_version`: what was computed, with which version of the
#'   method;
#' * `unit_type`, `unit_id`, `unit_label`: what it was computed for;
#' * `observed`, `expected`, `statistic`, `n`: the numbers behind it;
#' * `flagged`: whether it passes the threshold. Unflagged rows are kept so
#'   that thresholds can be examined;
#' * `evidence_level`: always `"signal"`;
#' * `detail`, `alternatives`: what was found and how else it can be
#'   explained.
#'
#' Rows are ordered by unit identifier, never by score.
#'
#' @inheritParams fs_signal_retractions
#' @param ... Not used.
#' @return An `fs_signals` data frame.
#' @family signals
#' @export
#' @examples
#' s <- fs_signals(fs_simulate(seed = 1))
#' s
#' table(s$signal, s$flagged)
fs_signals <- function(corpus, ...) {
  check_corpus(corpus)
  bind_signals(fs_signal_retractions(corpus), fs_signal_citation_stacking(corpus),
               fs_signal_title_templates(corpus), fs_signal_tortured_phrases(corpus))
}

bind_signals <- function(...) {
  parts <- Filter(function(x) nrow(x) > 0, list(...))
  if (!length(parts)) return(signal_frame("none", "1.0.0", character(), character()))
  out <- do.call(rbind, lapply(parts, as.data.frame))
  rownames(out) <- NULL
  structure(out, class = c("fs_signals", "data.frame"))
}

check_corpus <- function(corpus) {
  if (!inherits(corpus, "fs_corpus")) stop("`corpus` must be an fs_corpus; see fs_corpus().", call. = FALSE)
}

#' @export
print.fs_signals <- function(x, ...) {
  if (!all(c("signal", "flagged") %in% names(x))) return(print(as.data.frame(x), ...))
  cat("<FoSci signals> ", nrow(x), " rows, ", sum(x$flagged, na.rm = TRUE), " flagged\n", sep = "")
  if (nrow(x)) {
    tab <- table(x$signal, factor(x$flagged, c(TRUE, FALSE), c("flagged", "not flagged")))
    print(tab)
  }
  cat("Signals are not evidence of misconduct; see the `alternatives` column.\n")
  invisible(x)
}
