`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Evidence levels of the metascience vocabulary (vocabulary_version 0.2.0).
evidence_levels <- c("signal", "pattern", "documentary", "established")

na_chr <- function(x) if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1]])
na_int <- function(x) if (is.null(x) || length(x) == 0L) NA_integer_ else as.integer(x[[1]])
na_lgl <- function(x) if (is.null(x) || length(x) == 0L) NA else as.logical(x[[1]])

extdata_csv <- function(file) {
  utils::read.csv(system.file("extdata", file, package = "FoSci"), encoding = "UTF-8",
                  stringsAsFactors = FALSE)
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# Wilson score interval for k successes in n trials.
wilson <- function(k, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- ifelse(n > 0, k / n, NA_real_)
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(lower = centre - half, upper = centre + half)
}

robust_z <- function(x) {
  s <- stats::mad(x)
  if (!is.finite(s) || s == 0) s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - stats::median(x)) / s
}

# Connected components of an undirected graph given as an edge list.
components <- function(nodes, from, to) {
  parent <- stats::setNames(nodes, nodes)
  find <- function(x) {
    while (parent[[x]] != x) x <- parent[[x]]
    x
  }
  for (i in seq_along(from)) {
    a <- find(from[i])
    b <- find(to[i])
    if (a != b) parent[[max(a, b)]] <- min(a, b)
  }
  vapply(nodes, find, "", USE.NAMES = FALSE)
}

# One row per signal; the columns every signal function returns.
signal_frame <- function(signal, method_version, unit_type, unit_id, unit_label = NA_character_,
                         observed = NA_real_, expected = NA_real_, statistic = NA_real_,
                         n = NA_integer_, flagged = NA, detail = NA_character_,
                         alternatives = character()) {
  n_rows <- length(unit_id)
  if (!n_rows) {
    return(data.frame(signal = character(), method_version = character(), unit_type = character(),
                      unit_id = character(), unit_label = character(), observed = numeric(),
                      expected = numeric(), statistic = numeric(), n = integer(), flagged = logical(),
                      evidence_level = character(), detail = character(), alternatives = character(),
                      stringsAsFactors = FALSE))
  }
  out <- data.frame(signal = signal, method_version = method_version, unit_type = unit_type,
                    unit_id = unit_id, unit_label = unit_label, observed = observed,
                    expected = expected, statistic = statistic, n = as.integer(n),
                    flagged = flagged, evidence_level = "signal", detail = detail,
                    alternatives = paste(alternatives, collapse = " | "), stringsAsFactors = FALSE)
  # Never ordered by score: by unit identifier only.
  out <- out[order(out$unit_id), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("fs_signals", "data.frame"))
}
