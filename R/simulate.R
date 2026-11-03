#' Simulated corpus with known outcomes
#'
#' Generates a synthetic corpus with planted cases, so that every signal can be
#' checked against a known truth ([fs_validate()]). All journals, authors,
#' titles and abstracts are fictitious. Planted cases:
#'
#' * journal `S3` has a retraction rate of about 30 % (corpus about 1 %);
#' * journals `S7` and `S8` cite each other far above expectation;
#' * twelve works share a template title, four of them with tortured phrases;
#' * four further works contain tortured phrases;
#' * author `A999` publishes 30 works in 2022.
#'
#' @param seed Random seed; the same seed always gives the same corpus.
#' @param n_works Number of ordinary works (planted works are added).
#' @return An `fs_corpus` with provenance `simulated = TRUE` and an attribute
#'   `"truth"`: a data frame with columns `signal`, `unit_type` and `unit_id`
#'   listing the planted positives.
#' @export
#' @examples
#' x <- fs_simulate(seed = 1)
#' attr(x, "truth")
fs_simulate <- function(seed = 1, n_works = 400) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)

  words <- c("adaptive", "analysis", "bacterial", "behaviour", "cellular", "climate", "cohort",
             "coastal", "cognitive", "dynamics", "ecology", "electron", "emission", "enzyme",
             "evolution", "fluid", "forest", "genetic", "glacier", "growth", "habitat", "immune",
             "inflammation", "isotope", "kinetics", "language", "lattice", "learning", "lipid",
             "magnetic", "membrane", "metabolic", "microbial", "migration", "molecular", "neural",
             "nitrogen", "ocean", "optical", "oxidative", "particle", "pathway", "plasma", "polymer",
             "population", "protein", "quantum", "receptor", "regional", "resistance", "river",
             "rural", "sediment", "signalling", "soil", "spectral", "stress", "surface", "synthesis",
             "thermal", "tissue", "transport", "tumour", "urban", "vascular", "viral", "wetland")
  n_src <- 8
  sources <- paste0("S", seq_len(n_src))
  ids <- sprintf("W%04d", seq_len(n_works))
  works <- data.frame(
    work_id = ids, doi = paste0("10.9999/sim.", seq_len(n_works)),
    title = vapply(seq_len(n_works), function(i) {
      t <- paste(sample(words, 7), collapse = " ")
      paste0(toupper(substr(t, 1, 1)), substring(t, 2))
    }, ""),
    abstract = NA_character_, year = sample(2018:2023, n_works, replace = TRUE), type = "article",
    source_id = sample(sources, n_works, replace = TRUE), stringsAsFactors = FALSE
  )
  works$abstract <- vapply(seq_len(n_works), function(i) paste(sample(words, 25, replace = TRUE), collapse = " "), "")
  works$is_retracted <- stats::runif(n_works) < 0.01
  s3 <- works$source_id == "S3"
  works$is_retracted[s3] <- stats::runif(sum(s3)) < 0.30

  # Template titles: one frame, three slots.
  n_tpl <- 12
  tpl <- data.frame(
    work_id = sprintf("W9%03d", seq_len(n_tpl)), doi = paste0("10.9999/sim.t", seq_len(n_tpl)),
    title = sprintf("Research on the application of %s algorithm in %s management based on %s model",
                    sample(c("improved genetic", "fuzzy clustering", "neural network", "particle swarm"), n_tpl, TRUE),
                    sample(c("hospital", "enterprise", "supply chain", "smart city", "campus", "energy"), n_tpl, TRUE),
                    sample(c("big data", "cloud", "edge computing", "internet of things"), n_tpl, TRUE)),
    abstract = vapply(seq_len(n_tpl), function(i) paste(sample(words, 25, replace = TRUE), collapse = " "), ""),
    year = sample(2021:2023, n_tpl, TRUE), type = "article",
    source_id = sample(c("S5", "S6"), n_tpl, TRUE), is_retracted = FALSE, stringsAsFactors = FALSE
  )
  phrases <- fs_tortured_phrases()$tortured
  tpl$abstract[1:4] <- paste(tpl$abstract[1:4], "using", sample(phrases, 4), "methods")
  tpl$is_retracted[1:2] <- TRUE
  extra <- sample(which(!s3), 4)
  works$abstract[extra] <- paste(works$abstract[extra], "with", sample(phrases, 4))
  works <- rbind(works, tpl)
  works$source_name <- paste("Journal", works$source_id)
  works$publisher <- ifelse(works$source_id %in% c("S1", "S2", "S3", "S4"), "Publisher P1", "Publisher P2")
  works$cited_by_count <- stats::rpois(nrow(works), 8)
  works <- works[, corpus_columns$works]

  # Authorships: three authors per work; A999 on 30 works of 2022.
  authors <- sprintf("A%03d", 1:300)
  authorships <- do.call(rbind, lapply(works$work_id, function(w) {
    data.frame(work_id = w, author_id = sample(authors, 3), position = c("first", "middle", "last"),
               stringsAsFactors = FALSE)
  }))
  y2022 <- works$work_id[works$year == 2022 & !startsWith(works$work_id, "W9")]
  burst <- sample(y2022, min(30, length(y2022)))
  authorships$author_id[authorships$work_id %in% burst & authorships$position == "last"] <- "A999"
  authorships$author_name <- paste("Author", authorships$author_id)
  authorships$institution_id <- paste0("I", sample(1:40, nrow(authorships), TRUE))
  authorships$ror <- NA_character_
  authorships$country_code <- sample(c("ES", "FR", "DE", "BR", "CN", "IN", "US", "NG"), nrow(authorships), TRUE)
  authorships <- authorships[, corpus_columns$authorships]

  # References: 15 per work; S7 and S8 cite each other heavily.
  references <- do.call(rbind, lapply(seq_len(nrow(works)), function(i) {
    src <- works$source_id[i]
    pool <- works$work_id[-i]
    if (src %in% c("S7", "S8")) {
      other <- works$work_id[works$source_id == setdiff(c("S7", "S8"), src)]
      cited <- c(sample(other, 9, replace = length(other) < 9), sample(pool, 6))
    } else {
      cited <- sample(pool, 15)
    }
    data.frame(work_id = works$work_id[i], cited_id = unique(cited), stringsAsFactors = FALSE)
  }))

  truth <- rbind(
    data.frame(signal = "retractions", unit_type = "source", unit_id = "S3"),
    data.frame(signal = "citation_stacking", unit_type = "source_pair", unit_id = c("S7->S8", "S8->S7")),
    data.frame(signal = "title_templates", unit_type = "work", unit_id = tpl$work_id),
    data.frame(signal = "tortured_phrases", unit_type = "work", unit_id = c(tpl$work_id[1:4], works$work_id[extra])),
    data.frame(signal = "productivity", unit_type = "author_year", unit_id = "A999@2022")
  )
  corpus <- fs_corpus(works, authorships, references,
                      provenance = list(source = "fs_simulate", seed = seed, simulated = TRUE))
  attr(corpus, "truth") <- truth
  corpus
}

#' Validate signals against known outcomes
#'
#' Compares the flagged rows of a signal table with a list of known positives
#' (for example `attr(fs_simulate(), "truth")` or a curated benchmark of
#' documented cases) and reports, for each signal, true positives, false
#' positives, false negatives, precision and recall.
#'
#' @param signals An `fs_signals` data frame.
#' @param truth A data frame with columns `signal`, `unit_type` and `unit_id`.
#' @return A data frame with one row per signal in `truth`.
#' @export
#' @examples
#' x <- fs_simulate(seed = 1)
#' s <- fs_signals(x)
#' fs_validate(s, attr(x, "truth"))
fs_validate <- function(signals, truth) {
  if (!all(c("signal", "unit_type", "unit_id") %in% names(truth))) {
    stop("`truth` needs columns `signal`, `unit_type` and `unit_id`.", call. = FALSE)
  }
  key <- function(d) paste(d$signal, d$unit_type, d$unit_id)
  flagged <- signals[signals$flagged %in% TRUE, , drop = FALSE]
  out <- lapply(unique(truth$signal), function(s) {
    t <- key(truth[truth$signal == s, ])
    f <- key(flagged[flagged$signal == s & flagged$unit_type %in% truth$unit_type[truth$signal == s], ])
    tp <- length(intersect(t, f))
    fp <- length(setdiff(f, t))
    fn <- length(setdiff(t, f))
    data.frame(signal = s, true_positives = tp, false_positives = fp, false_negatives = fn,
               precision = if (tp + fp) tp / (tp + fp) else NA_real_,
               recall = if (tp + fn) tp / (tp + fn) else NA_real_, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
