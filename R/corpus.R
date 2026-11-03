corpus_columns <- list(
  works = c("work_id", "doi", "title", "abstract", "year", "type", "source_id", "source_name",
            "publisher", "is_retracted", "cited_by_count"),
  authorships = c("work_id", "author_id", "author_name", "position", "institution_id",
                  "ror", "country_code"),
  references = c("work_id", "cited_id")
)

#' Build a corpus of publications
#'
#' A corpus is three tables: one row per work, one row per authorship
#' (work × author × institution) and one row per reference (work cites work).
#' [fs_normalise_openalex()] builds one from OpenAlex; `fs_corpus()` builds it
#' from your own tables, for example exported from Web of Science, Scopus or
#' Dimensions and mapped to these columns.
#'
#' Required columns:
#' * `works`: `work_id`, `doi`, `title`, `abstract`, `year`, `type`,
#'   `source_id` (journal), `source_name`, `publisher`, `is_retracted`,
#'   `cited_by_count`;
#' * `authorships`: `work_id`, `author_id`, `author_name`, `position`,
#'   `institution_id`, `ror`, `country_code`;
#' * `references`: `work_id`, `cited_id`.
#'
#' Missing values are allowed; missing columns are not.
#'
#' @param works,authorships,references Data frames with the columns above.
#' @param provenance A list describing where the data came from (source,
#'   query, retrieved_at); it is carried into reports.
#' @return An object of class `fs_corpus`.
#' @export
#' @examples
#' x <- fs_simulate(seed = 1)
#' x
#' str(unclass(x), max.level = 1)
fs_corpus <- function(works, authorships = NULL, references = NULL, provenance = list()) {
  authorships <- authorships %||% empty_table(corpus_columns$authorships)
  references <- references %||% empty_table(corpus_columns$references)
  tables <- list(works = works, authorships = authorships, references = references)
  for (nm in names(tables)) {
    t <- tables[[nm]]
    if (!is.data.frame(t)) stop("`", nm, "` must be a data frame.", call. = FALSE)
    missing <- setdiff(corpus_columns[[nm]], names(t))
    if (length(missing)) {
      stop("`", nm, "` lacks column(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  if (anyDuplicated(works$work_id)) stop("`works$work_id` must be unique.", call. = FALSE)
  works$is_retracted <- as.logical(works$is_retracted)
  works$year <- as.integer(works$year)
  structure(c(tables, list(provenance = provenance)), class = "fs_corpus")
}

empty_table <- function(cols) {
  as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols),
                stringsAsFactors = FALSE)
}

#' @export
print.fs_corpus <- function(x, ...) {
  cat("<forsci corpus> ", nrow(x$works), " works, ", length(unique(x$authorships$author_id)),
      " authors, ", length(unique(x$works$source_id[!is.na(x$works$source_id)])), " sources, ",
      nrow(x$references), " references\n", sep = "")
  yrs <- range(x$works$year, na.rm = TRUE)
  if (all(is.finite(yrs))) cat("  years ", yrs[1], "-", yrs[2], "\n", sep = "")
  if (!is.null(x$provenance$source)) {
    cat("  from ", x$provenance$source, if (!is.null(x$provenance$retrieved_at)) paste0(" (", x$provenance$retrieved_at, ")"),
        "\n", sep = "")
  }
  if (isTRUE(x$provenance$simulated)) cat("  simulated data with known outcomes (attr \"truth\")\n")
  invisible(x)
}

openalex_id <- function(x) if (is.null(x)) NA_character_ else sub("^https://openalex\\.org/", "", x)

# OpenAlex stores abstracts as an inverted index: word -> positions.
abstract_text <- function(inv) {
  if (is.null(inv) || !length(inv)) return(NA_character_)
  pos <- unlist(lapply(names(inv), function(w) stats::setNames(unlist(inv[[w]]), rep(w, length(inv[[w]])))))
  paste(names(sort(pos)), collapse = " ")
}

#' Normalise OpenAlex works into a corpus
#'
#' Turns one or more parsed responses of the OpenAlex `/works` endpoint (from
#' `jsonlite::fromJSON(simplifyVector = FALSE)`) into an `fs_corpus`. The
#' function does not use the network.
#'
#' @param json A parsed response, or a list of them (one per page).
#' @param provenance Passed to [fs_corpus()].
#' @return An `fs_corpus`.
#' @export
#' @examples
#' path <- system.file("extdata", "openalex_example.json", package = "forsci")
#' fs_normalise_openalex(jsonlite::fromJSON(path, simplifyVector = FALSE))
fs_normalise_openalex <- function(json, provenance = list(source = "openalex")) {
  pages <- if (!is.null(json$results)) list(json) else json
  results <- unlist(lapply(pages, function(p) p$results), recursive = FALSE)
  works <- do.call(rbind, c(list(empty_table(corpus_columns$works)), lapply(results, function(w) {
    src <- w$primary_location$source
    data.frame(work_id = openalex_id(w$id), doi = na_chr(sub("^https://doi\\.org/", "", w$doi %||% NA)),
               title = na_chr(w$title), abstract = abstract_text(w$abstract_inverted_index),
               year = na_int(w$publication_year), type = na_chr(w$type),
               source_id = openalex_id(src$id), source_name = na_chr(src$display_name),
               publisher = na_chr(src$host_organization_name), is_retracted = na_lgl(w$is_retracted),
               cited_by_count = na_int(w$cited_by_count), stringsAsFactors = FALSE)
  })))
  authorships <- do.call(rbind, c(list(empty_table(corpus_columns$authorships)), lapply(results, function(w) {
    rows <- lapply(w$authorships %||% list(), function(a) {
      inst <- a$institutions %||% list(list())
      do.call(rbind, lapply(inst, function(i) {
        data.frame(work_id = openalex_id(w$id), author_id = openalex_id(a$author$id),
                   author_name = na_chr(a$author$display_name), position = na_chr(a$author_position),
                   institution_id = openalex_id(i$id), ror = na_chr(sub("^https://ror\\.org/", "", i$ror %||% NA)),
                   country_code = na_chr(i$country_code), stringsAsFactors = FALSE)
      }))
    })
    do.call(rbind, rows)
  })))
  references <- do.call(rbind, c(list(empty_table(corpus_columns$references)), lapply(results, function(w) {
    cited <- unlist(w$referenced_works)
    if (!length(cited)) return(NULL)
    data.frame(work_id = openalex_id(w$id), cited_id = openalex_id(cited), stringsAsFactors = FALSE)
  })))
  works$doi[works$doi %in% "NA"] <- NA_character_
  fs_corpus(works, authorships, references, provenance)
}

#' Collect works from OpenAlex
#'
#' Queries the OpenAlex `/works` endpoint with a filter (for example a journal,
#' an institution or a topic) and normalises the result. Set the environment
#' variable `OPENALEX_API_KEY` for a larger daily budget.
#'
#' @param filter An OpenAlex filter string, for example
#'   `"primary_location.source.id:S123,publication_year:2020-2023"`.
#' @param max_works Maximum number of works to retrieve.
#' @return An `fs_corpus` whose provenance records the query and time.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   fs_collect_openalex("institutions.ror:03ths8210,publication_year:2023", max_works = 50)
#' }
#' }
fs_collect_openalex <- function(filter, max_works = 1000) {
  if (!is.character(filter) || length(filter) != 1L || !nzchar(filter)) {
    stop("`filter` must be a single OpenAlex filter string.", call. = FALSE)
  }
  select <- "id,doi,title,publication_year,type,is_retracted,primary_location,authorships,referenced_works,cited_by_count,abstract_inverted_index"
  key <- Sys.getenv("OPENALEX_API_KEY")
  per_page <- min(200L, as.integer(max_works))
  pages <- list()
  n <- 0L
  page <- 1L
  repeat {
    url <- paste0("https://api.openalex.org/works?filter=", utils::URLencode(filter, reserved = FALSE),
                  "&per-page=", per_page, "&page=", page, "&select=", select,
                  if (nzchar(key)) paste0("&api_key=", key))
    json <- get_json(url)
    pages[[length(pages) + 1L]] <- json
    n <- n + length(json$results)
    if (!length(json$results) || n >= max_works || n >= (json$meta$count %||% 0)) break
    page <- page + 1L
  }
  fs_normalise_openalex(pages, provenance = list(source = "openalex", query = filter,
                                                 retrieved_at = utc_now()))
}

get_json <- function(url) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, Accept = "application/json",
                          `User-Agent` = "forsci R package (https://github.com/pablosancheznunez/forsci)")
  res <- curl::curl_fetch_memory(url, handle = h)
  if (res$status_code >= 400) stop("HTTP ", res$status_code, " from ", sub("api_key=[^&]+", "api_key=***", url), call. = FALSE)
  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
}

#' Read and link the Retraction Watch database
#'
#' `fs_read_retraction_watch()` reads the Retraction Watch CSV (distributed
#' openly by Crossref) and keeps the columns used here. `fs_link_retractions()`
#' adds, for each work of a corpus whose DOI appears as an original paper, the
#' nature of the notice, its date, its DOI and the stated reasons.
#'
#' A retraction notice is a document of the publisher; its stated reasons are
#' the publisher's. The linked rows are therefore `documentary` evidence,
#' except notices of nature "Retraction" whose reasons are stated, which are
#' `established` by the publisher and must be cited as such.
#'
#' @param path Path to the CSV file.
#' @return `fs_read_retraction_watch()`: a data frame with columns
#'   `original_doi`, `nature`, `retraction_date`, `retraction_doi`, `reasons`
#'   and `journal`.
#' @export
#' @examples
#' rw <- fs_read_retraction_watch(system.file("extdata", "retraction_watch_example.csv",
#'                                            package = "forsci"))
#' rw
#' fs_link_retractions(fs_simulate(seed = 1), rw)
fs_read_retraction_watch <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
  key <- tolower(gsub("[^A-Za-z]", "", names(raw)))
  col <- function(k) {
    i <- match(k, key)
    if (is.na(i)) stop("Not a Retraction Watch file: column '", k, "' not found.", call. = FALSE)
    raw[[i]]
  }
  reasons <- gsub("^;|;$", "", gsub("\\s*;\\s*\\+?", ";", sub("^\\+", "", col("reason"))))
  data.frame(original_doi = tolower(trimws(col("originalpaperdoi"))), nature = col("retractionnature"),
             retraction_date = col("retractiondate"), retraction_doi = tolower(trimws(col("retractiondoi"))),
             reasons = reasons, journal = col("journal"), stringsAsFactors = FALSE)
}

#' @rdname fs_read_retraction_watch
#' @param corpus An `fs_corpus`.
#' @param rw A data frame from `fs_read_retraction_watch()`.
#' @return `fs_link_retractions()`: a data frame with one row per matched
#'   notice: `work_id`, `doi`, the notice columns and `evidence_level`.
#' @export
fs_link_retractions <- function(corpus, rw) {
  if (!inherits(corpus, "fs_corpus")) stop("`corpus` must be an fs_corpus.", call. = FALSE)
  works <- corpus$works[!is.na(corpus$works$doi), c("work_id", "doi")]
  works$doi <- tolower(works$doi)
  out <- merge(works, rw, by.x = "doi", by.y = "original_doi")
  out$evidence_level <- ifelse(out$nature == "Retraction" & nzchar(out$reasons), "established", "documentary")
  out <- out[order(out$work_id), c("work_id", "doi", "nature", "retraction_date", "retraction_doi",
                                   "reasons", "journal", "evidence_level")]
  rownames(out) <- NULL
  out
}
