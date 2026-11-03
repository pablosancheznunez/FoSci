oa <- function() {
  jsonlite::fromJSON(system.file("extdata", "openalex_example.json", package = "forsci"), simplifyVector = FALSE)
}

test_that("OpenAlex responses normalise into the three tables", {
  x <- fs_normalise_openalex(oa())
  expect_s3_class(x, "fs_corpus")
  expect_equal(x$works$work_id, c("W0000001", "W0000002", "W0000003"))
  expect_equal(x$works$doi, c("10.9999/example.1", "10.9999/example.2", NA))
  expect_equal(x$works$abstract[1], "We apply counterfeit consciousness to records.")
  expect_true(is.na(x$works$abstract[2]))
  expect_equal(x$works$is_retracted, c(TRUE, FALSE, FALSE))
  expect_equal(nrow(x$authorships), 3)
  expect_equal(x$authorships$ror[1], "03ths8210")
  expect_equal(x$references$cited_id, c("W0000002", "W0000003"))
  expect_output(print(x), "3 works")
})

test_that("several pages and empty responses are accepted", {
  x <- fs_normalise_openalex(list(oa(), list(results = list())))
  expect_equal(nrow(x$works), 3)
  expect_equal(nrow(fs_normalise_openalex(list(results = list()))$works), 0)
})

test_that("fs_corpus validates its tables", {
  x <- fs_simulate(seed = 1)
  expect_error(fs_corpus(x$works[, -1]), "lacks column")
  expect_error(fs_corpus(rbind(x$works[1, ], x$works[1, ])), "unique")
  expect_error(fs_corpus("a"), "data frame")
  expect_s3_class(fs_corpus(x$works), "fs_corpus")
})

test_that("Retraction Watch notices link by DOI, case-insensitively", {
  rw <- fs_read_retraction_watch(system.file("extdata", "retraction_watch_example.csv", package = "forsci"))
  expect_equal(rw$reasons[1], "Paper Mill;Concerns/Issues About Data")
  links <- fs_link_retractions(fs_simulate(seed = 1), rw)
  expect_equal(links$work_id, c("W9001", "W9002"))
  expect_equal(links$evidence_level, c("established", "documentary"))
  bad <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", bad)
  expect_error(fs_read_retraction_watch(bad), "Not a Retraction Watch file")
})

test_that("the simulation is reproducible and leaves the RNG untouched", {
  set.seed(42)
  before <- stats::runif(1)
  set.seed(42)
  a <- fs_simulate(seed = 3)
  after <- stats::runif(1)
  expect_identical(before, after)
  expect_identical(a$works, fs_simulate(seed = 3)$works)
  expect_true(isTRUE(a$provenance$simulated))
})
