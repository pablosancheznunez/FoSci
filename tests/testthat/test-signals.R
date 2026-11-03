x <- fs_simulate(seed = 1)

test_that("every signal recovers the planted cases without false positives", {
  s <- rbind(fs_signals(x), fs_signal_productivity(x, include_individuals = TRUE))
  v <- fs_validate(s, attr(x, "truth"))
  expect_equal(v$recall, rep(1, nrow(v)))
  expect_equal(v$precision, rep(1, nrow(v)))
})

test_that("signal tables share columns, level and ordering rules", {
  s <- fs_signals(x)
  expect_s3_class(s, "fs_signals")
  expect_named(s, c("signal", "method_version", "unit_type", "unit_id", "unit_label", "observed",
                    "expected", "statistic", "n", "flagged", "evidence_level", "detail", "alternatives"))
  expect_true(all(s$evidence_level == "signal"))
  expect_true(all(nzchar(s$alternatives)))
  for (sig in unique(s$signal)) {
    ids <- s$unit_id[s$signal == sig & s$unit_type == s$unit_type[s$signal == sig][1]]
    expect_identical(ids, sort(ids))
  }
  expect_output(print(s), "not evidence of misconduct")
})

test_that("reciprocal citation stacking is labelled", {
  s <- fs_signal_citation_stacking(x)
  expect_equal(s$detail[s$unit_id %in% c("S7->S8", "S8->S7")], c("reciprocal", "reciprocal"))
})

test_that("person-level signals need an explicit opt-in", {
  expect_error(fs_signal_productivity(x), "include_individuals")
})

test_that("retraction rates use Wilson intervals and minimum sizes", {
  s <- fs_signal_retractions(x, min_works = 1e6)
  expect_false(any(s$flagged))
  expect_equal(fs_signal_retractions(x, by = "publisher")$unit_type[1], "publisher")
})

test_that("tortured phrases report what was found and what cannot be screened", {
  w <- x$works
  w$title[1] <- NA
  w$abstract[1] <- NA
  y <- fs_corpus(w, x$authorships, x$references)
  s <- fs_signal_tortured_phrases(y)
  expect_equal(attr(s, "not_screened"), 1)
  expect_match(s$detail[1], "for \"")
  own <- data.frame(tortured = "research on the application", expected = "-")
  expect_equal(nrow(fs_signal_tortured_phrases(x, own)), 12)
  expect_error(fs_signal_tortured_phrases(x, data.frame(a = 1)), "tortured")
})

test_that("signals handle tiny corpora", {
  tiny <- fs_corpus(x$works[1, ])
  expect_equal(nrow(fs_signal_title_templates(tiny)), 0)
  expect_equal(nrow(fs_signal_citation_stacking(tiny)), 0)
  expect_error(fs_signals("x"), "fs_corpus")
})
