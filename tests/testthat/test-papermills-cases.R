x <- fs_simulate(seed = 1)

test_that("paper mill screen promotes only converging signals to pattern", {
  rw <- fs_read_retraction_watch(system.file("extdata", "retraction_watch_example.csv", package = "forsci"))
  pm <- fs_pm_screen(x, fs_link_retractions(x, rw))
  expect_true(all(pm$evidence_level %in% c("signal", "pattern")))
  expect_equal(pm$evidence_level == "pattern", pm$n_signals >= 2)
  expect_setequal(pm$work_id[pm$evidence_level == "pattern"], c("W9001", "W9002", "W9003", "W9004"))
  expect_true(pm$notice_states_paper_mill[pm$work_id == "W9001"])
  expect_equal(pm$notice_evidence_level[pm$work_id == "W9001"], "established")
})

test_that("accusatory wording is refused for signals and patterns", {
  expect_length(fs_check_wording("Twelve titles share one template.", "pattern"), 0)
  expect_equal(fs_check_wording("This work was Fabricated.", "signal"), "fabricated")
  expect_length(fs_check_wording("The publisher found fabrication.", "established"), 0)
})

test_that("cases enforce sources, alternatives and neutral wording", {
  expect_error(fs_case("t", "Which authors committed fraud?"), "neutrally")
  case <- fs_case("Templates", "Do these works share a template?")
  expect_error(fs_case_add(case, "W1", "Retracted.", "documentary"), "citable")
  expect_error(fs_case_add(case, "W1", "Similar titles.", "signal"), "alternatives")
  expect_error(fs_case_add(case, "W1", "Fake paper.", "signal", alternatives = "x"), "accusation")
  expect_error(fs_case_add(case, "W1", "x", "proven"), "evidence_level")
  case <- fs_case_add_signals(case, fs_signal_title_templates(x))
  case <- fs_case_add(case, "W9001", "Retracted by the publisher.", "established",
                      source = "https://doi.org/10.9999/notice.1")
  expect_equal(nrow(case$items), 13)
  expect_output(print(case), "established 1")
})

test_that("reports carry the notice, levels, alternatives and provenance", {
  case <- fs_case_add_signals(fs_case("Templates", "Do these works share a template?"),
                              fs_signal_title_templates(x))
  path <- fs_report(case, tempfile(fileext = ".html"), corpus = x)
  html <- paste(readLines(path, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Neither is evidence of misconduct")
  expect_match(html, "class=\"lvl signal\"")
  expect_match(html, "Conference series")
  expect_match(html, "seed = 1")
  path2 <- fs_report(fs_signals(x), tempfile(fileext = ".html"))
  expect_match(paste(readLines(path2), collapse = ""), "retractions@1.0.0")
  expect_error(fs_report(list(), tempfile()), "fs_case or fs_signals")
})
