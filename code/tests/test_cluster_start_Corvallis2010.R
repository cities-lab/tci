require(testthat)

context("Test running scenarios for the Corvallis project with the cluster-based approach")

CLEAN.UP <- FALSE
#disable stdout
capture.output(source("../cluster/start_Corvallis_scenarios.R"))

test_that("Expected results is produced", {
  #results data
  expect_true(exists("tcost.trip"))
  expect_true(exists("tcost.hh"))
  expect_true(exists("tcost.inc"))
  expect_true(exists("tcost.tpurp"))
  expect_true(exists("tcost.HTAZ"))
  #plots
  expect_true(exists("pbox.ic"))
  expect_true(exists("pbox.income"))
  expect_true(exists("pbox.pr.ic"))
  expect_true(exists("pden.hhsize"))
  expect_true(exists("pden.ic"))
  expect_true(exists("pline.pr.ic"))
  expect_true(exists("maps"))
})
