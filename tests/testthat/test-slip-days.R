test_that("order assignments - correct order", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 1", "Lab 2")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2"))
})

test_that("order assignments - reverse order", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 2", "Lab 1")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2"))
})

test_that("order assignments - same time submission, keep gs order", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 2", "Lab 1")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2"))
})

test_that("order assignments - one missing submission time, add last", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c(NA, NA, NA),
    `Lab 3 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59")
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 1", "Lab 2", "Lab 3")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 3", "Lab 2"))
})

test_that("order assignments - two missing submission time, add last", {
  gs <- tibble::tibble(
    `Lab 1 - Submission Time` = c("1/15/2024 11:11:11", "1/15/2024 05:11:11", "1/15/2024 11:59:59"),
    `Lab 2 - Submission Time` = c(NA, NA, NA),
    `Lab 3 - Submission Time` = c("2/15/2024 11:11:11", "2/15/2024 05:11:11", "2/15/2024 11:59:59"),
    `Lab 4 - Submission Time` = c(NA, NA, NA),
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 3", "Lab 2", "Lab 4"))
})


test_that("order assignments - all missing submission time, add in alpha order", {
  gs <- tibble::tibble(
    `Lab 3 - Submission Time` = c(NA, NA, NA),
    `Lab 2 - Submission Time` = c(NA, NA, NA),
    `Lab 1 - Submission Time` = c(NA, NA, NA)
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 5,
    assignments = c("Lab 2", "Lab 1", "Lab 3")
  )
  actual <- order_assignments(gs, policy_item)
  expect_equal(actual,c("Lab 1", "Lab 2", "Lab 3"))
})

test_that("calculate slip days - all lateness removed", {
  gs <- tibble::tibble(
    `Lab 1 - Lateness (H:M:S)` = c("00:00:00", "47:00:00", "23:00:00"),
    `Lab 2 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "52:00:00"),
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 4,
    assignments = c("Lab 1", "Lab 2")
  )
  expected <- tibble::tibble(
    `Lab 1 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00"),
    `Lab 2 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00"),
    `Slip Days 1` = c(4,2, 0)
  )
  expect_equal(calculate_slip_days(gs, policy_item), expected)
})

test_that("calculate slip days - slip days run out for one student", {
  gs <- tibble::tibble(
    `Lab 1 - Lateness (H:M:S)` = c("00:00:00", "23:00:00", "23:00:00"),
    `Lab 2 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "52:00:00"),
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 2,
    assignments = c("Lab 1", "Lab 2")
  )
  expected <- tibble::tibble(
    `Lab 1 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00"),
    `Lab 2 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "28:00:00"),
    `Slip Days 1` = c(2,1, 0)
  )
  expect_equal(calculate_slip_days(gs, policy_item), expected)
})


test_that("calculate slip days - slip days run out for two student", {
  gs <- tibble::tibble(
    `Lab 1 - Lateness (H:M:S)` = c("00:00:00", "47:00:00", "23:00:00"),
    `Lab 2 - Lateness (H:M:S)` = c("00:00:00", "01:00:00", "52:00:00"),
    `Lab 3 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "23:00:17"),
  )
  policy_item <- list(
    name = "Slip Days 1",
    num_slip_days = 2,
    assignments = c("Lab 1", "Lab 2")
  )
  expected <- tibble::tibble(
    `Lab 1 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00"),
    `Lab 2 - Lateness (H:M:S)` = c("00:00:00", "01:00:00", "28:00:00"),
    `Lab 3 - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "23:00:17"),
    `Slip Days 1` = c(2,0, 0)
  )
  expect_equal(calculate_slip_days(gs, policy_item), expected)
})
