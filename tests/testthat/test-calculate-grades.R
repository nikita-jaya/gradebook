test_that("no clobber - returns same gs", {
  policy <- list(
    categories = list(
      list(
        category = "Lab",
        aggregation = "equally_weighted",
        assignments = c("Lab 1", "Lab 2")
      ),
      list(
        category = "Quiz",
        aggregation = "weighted_by_points",
        assignments = c("Quiz 1", "Quiz 2")
      )
    )
  )
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00")
  )
  
  actual <- apply_clobber(gs, policy)
  
  expect_equal(actual, gs)
})

test_that("no clobber - returns same gs", {
  policy <- list(
    categories = list(
      list(
        category = "Lab",
        aggregation = "equally_weighted",
        clobber = "Quiz",
        assignments = c("Lab 1", "Lab 2")
      ),
      list(
        category = "Quiz",
        aggregation = "weighted_by_points",
        assignments = c("Quiz 1", "Quiz 2")
      )
    )
  )
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab` = c(1, 0, 0.9, 0.5),
    `Quiz` = c(.5, 1, .85, .6)
  )
  
  actual <- apply_clobber(gs, policy)
  
  expected <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab` = c(1, 1, 0.9, 0.6),
    `Quiz` = c(.5, 1, .85, .6)
  )
  
  expect_equal(actual, expected)
})
