test_that("score function - raw_over_max", {
  gs <- tibble::tibble(`Lab 1` = 1:5,
             `Lab 1 - Max Points` = rep(5,5)
             )
  grades_mat <- data.matrix(gs)
  
  actual <- raw_over_max(grades_mat, assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Lab 1"] <- expected[, "Lab 1"]/5
  
  expect_equal(actual, expected)
})


test_that("aggregation function - equally_weighted", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- equally_weighted(grades_mat, category = "Labs", 
                               assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- c(31, 32, 33, 34, 35)/60
  
  expect_equal(actual, expected)
})

test_that("aggregation function - weighted_by_points", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                   `Lab 1 - Max Points` = rep(5,5),
                   `Lab 2` = (5:1)/6,
                   `Lab 2 - Max Points` = rep(6,5),
                   Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- weighted_by_points(grades_mat, category = "Labs", 
                         assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- 6/11
  
  expect_equal(actual, expected)
})

test_that("aggregation function - max_score", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- max_score(grades_mat, category = "Labs", 
                               assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- c(5/6, 4/6, 3/5, 4/5, 1)
  
  expect_equal(actual, expected)
})

test_that("aggregation function - min_score", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- min_score(grades_mat, category = "Labs", 
                      assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- c(1/5, 2/5, 3/6, 2/6, 1/6)
  
  expect_equal(actual, expected)
})

test_that("aggregation function - min_score with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- min_score(grades_mat, category = "Labs", 
                      assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
  expect_equal(actual, expected)
})

test_that("aggregation function - max_score with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- max_score(grades_mat, category = "Labs", 
                      assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
  expect_equal(actual, expected)
})

test_that("aggregation function - none with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- none(grades_mat, category = "Labs", 
                               assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
  expect_equal(actual, expected)
})


test_that("aggregation function - none with more than one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- none(grades_mat, category = "Labs", 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <-c(31, 32, 33, 34, 35)/60
  
  expect_equal(actual, expected)
})
