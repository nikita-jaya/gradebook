test_that("check_keys - correct format",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quiz",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_equal(check_keys(policy, gs), policy)
})

test_that("check_keys - missing category argument",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_error(check_keys(policy, gs))
})

test_that("check_keys - incorrect key",{
  categories <- list(
    list(
      category = "Labs",
      agg = "min_score", #wrong key here
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_error(check_keys(policy, gs))
})