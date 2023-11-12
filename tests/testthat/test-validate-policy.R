test_that("flatten policy - works correctly", {
  policy <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      weight = 0.50,
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "equally_weighted",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "equally_weighted",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      weight = 0.50,
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  actual <- flatten_policy(policy)
  
  expected <- list(
    list(
      category = "Lab 1",
      aggregation = "equally_weighted",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      aggregation = "equally_weighted",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      weight = 0.50,
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  expect_equal(actual, expected)
  
})

test_that("flatten policy - unnested policy", {
  policy <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      weight = 0.50,
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  actual <- flatten_policy(policy)
  
  expect_equal(actual, policy)
  
})
