test_that("Extracting Weights", {
  cats <- list(list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Labs",
        aggregation = "equally_weighted",
        weight = 0.3,
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4", 
                        "Lab 5", "Lab 6")
      ), 
      list(
        category = "Discussions",
        aggregation = "weighted_by_points",
        weight = 0.1,
        assignments = c("Discussion 1", "Discussion 2")
      ),
      list(
        category = "Exams",
        aggregation = "weighted_mean",
        weight = 0.6,
        assignments = list(
          list(
            category = "Midterm Exam",
            weight = 0.2,
            assignment = "Midterm"
          ),
          list(
            category = "Final Exam",
            weight = 0.3,
            assignment = "Final"
          )
        )
      )
    )
  ))
  pol <- list(categories = cats)
  actual <- find_weights(pol)
  
  cats2 <- list(list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
   
    assignments = list(
      list(
        category = "Labs",
        aggregation = "equally_weighted",
        weight = 0.3,
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4", 
                        "Lab 5", "Lab 6")
      ), 
      list(
        category = "Discussions",
        aggregation = "weighted_by_points",
        weight = 0.1,
        assignments = c("Discussion 1", "Discussion 2")
      ),
      list(
        category = "Exams",
        aggregation = "weighted_mean",
        weight = 0.6,
        
        assignments = list(
          list(
            category = "Midterm Exam",
            weight = 0.2,
            assignment = "Midterm"
          ),
          list(
            category = "Final Exam",
            weight = 0.3,
            assignment = "Final"
          )
        ),
        weights = c(0.4, 0.6))
    ),
    weights = c(0.3, 0.1, 0.6)
  ))
  expected <- list(categories = cats2)
  expect_equal(actual, expected)
})