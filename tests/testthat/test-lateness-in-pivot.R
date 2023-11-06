test_that("create lateness policy - works correctly with one lateness policy", {
  policy <- list(
      list(
          category = "Labs",
          aggregation = "equally_weighted",
          weight = .5,
          lateness = list(
              interval = list(
                  c(-Inf, "00:00:00"),
                  c("00:00:01", Inf)
              ),
              scale = list(1, 0)
          ),
          assignments = c("Lab 1", "Lab 2", "Lab 3")
      )
  )
  
  lateness_table <- create_lateness_table(policy)
  
  expected_table <- data.frame(
      assignments = c("Lab 1", "Lab 2", "Lab 3"),
      category = c("Labs", "Labs", "Labs"),
      scale1 = c(1,1,1),
      scale2 = c(0,0,0),
      lb1 = c("-Inf", "-Inf", "-Inf"),
      ub1 = c(0,0,0),
      lb2 = c(1/60,1/60,1/60),
      ub2 = c("Inf", "Inf", "Inf")
  )
  
  expect_equal(lateness_table, expected_table)
  
})


test_that("create lateness policy -  two lateness policy of different length", {
    policy <- list(
        list(
            category = "Labs",
            aggregation = "equally_weighted",
            weight = .5,
            lateness = list(
                interval = list(
                    c(-Inf, "00:00:00"),
                    c("00:00:01", Inf)
                ),
                scale = list(1, 0)
            ),
            assignments = c("Lab 1", "Lab 2", "Lab 3")
        ),
        list(
            category = "Quiz",
            aggregation = "equally_weighted",
            weight = .5,
            lateness = list(
                interval = list(
                    c(-Inf, "00:00:00"),
                    c("00:00:01", "24:00:00"),
                    c("24:00:01", Inf)
                ),
                scale = list(1, 0.5, 0)
            ),
            assignments = c("Quiz 1", "Quiz 2")
        )
    )
    
    lateness_table <- create_lateness_table(policy)
    
    expected_table <- data.frame(
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Quiz 1", "Quiz 2"),
        category = c("Labs", "Labs", "Labs", "Quiz", "Quiz"),
        scale1 = c(1,1,1,1,1),
        scale2 = c(0,0,0, 0.5, 0.5),
        lb1 = c("-Inf", "-Inf", "-Inf", "-Inf", "-Inf"),
        ub1 = c(0,0,0,0,0),
        lb2 = c(1/60,1/60,1/60, 1/60, 1/60),
        ub2 = c("Inf", "Inf", "Inf", 1440, 1440),
        scale3 = c(NA, NA,NA, 0, 0),
        lb3 = c(NA, NA,NA, 1440+1/60, 1440+1/60),
        ub3 = c(NA, NA,NA, "Inf", "Inf")
    )
    
    expect_equal(lateness_table, expected_table)
    
})

test_that("create_lateness_policy - no lateness policy", {
    
    policy <- list(
        list(
            category = "Labs",
            aggregation = "equally_weighted",
            weight = .5,
            assignments = c("Lab 1", "Lab 2", "Lab 3")
        )
    )
    
    lateness_table <- create_lateness_table(policy)
    
    expect_null(lateness_table)
    
})