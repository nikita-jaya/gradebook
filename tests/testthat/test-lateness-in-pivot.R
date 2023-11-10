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

test_that("compute lateness - no lateness policy", {
    
    policy <- list(
        list(
            category = "Labs",
            aggregation = "equally_weighted",
            weight = .5,
            assignments = c("Lab 1", "Lab 2", "Lab 3")
        )
    )
    
    gs <- data.frame(
        SID = c(001, 002, 003),
        Lab = c(0.9, 0.8, 0.7),
        quiz = c(0.7, 0.8, 0.9)
    )
    
    expect_equal(compute_lateness(gs, policy), gs)
})

test_that("calculate lateness - works correctly with two lateness intervals", {
    
    lateness_table <- tibble::tibble(
        SID = c(001, 001, 001),
        Score = c(0.9, 0.9, 0.9),
        `Lateness (H:M:S)` = c("00:00:00", "00:05:00", "100:00:00"),
        assignments = c("Lab 1", "Lab 2", "Lab 3"),
        category = c("Labs", "Labs", "Labs"),
        scale1 = c(1,1,1),
        scale2 = c(0,0,0),
        lb1 = c("-Inf", "-Inf", "-Inf"),
        ub1 = c(0,0,0),
        lb2 = c(1/60,1/60,1/60),
        ub2 = c("Inf", "Inf", "Inf")
    )
    
    actual <- calculate_lateness(lateness_table)
    
    expected <- tibble::tibble(
        SID = c(001, 001, 001),
        assignments = c("Lab 1", "Lab 2", "Lab 3"),
        score_after_lateness = c(0.9, 0,0)
    )
    
    expect_equal(actual, expected)
    
})

test_that("calculate lateness - works correctly with three lateness intervals", {
    
    lateness_table <- tibble::tibble(
        SID = c(001, 001, 001),
        Score = c(0.9, 0.9, 0.9),
        `Lateness (H:M:S)` = c("00:00:00", "00:05:00", "100:00:00"),
        assignments = c("Lab 1", "Lab 2", "Lab 3"),
        category = c("Labs", "Labs", "Labs"),
        scale1 = c(1,1,1),
        scale2 = c(0.5,0.5,0.5),
        scale3 = c(0,0,0),
        lb1 = c("-Inf", "-Inf", "-Inf"),
        ub1 = c(0,0,0),
        lb2 = c(1/60,1/60,1/60),
        ub2 = c(1400, 1400, 1400),
        lb3 = c(1400+1/60,1400+1/60,1400+1/60),
        ub3 = c("Inf", "Inf", "Inf")
    )
    
    actual <- calculate_lateness(lateness_table)
    
    expected <- tibble::tibble(
        SID = c(001, 001, 001),
        assignments = c("Lab 1", "Lab 2", "Lab 3"),
        score_after_lateness = c(0.9, 0.45,0)
    )
    
    expect_equal(actual, expected)
    
})

test_that("calculate lateness - works correctly with two different lateness policies", {
    
    lateness_table <- expected_table <- tibble::tibble(
        SID = c(001, 001, 001, 001, 001),
        Score = c(0.9, 0.9, 0.9, 0.8, 0.8),
        `Lateness (H:M:S)` = c("00:00:00", "00:05:00", "100:00:00", "00:00:00", "25:00:00"),
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
    
    actual <- calculate_lateness(lateness_table)
    
    expected <- tibble::tibble(
        SID = c(001, 001, 001, 001, 001),
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Quiz 1", "Quiz 2"),
        score_after_lateness = c(0.9, 0,0, 0.8, 0)
    )
    
    expect_equal(actual, expected)
    
})
