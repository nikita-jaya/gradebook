test_that("merge data has accurate dimensions", {
  data <- data.frame(
      sid = c(3032412514, NA, 3032412516,
              3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),

      name = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
               "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Anderson"),
      email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
                "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
                "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
                "patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"
                )
     )
    processed_data <- process_id(data)
    
    expect_equal(nrow(processed_data), 7)
    expect_equal(ncol(processed_data), 3)
})

test_that("no duplicate student id's in sid column", {
  data <- data.frame(
    sid = c(3032412514, NA, 3032412516,
            3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),
    
    name = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
             "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Anderson"),
    email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
              "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
              "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
              "patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"
    )
  )
  processed_data <- process_id(data)
  
  unique_sids <- length(unique(processed_data$sid))
  total_rows <- nrow(processed_data)
  
  expect_equal(unique_sids, total_rows)
})


test_that("merges max value", {
  data <- data.frame(
    sid = c(3032412521, 3032412521),
    
    name = c("Patricia Anderson", "Patricia Anderson"),
    email = c("patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"),
    lab1 = c(0.8, NA),
    lab2 = c(0.9, NA),
    lab3 = c(NA, 0.85),
    #even if you have 2 values - keep higher
    project1 = c(0.9, 0.5),
    project2 = c(0, 1)
  )
  processed_data <- process_id(data)
  
  #the new data should look like this:
  new_data <- tibble(
    sid = c(3032412521),
    name = c("Patricia Anderson"),
    email = c("patricia.anderson@berkeley.edu"),
    lab1 = c(0.8),
    lab2 = c(0.9),
    lab3 = c(0.85),
    project1 = c(0.9),
    project2 = c(1)
  )
  
  expect_equal(processed_data, new_data)
})
