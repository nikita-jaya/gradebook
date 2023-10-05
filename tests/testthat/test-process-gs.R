test_that("merge data has accurate dimensions - duplicate sid", {
  data <- data.frame(
    sid = c(3032412514, 3032412314, 3032412516,
            3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),
    
    names = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
             "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Anderson"),
    email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
              "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
              "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
              "patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"
    )
  )
  processed_data <- process_id(data)
  
  expect_equal(nrow(processed_data), 8)
  expect_equal(ncol(processed_data), 3)
})

test_that("no duplicate student id's in sid column", {
  data <- data.frame(
    sid = c(3032412514, NA, 3032412516,
            3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),
    
    names = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
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
    
    names = c("Patricia Anderson", "Patricia Anderson"),
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
    names = c("Patricia Anderson"),
    email = c("patricia.anderson@berkeley.edu"),
    lab1 = c(0.8),
    lab2 = c(0.9),
    lab3 = c(0.85),
    project1 = c(0.9),
    project2 = c(1)
  )
  
  expect_equal(processed_data, new_data)
})


test_that("perfect data - change nothing", {
  data <- data.frame(
    sid = c(3032412514, 3032412314, 3032412516,
            3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032232521),
    
    names = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
             "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Jane"),
    email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
              "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
              "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
              "patricia.anderson@berkeley.edu", "patricia.jane@berkeley.edu"
    )
  )
  processed_data <- process_id(data)
  
  expect_equal(nrow(processed_data), 9)
  expect_equal(ncol(processed_data), 3)
})



test_that("process_assignments - remove white spaces from colnames", {
  processed_data <- tibble::tibble(
    `lab1` = c(1),
    `lab1 - Max Points` = c(1),
    `lab1 - Submission Time` = c("1/19/2023 9:25:00 AM"),
    `lab1 - Lateness (H:M:S)` = c("0:00:00"),
    
    `lab2` = c(1),
    `lab2 - Max Points` = c(1),
    `lab2 - Submission Time` = c("1/20/2023 9:25:00 AM"),
    `lab2 - Lateness (H:M:S)` = c("0:00:00"),
  )
  no_white_spaces <- process_assignments(processed_data)
  
  
  new_col_names <- c("lab1", "lab1_-_Max_Points", "lab1_-_Submission_Time","lab1_-_Lateness_(H_M_S)","lab2",
                     "lab2_-_Max_Points","lab2_-_Submission_Time","lab2_-_Lateness_(H_M_S)" )            
  
  expect_equal(new_col_names, colnames(no_white_spaces))
  
})
