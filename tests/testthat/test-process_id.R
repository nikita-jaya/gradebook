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

test_that("merge data removes all duplicate student id's from sid column", {
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