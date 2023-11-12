
test_that("pivot gs - works correctly", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Sections` = c("Class 001", "Class 001", "Class 001", "Class 001"),
    
    `Names` = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson"),
    `Email` = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
                "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  actual <- pivot_gs(data)
  
  expected <- tibble::tibble(
    `SID` = rep(c(3032412514, 3032122516, 3032412516,3032412517), each = 4),
    `Sections` = rep("Class 001", times = 16),
    
    `Names` = rep(c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson"), each = 4),
    `Email` = rep(c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
                "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu"), each = 4),
    `Assignments` = rep(c("Lab 1", "Lab 2", "Lab 3", "Project 1"), times = 4),
    `Score` = c(1, 1, 0, 0.9, 0, 0, 0, 0, 0.9, 0.9, 0.9, 0.4, 0.5, 0.5, 0.5, 0),
    `Max Points` = rep(1, times = 16),
    `Submission Time` = c("1/19/2023 9:25:00 AM", "1/20/2023 9:25:00 AM", "0",
                          "1/22/2023 9:25:00 AM", "0", "0", "0", "0", 
                          "1/19/2023 10:00:00 AM", "1/20/2023 10:00:00 AM", 
                          "1/21/2023 10:00:00 AM", "1/22/2023 10:00:00 AM", "0",
                          "0", "1/21/2023 9:50:00 AM", "0"),
    `Lateness (H:M:S)` = rep("0:00:00", times = 16)
  )
  expect_equal(actual, expected)
})