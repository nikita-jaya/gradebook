#
# process_assignments should have the following tests
# 1) 
#



test_that("", {
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
  lower <- process_assignments(processed_data)
  
         
  new_col_names <- c("lab1", "lab1_-_Max_Points", "lab1_-_Submission_Time","lab1_-_Lateness_(H_M_S)","lab2",
                     "lab2_-_Max_Points","lab2_-_Submission_Time","lab2_-_Lateness_(H_M_S)" )            
                
  expect_equal(new_col_names, colnames(lower))
  
})
