test_that("from capital col names to lower case", {
    data <- data.frame(
      SID = c(3032412514),
      NAME = c("John Smith"),
      EMAIL = c("john.smith@berkeley.edu"))
    
    lower_data <- check_colnames(data)
    
    correct_data <- data.frame(
      sid = c(3032412514),
      name = c("John Smith"),
      email = c("john.smith@berkeley.edu"))

    expect_equal(lower_data, correct_data)
})

test_that("from  Title Case col names lto ower case", {
  data <- data.frame(
    `Sid` = c(3032412514),
    `User Name` = c("John Smith"),
    `Email` = c("john.smith@berkeley.edu"))
  
  lower_data <- check_colnames(data)
  
  correct_data <- data.frame(
    sid = c(3032412514),
    `user name` = c("John Smith"),
    email = c("john.smith@berkeley.edu"))
  
  expect_equal(lower_data, correct_data)
})

test_that("no changes", {
  data <- data.frame(
    `sid` = c(3032412514),
    `user name` = c("John Smith"),
    `email` = c("john.smith@berkeley.edu"))
  
  lower_data <- check_colnames(data)
  
  correct_data <- data.frame(
    sid = c(3032412514),
    `user name` = c("John Smith"),
    email = c("john.smith@berkeley.edu"))
  
  expect_equal(lower_data, correct_data)
})


