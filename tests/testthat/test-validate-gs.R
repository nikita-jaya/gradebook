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

# test_that("get duplicate ids", {
#   data <- data.frame(
#     sid = c(3032412514, 3032412514),
#     `name` = c("John Smith", "John Smith"),
#     email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu"))
# 
#   one_duplicate <- get_duplicate_ids(data)
# 
#   correct_data <- data.frame(
#     `sid` = c(3032412514),
#     `name` = c("John Smith"),
#     `email` = c("john.smith@berkeley.edu"))
# 
#   expect_equal(one_duplicate, correct_data)
# })

test_that("get duplicate id - correct dimensions", {
  data <- data.frame(
    sid = c(3032412514, 3032412514),
    `name` = c("John Smith", "John Smith"),
    email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu"))
    
  one_duplicate <- get_duplicate_ids(data)
  
  expect_equal(nrow(one_duplicate), 1)
  expect_equal(ncol(one_duplicate), 3)
})

