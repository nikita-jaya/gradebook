test_that("from capital col names to lower case", {
    data <- data.frame(
      SID = c(3032412514),
      NAMES = c("John Smith"),
      EMAIL = c("john.smith@berkeley.edu"))
    
    lower_data <- lower_colnames(data)
    
    correct_data <- data.frame(
      sid = c(3032412514),
      names = c("John Smith"),
      email = c("john.smith@berkeley.edu"))

    expect_equal(lower_data, correct_data)
})

test_that("from  Title Case col names lto ower case", {
  data <- data.frame(
    `Sid` = c(3032412514),
    `User Name` = c("John Smith"),
    `Email` = c("john.smith@berkeley.edu"))
  
  lower_data <- lower_colnames(data)
  
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
  
  lower_data <- lower_colnames(data)
  
  correct_data <- data.frame(
    sid = c(3032412514),
    `user name` = c("John Smith"),
    email = c("john.smith@berkeley.edu"))
  
  expect_equal(lower_data, correct_data)
})

test_that("get duplicate ids - merge 2 duplicate sid to 1", {
  data <- data.frame(
    sid = c(3032412514, 3032412514),
    `names` = c("John Smith", "John Smith"),
    email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu"))

  one_duplicate <- get_duplicate_ids(data)

  correct_data <- data.frame(
    `sid` = c(3032412514),
    `names` = c("John Smith"),
    `email` = c("john.smith@berkeley.edu"))

  expect_equal(one_duplicate, correct_data)
})

test_that("get duplicate id - correct dimensions", {
  data <- data.frame(
    sid = c(3032412514, 3032412514),
    `names` = c("John Smith", "John Smith"),
    email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu"))
    
  one_duplicate <- get_duplicate_ids(data)
  
  expect_equal(nrow(one_duplicate), 1)
  expect_equal(ncol(one_duplicate), 3)
})


test_that("get duplicate ids - test with 2 duplicates  ", {
  data2 <- data.frame(
    sid = c(3032412514, 3032412514, 3032412515, 3032412515),
    `names` = c("John Smith", "John Smith", "John Doe", "John Doe"),
    email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"))
  
  two_duplicate <- get_duplicate_ids(data2)
  
  correct_data <- data.frame(
    `sid` = c(3032412514, 3032412515),
    `names` = c("John Smith", "John Doe"),
    `email` = c("john.smith@berkeley.edu", "john.doe@berkeley.edu"))
  
  expect_equal(two_duplicate, correct_data)
})

test_that("check data names - correct formatting", {
    data <- tibble::tibble(
        Names = c("John Smith", "John Smith", "John Doe", "John Doe"),
        Email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
        SID = c(3032412514, 3032412514, 3032412515, 3032412515),
        Sections = c("class-001", "class-001", "class-001", "class-001"),
        `Lab` = c(7, 10, 8, 9),
        `Lab - Max Points` = c(10, 10, 10, 10),
        `Lab - Submission Time` = c("1/20/2023 7:22:00 AM", "1/20/2023 9:25:00 AM", "1/20/2023 10:25:00 AM", "1/21/2023 1:25:00 AM"),
        `Lab - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "01:25:00")
    )
    expect_no_error(check_data_names(data))
})

test_that("check data names - incorrect number of rows", {
    data <- tibble::tibble(
        Names = c("John Smith", "John Smith", "John Doe", "John Doe"),
        Email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
        SID = c(3032412514, 3032412514, 3032412515, 3032412515),
        Sections = c("class-001", "class-001", "class-001", "class-001"),
        `Lab` = c(7, 10, 8, 9),
        `Lab - Max Points` = c(10, 10, 10, 10),
        `Lab - Submission Time` = c("1/20/2023 7:22:00 AM", "1/20/2023 9:25:00 AM", "1/20/2023 10:25:00 AM", "1/21/2023 1:25:00 AM"),
        `Lab - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "01:25:00"),
        `Lab 2` = c(4,5,5,5)
    )
    expect_error(check_data_names(data))
})

test_that("check data names - SID label is incorrect", {
    data <- tibble::tibble(
        Names = c("John Smith", "John Smith", "John Doe", "John Doe"),
        Email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
        sid = c(3032412514, 3032412514, 3032412515, 3032412515),
        Sections = c("class-001", "class-001", "class-001", "class-001"),
        `Lab` = c(7, 10, 8, 9),
        `Lab - Max Points` = c(10, 10, 10, 10),
        `Lab - Submission Time` = c("1/20/2023 7:22:00 AM", "1/20/2023 9:25:00 AM", "1/20/2023 10:25:00 AM", "1/21/2023 1:25:00 AM"),
        `Lab - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "01:25:00")
    )
    expect_error(check_data_names(data))
})


test_that("check data names - incorrect assignment formatting", {
    data <- tibble::tibble(
        Names = c("John Smith", "John Smith", "John Doe", "John Doe"),
        Email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
        SID = c(3032412514, 3032412514, 3032412515, 3032412515),
        Sections = c("class-001", "class-001", "class-001", "class-001"),
        `Lab` = c(7/10, 10/10, 8/10, 9/10),
        `Lab - Submission Time` = c("1/20/2023 7:22:00 AM", "1/20/2023 9:25:00 AM", "1/20/2023 10:25:00 AM", "1/21/2023 1:25:00 AM"),
        `Lab - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "01:25:00")
    )
    expect_error(check_data_names(data))
})
