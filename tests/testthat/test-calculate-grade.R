test_that("drop na assignments -- drops nas", {
    merged_files <- data.frame(names = c("John Smith","John Smith", "John Smith", "John Doe", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412514, 3032412515, 3032412515, 3032412515),
                           sections = c("class-001", "class-001", "class-001", "class-001", "class-001", "class-001"),
                           assignments = c("lab_1", "lab_1", "dropped_discussion", "lab_2", "lab_2", "dropped_discussion"),
                           max_points = c(10, 10, 0, 30, 30, 0),
                           submission_time = c("1/19/23 22:20", "1/19/23 22:20","1/19/23 22:20", "1/19/23 22:20","1/19/23 22:20", "1/19/23 22:20"),
                           `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           category = c("Lab", "Lab", NA, "Lab", "Lab", NA),
                           slipdays = c(0,0, NA, 0,0, NA),
                           late_time1 = c("00:00:00", "00:00:00", NA, "00:00:00", "00:00:00", NA),
                           late_time2 = c("00:00:00", "00:00:00", NA, "00:00:00", "00:00:00", NA),
                           late_scale1 = c(0,0, NA, 0,0, NA),
                           late_scale2 = c(0,0, NA, 0,0, NA),
                           after = c(TRUE, TRUE, NA, TRUE, TRUE, NA),
                           weights = c(0,0,NA, 0,0, NA),
                           drops = c(0,0,NA,0,0, NA),
                           weighted_method = c("equally", "equally", NA, "equally", "equally", NA),
                           clobber = c("None", "None", NA, "None", "None", NA)
    )
    
    expected <- data.frame(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                                       email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                                       sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                                       sections = c("class-001", "class-001","class-001", "class-001"),
                                       assignments = c("lab_1", "lab_1", "lab_2", "lab_2"),
                                       max_points = c(10, 10, 30, 30),
                                       submission_time = c("1/19/23 22:20", "1/19/23 22:20","1/19/23 22:20", "1/19/23 22:20"),
                                       `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                                       category = c("Lab", "Lab", "Lab", "Lab"),
                                       slipdays = c(0,0, 0,0),
                                       late_time1 = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                                       late_time2 = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                                       late_scale1 = c(0,0, 0,0),
                                       late_scale2 = c(0,0, 0,0),
                                       after = c(TRUE, TRUE, TRUE, TRUE),
                                       weights = c(0,0, 0,0),
                                       drops = c(0,0,0,0),
                                       weighted_method = c("equally", "equally", "equally", "equally"),
                                       clobber = c("None", "None", "None", "None")
    )
  expect_equal(drop_na_assignments(merged_files), expected)
})

test_that("drop na assignments -- if no nas, no change", {
    merged_files <- data.frame(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                           sections = c("class-001", "class-001","class-001", "class-001"),
                           assignments = c("lab_1", "lab_1", "lab_2", "lab_2"),
                           max_points = c(10, 10, 30, 30),
                           submission_time = c("1/19/23 22:20", "1/19/23 22:20","1/19/23 22:20", "1/19/23 22:20"),
                           `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           category = c("Lab", "Lab", "Lab", "Lab"),
                           slipdays = c(0,0, 0,0),
                           late_time1 = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           late_time2 = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           late_scale1 = c(0,0, 0,0),
                           late_scale2 = c(0,0, 0,0),
                           after = c(TRUE, TRUE, TRUE, TRUE),
                           weights = c(0,0, 0,0),
                           drops = c(0,0,0,0),
                           weighted_method = c("equally", "equally", "equally", "equally"),
                           clobber = c("None", "None", "None", "None")
    )
    
    expected <- merged_files
    
    expect_equal(drop_na_assignments(merged_files), expected)
})


