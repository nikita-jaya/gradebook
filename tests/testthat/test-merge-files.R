test_that("create assigns table - works correctly", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Lab", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c("lab_1", "lab_2")
                       ),
                       list(name = "Discussion", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c("discussion_1")
                       )
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0)
    )
    
    expected_assigns_table <- data.frame(assignments = c("lab_1", "lab_2", "discussion_1"),
                                   category = c("Lab", "Lab", "Discussion"),
                                   slipdays = c(0,0,0),
                                   late_time1 = c("00:00:00", "00:00:00", "00:00:00"),
                                   late_time2 = c("00:00:00", "00:00:00", "00:00:00"),
                                   late_scale1 = c(0,0,0),
                                   late_scale2 = c(0,0,0),
                                   after = c(TRUE, TRUE, TRUE),
                                   weights = c(0, 0, 0),
                                   drops = c(0, 0, 0),
                                   weighted_method = c("equally", "equally", "equally"),
                                   clobber = c("None", "None", "None")
                                   )
  expect_equal(create_assigns_table(policy), expected_assigns_table)
})


test_that("create assigns table - works correctly with null assignments", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Lab", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c("lab_1", "lab_2")
                       ),
                       list(name = "Discussion", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c()
                       )
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0)
    )
    
    expected_assigns_table <- data.frame(assignments = c("lab_1", "lab_2"),
                                   category = c("Lab", "Lab"),
                                   slipdays = c(0,0),
                                   late_time1 = c("00:00:00", "00:00:00"),
                                   late_time2 = c("00:00:00", "00:00:00"),
                                   late_scale1 = c(0,0),
                                   late_scale2 = c(0,0),
                                   after = c(TRUE, TRUE),
                                   weights = c(0, 0),
                                   drops = c(0, 0),
                                   weighted_method = c("equally", "equally"),
                                   clobber = c("None", "None")
    )
    expect_equal(create_assigns_table(policy), expected_assigns_table)
})

test_that("create assigns table - works correctly with no assignments in policy", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Lab", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c()
                       ),
                       list(name = "Discussion", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c()
                       )
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0)
    )
    
    expected_assigns_table <- data.frame(category = character(),
                                         slipdays = double(),
                                         late_time1 = character(),
                                         late_time2 = character(),
                                         late_scale1 = double(),
                                         late_scale2 = double(),
                                         after = logical(),
                                         weights = double(),
                                         drops = double(),
                                         weighted_method = character(),
                                         clobber = character()
                                         )

    expect_equal(create_assigns_table(policy), expected_assigns_table)
})


test_that("data merge assigns - compatible assigns_table and pivot_df",{
    assigns_table <- data.frame(assignments = c("lab_1", "lab_2"),
                                category = c("Lab", "Lab"),
                                slipdays = c(0,0),
                                late_time1 = c("00:00:00", "00:00:00"),
                                late_time2 = c("00:00:00", "00:00:00"),
                                late_scale1 = c(0,0),
                                late_scale2 = c(0,0),
                                after = c(TRUE, TRUE),
                                weights = c(0, 0),
                                drops = c(0, 0),
                                weighted_method = c("equally", "equally"),
                                clobber = c("None", "None")
    )
    
    pivot_df <- data.frame(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                     email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                     sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                     sections = c("class-001", "class_001", "class-001", "class-001"),
                     assignments = c("lab_1", "lab_1", "lab_2", "lab_2"),
                     max_points = c(10, 10, 30, 30),
                     submission_time = c("1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20"),
                     `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    )
    
    expected <- data.frame(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                           sections = c("class-001", "class_001", "class-001", "class-001"),
                           assignments = c("lab_1", "lab_1", "lab_2", "lab_2"),
                           max_points = c(10, 10, 30, 30),
                           submission_time = c("1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20"),
                           `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           category = c("Lab", "Lab", "Lab", "Lab"),
                           slipdays = c(0,0,0,0),
                           late_time1 = c("00:00:00", "00:00:00","00:00:00", "00:00:00"),
                           late_time2 = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           late_scale1 = c(0,0,0,0),
                           late_scale2 = c(0,0,0,0),
                           after = c(TRUE, TRUE, TRUE, TRUE),
                           weights = c(0,0,0,0),
                           drops = c(0,0,0,0),
                           weighted_method = c("equally", "equally", "equally", "equally"),
                           clobber = c("None", "None", "None", "None")
    )
    
    expect_equal(data_merge_assigns(assigns_table, pivot_df), expected)
})

test_that("data merge assigns - compatible assigns_table and pivot_df",{
    assigns_table <- data.frame(assignments = c("lab_1", "lab_2"),
                                category = c("Lab", "Lab"),
                                slipdays = c(0,0),
                                late_time1 = c("00:00:00", "00:00:00"),
                                late_time2 = c("00:00:00", "00:00:00"),
                                late_scale1 = c(0,0),
                                late_scale2 = c(0,0),
                                after = c(TRUE, TRUE),
                                weights = c(0, 0),
                                drops = c(0, 0),
                                weighted_method = c("equally", "equally"),
                                clobber = c("None", "None")
    )
    
    pivot_df <- data.frame(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                           sections = c("class-001", "class_001", "class-001", "class-001"),
                           assignments = c("lab_1", "lab_1", "lab_2", "lab_2"),
                           max_points = c(10, 10, 30, 30),
                           submission_time = c("1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20"),
                           `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    )
    
    expected <- data.frame(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                           sections = c("class-001", "class_001", "class-001", "class-001"),
                           assignments = c("lab_1", "lab_1", "lab_2", "lab_2"),
                           max_points = c(10, 10, 30, 30),
                           submission_time = c("1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20"),
                           `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           category = c("Lab", "Lab", "Lab", "Lab"),
                           slipdays = c(0,0,0,0),
                           late_time1 = c("00:00:00", "00:00:00","00:00:00", "00:00:00"),
                           late_time2 = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
                           late_scale1 = c(0,0,0,0),
                           late_scale2 = c(0,0,0,0),
                           after = c(TRUE, TRUE, TRUE, TRUE),
                           weights = c(0,0,0,0),
                           drops = c(0,0,0,0),
                           weighted_method = c("equally", "equally", "equally", "equally"),
                           clobber = c("None", "None", "None", "None")
    )
    
    expect_equal(data_merge_assigns(assigns_table, pivot_df), expected)
})

test_that("data merge assigns - still runs if pivot_df has extra assignments",{
    assigns_table <- data.frame(assignments = c("lab_1", "lab_2"),
                                category = c("Lab", "Lab"),
                                slipdays = c(0,0),
                                late_time1 = c("00:00:00", "00:00:00"),
                                late_time2 = c("00:00:00", "00:00:00"),
                                late_scale1 = c(0,0),
                                late_scale2 = c(0,0),
                                after = c(TRUE, TRUE),
                                weights = c(0, 0),
                                drops = c(0, 0),
                                weighted_method = c("equally", "equally"),
                                clobber = c("None", "None")
    )
    
    pivot_df <- data.frame(names = c("John Smith","John Smith", "John Smith", "John Doe", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412514, 3032412515, 3032412515, 3032412515),
                           sections = c("class-001", "class_001", "class-001", "class_001", "class-001", "class_001"),
                           assignments = c("lab_1", "lab_1", "dropped_discussion", "lab_2", "lab_2", "dropped_discussion"),
                           max_points = c(10, 10, 0, 30, 30, 0),
                           submission_time = c("1/19/23 22:20", "1/19/23 22:20","1/19/23 22:20", "1/19/23 22:20","1/19/23 22:20", "1/19/23 22:20"),
                           `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00", "00:00:00", "00:00:00")
    )
    
    expected <- data.frame(names = c("John Smith","John Smith", "John Smith", "John Doe", "John Doe", "John Doe"),
                           email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                           sid = c(3032412514, 3032412514, 3032412514, 3032412515, 3032412515, 3032412515),
                           sections = c("class-001", "class_001", "class-001", "class_001", "class-001", "class_001"),
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
    
    expect_equal(data_merge_assigns(assigns_table, pivot_df), expected)
})