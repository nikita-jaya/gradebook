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