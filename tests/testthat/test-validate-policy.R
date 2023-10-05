test_that("check names - policy file has correct names", {
  policy <- list(coursewide = list(course_name = "Course Name",
                                   description = "course description"),
                 categories = list(
                     list(name = "Name", slipdays = 0, late_time1 = "00:00:00",
                                        late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                                        after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                                        clobber = "None", assigns = c())
                     ),
                 cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0))
  
  expect_no_error(check_names(policy))
})

test_that("check names - policy file doesn't have nested files", {
    policy <- list(name = "Name", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c()
                            )
    
    expect_error(check_names(policy))
})


test_that("check names - cutoffs incorrectly formatted", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Name", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c())
                   ),
                   cutoff = c(90, 80, 70, 60, 0))
    
    expect_error(check_names(policy))
})


test_that("check names - one of categories is incorrectly formatted", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Name", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c()),
                       list(name = "Name", slipdays = 0, late_time1 = "00:00:00", late_scale1 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c())
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0))
    
    expect_error(check_names(policy))
})

test_that("check cat names - correct formatting", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Name", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c())
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0))
    category_names <- purrr::map(policy$categories, names) |> unlist()
    expect_true(check_cat_names(category_names))
})

test_that("check cat names - correct formatting", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Name", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00",after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c())
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0))
    category_names <- purrr::map(policy$categories, names)  |> unlist()
    expect_false(check_cat_names(category_names))
})

test_that("check assignment names - correct formatting", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Labs", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c("Lab 1", "Lab 2")
                            )
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0)
                   )
    
    pivot_df <- list(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                     email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                     sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                     sections = c("class-001", "class_001"),
                     assignments = c("Lab 1", "Lab 1", "Lab 2", "Lab 2"),
                     max_points = c(10, 10, 30, 30),
                     submission_time = c("1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20"),
                     `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
                     )
    
    expect_no_error(check_assignment_names(policy, pivot_df))
})

test_that("check assignment names - assignment is not in pivot_df", {
    policy <- list(coursewide = list(course_name = "Course Name",
                                     description = "course description"),
                   categories = list(
                       list(name = "Lab", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c("Lab 1", "Lab 2")
                       ),
                       list(name = "Discussion", slipdays = 0, late_time1 = "00:00:00",
                            late_time2 = "00:00:00", late_scale1 = 0, late_scale2 = 0,
                            after = TRUE, weight = 0, drops = 0, weighted_method = "equally",
                            clobber = "None", assigns = c("Discussions 1")
                       )
                   ),
                   cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0)
    )
    
    pivot_df <- list(names = c("John Smith","John Smith", "John Doe", "John Doe"),
                     email = c("john.smith@berkeley.edu", "john.smith@berkeley.edu", "john.doe@berkeley.edu", "john.doe@berkeley.edu"),
                     sid = c(3032412514, 3032412514, 3032412515, 3032412515),
                     sections = c("class-001", "class_001"),
                     assignments = c("Lab 1", "Lab 1", "Lab 2", "Lab 2"),
                     max_points = c(10, 10, 30, 30),
                     submission_time = c("1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20", "1/19/23 22:20"),
                     `lateness_(h_m_s)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    )
    
    expect_error(check_assignment_names(policy, pivot_df))
})