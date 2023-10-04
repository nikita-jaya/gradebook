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