test_that("policy flattens to the correct form", {
    policy_nested <- list(
        list(category = "problem_sets",
             aggregation = "equally_weighted",
             assignments = list(
                 list(category = "ps_1",
                      aggregation = "weighted_by_points",
                      assignments = c("ps_1_written", "ps_1_code")),
                 list(category = "ps_2",
                      aggregation = "weighted_by_points",
                      assignments = c("ps_2_written", "ps_2_code")))),
        list(category = "exam",
             aggregation = "none",
             assignments = c("final_exam")))
    
    policy_flat <- list(list(category = "ps_1",
                             aggregation = "weighted_by_points",
                             assignments = c("ps_1_written", "ps_1_code")),
                        list(category = "ps_2",
                             aggregation = "weighted_by_points",
                             assignments = c("ps_2_written", "ps_2_code")),
                        list(category = "problem_sets",
                             aggregation = "equally_weighted",
                             assignments = c("ps_1", "ps_2")),
                        list(category = "exam",
                             aggregation = "none",
                             assignments = c("final_exam")))
    
    expect_identical(flatten_policy(policy_nested), policy_flat)
})
