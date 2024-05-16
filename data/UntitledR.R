moderate_syllabus_data <- data.frame(
  names = c("Alice Smith", "Bob Johnson", "Charlie Brown", "David Wilson", "Eva White"),
  emails = c("alice.smith@berkeley.edu", "bob.johnson@berkeley.edu", "charlie.brown@berkeley.edu", "david.wilson@berkeley.edu", "eva.white@berkeley.edu"),
  SID = c(3032412515, 3032412516, 3032412517, 3032412518, 3032412519),
  sections = c("section 1", "section 1", "section 1", "section 1", "section 1"),
  "Homework 1" = c(1, 0.5, 1, 0.9, 1),
  "Midterm - assignment" = c(0.9, 0.5, 0.7, 0.8, 0.9),
  "Homework 2" = c(1, 0.7, 1, 1, 0.9),
  "Final - assignment" = c(0.7, 0.6, 0.8, 0.79, 0.86)
)


#save(df, file = "data/moderate_syllabus_data.rda")