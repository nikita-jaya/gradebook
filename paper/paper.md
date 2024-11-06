---
title: 'Nemo Gradebook: An R Package for Calculating Course Grades'
tags:
  - R
  - education
  - data science
authors:
- name: Nikita Jayaprakash
  orcid: "0009-0001-2708-0210"
  equal-contrib: true
  affiliation: 1
- name: Monika Voutov
  orcid: "0009-0000-9923-0954"
  equal-contrib: true
  affiliation: "1, 2"
- name: Andrew Bray
  orcid: "0000-0002-4037-7414"
  equal-contrib: true
  affiliation: 1
affiliations:
  - index: 1
    name: UC Berkeley, Department of Statistics
  - index: 2
    name: UC Berkeley, College of Engineering

citation_author: Jayaprakash et al.
date: "5 November 2024"
year: 2024
bibliography: paper.bib
output: 
  rticles::joss_article:
    keep_tex: true
journal: JOSS
---

# Summary

`Gradebook` allows for accurate and systematic computations of the final course letter grades. These computations require two inputs: a specifically structured YAML file representing the grading policy from the class syllabus and the assignment grades in CSV (comma-separated value) format from Gradescope [@10.1145/3051457.3051466] or other similar learning management systems. 
The package uses these two inputs to break down any complex syllabi into methodical computations that can be documented and tested.

# Statement of Need

While the final grade at the end of a course is an elementary part of most college courses, the computations for these grades quickly become deceptively intricate, especially with larger STEM classes that use various complexities to accommodate a diverse student body. Even though most classes use slight variations of the same policies, many LMS cannot sustain these complex computations. In response, courses will turn to hard-coded scripts. These scripts quickly accumulate hundreds of lines of code, and there is no method to assess accuracy of the final computation. 



`Gradebook` is an R package that maintains the structure and complexity of a course grade while guaranteeing accuracy through comprehensive unit-testing. The challenges of consistency and precision in grading systems are addressed by applying the practices of data analysis and the principles of software development. The rigorous unit-testing in the package minimizes computational error and reduces the manual inputs, significantly lowers the risks of typographic and logical errors in scripts ... [insert reference about software practices, testing, etc.]. Because of this, course grades can be computed accurately and quickly: the accuracy allows course instructors to have reliable grade computations, and the speed allows them to compute grades throughout the semester in order to monitor student progress. The structure of the package -- and the open-source nature of it -- allows for courses to contribute functionality that is unique to their course. This R package also functions as the backend of the NemoGB Shiny app [@Gradebook-App], which lets the user create their grading policy file in a straightforward way. 

Grading Workflow WITH Nemo Gradebook             |  Grading Workflow WITHOUT Nemo Gradebook
:-------------------------:|:-------------------------:
![](with_nemogb_workflow.png){height="150pt"}  |  ![](without_nemogb_workflow.png){height="150pt"}






# Underlying Principle

`Gradebook` breaks down the calculation of a course grade into a series of nested aggregations. It accommodates the generic policies included in most syllabi: applying lateness penalties, dropping the *n* lowest scores in a category, and using averages or weighted averages to aggregate assignment scores into overarching category scores. As previously mentioned, the structure of this package also allows for outside contribution of unique policies in order for any course structure to be computed with this package.

The details of the course grading structure -- usually detailed in the syllabus or on the class website -- can be articulated in YAML format using a series of accepted keys (e.g. `score`, `aggregation`, `lateness`, `drop_n_lowest`, etc.). More direction about creating a policy file is provided in the `Building a Policy File` vignette. The nested structure of this policy file reflects the nested structure of the course grade. The assignment scores come directly from Gradescope in a .csv file. These two files (the YAML policy file and the Gradescope data) function as the two inputs for `gradebook`'s primary and overarching function: `get_grades()`. After reading in the assignment data from Gradescope using `read_gs()`and reading in their YAML policy file (that reflects their course syllabus) using `read_policy()`, this singular function computes the entirety of the final course grade computation.

While `get_grades()` encapsulates the entire computational functionality of the R package, it is compromised for four sequential functions:

-   `process_gs()` ensures the correct format of the Gradescope csv.

-   `process_policy()` similarly ensures the correct format of the policy file.

-   `reconcile_policy_with_gs()` checks the compatibility of the policy file and the Gradescope data.

-   `calculate_grades()` computes the course grades and returns the final grade (and the scores for every intermediate category) appended to the original Gradescope data.


# Comparison to Other Packages

Most other commonly-used packages -- particularly for R packages -- are used for
grading on an assignment-level basis. For example, the `gradeR` package "helps
grade your students’s assignment submissions that are R Scripts" [@GradeR] whereas `gradebook` is used for the computations of the final, overall course grade.
The software that has the most similar computational purpose as `gradebook` are
popular learning-management systems (LMS) used in higher education. This includes
Canvas [@canvas_gradebook_guide], Moodle [@moodle_grade_calculations], Blackboard Learn [@blackboard_calculate_grades], and D2L Brightspace [@d2l_about_grades], all of which provide an
integrated gradebook that allow the instructor to specify the manner in which
assessment scores are used to calculate a final course grade. What makes Gradebook unique is its flexibility of functionality and its capacity for instructor collaboration and contribution:
the flexible YAML structure allows for the former and the open-source nature of the project allows for the latter.

# Acknowledgements

The authors would like to thank lain Carmichael, Calvin Carter, and Zach Turner for helpful ideas and discussions throughout the development of this project.

As of summer 2024, this project was funded by the an Instructional Technology and Innovation Micro Grant Program through the University of California, Berkeley. 

As of October 2024, there is a pending poster submission to Technical Symposium on Computer Science Education (SIGCSE) that utilizes this package, called "Your Grades Are Wrong - Nemo Gradebook: A tool for easy, accurate course grades". The poster was submitted by the same set of authors.


# References
