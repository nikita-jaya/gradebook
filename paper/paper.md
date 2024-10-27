---
title: 'Nemo Gradebook: An R Package for Calculating Course Grades'
tags:
  - R
  - education
  - data science
authors:
  - name: Nikita Jayaprakash
    orcid: 0009-0001-2708-0210
    equal-contrib: true
    affiliation: 1
  - name: Monika Voutov
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: "1, 2"
  - name: Andrew Bray
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: UC Berkeley, Department of Statistics
   index: 1
   ror: 00hx57361
 - name: UC Berkeley, College of Engineering
   index: 2
citation_author: Jayaprakash et al.
date: 25 October 2024
year: 2024
bibliography: paper.bib
journal: JOSS
---

# Summary

`Gradebook` allows for accurate and systematic computations of the final course letter grades. These computations require two inputs: a specifically structured YAML grading policy file representing the class syllabus and the assignment grades from Gradescope [@10.1145/3051457.3051466] (or other similar learning management systems). 
The YAML policy file in conjunction with the Gradescope comma-separated value (CSV) break down wide range of complex syllabi into the series of nested methodical aggregation steps.

# Statement of Need

While the final grade at the end of a course is an elementary part of most college courses, the computations for these grades quickly can be deceptively intricate, especially with larger STEM classes that use various complexities to accommodate a diverse student body. While most classes use slight variations of the same policies, many LMS cannot sustain these complex computations. In response, courses will turn to hard-coded scripts.These scripts quickly accumulate hundreds of lines of code, and there is no method to assess accuracy of the final computation.

`gradebook` is an R package that maintains the structure and complexity of a course grade while guaranteeing accuracy. Through the principles of software development, ..., [insert reference about software practices, testing, etc.]. Because of this, course grades can be computed accurately and quickly: the accuracy allows course instructors to have reliable grade computations, and the speed allows them to compute grades throughout the semester in order to monitor student progress. The structure of the package -- and the open-source nature of it -- allows for courses to contribute functionality that is unique to their course. This R package also functions as the backend of the NemoGB Shiny app, which lets the user create their grading policy file in a straightforward way [reference for gradebook-app github]. 

# Underlying Principle

`gradebook` is an R package that breaks down the calculation of a course grade into a series of nested aggregations. It accommodates the generic policies included in most syllabi: using averages or weighted averages to aggregate assignment scores into overarching category scores, applying lateness penalties, dropping the *n* lowest scores in a category. As previously mentioned, the structure of this package also allows for outside contribution of unique policies in order for any course structure to be computed with this package.

The details of the course grading structure -- usually detailed in the syllabu or on the class website -- can be articulated in YAML format using a series of accepted keys (e.g. `score`, `aggregation`, `lateness`, `drop_n_lowest`, etc.) and their corresponding inputs. The nested structure of this policy file should reflect the nested structure of the course grade. The assignment scores come directly from Gradescope in .csv format. These two files (the YAML policy file and the Gradescope .csv) function as the two inputs for `gradebook`'s primary and overarching function: `get_grades()`. After pulling the assignment data from Gradescope and converting their syllabus into a YAML policy file, this singular function computes the entirety of the final course grad computation.

While `get_grades()` encapsulates the entire functionality of the R package, it is compromised for four sequential functions:

-   `process_gs()` ensures the correct format of the Gradescope csv.

-   `process_policy()` similarly ensures the correct format of the policy file.

-   `reconcile_policy_with_gs()` checks the compatibility of the policy file and the Gradescope data.

-   `calculate_grades()` computes the course grades and returns the final grade (and the scores for every intermediate category) appended to the original Gradescope data.


# Comparison to Other Packages

Answer following question:  Do the authors describe how this software compares to other commonly-used packages?
[insert some reference about different gradebooks]

# Figures

Section of figures (if needed)...

# Acknowledgements

The authors would like to thank lain Carmichael, Calvin Carter, and Zach Turner for helpful ideas and discussions throughout the development of this project.

# References
