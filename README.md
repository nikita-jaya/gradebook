# gradebook

An R package to make course grade calculations easy, fast, and most importantly,
correct. Course grades are produced from a data frame of assignment grades along
with a grading policy file that specifies how they should be weighted when
combined into a final grade.

### Statement of Need

While the final grade at the end of a course is an elementary part of most
college courses, the computations for these grades quickly become deceptively
intricate, especially with larger STEM classes that use various complexities to
accommodate a diverse student body. Even though most classes use slight
variations of the same policies, many LMS cannot sustain these complex
computations. In response, courses will turn to hard-coded scripts. These scripts
quickly accumulate hundreds of lines of code, and there is no method to assess
accuracy of the final computation. 

`Gradebook` is an R package that maintains the structure and complexity of a
course grade while guaranteeing accuracy through comprehensive unit-testing. The
challenges of consistency and precision in grading systems are addressed by
applying the practices of data analysis and the principles of software development.
The rigorous unit-testing in the package minimizes computational error and
reduces the manual inputs, significantly lower the risks of typographic and
logical errors in scripts. Because of this, course grades can be computed accurately and
quickly: the accuracy allows course instructors to have reliable grade
computations, and the speed allows them to compute grades throughout the semester
in order to monitor student progress. The structure of the package -- and the
open-source nature of it -- allows for courses to contribute functionality that
is unique to their course. This R package also functions as the backend of the
[NemoGB Shiny app](https://github.com/gradebook-dev/gradebook-app.git), which lets the user create their grading policy file in a
straightforward way. 

------------------------------------------------------------------------

### Installation Instructions

To install `gradebook`, make sure to install `devtools` first. The `pkgdown` website for this version is at [gradebook-dev.github.io/gradebook/](https://gradebook-dev.github.io/gradebook/).

``` r
# install.packages("devtools")
devtools::install_github("gradebook-dev/gradebook")
library(gradebook)
```

------------------------------------------------------------------------

### Example Usage

Load in your Gradescope data using `read_gs()`.
```r
library(gradebook)
gs_data <- read_gs("gs_demo.csv")
```

Start by building a policy file that reflects the assignments from your Gradescope file and the structure of the syllabus. 
Using `get_assignments()` will give you a list of all the assignments' names within your Gradescope file.
```r
get_assignments(gs_data)
```
[1] "Discussion 1" "Discussion 2" "Final"        "Lab 1"        "Lab 2"        "Lab 3"        "Lab 4"       
[8] "Lab 5"        "Lab 6"        "Midterm" 


More information and guidance on building your policy file can be found in the [Building a Policy File vignette](https://gradebook-dev.github.io/gradebook/articles/policy-files.html).

This should be loaded in with `read_policy()`, and then course grades can be computed with `get_grades()`.
```r
policy <- read_policy("policy_demo.yaml")

grades <- get_grades(policy = policy, gs = gs_data)
```

------------------------------------------------------------------------

### Community Guidelines

We welcome the contribution and feedback of others in order to make grading an effective and informative task for any course. Please review our
[contributing](https://github.com/gradebook-dev/gradebook/blob/main/CONTRIBUTING.md) guidelines. By contributing to this project, you agree to abide by this guidelines.

------------------------------------------------------------------------

