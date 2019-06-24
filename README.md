# Linear Model R Visualization Package

## Summer Research, 2019

#### _Students:_ Jack Langston

#### _Faculty advisor:_ Hunter Glanz

### Objective

The objective of this summer research is to create an R package that allows user to visualize "all" visualizable linear models, with ease.

### Deliverables

**A GitHub repository which contains the following:**

1.  An R package for the linear model visualization functions. 

2.  A log of hours spent on the summer research by each student, which includes date, hours, and activity summary.

3.  A presentation to the Statistics Department.

4.  A presentation at the CSM annual research conference.

5.  A manuscript to submit to the [R Journal](https://journal.r-project.org/) detailing the work and submit an abstract to the [RStudio Conference](https://www.rstudio.com/conference/).

### Specific Aims

**1.  Utilize GitHub to collaborate on project materials and updates.**

  * [Github Desktop](https://desktop.github.com/)

  * Karl Broman's [github tutorial](http://kbroman.org/github_tutorial/)

  * Jenny Bryan's [Happy git with R](http://happygitwithr.com/).
  
  * Also check out [using version control with RStudio](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN) and [this video on Git and RStudio](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-managing-part-2/).
  
  * Introduction to [R projects](https://teachdatascience.com/projects/)


**2.  Adhere to good programming practices.**
  
  * Write all R code according to [Hadley Wickam's Style Guide](http://adv-r.had.co.nz/Style.html).
  
  * Use the [tidyverse style guide](http://style.tidyverse.org/) for an additional reference.
  
  * Use Hadley Wickham's [R for Data Science](http://r4ds.had.co.nz/) book as a reference (Ch19 also discusses functions).
  
  
  **3.  Create an R package that contains visualization functions.**  At a minimum, this should be downloadable through devtools; as time allows, consider putting it on CRAN.
    
  *  Hillary Parker's blog post [Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
  
  *  Hadley Wickham's [R packages](http://r-pkgs.had.co.nz/) book.
  
  *  RStudio's video on [Package writing in RStudio](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-programming-part-3/).
  
  
   **4.  Provide documentation for the R package.**

  *  Use the [roxygen package](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) to document code.
  
  *  Write a [vignette](http://r-pkgs.had.co.nz/vignettes.html) to accompany the package.
  
  *  Consider using [pkgdown](http://pkgdown.r-lib.org/index.html) to create a website. 
  
  
  **5. Review existing R packages and functionality for linear model visualization.**
  
  *  geom_smooth() in [ggplot2](https://ggplot2.tidyverse.org/)?
  
  *  How do you plot a regression line in plotly?
  
  *  Many approaches out there just plot predictions of fitted regression model. Find and review these!
  
  **6.  Create functions (or identify best existing ones) for items that are not currently easy to achieve in R.  (Make sure that these cannot be accomplished in the existing R packages).**
  
  *  Simple linear regression line
  
  *  Regression model with one categorical variable and one quantitative variable (parallel lines model)
  
  *  Regression model with one categorical variable, one quantitative variable, and the interaction term (different intercepts and slopes)
  
  *  Regression model with one quantitative variable and the interaction with a categorical variable (different slopes, but common intercept)
  
  *  Can these all accommodate polynomial models that include higher order terms of the one quantitative variable?
  
