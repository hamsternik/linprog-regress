## Linear programming optimization problem involving linear regression

[![License](http://img.shields.io/:license-mit-blue.svg)](LICENSE)

Project based on building linear regression models which involving as a main part of linear programming task.

## Preffered algorithms used in project:

- **lm.fit** [building linear regression models for criterion and state variables]
- **linprog** [solving linear programming problem]

## Installation 

To run project from source, you should install next dependencies:

- Install [Shiny](https://shiny.rstudio.com/tutorial/lesson1/).
```R
> install.packages("shiny")
```
- Install [Rglpk](https://cran.r-project.org/web/packages/Rglpk/index.html).
```R
> install.packages("Rglpk")
```
- Install [XLConnect](https://cran.r-project.org/web/packages/XLConnect/index.html) and support XLConnectJars.
```R
> install.packages("XLConnect")
> install.packages("XLConnectJars")
```
- Install [modopt.matlab](https://github.com/rhochreiter/modopt.matlab).

For installation you should have installed **devtools**.
```R
> install_github("modopt.matlab", user="rhochreiter")
```

## Warning ⚠️️

If you use macOS as primary system and you get a trouble to run a project or troubles with XLConnection package, 
install a legacy java v.1.6. It will help.

Please, follow through [this guide](https://www.r-bloggers.com/getting-r-and-java-1-8-to-work-together-on-osx/) withing accomplish.

