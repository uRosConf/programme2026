
## Working with categorical data and codelists in R

Categorical variables are very common in official statistics. In this workshop
we will show the different ways of working with categorical variables and code
lists in R; from what is available in base R to more specialised packages.

[Back to program page](../README.md)


### Basic information

|Item         | entry                                |
|-------------|--------------------------------------|
|Presentor(s) | Jan van der Laan and Edwin de Jonge  |
|Duration     | 2h:30m                               |
|Materials    | [course materials](https://codeberg.org/CBS-ResearchAndDevelopment/uRos-2026-Workshop-WorkingWithCategoricalDataAndCodeLists) |




### Learning Goals

Categorical variables are very common in official statistics. Most of the
output statistical offices produce are tables where the population of interest
is summarised according to different categories from a standard statistical
classification.  Examples are age categories, economical activities (NACE),
educational level and many more. The possible values of these categories are
often stored in code lists. These consist of a set of permissible codes with
associated meta data such as labels, descriptions and hierarchies.

Then goal of this workshop is to give an overview of the different options
available in R for working with categorical data and code lists. After this
course, participants will

- Know in more detail how factors are handled in R. 
- Know which packages are available to make working with this type of data
  easier. 


### Course contents

In this workshop we will show the different ways of working with categorical
variables and code lists in R. 

- We start with the basics: the *factor* and different methods in base R for
  working with these and their pitfalls.
- The `forcats` package that provides methods that make some common operations
  such as recoding easier. 
- The *labelled* type used by the `haven` package which provides basis support
  for code lists. 
- And, finally, the `codelist` package which offers more extensive support for
  working with code lists.


### Target audience

The workshop is aimed at R-users of all levels and most of it will be hands-on
in the form of demonstrations in combination with short exercises. No prior
knowledge other than basic R skill is required. 

### Before coming to the course

Please make sure you have installed R 4.6 and an IDE (such as RStudio) for working
interatively with R and make sure that the 
[course materials](https://codeberg.org/CBS-ResearchAndDevelopment/uRos-2026-Workshop-WorkingWithCategoricalDataAndCodeLists)
are available on your computer. Further, make sure the latest versions of the following
packages are installed:

- `forecats`
- `haven`
- `codelist`

All of these packages are available on CRAN. 
