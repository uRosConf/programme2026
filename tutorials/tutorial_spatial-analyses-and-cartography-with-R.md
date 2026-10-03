# Introduction to Spatial Analysis and Cartography with R

This tutorial offers an introduction to the main packages and tools available in the R ecosystem for manipulating spatial data and making thematic maps, with a focus on vector data.

A large part of the workshop will be devoted to handling spatial data with the `sf` package and performing simple geoprocessing operations. We will also cover the creation of static thematic maps with `ggplot2` and `mapsf`, as well as the use of `mapview` to easily create interactive maps for data exploration.

Finally, a short introduction to more specific spatial tools and topics will cover raster data, spatial smoothing, geocoding, OpenStreetMap data, and related questions.

The various concepts and tools will be illustrated through practical applications using the annual database of road traffic injury accidents.

[Back to program page](../README.md)

### Basic information

| Item         | Entry                                                                  |
| ------------ | ---------------------------------------------------------------------- |
| Presenter(s) | [Kim Antunez](https://github/antuki)                                   |
| Duration     | 90:00                                                                  |
| Materials    | [Course materials](https://antuki.github.io/uros2026_spatialanalysis/) |

### Learning Goals

After this course, participants will...

* understand the main concepts and data structures used for spatial analysis in R;
* be able to import, manipulate and analyse vector spatial data using the `sf` package;
* be able to perform basic spatial operations and geoprocessing (union, intersections, merges...) ;
* be able to create simple static thematic maps with `ggplot2` and / or `mapsf`;
* be able to create simple interactive maps for spatial data exploration with `mapview`;


### Course contents

The following topics will be treated:

* Introduction to spatial data and the R spatial ecosystem ;
* Vector data and the `sf` package ;
* Coordinate reference systems and spatial transformations;
* Basic spatial data manipulation and geoprocessing ;
* Thematic mapping with `ggplot2` ;
* Thematic mapping with `mapsf`  ;
* Interactive spatial data exploration with `mapview` ;
* More specific spatial tasks (raster data, spatial smoothing, geocoding, OpenStreeMap data...)


### Target audience

This workshop is an introduction. It is designed for R users who would like to **discover** spatial analysis and cartography in R.

We expect participants to have a basic working knowledge of R and to be familiar with using RStudio. No prior experience with spatial analysis or cartography is required.

### Before coming to the course

Please make sure you have installed R and RStudio and have the latest versions of the following packages installed:

```
packages <- c("dplyr", "tidygeocoder", "mapview", "sf", "osmdata",
              "RColorBrewer", "ggplot2", "readr",
              "ggspatial", "knitr", "sfnetworks", "tidygraph", "remotes",
              "btb")
install.packages(setdiff(packages, rownames(installed.packages()))) 
```

The course materials is available on [github](https://github.com/antuki/uros2026_spatialanalysis).

