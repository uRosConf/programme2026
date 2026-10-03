
## Managing statistical disclosure control from microdata to protected linked tables with rtauargus 

In this tutorial, we will follow a dissemination project, from microdata and
dissemination template to protected linked tables, including non-nested
hierarchies. 


[Back to program page](../README.md)


### Basic information

|Item     |entry |
|---------|------|
|Presentor(s) | [Nadège Ferrer-Pradines](https://github.com/nadegeferrerpradines) and [Julien Desclodure](https://jdesclodure.github.io/) |
|Duration |90:00 |
|Materials|[course materials](https://mywebsite.com/uroscourse.zip)|




### Learning Goals

After this course, participants will understand the basics of statistical disclosure control on tabulate data.

They will be able to:

- call tau-Argus from R with `rtauargus` to protect a single table or protect linked tables;
- produce hierarchies files needed by `rtauargus`;
- protect linked tables with non-nested hierarchies;
- assess the quantity of secrecy produced by tau-Argus


### Course contents

The following topics will be treated:

- tabulating fit-for-rtauargus data from microdata
- the difference between primary and secondary suppression;
- calling tab_rtauargus() on a single table to protect;
- how tables may be linked by their margins;
- calling tab_multi_manager() on a set of linked tables;
- how dissemination may follow intricate nomenclatures and lead to deep and/or non-nested hierarchies;
- producing .hrc files to describe hierarchies;
- calling tab_multi_manager() on a set of linked tables with hierarchies files;
- assessing the quantity of secrecy with `summary_secret = TRUE`
- further parameters: protection interval, alternative totals, etc.


### Target audience

What should a participating member already know?

We expect participants to have a working knowledge of base R, and RStudio projects.
Furthermore, some experience with `dplyr` is recommendable. 
Participants should also be familiar with tabulated data for dissemination in official statistics. Experience as data producer is not mandatory but would give more context to the goals pursued.


### Before coming to the course

Warning: you will **only be able to follow this tutorial with a Windows operating system**, as tau-Argus is not compiled to Linux to date.
Follow the instructions for TauArgus installation:
- [TauArgus4.2.3](https://github.com/sdcTools/tauargus/releases/tag/v4.2.3) : unzip the downloaded zip archive into a folder where you have read, write, and execute permissions; install the software in a directory path that does not contain spaces (for example, use C:\TauArgus instead of C:\Program Files\TauArgus) to prevent runtime errors.

Please make sure you have installed R and RStudio then install rtauargus with the following R command:

```
install.packages("remotes")
remotes::install_github(
  "InseeFrLab/rtauargus",
  build_vignettes = FALSE,
  upgrade = "never"
)
```

A GitHub repository will soon be available to download the R project dedicated to the tutorial.


