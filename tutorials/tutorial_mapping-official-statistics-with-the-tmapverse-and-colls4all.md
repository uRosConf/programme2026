
## Mapping Official Statistics with the tmapverse and cols4all

Maps are among the most effective ways to communicate official statistics, but
producing them well requires both the right tools and careful design choices.
This tutorial introduces the
[tmapverse](https://cran.r-project.org/package=tmapverse): the tmap package
together with its growing family of extensions. With tmap (version 4),
participants learn to build thematic maps using a layered, grammar-of-graphics
approach that scales from quick exploration to publication-ready output, in
both static and interactive form.

[Back to program page](../README.md)


### Basic information

|Item     |entry |
|---------|------|
|Presentor(s) |[Martijn Tennekes](https://10mapz.com/) |
|Duration |90:00 |
|Materials|[course materials](https://github.com/r-tmap/tmap-tutorial-uros2026)|


### Learning Goals

After this course, participants will...

- understand the grammar-of-graphics logic behind tmap: shapes, layers,
  visual variables, scales, and layout
- be able to create common thematic map types (choropleths, bubble maps, and
  categorical maps) from vector data, and raster/RGB maps from stars or terra
  objects
- be able to use facets to create small multiples, including interactively
  with synced views
- be able to switch between static ("plot") and interactive ("view") map
  modes with the same code, and embed maps in R Markdown/Quarto documents and
  Shiny apps
- be able to choose and apply colorblind-friendly, perceptually sound color
  palettes with cols4all
- have seen the wider tmapverse in action (cartograms, glyph maps, and
  network maps) and know where to learn more


### Course contents

The following topics will be treated

- Introduction to tmap's grammar of graphics: `tm_shape()`, layers, and
  visual variables (fill, col, size, shape, ...)
- Vector data with sf: choropleth maps, bubble maps, and categorical maps
- Raster and spatiotemporal data with stars and terra: raster maps and RGB
  maps
- Facets for small multiples, including interactive facets with synced views
- Map layout: legends, titles, scale bars, north arrows
- Static vs. interactive maps: switching modes with `tmap_mode()`, and
  embedding maps in R Markdown/Quarto reports and Shiny apps
- Choosing color palettes with cols4all: perceptual uniformity,
  colorblind-friendliness, and how to pick a palette for your data
- A tour of the wider tmapverse: cartograms, glyph maps, and network maps,
  illustrated to give an overview of what's possible


### Target audience

What should a participating member already know?

We expect participants to have a working knowledge of base R and RStudio
projects. Prior exposure to spatial data (e.g. having seen an `sf` object
before) is helpful but not required.


### Before coming to the course

Please make sure you have installed R and RStudio and have the latest versions
of the following packages installed:

- tmapverse (which installs tmap, sf, and cols4all, among others)

You can install it with:

```r
install.packages("tmapverse")
```
