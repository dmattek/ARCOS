
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ARCOS

<!-- badges: start -->
<!-- badges: end -->

ARCOS stands for **A**utomated **R**ecognition of **Co**llective
**S**ignalling. The package implements an algorithm for identification
and tracking of spatially clustered objects in time series data. It
works with 1-, 2-, and 3D geometries.

The algorithm identifies collective protein activation in 2- and 3D cell
cultures over time. Such collective waves of protein activation have
been recently identified in various biological systems. They have been
demonstrated to play an important role in the maintenance of epithelial
homeostasis ([Gagliardi et al.,
2020](https://doi.org/10.1016/j.devcel.2021.05.007), [Takeuchi et al.,
2020](https://doi.org/10.1016/j.cub.2019.11.089), [Aikin et al.,
2020](https://doi.org/10.7554/eLife.60541)), in the acinar morphogenesis
([Ender et al., 2020](https://doi.org/10.1101/2020.11.20.387167)),
osteoblast regeneration ([De Simone et al.,
2021](https://doi.org/10.1038/s41586-020-03085-8)), and in the
coordination of collective cell migration ([Aoki et al.,
2017](https://doi.org/10.1016/j.devcel.2017.10.016), [Hino et al.,
2020](https://doi.org/10.1016/j.devcel.2020.05.011)).

Despite the focus on cell signalling, the algorithm can be also applied
to other spatially correlated phenomena that occur over time.

![](vignettes/resources/VideoS9_relativeto4D_half.mp4)

## Data format

ARCOS defines an `arcosTS` object that extends the `data.table`
[class](https://cran.r-project.org/web/packages/data.table/). In
practice, the `arcosTS` function adds additional attributes that
prescribe column names relevant for the analysis to the existing
`data.table` object.

Time series should be arranged in **long format**, where each row is
object’s location, time, and optionally the measurement value.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dmattek/ARCOS")
```

## Example

The following synthetic dataset contains 81 objects spaced on a 2D 9x9
lattice. Each object has an ID (columns `id`) and can assume two values
0 and 1 (column `m`), which corresponds to an inactive and active state.
The evolution of active states takes place over 8 consecutive frames
(column `t`).

``` r
library(ARCOS)
library(ggplot2)

dts = ARCOS::genSynth2D(inSeed = 7)
```

|   t |      x |        y |   m |  id |
|----:|-------:|---------:|----:|----:|
|   1 | 0.2287 | -0.15890 |   0 |   1 |
|   1 | 0.8803 | -0.11770 |   0 |   2 |
|   1 | 1.9310 |  0.07860 |   0 |   3 |
|   1 | 2.9590 |  0.18980 |   0 |   4 |
|   1 | 3.9030 | -0.04138 |   0 |   5 |
|   1 | 4.9050 |  0.06777 |   0 |   6 |

In the plot below, grey circles correspond to inactive and black to
active states of objects. The collective activation (*wave*) develops
over 8 time points.

<img src="man/figures/README-ex1plotTS-1.png" width="100%" />

The following snippet will identify the collective event and will store
the result in `dcoll`. We are interested in a collective event comprised
of *active* object, hence we select rows with `m>0`.

``` r
# Track collective events
dcoll = ARCOS::trackColl(dts[m>0], 
                         eps = 2.)
```

|   t |  id | collid.frame | collid |     x |     y |   m |
|----:|----:|-------------:|-------:|------:|------:|----:|
|   2 |  41 |            1 |      1 | 4.157 | 3.915 |   1 |
|   3 |  32 |            2 |      1 | 3.890 | 2.989 |   1 |
|   3 |  40 |            2 |      1 | 3.086 | 4.194 |   1 |
|   3 |  41 |            2 |      1 | 3.998 | 3.955 |   1 |
|   3 |  42 |            2 |      1 | 5.060 | 4.063 |   1 |
|   3 |  50 |            2 |      1 | 3.838 | 4.911 |   1 |

The `dcoll` table contains the results of spatio-temporal clustering.
Column `collid` stores a unique identifier of collective event. The
`collid.frame` column stores an identifier of collective event that is
unique only within a frame.

For visualisation, we can add convex hulls around collective events.

``` r
# Create convex hulls around collective events for visualisation
dcollch = dcoll[,
                .SD[grDevices::chull(x, y)],
                by = .(t,
                       collid)]
```

In the following plot, objects that participate in a collective event
are indicated by red dots. The red polygon indicates a convex hull.

``` r
p2 = ggplot(dts,
            aes(x = x,
                y = y)) +
  geom_point(aes(color = as.factor(m)), size = 5) +
  scale_color_manual(values = c("grey80",
                                "grey20")) +
  ggnewscale::new_scale_color() +
  geom_point(data = dcoll,
             aes(color = as.factor(collid)), size = 1) +
  geom_polygon(data = dcollch,
               aes(color = as.factor(collid)),
               fill = NA, 
               size = 1) +
  facet_wrap(~ t, ncol = 4) +
  coord_fixed(ratio=1) +
  theme_void() +
  theme(text = element_text(size = 20),
        legend.position = "none")

p2
```

<img src="man/figures/README-ex1plotColl-1.png" width="100%" />

### Save frames

The snippet below will save individual time frames as `png` files in the
`frames` folder located in the current working directory.

``` r
ARCOS::savePlotColl2D(dts, dcoll, 
                      outdir = "./frames",
                      xlim = c(-.5,9),
                      ylim = c(-.5,9),
                      plotwh = c(4,3),
                      imtype = "png")
```

Individual files can be later combined into a movie using software such
as [ffmpeg](http://ffmpeg.org).

For example, if you have `ffmpeg` installed on your system, create an
`mp4` movie at 2 frames/second and a 520 by 420 px resolution by typing
the following line in the command line:

``` bash
ffmpeg -framerate 2 -i "frames/F%04d.png" -vcodec libx264 -s 560x420 -pix_fmt yuv420p frames-all.mp4
```

### Visualise interactively

Interactive visualisation with an accompanying shiny app located in
`inst/shiny-examples/collVisApp`.

``` r
library(shiny)
library(plotly)
library(RColorBrewer)

ARCOS::runCollVis(dts, dcoll)
```
