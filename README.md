
# ARCOS

<!-- badges: start -->
<!-- badges: end -->

ARCOS stands for **A**utomatic **R**ecognition of **Co**llective **S**ignalling. 

The goal of ARCOS is to identify and track spatially clustered objects in time series data in 1-, 2-, and 3D geometries. The algorithm tackles the problem of identification of protein activation in 2- and 3D cell cultures that occur collectively in neighbouring cells over time. Despite its focus on cell signalling, the algorithm can be also applied to other spatially correlated phenomena that occur over time.

Collective waves of protein activation have been recently identified in various biological systems. They have been demonstrated to play an important role in the maintenance of epithelial homeostasis ([1](https://doi.org/10.1101/2020.06.11.145573), [2](https://doi.org/10.1016/j.cub.2019.11.089), [3](https://doi.org/10.7554/eLife.60541)), in the acinar morphogenesis ([4](https://doi.org/10.1101/2020.11.20.387167)), and in the coordination of collective cell migration ([5](https://doi.org/10.1016/j.devcel.2017.10.016), [6](https://doi.org/10.1016/j.devcel.2020.05.011)).


Key features of the algorithm implemented in the `ARCOS::trackCollEvents` function:

- data for tracking should be organised in the long format where each row is object's location and time,
- the function accepts objects in a long-format `data.table`,
- the `data.table` [package](https://cran.r-project.org/web/packages/data.table/) is used as the main data structure throughout the ARCOS package,
- the `dbscan` [package](https://cran.r-project.org/web/packages/dbscan/) is used for the spatial clustering.


General flow of the algorithm:

1. In the first frame, every available object becomes a *seed* of a collective event.
3. The `dbscan` algorithm aims to cluster all objects in the current frame. Objects within a *threshold distance* are clustered into collective events with a minimum *threshold size*.
4. Move to the next frame and match objects to collective events identified in previous frames. To match objects between frames, calculate the Cartesian product of two long-format tables. One holds all current objects, the other holds all objects from collective events in the previous frame(s). 
5. All unmatched objects in the current frame form *seeds* of new collective events.

The algorithm flow prepared with the [code2flow](https://app.code2flow.com/nboDrmgQxXvp) web app.

![The algorithm flow](README-images/code2flow_R9K3s8.png =360x)

## Installation

You can install the source version of ARCOS from [GitHub](https://github.com/dmattek/ARCOS) with:

``` r
install.packages("devtools")
devtools::install_github("dmattek/ARCOS")
```

## Example

In this example 4 distinct objects are moving in 1 dimension over 5 time points. We aim to identify clusters of objects moving close to each other.

### Time sequence

The minimal data in the long format consists of 3 columns:

- `frame` with the frame number that corresponds to the time point,
- `objid` with the unique identifier of every object,
- `x` with the position of the object.


``` r
library(ARCOS)
library(data.table)

dtIn = data.table(frame = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
                  objid = c(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 4, 1, 4),
                  x = c(1, 3, 1.2, 2.5, 3.5, 0.9, 2.6, 2.9, 3.2, 1.1, 2.8, 3.1, 1, 3))
```

```
> head(dtIn, 3)
    frame objid   x
 1:     1     1 1.0
 2:     1     2 3.0
 3:     2     1 1.2
```

Each object has a distinct identifier represented by a different colour in the plot:

![Input data](README-images/4obj-5tpts.png =360x)

### Detection and tracking

In this step 3 objects on the right are grouped into a single collective event that spans 5 frames. A single object on the left forms a trivial single-object event. 

The most important parameter of the `trackCollEvents` function is the search radius `inEps`, which sets the distance for:

- the `dbscan` spatial clustering in a single time frame,
- possible objects that can be part of collective events identified in previous frame(s).

The minimum size of the spatial cluster is set using the `inMinPts` parameter, which is also passed to `dbscan`. The parameter `inNprev` determines the number of previous frames that are searched for collective events in order to match them to objects in the current frame.

The parameter `inCols` contains a list with column names of the input data (`frame`, `id`, `x`, `y`, `z`) and the name of the column with identifiers of collective events in the output (`collid`). The `trackCollEvents` function works in 1-, 2-, or 3D, therefore the names of respective position columns x/y/z need to be supplied depending on the geometry.

``` r
dtColl = trackCollEvents(dtIn,
                         inEps = 0.6,
                         inMinPts = 1L,
                         inNprev = 1L,
                         inCols = list(frame = "frame",
                                       x = "x",
                                       id = "objid",
                                       collid = "collid"),
                         inDeb = F)
```

The output contains 3 columns with the frame number, object identifier, and the calculated identifier of the collective event:

``` r
> head(dtColl, 3)
   frame objid collid
1:     1     1      1
2:     1     2      2
3:     2     1      1
```

### Visualisation

In order to visualise collective events we merge the table computed by the `trackCollEvents` function with the original table by the frame number (column `time`) and the object identifier (column `objid`):


``` r
dtIn = merge(dtIn, 
             dtColl, 
             by = c("frame", "objid"))
```

``` r
> head(dtIn, 3)
   frame objid   x collid
1:     1     1 1.0      1
2:     1     2 3.0      2
3:     2     1 1.2      1
```

Each trace is assigned an identifier of the collective event, which is represented by the shape of the point in the plot:

![Visualisation of collective events](README-images/4obj-5tpts-2coll.png =360x)
