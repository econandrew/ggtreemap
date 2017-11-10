# `ggtreemap` - (More) treemaps for `ggplot2`

This is a package for building treemaps in ggplot2. You can see the main features at

https://rawgit.com/econandrew/ggtreemap/master/poverty-treemap.html

The major advantage over other treemap packages it that you can do comparative treemaps of the kind seen there.

## Installing

It is not now, and may never be, on CRAN, so install it using devtools:

```r
devtools::install_github("econandrew/ggtreemap")
```

## Connection to `treemapify`

This package relies on the layout algorithms in David Wilkin's `treemapify`(https://github.com/wilkox/treemapify) package.
It takes a different philosophy to how treemaps work with `ggplot2`, however, treating a treemap as a kind of statistical
transformation (like a boxplot). This simplifies things, since you can then just use `geom_rect` and `geom_text` with `stat="treemap"` to
draw and label treemaps, respectively, rather than having to use special geoms.

It also obeys scale transformations (mainly `scale_y_reverse` and `scale_x_reverse`, but in principle also log scales and the like),
which `treemapify` does not.

It does, however, interact poorly with `gganimate`, though I haven't really figured out why yet.

Treemaps don't seem to fit that naturally into the grammar of graphics, so I think it's an open question as to whether this approach
is better or worse than that taken by `treemapify`.
