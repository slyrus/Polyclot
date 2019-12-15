
* tidyverse/tibble/dplyr vs. dataframes

Are we trying to capture the functionality of data frames or the data
manipulation features of tidyverse as embodied in tiblles and packages
like dplyr that operate on them? The dplyr mutate, select, filter,
pull, etc... functions are very nice and mean that my R code has much
less [...] and $ cruft in it. I'd like to shoot for an interface as
powerful and readable as the tidyverse stuff but there may be
arguments for the lower-level dataframe features as well
(performance?).

* make-data-frame

In R, this is usually done column-wise, not row-wise. Not sure which I
prefer.

* immutable interface?

What about add-rows / add-cols that return a new instance of a
dataframe without modifying the original?

* add-cols! function evaluation

I would have expected that the low-level add-cols! function would take
pairs of arguments of the form <name sequence-of-values>. I like the
functional interface, but wouldn't expect that to be the the default
low level column-adding funciton.

* add-cols! equivalent that adds or replaces the named column

It would be nice if there were a function (maybe there is?) like
add-cols! that would add a new column of the specified name if it
didn't exist, but would replace the column with the new values if it
previously existed.

* sel rows vs cols

I like the idea of splitting up the functions that return a subset of
columns (dplyr::select) and that return a subset of rows
(dplyr::filter). The names are arbitrary and imperfect, but I like
breaking these concepts up. Something like the R dataframe selection
operator that takes slices of rows and cols can be nice but is
something I find myself using less and less now that the dplyr::filter
and dplyr::select functions exist.

* joins

the dplyr inner\_join, left\_join, etc... work well enough. we should
start with these.

* row-based vs col-based

Why row-based vs. col-based?

* split dataframes off of Polyclot?

Should this be in a separate package? I can see uses outside of the
layered graphics stuff.

* dataframe vs data-frame?

We should be consistent.

* chart vs plot

Why do we call the stub a chart instead of a plot?
