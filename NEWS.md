# ggcognigen 1.2.2

* Updates based on changes to ggplot2 behavior related to linewidth and size aesthetics.

# ggcognigen 1.2.1

* Added `median_symbol` argument to `geom_boxplot2` to allow for a median line to be used instead of the symbol.
* Updates based on changes to dependencies ggplot2 and flextable.
* Fix height in `get_device_size` for 3 plot per page layout.
* Fix bug in `make_gmr_data` where invalid rows are removed.

# ggcognigen 1.2.0

* Updated to host publicly on GitHub.

# ggcognigen 1.1.2
* Added `spacing` argument to `geom_boxcount` to control the placement of the counts.
* Exported `make_gmr_table`.

# ggcognigen 1.1.1
* Update various `class` checks to use `inherits`.
* Remove dependency on kiwiexport package.
* Fix bug in `make_forestplot` where labels on the right side were wrongly ordered when data was re-ordered and stratified.

# ggcognigen 1.1.0

* Include new functions supporting the creation of forest plots and associated tables: `make_gmr_data`, `make_gmr_table`, and `make_forestplot`
* Update `scale_discrete_cognigen` so linetype aesthetics are respected.

# ggcognigen 1.0.1

* In `geom_boxcount`, fixed count placements when no outliers are shown.

# ggcognigen 1.0.0

* Added a `NEWS.md` file to track changes to the package.
