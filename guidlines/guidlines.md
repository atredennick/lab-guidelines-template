# Lab guidelines for project, data, and code management

1. Excel is to be used exclusively in "read-only" mode.
2. All manipulations of raw data must be scripted, and the raw data file should 
never change.
3. Derived data and/or cleaned data must be saved in a different location (e.g.
a new folder called `clean_data`). Saving derived data in a different format
(e.g., .RDS or .RData) is at the discretion of the user, but be consistent!
4. Script data checks and tests before doing any analyses. Plot your data to
identify "weird" data points.
5. Write functional code.
6. Test all functions to make sure they give expected results. See the 
`testthat` R package for ideas (https://github.com/r-lib/testthat).