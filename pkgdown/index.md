<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/shinyfilter)](https://CRAN.R-project.org/package=shinyfilter)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/shinyfilter)](https://cranlogs.r-pkg.org/badges/grand-total/shinyfilter)
<!-- badges: end -->

# <img src="../man/figures/logo.png" width = "125px" align = "left" style="margin-right:30px; margin-top:50px;"/>

## Interdependent filters on table columns in shiny apps


### About *shinyfilter*

**`shinyfilter`** allows to connect `selectizeInputs` widgets as filters to a `reactable` table.

As known from **spreadsheet** applications, **column filters** are **interdependent**, so each filter only shows the values that are really available at the moment based on the current **selection in other filters**. Filter values currently not available (and also those being available) can be shown via **popovers** or **tooltips**.


### How you install *shinyfilter*

Execute `install.packages("shinyfilter", dependencies = TRUE)` in the R console to install the package including all packages it depends on.


### Contact

Follow me on Twitter: [@jsugarelli](https://twitter.com/jsugarelli)

Visit the package repo on GitHub: [https://github.com/jsugarelli/shinyfilter](https://github.com/jsugarelli/shinyfilter)
