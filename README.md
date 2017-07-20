# lme4plotpartial

# Short Description

R package to plot the complete pooling, no pooling, and partially pooled estimates for a mixed effects model based on [code from Mahr](https://tjmahr.github.io/plotting-partial-pooling-in-mixed-effects-models/).

# Installation

    devtools::install_github("jrosen48/lme4plotpartial")

# Use

Has one function, `plot_partial_pooling()`, used as follows:

    library(dplyr)
    plot_partial_pooling(storms, y_var = wind, x_var = pressure, group = year)
