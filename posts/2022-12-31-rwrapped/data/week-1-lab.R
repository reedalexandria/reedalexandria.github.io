## ---------------------------------------------------------------------
# sandy's add on: check which package is not available on my machine.
# define packages needed for this class in a vector
pkg <- c("tidymodels",
    "gghighlight",
    "glue",
    "ggmosaic",
    "ggridges",
    "gridExtra",
    "infer",
    "janitor",
    "knitr",
    "kableExtra",
    "maps",
    "openintro",
    "patchwork",
    "quantreg",
    "tidyverse",
    "scales",
    "skimr",
    "caret",
    "palmerpenguins",
    "survival",
    "waffle",
    "ggrepel",
    "ggpubr",
    "openintro")
# which packages are not in my list of already installed package?
# these are the only ones i need to install
pkg2install <- pkg[which(!(pkg %in% rownames(installed.packages())))]

suppressMessages({install.packages(pkg2install)})


## ----setup, include=TRUE, echo=TRUE, eval=FALSE-----------------------
## suppressMessages({
##   install.packages(
##     "tidymodels",
##     "gghighlight",
##     "glue",
##     "ggmosaic",
##     "ggridges",
##     "gridExtra",
##     "infer",
##     "janitor",
##     "knitr",
##     "kableExtra",
##     "maps",
##     "openintro",
##     "patchwork",
##     "quantreg",
##     "tidyverse",
##     "scales",
##     "skimr",
##     "caret",
##     "palmerpenguins",
##     "survival",
##     "waffle",
##     "ggrepel",
##     "ggpubr",
##     "openintro"
##   )
## })


## ---- include=TRUE----------------------------------------------------

# You probably already have these packages installed, so let's just load them
library(tidyverse)
library(readr)
library(gt)
library(openintro)

# Where is the root directory where all your labs live? Change this filepath to work on your computer.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")


## ----include=TRUE-----------------------------------------------------
# `source()` is a helpful function for calling and running other R scripts.
?source

# `file.path()` is a helpful function for creating file paths from directory and file names
?file.path


## ----include=TRUE-----------------------------------------------------
source(file.path(rootdir,"labs","_common.R"))


## ----include=TRUE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

