## 0_run_ME.R --------------------
## olivier.duperrex@unisante.ch
## 2021-08-26
##
## Run these lines to update the Rmd files

## check where you are
here::here()


## create folders if they don't exists ------------------------------

create_folder <- function(x){
  if (!fs::dir_exists(x))
  fs::dir_create(x)
}


## do only once to create each folder --
# create_folder(here::here('output'))
# create_folder(here::here('output', 'png4ppt'))
# create_folder(here::here('output', 'xlsx'))


## workflowr manages the website ------------------------------------
## when you install it the first time in RStudio and you restart RStudio, it should automatically be uploaded
## if not, just run load it with pacman
# pacman::p_load(workflowr)

# wflow_status()


# 01. index.Rmd ----------------------------------------------------
## Update when adding a new Rmd page
wflow_publish('analysis/index.Rmd',
              'add VE')

wflow_publish('analysis/about.Rmd',
              'Update info')

wflow_publish('analysis/license.Rmd',
              'Update info')

wflow_publish('README.md', 'Update text')

wflow_publish('analysis/VE_nomogram.Rmd',
              'Update text')
