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
# create_folder(here::here('output', 'RData'))


## workflowr manages the website ------------------------------------
## when you install it the first time in RStudio and you restart RStudio, it should automatically be uploaded
## if not, just run load it with pacman
# pacman::p_load(workflowr)

# wflow_status()


# 01. index.Rmd ----------------------------------------------------
## Update when adding a new Rmd page
wflow_publish('analysis/index.Rmd',
              'Update text')

wflow_publish('analysis/about.Rmd',
              'Update info')

wflow_publish('analysis/license.Rmd',
              'Add complete links to license')

wflow_publish('README.md', 'Update text')

wflow_publish('analysis/01_VE_nomogram.Rmd',
              'Format numbers in text with sjmisc::big_mark()')

wflow_publish('analysis/02a_VE_screening_method_overall.Rmd',
              'Initial commit')

## to rename files
# wflow_rename(
#   here::here('analysis', 'VE_nomogram.Rmd'),
#   here::here('analysis', '01_VE_nomogram.Rmd')
#   )

