## HERE are the codes to update the Rmd files --------------------

## check where you are
here::here()




# wflow_rename_proj("vaccine_effectiveness_screening_method")

## workflowr manages the website ------------------------------------
# pacman::p_load(workflowr)
wflow_status()

wflow_publish(all = TRUE, 'Update project name')

# 01. index.Rmd ----------------------------------------------------
## Update when adding a new Rmd page
wflow_publish('analysis/index.Rmd',
              'add VE')

wflow_publish('analysis/about.Rmd',
              'Update info')

wflow_publish('analysis/license.Rmd',
              'Update info')

wflow_publish('analysis/VE_nomogram.Rmd',
              'Initial commit')



wflow_use_github("duperrex")
