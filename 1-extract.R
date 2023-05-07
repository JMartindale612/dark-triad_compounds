# 1 Extract

# Set up and packages ----

# Initialised reproducible environment 
# renv::init()

require("osfr")



## Emotions project -----

# Secondary data obtained from EMOTIONS project, social interactions during COVID-19 pandemic
# https://osf.io/6kzx3/

test <- osf_retrieve_node("6kzx3") # Retrieve EMOTIONS project

osf_ls_files(test) # List of files
