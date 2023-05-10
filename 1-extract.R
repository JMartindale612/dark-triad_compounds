# 1 Extract

# Set up and packages ----

# Initialised reproducible environment 
# renv::init()

require("osfr")

renv::snapshot()

## 1 - B5/FFM/HEXACO - Dark Triad converge -----

# Need to test with the Big Five / FFM / HEXACO first
# Maybe FFNI / EPA / FFMI? Too close maybe

# Vize (2020) -----

# The "Core" of the Dark Triad: A test of competing hypotheses
# https://psycnet.apa.org/record/2019-75319-001
# https://osf.io/xey8h/



## 2 - Outcomes -----

# EMOTIONS

# Understanding Psychological Responses to the COVID-19 Pandemic Through ESM Data: 
# The EMOTIONS Project
# https://openpsychologydata.metajnl.com/articles/10.5334/jopd.83
# https://osf.io/6kzx3/

test <- osf_retrieve_node("6kzx3") # Retrieve EMOTIONS project
osf_ls_files(test) # List of files

osf_download(test)

?osf_download

## Zettler (2021) - Personality & COVID

# The Role of Personality in COVID-19-Related Perceptions, Evaluations, and Behaviors:
# Findings Across Five Samples, Nine Traits, and 17 Criteria
# https://journals-sagepub-com.manchester.idm.oclc.org/doi/pdf/10.1177/19485506211001680
# https://osf.io/c96dx/


## Zettler (2020) - HEXACO meta-analysis

# The Nomological Net of the HEXACO Model of Personality: A Large-Scale Meta-Analytic Investigation
# https://osf.io/3ykq8/
