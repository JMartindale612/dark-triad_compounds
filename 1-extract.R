# 1 Extract

# Set up and packages ----

# Initialised reproducible environment 
# renv::init()

require("dplyr")
require("psych")
require("Hmisc")
require("lavaan")

renv::snapshot()

square_correlation_matrix <- function(x, # Input is a data table
                                      method=c("pearson", "spearman"), 
                                      result=c("none", "html","latex")){
  # Compute the correlation matrix
  require(Hmisc) # Package used to calculate correlations
  x <- as.matrix(x) # Converts data table to matrix
  correlation_matrix<-rcorr(x, # Calculate correlation matrix 
                            type=method[1]) # Pearson by default
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-values
  
  ## Define significance levels, spacing is important
  mystars <- ifelse(p < .001, "*** ", 
                    ifelse(p < .01, "**  ", 
                           ifelse(p < .05, "*   ", "    ")))
  
  ## Truncate the correlation matrix to two decimals
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that maps correlations to stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R)," ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  Rnew <- as.data.frame(Rnew)
  
  ## Return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}

# 1 - B5/FFM/HEXACO - Dark Triad converge -----

# Need to test with the Big Five / FFM / HEXACO first
# Maybe FFNI / EPA / FFMI? Too close maybe

# Mottus (2020) discusses the utility of this work on p. 1182

## FFNI, FFMI, EPA development papers

# Test the convergence between an EPA, FFNI and FFMI prototype developed using the 
# FFM scores in their original development papers
# and the actual EPA, FFNI and FFMI scores
# then somehow replicate that with the Vize paper
# also check convergence with the psychopathy, narcissism and machiavellianism scores

## Vize (2020) - The core of the Dark Triad -----

# The "Core" of the Dark Triad: A test of competing hypotheses
# https://psycnet.apa.org/record/2019-75319-001
# https://osf.io/xey8h/

data_vize <- read.csv(url("https://osf.io/cv74y/download"), header = TRUE)

data1 <- filter(data, Sample_IDr==1)
psych::describe(data1[,3:30], fast = T)


test <- data_vize %>%
  select(ID, Sample_IDr,
         SD3_mach:HH_modesty,
         BFI_A:BFI2_trust,
         ipip_a1:ipip_a6,
         ipip_n1:ipip_c6)

test <- test %>%
  mutate(
    IPIP_A_mean = rowMeans(select(., starts_with("ipip_a")), na.rm=TRUE),
    IPIP_E_mean = rowMeans(select(., starts_with("ipip_e")), na.rm=TRUE),
    ipip_C_mean = rowMeans(select(., starts_with("ipip_c")), na.rm=TRUE),
    ipip_N_mean = rowMeans(select(., starts_with("ipip_n")), na.rm=TRUE),
    ipip_O_mean = rowMeans(select(., starts_with("ipip_o")), na.rm=TRUE)
  )

test <- test %>%
  mutate(
    IPIP_A_mean_rev = 6 - IPIP_A_mean,
    IPIP_E_mean_rev = 6 - IPIP_E_mean,
    ipip_C_mean_rev = 6 - ipip_C_mean,
    ipip_N_mean_rev = 6 - ipip_N_mean,
    ipip_O_mean_rev = 6 - ipip_O_mean
  )

test <- test %>%
  mutate(
    psych_AC = rowMeans(cbind(IPIP_A_mean_rev, ipip_C_mean_rev), na.rm=TRUE),
    psych_ACN = rowMeans(cbind(IPIP_A_mean_rev, 
                               ipip_C_mean_rev, 
                               ipip_N_mean_rev), na.rm=TRUE),
    psych_ACE = rowMeans(cbind(IPIP_A_mean_rev, 
                               ipip_C_mean_rev, 
                               IPIP_E_mean), na.rm=TRUE),
    psych_ACEN = rowMeans(cbind(IPIP_A_mean_rev, 
                               ipip_C_mean_rev, 
                               IPIP_E_mean,
                               ipip_N_mean_rev), na.rm=TRUE),
    narc_GN = rowMeans(cbind(IPIP_A_mean, IPIP_E_mean), na.rm=TRUE),
    narc_VN = rowMeans(cbind(IPIP_A_mean_rev, ipip_N_mean), na.rm=TRUE),
    mach = rowMeans(cbind(IPIP_A_mean_rev, ipip_C_mean), na.rm=TRUE)
  )

cormat <- test %>%
  select(SD3_mach:SRP_total,
         psych_AC:mach) %>%
  square_correlation_matrix()



# IPIP a2 is straightforwardness, a5 is modesty

IPIP.DD_model.sub<-'
#Measurement model  
  IPIP_A=~ipip_a1+ipip_a3+ipip_a4+ipip_a6
  DT_DD=~DD_mach+DD_narc+DD_psych
#Latent Corr
  IPIP_A ~ DT_DD
'

IPIP.DD_fit.sub<-cfa(IPIP.DD_model.sub, data=data_vize, group="Sample_IDr", estimator="MLM")
summary(IPIP.DD_fit.sub, standardized=T, fit.measures=T)







# 2 - Life outcomes ----


## EMOTIONS ----

# Understanding Psychological Responses to the COVID-19 Pandemic Through ESM Data: 
# The EMOTIONS Project
# https://openpsychologydata.metajnl.com/articles/10.5334/jopd.83
# https://osf.io/6kzx3/

osf_retrieve_file("cv74y") %>%
  osf_download(path = "data")


## Zettler (2021) - Personality & COVID

# The Role of Personality in COVID-19-Related Perceptions, Evaluations, and Behaviors:
# Findings Across Five Samples, Nine Traits, and 17 Criteria
# https://journals-sagepub-com.manchester.idm.oclc.org/doi/pdf/10.1177/19485506211001680
# https://osf.io/c96dx/

## Zettler (2020) - HEXACO meta-analysis

# The Nomological Net of the HEXACO Model of Personality: A Large-Scale Meta-Analytic Investigation
# https://osf.io/3ykq8/





## Big Five experimental paper? -----

## Life outcomes of personality -----

# Seeboth & Mottus (2018) also use this one
# https://osf.io/d3xb7/

# 3 - Experience sampling ----

# Daily dynamics of grandiose narcissism
# link - https://osf.io/we6tg/?view_only=f3a2eff968b044a4a136db10875ac3c7

data_narc <- read.csv(url("https://osf.io/7jeg5?view_only=f3a2eff968b044a4a136db10875ac3c7"), header = TRUE)
url <- "https://osf.io/4hg3b/download?view_only=f3a2eff968b044a4a136db10875ac3c7"

# Read the .csv file directly into a data frame
data_flux_state <- read.csv(url("https://osf.io/4hg3b/download?view_only=f3a2eff968b044a4a136db10875ac3c7"), header = TRUE)
data_flux_trait <- read.csv(url("https://osf.io/7jeg5/download?view_only=f3a2eff968b044a4a136db10875ac3c7"), header = TRUE)

data_flip_state <- read.csv(url("https://osf.io/ej482/download?view_only=f3a2eff968b044a4a136db10875ac3c7"), header = TRUE)
data_flip_trait <- read.csv(url("https://osf.io/sztq9/download?view_only=f3a2eff968b044a4a136db10875ac3c7"), header = TRUE)

# 4 - Assessment Centre / experiments ----

# Breil (2022) - https://onlinelibrary.wiley.com/doi/epdf/10.1111/peps.12507
# Interpersonal behavior in assessment center role-play exercises: Investigating structure, consistency, and effectiveness
# https://osf.io/by5qm/
# BFI-2-S used


