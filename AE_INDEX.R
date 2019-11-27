## Data analysis files for: "Stratified Reward Structures and
## Competition in Markets for Creative Production"
##
## Balietti and Riedl (2019)
## -------------------------
##
## Experiment conducted on Amazon Mechanical Turk with nodeGame 3.5
## ----------------------------------------------------------------
##
## This file loads the initial data.
## ---------------------------------

## DIRECTORIES.

## Absolute path to this directory.
## (changed if R runs from a different folder than this, must end with "/").
THISDIR <- paste0(getwd(), '/')

## Experimental data directory.
DIR <- paste0(THISDIR, 'data/')

## Output images directory.
IMGDIR <- paste0(THISDIR, 'img/')
# Create IMGDIR if not existing
if (!file.exists(IMGDIR)) {
  dir.create(file.path(IMGDIR))
}
## Set to TRUE to save images to file system.
SAVEIMG <- FALSE

## DROPOUT ANALYSIS

## Set to TRUE to load data also from dropouts (required only for 3.2.5).
DO.DROPOUTS <- FALSE

## LOAD DATA.

source(paste0(THISDIR, 'AE_LOAD_DATASET.R'))


## CONSUMERS.
#############

## 3.1.1 Quality Across Markets: AE_reviews.R

## 3.1.2 Quality Consumption Under Constraints: AE_simulations_budget.R

## 3.1.3 Innovation: AE_innovation.R

## PRODUCERS.
#############

## 3.2.1 Heterogeneous Responses to Stratification: AE_heterogeneous.R

## 3.2.2 How Previous Success Impacts Innovation: AE_success_and_innovation.R

## 3.2.3 Fairness of Reviews: AE_fairness.R

## 3.2.4 Inequality in Earnings: AE_inequality.R

## 3.2.5 Participation: AE_dropouts.R

## A.4.3 Innovation: Alternative Measures: AE_innovation_alternative.R

## A.8 Copy Behavior: AE_copy.R

## MARKET.
##########

## 3.3.1 Emergence of a Quality Standard: AE_quality_standard.R

## 3.3.2 Emergence of a Style Norm: AE_style_norm.R

## 3.3.3 Miscoordination: AE_misco.R

## 3.3.4 Market Failures: AE_type1and2_errors.R


## EXPLORE FACES
################

## Visualize faces with certain featuers: AE_faces.R

## TIMINGS
##########

## Analysis of timing of different game steps: AE_times.R
