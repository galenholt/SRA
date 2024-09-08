# simple R
source('directorySet.R')
library(eFlowEval)
library(foreach)
library(doFuture)
library(progressr)
doFuture::registerDoFuture()
plan(sequential)

source('R/woody_general.R')
source('R/red_gum.R')

with_progress({
  out_tib <- process_with_checks(
    process_function = 'red_gum',
    catchment = 'all',
    out_dir = datOut,
    nchunks = 1)
})
