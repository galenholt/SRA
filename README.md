# SRA
Analyses for the SRA project

## Overview

This repo (will) use two sets of analyses- one examining inundation in ANAE polygons for bird breeding opportunity, diversity of wetland types, and vegetation, and the other using the WERP toolkit to assess EWRs from flow. 

The first (merstyle) uses the [eFlowEval](https://github.com/galenholt/CC2) workflow. Notebooks to control the data processing, response models, and analyses are in `/merstyle`, and require `eFlowEval` (the dev branch) to be installed to run (see below).

The second (werpstyle) uses the [WERP toolkit](https://github.com/MDBAuth/WERP_toolkit) to run. 

The main goals are outlined in the "workplan" at `QAEL - SRA - SRA/SRA Environment Theme Workplan.docx`. 

Input data for eFlowEval analyses is largely in `QAEL - MER/Model/dataBase`.
Input data for WERP analyses are in ???

Output data (e.g. processed inundation into ANAEs, responses, EWRs) is in `QAEL - SRA - SRA/data_out`.

There is some processed data I've left on the HPC because it's 10s of Gb. 

## Getting started

Clone this repo.

Open it via the package file. It will auto-install renv and ask you to install packages. Try that. If it fails, see next.

Renv struggles to install git packages from the lockfile for some reason. The following will probably be done initially, and whenever we want to update these packages. Then after you manually install the toolkit and eFlowEval, run renv::install() again.

If you have [generated ssh keys](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent) and  [added them to github](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account), then use this. You might have to for werp.

```
renv::install('git@github.com:galenholt/CC2.git@dev', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)

renv::install('git@github.com:MDBAuth/WERP_toolkit.git', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)
```

If not, but are able to do credentials manually, i.e. [set up a PAT](https://github.com/settings/tokens). 

If you set up a PAT, then use the following to manage it in R.
```
credentials::set_github_pat()
```

We used to have to install the toolkit with ssh because we're not able to have personal PAT on MDBA. But it just worked for me? And Renv didnt work for CC2 but devtools did.

Then install with
```
renv::install('MDBAuth/WERP_toolkit')

renv::install('galenholt/CC2', ref = 'dev')
```

or 
```
devtools::install_github('MDBAuth/WERP_toolkit')

devtools::install_github('galenholt/CC2', ref = 'dev')
```



Then 
```
renv::install()
```
to get everything else.

If sf fails, you'll need to install the requisite C libraries, which usually involves asking the HPC maintainer.

## HPC sequence

I want to work in notebooks that work either locally or on an HPC.

I want to have R control the HPC parallelisation through foreach and future, and so use foreach loops with %dofuture% and modify the `plan`. 

That requires a central control process to spawn subsidiary runs. 

So, the thinking is

Use `any_R.sh` as the control process. It should point to the file to run.
But because we use notebooks, we need to `knitr::purl` them to R scripts. 
So any_R.sh calls `run_r_hpc.R`, which purls a notebook or passes through a script, and then runs it.
Then, that script should start a bunch of jobs.

HPCs often have to have some set of packages already installed that need compiled C libraries (especially sf). To access those, we need to add to libPaths, but that doesn't propagate through {future}s if it's done in a script. So, in the .Rprofile, add

```
if (grepl('^HPCNAME', Sys.info()["nodename"])) {
  renvpaths <- .libPaths()
  .libPaths(new = c(renvpaths,'/path/to/hpc/R/library' ))
}
```

where you get the path to the HPC R library by opening R outside the renv and typing `.libPaths()`.

### Typical run

Once everything's set up, use something like 

```
sbatch any_R.sh run_r_hpc.R "MER_data_processing/anae_area_inundated.qmd"
```

To start a master process in run_r_hpc that then fires off sub-slurms (presumably) in anae_area_inundated.qmd.

### HPC instructions

clone this to an HPC system. 
Send the data over as well.

cd into it

To initialise, we want renv to manage environments.

```
module load R/4.3`
R
renv::status()
```


