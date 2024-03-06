# SRA
Analyses for the SRA project

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

Renv struggles to install git packages from the lockfile for some reason. The following should be done initially, and whenever we want to update the package.

```
renv::install('git@github.com:galenholt/CC2.git@dev', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)
```

Then 
```
renv::install()
```
to get everything else.

If sf fails, you'll need to install the requisite C libraries, which usually involves asking the HPC maintainer.
