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

Renv struggles to install git packages from the lockfile for some reason.

```
renv::install('git@github.com:galenholt/CC2.git@dev', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)
```

Then 
```
renv::install()
```
to get everything else.

If sf fails, you'll need to install the requisite C libraries, which usually involves asking the HPC maintainer.
