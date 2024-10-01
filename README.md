# Maps


# Habitat vs. Process models

Analyses for the habitat-v-process analyses, which were a subset of the
SRA project.

## Overview

The analyses in `/merstyle` uses the
[eFlowEval](https://github.com/galenholt/eFlowEval) workflow. Notebooks
to control the data processing, response models, and analyses are in
`/merstyle`, and require `eFlowEval` (Holt, Macqueen, and Lester 2024)
to be installed to run (see below).

Input data for eFlowEval analyses are from ANAE (Brooks 2021) ALA
(Westgate et al. 2024) two-monthly inundation (Teng et al. 2023) and
soil moisture (Frost, Ramchurn, and Smith 2018). We expect them to be in
`QAEL - MER/Model/dataBase`, references to this location should change
if they are elsewhere.

Output data (e.g. processed inundation into ANAEs, responses, EWRs) goes
to datOut, typically `QAEL - SRA - SRA/data_out`.

## Getting started

Clone this repo.

Open it via the package file. It will auto-install renv and ask you to
install packages. Try that. If it fails, see next.

Renv struggles to install git packages from the lockfile for some
reason. The following will probably be done initially, and whenever we
want to update these packages. Then after you manually install the
toolkit and eFlowEval, run renv::install() again.

Then

    renv::install()

to get everything else.

If sf fails on Linux, you’ll need to install the requisite C libraries,
which usually involves asking the HPC maintainer.

## HPC sequence

I want to work in notebooks that work either locally or on an HPC.

I want to have R control the HPC parallelisation through foreach and
future, and so use foreach loops with %dofuture% and modify the `plan`.

That requires a central control process to spawn subsidiary runs.

So, the thinking is

Use `any_R.sh` as the control process. It should point to the file to
run. But because we use notebooks, we need to `knitr::purl` them to R
scripts. So any_R.sh calls `run_r_hpc.R`, which purls a notebook or
passes through a script, and then runs it. Then, that script should
start a bunch of jobs.

HPCs often have to have some set of packages already installed that need
compiled C libraries (especially sf). To access those, we need to add to
libPaths, but that doesn’t propagate through {future}s if it’s done in a
script. So, in the .Rprofile, add

    if (grepl('^HPCNAME', Sys.info()["nodename"])) {
      renvpaths <- .libPaths()
      .libPaths(new = c(renvpaths,'/path/to/hpc/R/library' ))
    }

where you get the path to the HPC R library by opening R outside the
renv and typing `.libPaths()`.

### Typical run

Once everything’s set up, use something like

    sbatch any_R.sh run_r_hpc.R "MER_data_processing/anae_area_inundated.qmd"

To start a master process in run_r_hpc that then fires off sub-slurms
(presumably) in anae_area_inundated.qmd.

### HPC instructions

clone this to an HPC system. Send the data over as well.

cd into it

To initialise, we want renv to manage environments.

    module load R/4.3`
    R
    renv::status()

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-brooks2021" class="csl-entry">

Brooks, Shane. 2021. “ANAE Classification of the Murray-Darling Basin
Technical Report, Revision 3.0.”

</div>

<div id="ref-frost2018" class="csl-entry">

Frost, A. J., A. Ramchurn, and A. Smith. 2018. “The Australian Landscape
Water Balance Model AWRA-l V6. Technical Description of the Australian
Water Resources Assessment Landscape Model Version 6.”

</div>

<div id="ref-holt2024" class="csl-entry">

Holt, Galen, Ashley Macqueen, and Rebecca E. Lester. 2024. “A Flexible
Consistent Framework for Modelling Multiple Interacting Environmental
Responses to Management in Space and Time.” *Journal of Environmental
Management* 367 (September): 122054.
<https://doi.org/10.1016/j.jenvman.2024.122054>.

</div>

<div id="ref-teng2023" class="csl-entry">

Teng, Jin, Dave Penton, Catherine Ticehurst, Ashmita Sengupta, Andrew
Freebairn, Steve Marvanek, Darran King, and Carmel A. Pollino. 2023.
“Two-Monthly Maximum Flood Water Depth Spatial Timeseries for the MDB
V20.” *CSIRO Data Collection*.
https://doi.org/<https://doi.org/10.25919/c5ab-h019>.

</div>

<div id="ref-westgate2024" class="csl-entry">

Westgate, Martin, Matilda Stevenson, Dax Kellie, and Peggy Newman. 2024.
*Galah: Biodiversity Data from the GBIF Node Network*.
<https://CRAN.R-project.org/package=galah>.

</div>

</div>
