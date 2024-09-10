# I don't think the renv install is necessary-it should auto-install since there's a project skeleton
# install.packages('renv')
install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform['pkgType'], R.Version()['os'], R.Version()['arch']))

# use pak to handle system dependencies on linux
if (grepl("unix", .Platform$OS.type)) {
    renv::install('yaml')
    deps <- renv::dependencies()
    depchars <- c(deps$Package, 'scico', 'ggthemes', 'furrr', 'git2r')
    depchars <- depchars[depchars != 'R']
    depchars <- depchars[depchars != 'HydroBOT']
    depchars <- unique(depchars)
    sysdeps <- pak::pkg_sysreqs(depchars)

    # build and run those
    system2(command = 'sudo', args = sysdeps$pre_install)
    system2(command = 'sudo', args = sysdeps$install_scripts)
    if (length(sysdeps$post_install) > 0) {
        system2(command = 'sudo', args = sysdeps$post_install)
    }

    # Should work, but doesn't always. I think because it only fixes *installed* packages, and renv won't install if they fail
    # pak::sysreqs_fix_installed(depchars)
}

# install R packages
# renv is much faster than remotes because it takes advantage of caching, but is much trickier to get the right commit.
  # It *should* be working now by using
  # `'git@github.com:MDBAuth/WERP_toolkit.git@BRANCH_NAME'` and rebuild = TRUE
  # and auto-updating the version in the package. But if not, use the remotes
  # line below

install.packages("devtools")
#devtools::install_git("git@github.com:MDBAuth/HydroBOT.git", ref = 'master', force = TRUE, upgrade = 'ask', git = 'external')


#renv::install('git@github.com:MDBAuth/WERP_toolkit.git@Georgia_fast', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)
renv::install('git@github.com:MDBAuth/WERP_toolkit.git@galen_working', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)
#renv::install('git@github.com:MDBAuth/WERP_toolkit.git', rebuild = TRUE, upgrade = 'always', git = 'external', prompt = FALSE)

#eFlowEval
renv::install('galenholt/eFlowEval')

#OTHER
renv::install('galenholt/galenholt.github.io@dev')

renv::install()
# Some extras
# renv without {remotes} will only install from main. So if we want to use a branch, we need to go with remotes directly
renv::install(c('scico', 'ggthemes', 'furrr', 'git2r', 'rmarkdown'))

# The newest version of the toolkit

# renv sometimes struggles with rebuilding and non-main branhes.
# so if you need to install something other than main, or main with the same version number, use remotes, but that installs all dependencies and is slow.
# renv::install('remotes')
# remotes::install_git('git@github.com:MDBAuth/WERP_toolkit.git', ref = 'galen_working', force = TRUE, upgrade = 'always', git = 'external')
