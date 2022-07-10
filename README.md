  <!-- badges: start -->
  [![General badge](https://img.shields.io/badge/Field-Demography-red.svg)](https://shields.io/)
  [![Generic badge](https://img.shields.io/badge/DATA-HMD/HFD-blue.svg)](https://shields.io/)
  [![CRAN status](https://www.r-pkg.org/badges/version/Families)](https://CRAN.R-project.org/package=Families)
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#stablel)
  [![CircleCI build status](https://img.shields.io/circleci/build/github/jupyterhub/jupyterhub?logo=circleci)](https://circleci.com/gh/jupyterhub/jupyterhub)
  [![Github all releases](https://img.shields.io/github/downloads/Naereen/StrapDown.js/total.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
<!-- badges: end -->

*Families* identifies family and kin relationships in a virtual population
generated by *VirtualPop*. Individual identification
numbers (IDs) are the keys to retrieve information on individuals and
relationships. The package includes a few basic functions to navigate
the virtual population database. They retrieve information on mothers,
fathers, partners, children, siblings, aunts, uncles and cousins. They
also retrieve information on grandparents, great-grandparents and their
offspring. The individual information is limited to date of birth, date
of death, and IDs of partner, mother, father and children.

The multi-generation virtual population offers opportunities to map
social (family) networks and to adopt different perspectives on
population, including the perspective of a child or an elderly person.
The vignette *Families\_Virtual* describes
the package and illustrates its use. HTML and pdf versions of the
vignette are in folder *Families/doc*.

You should be able to install VirtualPop using the following R code:

        library(devtools)
        devtools::install_github("willekens/Families")
