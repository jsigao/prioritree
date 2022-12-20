# PrioriTree: a utility for improving geographic phylodynamic analyses in BEAST
An interactive browser utility---distributed as an R package (see https://jsigao.shinyapps.io/prioritree/ for a demo of the utility)---to help researchers specify priors in input files for---and process output files from---analyses of discrete biogeographic history performed using the `BEAST`.

## Installation
To install the stable version from CRAN (will be available soon):
```
install.packages("PrioriTree")
```

Alternatively, to install the developmental version directly from this GitHub repository:
```
install.packages("devtools")
devtools::install_github("jsigao/prioritree")
```

## Launching the Shiny App (Browser Interface)

`PrioriTree` is designed as an `R Shiny` app so most of the functionalities requires user interaction with the browser interface.
To launch the interface, use the following command:
```
library(PrioriTree)
launchPrioriTree()
```

## Manual
A manual of `PrioriTree` can be found at https://bookdown.org/jsigao/prioritree_manual/.
See the [`Quick Start`](https://bookdown.org/jsigao/prioritree_manual/quick-start.html) section in the manual for basic introduction to the program to get started, and refer to the [`Detailed Guide`](https://bookdown.org/jsigao/prioritree_manual/thorough-guide.html) section there for more detailed explanation and additional features of the program.

## Citation and Authors
`PrioriTree` is developed and maintained by [Jiansi Gao](mailto:jsigao@gmail.com) (whom to correspond with upon any questions or comments about the program), [Michael R. May](https://rothfelslab.berkeley.edu/home/mike-may/), [Bruce Rannala](http://www.rannala.org/), [Brian R. Moore](http://phylolab.org/).

The recommended citation for the current version of `PrioriTree` is:
```
Gao, Jiansi, Michael R May, Bruce Rannala, and Brian R Moore. 2022. “PrioriTree: a utility for improving phylodynamic analyses in BEAST.” bioRxiv. https://www.biorxiv.org/content/10.1101/2022.08.24.505196v1
```

## Description
`PrioriTree` is motivated by properties of the discrete-geographic models implemented in `BEAST` ([Lemey _et al_. 2009](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000520); [Edwards _et al_. 2011](https://www.sciencedirect.com/science/article/pii/S0960982211006452)) and issues with the commonly used default priors in `BEAST`, identified by [Gao _et al_. (2022)](https://www.medrxiv.org/content/10.1101/2022.08.24.22278802v1).
`PrioriTree` allows users to specify priors on the discrete-biogeographic model parameters in a biologically informed way (*e.g.*, specifying the prior on the average dispersal rate by expressing understanding on the expected number of pathogen dispersal events over the entire epidemic history).
The program also allows users to specify these prior assumptions in an interactive manner; it dynamically visualizes the resulting prior distribution according to users' specification in real time.
At the end, it generates a readily-runnable `BEAST` XML script (as well as the associated methods template) to perform the analysis that the user conceives.
The other main functionality provided by `PrioriTree` is setting up additional analyses (including posterior-predictive checking, data cloning, and robust Bayesian) to evaluate the impact of alternative discrete-biogeographic (prior) model specification and visualizing the result.
