
library(rmutil)

description_deltaprior <- function(states_num, symmetry = T, delta_prior = "Poisson", poisson_default = T, poisson_lambda = 0, 
                                   alpha_beta = 1, beta_beta = 1) {
  
  delta_max <- ((!symmetry) + 1) * choose(states_num, 2)
  poisson_offset <- ifelse(symmetry, states_num - 1, 0)
  
  if (delta_prior == "Poisson") {
    
    poisson_offset_str <- ""
    if (symmetry) {
      poisson_offset_str <- paste0("<em>greater than</em> $k - 1$ (where $k$ is the number of areas, ", states_num, " for this dataset) ")
    }
    prior_dist <- paste0("is a Poisson distribution with rate parameter, ${\\lambda = ", formatC(poisson_lambda, digits = 2, format = "fg"), "}$ ")
    
    prior_mean95interval <- paste0("(prior mean = ", formatC(poisson_lambda, digits = 2, format = "fg"), "; $95\\%$ prior interval = ${[", 
                                   paste(qpois(c(0.025, 0.975), lambda = poisson_lambda), collapse = ", "), "]}$")
    
    poisson_signif <- "; "
    poisson_beastdefault_confirm <- ""
    if (poisson_default) {
      poisson_signif <- "), expressing an explicit and very informative prior preference for biogeographic models with the minimal number of dispersal routes ("
      
      poisson_beastdefault_citation <- c("(Lemey et al., 2009)", "(Edwards et al. 2011)")[(!symmetry) + 1]
      poisson_beastdefault_confirm <- paste0(" This is the default prior in BEAST for ", ifelse(symmetry, "symmetric", "asymmetric"),
                                                 " model ", poisson_beastdefault_citation, ".")
    }
    
    pprior_dist <- paste0("<br><br>The prior probability that each dispersal route exists is thus ", formatC((poisson_lambda + poisson_offset)/delta_max, digits = 2, format = "fg"), ".")
    
    deltaprior_description <- paste0("The prior on the number of dispersal routes ", poisson_offset_str, prior_dist, 
                                     prior_mean95interval, poisson_signif, "see figure on the right). ", pprior_dist, poisson_beastdefault_confirm)
    
  } else if (delta_prior == "Uniform") {
    
    prior_dist <- paste0("is a Uniform distribution over 0 to the maximum value, ", delta_max, " (when dispersal-route indicators are all one). ")
    prior_mean95interval <- paste0("(prior mean = ", delta_max/2, "; $95\\%$ prior interval = ${[", 
                                   paste(ceiling(qunif(c(0.025, 0.975), min = 0, max = delta_max)), collapse = ", "), "]}$")
    
    pprior_dist <- paste0("<br><br>The prior probability that each dispersal route exists is thus uniformly distributed between 0 and 1.")
    
    deltaprior_description <- paste0("The prior on the number of dispersal routes ", prior_dist, prior_mean95interval, 
                                     "; see figure on the right). ", pprior_dist)
    
  } else if (delta_prior == "Beta-Binomial") {
    
    prior_dist <- "is a Binomial distribution " 
    prior_mean95interval <- paste0("(prior mean = ", formatC(alpha_beta/(alpha_beta + beta_beta) * delta_max, digits = 2, format = "fg"), "; $95\\%$ prior interval = ${[", 
                                   paste(rmutil::qbetabinom(c(0.025, 0.975), size = delta_max, m = alpha_beta/(alpha_beta + beta_beta), s = alpha_beta + beta_beta), collapse = ", "), "]}$")
    
    pprior_dist <- paste0("<br><br>The success probability ($\\textit{i.e.}$, the probability that each dispersal route exists), $p$, of the Binomial distribution is treated as a random variable to be estimated from the data;", 
                          " the prior on $p$ is a Beta distribution with the first shape parameter = ", alpha_beta, ", and the second shape parameter = ", beta_beta)
    pprior_mean95interval <- paste0(" (prior mean = ", formatC(alpha_beta/(alpha_beta + beta_beta), digits = 2, format = "fg"), "; $95\\%$ prior interval = ${[", 
                                    paste(formatC(qbeta(c(0.025, 0.975), alpha_beta, beta_beta), digits = 2, format = "fg"), collapse = ", "), "]}$).")
                          
    deltaprior_description <- paste0("The prior on the number of dispersal routes ", prior_dist, prior_mean95interval, 
                                     "; see figure on the right). ", pprior_dist, pprior_mean95interval)
  }
  
  return(deltaprior_description)
}


description_muprior <- function(mu_prior = "CTMC rate-ref (BEAST default)", tree_length = 1, hierachexp_mu95interval = NULL, 
                                hierachexp_alphaofgamma = 0.5, hierachexp_dispersaleventsnummean = 1, 
                                hierachexp_dispersaleventsnum95interval = NULL, empinformed_dispersaleventsnummean = 1, parsimonyscore_quantile = 0.5) {
  
  muprior_additional <- ""
  dispersaleventsnumprior_parsimonyscore_quantile <- paste0("putting the parsimony score at the $", 
                                                            formatC(parsimonyscore_quantile * 100, digits = 3, format = "fg"),
                                                            "\\%$ quantile of the prior distribution.\n")
  
  if (mu_prior == "CTMC rate-ref (BEAST default)") {
    
    prior_dist <- paste0("a Gamma distribution with shape parameter ${\\alpha = 0.5}$ and rate parameter ${\\beta = T}$ (where $T$ is the sum of the durations of all branches in the tree, ", 
                         formatC(tree_length, digits = 2, format = "fg"), " for this dataset).")
    prior_mean95interval <- paste0(" (prior mean = ", formatC(0.5/tree_length, digits = 2, format = "fg"), "; $95\\%$ prior interval = ${[", 
                                   paste(formatC(qgamma(c(0.025, 0.975), shape = 0.5, rate = tree_length), digits = 2, format = "fg"), collapse = ", "), "]}$")
    
    muprior_additional <- "This prior is the default prior in $\\texttt{BEAST}$, referred to as the CTMC-rate reference prior (Ferreira and Suchard, 2008) in $\\texttt{BEAUTi}$. "

    dispersaleventsnumprior_distmean95interval <- "<br><br>The resulting prior on the expected number of dispersal events across the entire biogeographic history has mean 0.5 and $95\\%$ interval ${[0, 3]}$"

  } else if (mu_prior == "Hierarchical Exponential") {
    
    prior_dist <- "an Exponential distribution with rate parameter, $\\theta$,"
    prior_mean95interval <- paste0(" (prior mean = 1; $95\\%$ prior interval = ${[", 
                                   paste(formatC(hierachexp_mu95interval, digits = 2, format = "fg"), collapse = ", "), "]}$")
    
    priorhyper_dist <- paste0("<br><br>The mean of the Exponential distribution, ${1 / \\theta}$, is treated as a random variable to be estimated from the data; ",
                              "the prior on ${1 / \\theta}$ is a Gamma distribution with shape parameter = rate parameter = ${", hierachexp_alphaofgamma, "}$")
    priorhyper_mean95interval <- paste0(" (prior mean = 1; $95\\%$ prior interval = ${[", 
                                        paste(formatC(qgamma(c(0.025, 0.975), shape = hierachexp_alphaofgamma, rate = hierachexp_alphaofgamma), digits = 2, format = "fg"), collapse = ", "), "]}$). ")
    muprior_additional <- paste0(priorhyper_dist, priorhyper_mean95interval)
    
    dispersaleventsnumprior_distmean95interval <- paste0("<br><br>The resulting prior on the expected number of dispersal events across the entire biogeographic history",
                                                         " has mean ", formatC(hierachexp_dispersaleventsnummean, digits = 2, format = "fg"), " and $95\\%$ interval ${[", 
                                                         paste(formatC(hierachexp_dispersaleventsnum95interval, digits = 2, format = "fg"), collapse = ", "), "]}$")
    
  } else if (mu_prior == "Empirical-Informed Exponential") {
    
    empinformed_expmean <- empinformed_dispersaleventsnummean/tree_length
    prior_dist <- paste0("an Exponential distribution with rate parameter, ${\\theta = ", formatC(1/empinformed_expmean, digits = 2, format = "fg"), "}$,")
    prior_mean95interval <- paste0(" (prior mean = ", formatC(empinformed_expmean, digits = 2, format = "fg"), "; $95\\%$ prior interval = ${[", 
                                   paste(formatC(qgamma(c(0.025, 0.975), shape = 1, scale = empinformed_expmean), digits = 2, format = "fg"), collapse = ", "), "]}$")
    
    dispersaleventsnumprior_distmean95interval <- paste0("<br><br>The resulting prior on the expected number of dispersal events across the entire biogeographic history", 
                                                         " has mean ", formatC(empinformed_dispersaleventsnummean, digits = 2, format = "fg"), " and $95\\%$ interval ${[", 
                                                         paste(formatC(qnbinom(c(0.025, 0.975), size = 1, mu = empinformed_dispersaleventsnummean), digits = 2, format = "fg"), collapse = ", "), "]}$")
  }
  
  muprior_description <- paste0("The prior on the average dispersal rate is ", prior_dist, prior_mean95interval, "; see figure on the right, top panel). ",
                                muprior_additional, dispersaleventsnumprior_distmean95interval, " (see figure on the right, bottom panel), ", 
                                dispersaleventsnumprior_parsimonyscore_quantile)
  return(muprior_description)
}

section_wrapper <<- list(c("<h5>", "</h5>"), c("\\section*{", "}\n"), c("#### ", "\n"))
subsection_wrapper <<- list(c("<h6><i><b>", "</b></i></h6>"), c("\\subsection*{", "}\n"), c("##### ", "\n"))
section_ending <<- c("<br>", "\n", "\n")
italic_wrapper <<- list(c("<em>", "</em>"), c("\\textit{", "}"), c("*", "*"))
texttt_wrapper <<- list(c("<tt>", "</tt>"), c("\\texttt{", "}"), c("`", "`"))
equation_wrapper <<- list(c("\\begin{align*}\n", "\\end{align*}\n"), c("\\begin{align*}\n", "\\end{align*}\n"), c("\\[", "\\]"))
citep_wrapper <<- list(c("[@", "]"), c("\\citep{", "}"), c("[@", "]"))
citet_wrapper <<- list(c("@", ""), c("\\citet{", "}"), c("@", ""))
cite_sep <<- c("; @", ",", "; @")
all_format <<- c("HTML", "Latex", "Markdown")

tex_data <- function(taxa_num = 1, states_num = 1, format = "HTML") {
  
  format_idx <- which(all_format == format)
  data_title <- paste0(section_wrapper[[format_idx]][1], "Data", section_wrapper[[format_idx]][2])
  data_body <- paste0("Our dataset contains ", taxa_num, " sequences, each of which was sampled in one of the ", 
                      states_num, " geographic areas.")
  return(paste0(data_title, data_body, section_ending[format_idx]))
}

tex_model <- function(bssvs = T, symmetry = T, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  model_title <- paste0(section_wrapper[[format_idx]][1], "Model", section_wrapper[[format_idx]][2])
  model_generic <- paste0("We use a continuous-time Markov chain (CTMC) to describe the evolution of geographic areas over the tree, $\\Psi$.\n",
                          "For a biogeographic history with $k$ discrete areas, ", "this CTMC is fully specified by a ${k \\times k}$ instantaneous-rate matrix, $Q$, ",
                          "where an element of the matrix, $q_{ij}$, is the instantaneous rate of change between state $i$ and state $j$ ",
                          "(", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], ", the instantaneous rate of dispersal from area $i$ to area $j$).\n")
  
  matrix_construction <- ""
  if (bssvs) {
    matrix_construction <- paste0("We specify each element, $q_{ij}$, of the instantaneous-rate matrix, $Q$, as:\n",
                                  equation_wrapper[[format_idx]][1], "q_{ij} = r_{ij} \\delta_{ij},\n", equation_wrapper[[format_idx]][2], 
                                  "where $r_{ij}$ is the relative rate of dispersal between areas $i$ and $j$, ",
                                  "and $\\delta_{ij}$ is an indicator variable that takes one of two states (0 or 1).\n",
                                  "Alternative biogeographic models are specified by different configurations of dispersal routes, $\\boldsymbol{\\delta}$; ",
                                  "we average over biogeographic models by estimating the configuration from the data.\n")
  }

  if (symmetry) {
    modelsymmetry_statement <- paste0("We assume the instantaneous-rate matrix, $Q$, is symmetric, where ${q_{ij} = q_{ji}}$",
                                      ifelse(bssvs, paste0(" (", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], 
                                                           ", ${r_{ij} = r_{ji}}$ and ${\\delta_{ij} = \\delta_{ji}}$)"), ""), ".\n",
                                      "Accordingly, this symmetric geographic model ", 
                                      ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Lemeyetal2009", citep_wrapper[[format_idx]][2]), "(Lemey et al. 2009)"),
                                      " assumes that the instantaneous rate of dispersal from area $i$ to area $j$ ",
                                      "is equal to the dispersal rate from area $j$ to area $i$.\n")
  } else {
    modelsymmetry_statement <- paste0("We allow the $Q$ matrix to be asymmetric, where $q_{ij}$ and $q_{ji}$ are not constrained to be equal",
                                      ifelse(bssvs, paste0(" (", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], 
                                             ", $r_{ij}$ and $r_{ji}$ are not constrained to be equal; $\\delta_{ij}$ and $\\delta_{ji}$ are not constrained to be equal)"), ""), ".\n",
                                      "Accordingly, this asymmetric geographic model ", 
                                      ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Edwardsetal2011", citep_wrapper[[format_idx]][2]), "(Edwards et al. 2011)"),
                                      " allows the rate of dispersal from area $i$ to area $j$ to be different from the rate of dispersal from area $j$ to area $i$.\n")
  }
  
  mu_source <- paste0("We rescale the $Q$ matrix such that the expected number of dispersal events in one time unit ",
                      "(", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], ", the average dispersal rate)", 
                      " is equal to the parameter $\\mu$ ", 
                      ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Yang2014", citep_wrapper[[format_idx]][2]), "(Yang 2014)"), ".\n")
  
  return(paste0(model_title, model_generic, matrix_construction, modelsymmetry_statement, mu_source, section_ending[format_idx]))
}


tex_deltaprior <- function(states_num, symmetry = T, delta_prior = "Poisson", poisson_default = T, poisson_lambda = 0,
                           alpha_beta = 1, beta_beta = 1, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  delta_max <- ((!symmetry) + 1) * choose(states_num, 2)
  poisson_offset <- ifelse(symmetry, states_num - 1, 0)
  
  prior_title <- paste0(subsection_wrapper[[format_idx]][1], "Prior on the Number of Dispersal Routes", subsection_wrapper[[format_idx]][2])
  prior_explanation <- paste0("Note that for a given value of $\\Delta$, there may be multiple distinct biogeographic models; ",
                              "as we place a prior on $\\Delta$ (instead of directly placing a prior on biogeographic models), ",
                              "we are assuming that all biogeographic models with a given value of $\\Delta$ are equiprobable.\n")
  
  if (delta_prior == "Poisson") {
    
    prior_generic <- paste0("We specify ", ifelse(symmetry, paste0("an ", italic_wrapper[[format_idx]][1], "offset", italic_wrapper[[format_idx]][2], " Poisson"), "a Poisson"),
                            " prior on the total number of dispersal routes, $\\Delta$.\n")
    
    poisson_offset_explanation <- paste0("That is, the prior on $\\Delta$ assigns zero probability to all biogeographic models with fewer than $k - 1$ dispersal routes.\n")
    
    # poisson_offset_explanation <- paste0("That is, the prior on $\\Delta$ assigns zero probability to all biogeographic models with fewer than $k - 1$ dispersal routes; ",
    #                                      "this reflects the constraint that a dataset with $k$ geographic areas ",
    #                                      "cannot be realized under a CTMC with fewer than $k - 1$ non-zero $q_{ij}$ values (", 
    #                                      italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], ", dispersal routes).\n ",
    #                                      "(Note that the real constraint on the geographic model is that it must be ", 
    #                                      italic_wrapper[[format_idx]][1], "irreducible", italic_wrapper[[format_idx]][2], "; ",
    #                                      italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], 
    #                                      ", it must be possible to reach each area from every other area either directly or indirectly.\n",
    #                                      "A model with fewer than $k - 1$ dispersal routes cannot be irreducible; ",
    #                                      "however, a model with at least $k - 1$ disperal routes is not guaranteed to be irreducible.)\n")
    
    if (symmetry) prior_generic <- paste0(prior_generic, poisson_offset_explanation)
    
    prior_dist <- paste0("Specifically, we specify ", "a Poisson prior on $\\Delta$ ", 
                         ifelse(symmetry, paste0(italic_wrapper[[format_idx]][1], "greater than", italic_wrapper[[format_idx]][2], 
                                                 " $k - 1$ (where $k$ is ", states_num, " for this dataset) "), ""),
                         "with rate parameter, ${\\lambda = ", formatC(poisson_lambda, digits = 2, format = "fg"), "}$")
    
    poisson_signif <- ".\n"
    poisson_beastdefault_confirm <- ""
    if (poisson_default) {
      poisson_signif <- ", expressing an explicit and very informative prior preference for biogeographic models with the minimal number of dispersal routes.\n"
      
      poisson_beastdefault_citation <- c(ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Lemeyetal2009", citep_wrapper[[format_idx]][2]), "(Lemey et al. 2009)"), 
                                         ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Edwardsetal2011", citep_wrapper[[format_idx]][2]), "(Edwards et al. 2011)"))[(!symmetry) + 1]
      poisson_beastdefault_confirm <- paste0("This is the default prior in BEAST for ", ifelse(symmetry, "symmetric", "asymmetric"),
                                             " model ", poisson_beastdefault_citation, ".\n")
    }
    
    pprior_dist <- paste0("The prior probability that each dispersal route exists (", italic_wrapper[[format_idx]][1], "i.e.", 
                          italic_wrapper[[format_idx]][2], ", $\\delta_{ij}$ is one) is thus ", 
                          formatC((poisson_lambda + poisson_offset)/delta_max, digits = 2, format = "fg"), ".\n")
    
    deltaprior_tex <- paste0(prior_title, prior_generic, prior_dist, poisson_signif, poisson_beastdefault_confirm, pprior_dist, prior_explanation)
    
  } else if (delta_prior == "Uniform") {
    
    prior_generic <- paste0("We specify a Uniform prior on the total number of dispersal routes, $\\Delta$, expressing the prior belief that ", 
                            "all possible values (0 to the maximum value, ", delta_max, ") of $\\Delta$ are equiprobable.\n")
    pprior_dist <- paste0("The prior probability that each dispersal route exists (", italic_wrapper[[format_idx]][1], "i.e.", 
                          italic_wrapper[[format_idx]][2], ", $\\delta_{ij}$ is one) is thus uniformly distributed between 0 and 1.\n")
    deltaprior_tex <- paste0(prior_title, prior_generic, pprior_dist, prior_explanation)
    
  } else if (delta_prior == "Beta-Binomial") {
    
    prior_generic <- paste0("We specify a Beta-Binomial prior on the total number of dispersal routes, $\\Delta$.\n")
    prior_dist <- paste0("Specifically, we specify a Binomial prior on $\\Delta$ ",
                         "with the number-of-trials parameter, ${n = ", delta_max, "}$ (when dispersal-route indicators are all one), ",
                         "and treat the success-probability parameter (", italic_wrapper[[format_idx]][1], "i.e.", 
                         italic_wrapper[[format_idx]][2], ", the probability that each dispersal route exists), $p$, ",
                         "as a random variable to be estimated from the data.\n")
    pprior_dist <- paste0("We then specify a Beta prior on $p$ with the first shape parameter = ", alpha_beta, 
                          " and the second shape parameter = ", beta_beta, ".\n")
    
    deltaprior_tex <- paste0(prior_title, prior_generic, prior_dist, pprior_dist, prior_explanation)
  }
  
  return(paste0(deltaprior_tex, section_ending[format_idx]))
}


tex_muprior <- function(mu_prior = "CTMC rate-ref (BEAST default)", tree_length = 1, hierachexp_alphaofgamma = 0.5,
                        hierachexp_dispersaleventsnummean = 1, empinformed_dispersaleventsnummean = 1, parsimony_score = 1, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  prior_title <- paste0(subsection_wrapper[[format_idx]][1], "Prior on the Average Dispersal Rate", subsection_wrapper[[format_idx]][2])
  prior_explanation <- paste0("For a tree of length $T$ (", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], 
                              ", the sum of the durations of all branches in the tree), ",
                              "the expected number of dispersal events is ${\\mu \\times T}$.\n",
                              "Therefore, the prior on the average dispersal rate, $\\mu$, represents our prior belief about the number of dispersal events over the tree.\n")
  
  if (mu_prior == "CTMC rate-ref (BEAST default)") {
    
    prior_generic <- paste0("We specify ", "a Gamma prior on $\\mu$ with shape parameter ${\\alpha = 0.5}$ ",
                            "and rate parameter ${\\beta = T}$ ($T$ = ", formatC(tree_length, digits = 2, format = "fg"), "for this dataset).\n")
    prior_beastdefault <- paste0("This prior is the default prior in ", texttt_wrapper[[format_idx]][1], "BEAST", texttt_wrapper[[format_idx]][2],
                                 " and referred to as the CTMC-rate reference prior ", 
                                 ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "FerreiraSuchard2008", citep_wrapper[[format_idx]][2]), "(Ferreira and Suchard 2008)"), 
                                 " in ", texttt_wrapper[[format_idx]][1], "BEAUti", texttt_wrapper[[format_idx]][2], ".\n")
    dispersaleventsnumprior_dist <- paste0("The resulting prior on the expected number of dispersal events across the entire biogeographic history ",
                                           "has mean 0.5 and $95\\%$ interval ${[0, 3]}$, independent of $T$.\n")
    
    muprior_tex <- paste0(prior_title, prior_explanation, prior_generic, prior_beastdefault, dispersaleventsnumprior_dist)
    
  } else if (mu_prior == "Hierarchical Exponential") {
    
    prior_generic <- paste0("We specify ", "an Exponential prior with the rate parameter, $\\theta$, ",
                            "treating the mean of this Exponential distribution, ${1 / \\theta}$, ",
                            "as a random variable to be estimated from the data.\n")
    priorhyper_dist <- paste0("We then specify a Gamma prior on ${1 / \\theta}$; the shape parameter and rate parameter of this Gamma hyperprior are both ${",
                              hierachexp_alphaofgamma, "}$.\n")
    dispersaleventsnumprior_dist <- "The resulting prior distribution on the number of dispersal events scales with $T$."
    
    muprior_tex <- paste0(prior_title, prior_explanation, prior_generic, priorhyper_dist, dispersaleventsnumprior_dist)
    
  } else if (mu_prior == "Empirical-Informed Exponential") {
    
    empinformed_expmean <- empinformed_dispersaleventsnummean/tree_length
    dispersaleventnum_dividedbyparsimonyscore <- empinformed_dispersaleventsnummean/parsimony_score
    
    prior_generic <- paste0("We specify ", "an Exponential prior with the rate parameter, ${\\theta = ",
                            formatC(1/empinformed_expmean, digits = 2, format = "fg"), "}$, ")
    dispersaleventsnumprior_dist <- paste0("which allows the resulting prior on the expected number of dispersal events across the entire biogeographic history ",
                                           "to have a mean that is ", empinformed_dispersaleventsnummean, " (",
                                           dispersaleventnum_dividedbyparsimonyscore, " times of the parsimony score, ", parsimony_score, ").\n")
    
    muprior_tex <- paste0(prior_title, prior_explanation, prior_generic, dispersaleventsnumprior_dist)
  }
  
  return(paste0(muprior_tex, section_ending[format_idx]))
}

tex_prior <- function(deltaprior_text, muprior_text, format = "HTML") {
  format_idx <- which(all_format == format)
  prior_title <- paste0(section_wrapper[[format_idx]][1], "Prior", section_wrapper[[format_idx]][2])
  
  return(paste0(prior_title, deltaprior_text, muprior_text))
}

tex_bayesianinference <- function(empiricaltree_mh = T, tree_num = 1, bssvs = T, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  valid_sequential <- (tree_num > 1) && empiricaltree_mh
  if (valid_sequential) {
    posterior_eq <- paste0("P(\\Psi, \\boldsymbol{r}, ", ifelse(bssvs, "\\boldsymbol{\\delta}, ", ""), "\\mu \\mid G)")
  } else {
    posterior_eq <- paste0("P(\\boldsymbol{r}, ", ifelse(bssvs, "\\boldsymbol{\\delta}, ", ""), "\\mu \\mid G, \\Psi)")
  }
  likelihood_eq <- paste0("P(G \\mid \\boldsymbol{r}, ", ifelse(bssvs, "\\boldsymbol{\\delta}, ", ""), "\\mu, \\Psi)")
  prior_eq <- paste0(ifelse(valid_sequential, "P(\\Psi) ", ""), "P(\\boldsymbol{r}) ", ifelse(bssvs, "P(\\boldsymbol{\\delta}) ", ""), "P(\\mu)")

  bayes_eq <- paste0(equation_wrapper[[format_idx]][1], posterior_eq, " = \\frac{ ", likelihood_eq, prior_eq, "}{ P(G) },\n", equation_wrapper[[format_idx]][2])
  bayes_explanation <- paste0("where $\\boldsymbol{r}$ is a vector that contains all of the relative-rate parameters, ",
                              ifelse(bssvs, "$\\boldsymbol{\\delta}$ is a vector that contains all of the dispersal-route indicators, ", ""),
                              "$\\mu$ is the average rate of dispersal, ", "$G$ is the observed geographic data, ", "and $\\Psi$ is the phylogeny.\n")
  bayes_title <- paste0(section_wrapper[[format_idx]][1], "Bayesian Inference", section_wrapper[[format_idx]][2])

  bayesian_inference <- paste0(bayes_title, "We estimate the parameters of these biogeographic models in a Bayesian framework.\n",
                               "Following Bayes' theorem, the joint posterior probability distribution of the model parameters is ",
                               ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Bayes1763", citep_wrapper[[format_idx]][2]), "(Bayes 1763)"),
                               ":\n", bayes_eq, bayes_explanation)
  
  if (tree_num == 1) {
    tree_dist <- paste0("Specifically, $\\Psi$ is a summary phylogeny that has been estimated using the sequence data ",
                        "(", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], 
                        ", without incorporating the geographic data).\n")
  } else if (tree_num > 1) {
    tree_dist <- paste0("Specifically, $\\Psi$ is a distribution of phylogenies that has been estimated using the sequence data ",
                        "(", italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], 
                        ", without incorporating the geographic data).\n")
    if (empiricaltree_mh) {
      tree_proposal <- paste0("Here we use $\\Psi$ as a prior to estimate the joint posterior distribution of the geographic model parameters.\n",
                              "This ''sequential'' inference approach is theoretically equivalent to infer the tree and geographic model parameters ",
                              "jointly with both the sequence data and the geographic data.\n")
    } else {
      tree_proposal <- paste0("Here we average over $\\Psi$ by randomly proposing a new tree from it and ",
                              "always accepting the proposed tree without evaluating the acceptance ratio ",
                              "(in contrast to the other proposals in the Metropolis-Hastings MCMC algorithm), ",
                              "effectively assuming that, given a set of geographic model parameter values, ",
                              "the probability of observing the geographic data is independent of the underlying phylogeny.\n")
    }

    tree_dist <- paste0(tree_dist, tree_proposal)
  }
  
  return(paste0(bayesian_inference, tree_dist, section_ending[format_idx]))
}


tex_posterioranalysis <- function(mcmc_chainlength = 1, mcmc_samplingfreq = 1, mcmc_numreplicates = 2, format = "HTML") {
  
  format_idx <- which(all_format == format)
  postrior_title <- paste0(subsection_wrapper[[format_idx]][1], "Estimating the Joint Posterior Distribution of Geographic Model Parameters", subsection_wrapper[[format_idx]][2])
  posterior_body <- paste0("We approximate the joint posterior distribution using Markov chain Monte Carlo in ", 
                           texttt_wrapper[[format_idx]][1], "BEAST", texttt_wrapper[[format_idx]][2], ", ",
                           "running it for ", mcmc_chainlength, " generations and sampling every ",
                           mcmc_samplingfreq, " generations throughout the chain.\n")
  if (mcmc_numreplicates > 1) {
    posterior_body <- paste0(posterior_body, "We run ",  mcmc_numreplicates, 
                             " independent MCMC chains to assess the convergence among replicates.\n")
  }

  posterior_tex <- paste0(postrior_title, posterior_body, section_ending[format_idx])
  return(posterior_tex)
}


tex_summarystats <- function(do_stochasticmapping = "Stochastic mapping (complete history, simulation-based)",
                             markovjumps_total = T, markovjumps_pairwise = T, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  # summarystats_title <- paste0(subsection_wrapper[[format_idx]][1], "Estimating the Posterior Distribution of Biogeographic History", subsection_wrapper[[format_idx]][2])
  summarystats_title <- paste0(subsection_wrapper[[format_idx]][1], "Inferring Biogeographic History", subsection_wrapper[[format_idx]][2])
  
  ase <- paste0("We estimated the ancestral area at each internal node using the ancestral-state estimation algorithm ",
                ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Yang2014", citep_wrapper[[format_idx]][2]), "(Yang 2014)"), 
                " implemented in ", texttt_wrapper[[format_idx]][1], "BEAST", texttt_wrapper[[format_idx]][2], ".\n")
  
  # ase <- paste0("To estimate the ancestral areas, we sample an area at each internal node according to the posterior probability vector of that node; ",
  #               "we perform this sampling procedure for each sample of the approximated joint posterior distribution (", 
  #               italic_wrapper[[format_idx]][1], "i.e.", italic_wrapper[[format_idx]][2], ", each MCMC sample), ",
  #               "and then marginalize over all the samples to obtain the marginal posterior distribution of ancestral areas at each internal node.\n")
  
  stocmaps <- ""
  if (do_stochasticmapping == "Stochastic mapping (complete history, simulation-based)") {
    
    stocmaps <- paste0("We simulate the full biogeographic history using the stochastic mapping algorithm ", 
                       ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Nielsen2002", cite_sep[format_idx], "Rodrigueetal2007a", 
                                                      cite_sep[format_idx], "HobolthStone2009", citep_wrapper[[format_idx]][2]), 
                              "(Nielsen 2002; Rodrigue et al. 2007; Hobolth and Stone 2009)"), ".\n")
                       # "Specifically, we simulate the biogeographic history along each branch conditional on the ending state of that branch, ",
                       # "with a uniformization procedure (Mateiu and Rannala, 2006; Rodrigue et al., 2007; Hobolth and Stone, 2009).\n")
    
    if (markovjumps_total || markovjumps_pairwise) {
      stocmaps <- paste0(stocmaps, "We then marginalize the posterior distribution of histories to estimate the ",
                         ifelse(markovjumps_pairwise, "number of dispersal events between each pair of areas.", "total number of dispersal events between all areas."))
    }
    
  } else if (do_stochasticmapping == "Fast stochastic mapping (incomplete history, simulation-free)" && (markovjumps_total || markovjumps_pairwise)) {
    
    stocmaps <- paste0("We calculate the expected ", ifelse(markovjumps_pairwise, "number of dispersal events between each pair of areas", "total number of dispersal events between all areas"),
                       " using the ''fast stochastic mapping algorithm'' ", 
                       ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "MininSuchard2008a", cite_sep[format_idx], "MininSuchard2008b", 
                                                      cite_sep[format_idx], "Obrienetal2009", citep_wrapper[[format_idx]][2]), 
                              "(Minin and Suchard 2008a, 2008b; O'brien et al. 2009)"),
                       " implemented in ", texttt_wrapper[[format_idx]][1], "BEAST", texttt_wrapper[[format_idx]][2], ".\n",
                       "This algorithm does not simulate the full biogeographic history; ", "rather, it computes the expected number of events on each branch ",
                       "by analytically integrating the branch length out.\n")
  }
  
  summarystats_tex <- paste0(summarystats_title, ase, stocmaps, section_ending[format_idx])
  return(summarystats_tex)
}


tex_powerposterior <- function(ml_numstones = 0, ml_chainlengthperstone = 0, ml_samplingfreq = 0, ml_alphaofbeta = 0.3, mcmc_numreplicates = 2, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  powerposterior_title <- paste0(subsection_wrapper[[format_idx]][1], "Estimating Marginal Likelihood", subsection_wrapper[[format_idx]][2])
  powerposterior_body <- paste0("We estimate the marginal likelihood of our model through both", "thermodynamic integration ", 
                                ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "LartillotPhilippe2006", citep_wrapper[[format_idx]][2]), 
                                       "(Lartillot and Philippe 2006)"), 
                                " and stepping-stone sampling ", 
                                ifelse(render_citation, paste0(citep_wrapper[[format_idx]][1], "Xieetal2011", cite_sep[format_idx], "Baeleetal2012", citep_wrapper[[format_idx]][2]), 
                                       "(Xie et al. 2011; Baele et al. 2012)"), ".\n",
                                "We perform MCMC simulations over a sequence of power-posterior distributions (''stones''), ",
                                "where for stone $i$, the likelihood is raised to a power, $\\beta_i$, between 0 and 1.\n",
                                # "The first stone in the sequence has $\\beta_1 = 1.0$, so that it samples from the joint posterior distribution; ",
                                # "the final stone has $\\beta_k = 0.0$, so that it samples from the joint prior distribution.\n",
                                "We specify the sequence of $\\beta$ values following evenly-spaced quantiles of a Beta$(", ml_alphaofbeta, ", 1.0)$ distribution", 
                                ifelse(ml_alphaofbeta < 1, " , so that more values of $\\beta$ are put near 0 than near 1", ""), ".\n",
                                "Here we use ", ml_numstones, " stones, running independent MCMC at each stone for ", ml_chainlengthperstone, " generations and sampling every ",
                                ml_samplingfreq, " generations to approximate the power-posterior distributions.\n")
  if (mcmc_numreplicates > 1) {
    powerposterior_body <- paste0(powerposterior_body, "We run ",  mcmc_numreplicates, 
                                  " independent analysis replicates to assess the convergence of marginal likelihood estimates.\n")
  }

  powerposterior_tex <- paste0(powerposterior_title, powerposterior_body, section_ending[format_idx])
  return(powerposterior_tex)
}


tex_prioranalysis <- function(mcmc_chainlength = 1, mcmc_samplingfreq = 1, mcmc_numreplicates = 2, format = "HTML") {
  
  format_idx <- which(all_format == format)
  prior_title <- paste0(subsection_wrapper[[format_idx]][1], "Estimating the Joint Prior distribution of Geographic model parameters", subsection_wrapper[[format_idx]][2])
  prior_body <- paste0("We approximate the joint prior distribution using Markov chain Monte Carlo in ", 
                       texttt_wrapper[[format_idx]][1], "BEAST", texttt_wrapper[[format_idx]][2], " ",
                       "(by setting all the geographic state to ``?'' in the XML script), ",
                       "running it for ", mcmc_chainlength, " generations and sampling every ",
                       mcmc_samplingfreq, " generations throughout the chain.\n")
  if (mcmc_numreplicates > 1) {
    prior_body <- paste0(prior_body, "We run ",  mcmc_numreplicates,
                         " independent MCMC chains to assess the convergence among replicates.\n")
  }
  
  return(paste0(prior_title, prior_body, section_ending[format_idx]))
}

tex_dcanalysis <- function(lheat = 1, mcmc_chainlength = 1, mcmc_samplingfreq = 1, mcmc_numreplicates = 2, format = "HTML", render_citation = T) {
  
  format_idx <- which(all_format == format)
  dc_title <- paste0(subsection_wrapper[[format_idx]][1], "Asseing Prior Sensity Using Data Cloning", subsection_wrapper[[format_idx]][2])
  
  lheat_str <- 1
  if (length(lheat) == 1) {
    lheat_str <- lheat
  } else if (length(lheat) == 2) {
    lheat_str <- paste(lheat, collapse = " and ")
  } else if (length(lheat) >= 3) {
    lheat_str <- paste0(paste(lheat[1:(length(lheat) - 1)], collapse = ", "), ", and ", lheat[length(lheat)])
  }
  
  dc_body <- paste0("We use data cloning ", ifelse(render_citation, 
                                                    paste0(citep_wrapper[[format_idx]][1], "Robert1993", cite_sep[format_idx], "Lele2007", 
                                                           cite_sep[format_idx], "Ponciano2009", cite_sep[format_idx], "Ponciano2012", citep_wrapper[[format_idx]][2]), 
                                                    "(Robert 1993; Lele et al. 2007; Ponciano et al. 2009; Ponciano et al. 2012)"), 
                    " to assess how informative our priors are relative to our data.\n",
                    "Specifically, we duplicate our data to ", lheat_str,
                    " copies (effectively raising the likelihood to the power of ", lheat_str, ")", 
                    ifelse(length(lheat > 1), ", respectively,", ""), " to see how the parameter estimates change as the likelihood increasingly dominates the posterior.\n")
  
  dc_body <- paste0(dc_body, "We approximate this joint power posterior distribution using Markov chain Monte Carlo in ", 
                    texttt_wrapper[[format_idx]][1], "BEAST", texttt_wrapper[[format_idx]][2], " ",
                    "running it for ", mcmc_chainlength, " generations and sampling every ",
                    mcmc_samplingfreq, " generations throughout the chain.\n")
  
  if (mcmc_numreplicates > 1) {
    dc_body <- paste0(dc_body, "We run ",  mcmc_numreplicates,
                      " independent MCMC chains to assess the convergence among replicates.\n")
  }
  
  return(paste0(dc_title, dc_body, section_ending[format_idx]))
}

tex_analysis <- function(further_analysis = "", posterioranalysis_text = "", summarystats_text = "", powerposterior_text = "", 
                         prioranalysis_text = "", dcanalysis_text = "", format = "HTML") {
  
  format_idx <- which(all_format == format)
  analysis_title <- paste0(section_wrapper[[format_idx]][1], "Analysis", section_wrapper[[format_idx]][2])
  
  analysis_body <- ""
  if (further_analysis == "") {
    analysis_body <- paste0(posterioranalysis_text, summarystats_text)
  } else if (further_analysis == "Marginal likehood estimation") {
    analysis_body <- paste0(posterioranalysis_text, summarystats_text, powerposterior_text)
  } else if (further_analysis == "Under prior") {
    analysis_body <- paste0(prioranalysis_text, summarystats_text)
  } else if (further_analysis == "Data cloning") {
    analysis_body <- dcanalysis_text
  }
  
  return(paste0(analysis_title, analysis_body))
}

