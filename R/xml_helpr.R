
#' Add together two numbers
#' 
#' @param states_dat a data-frame object containing at least two columns: one for the name of each tip (column name specified by \code{taxon_name}), 
#' and the other for the trait state of each tip (column name specified by \code{discrete_trait_name})
#' @param taxon_name name of the column containing tip names
#' @param discrete_trait_name name of the column containing tip states
#' @param date_name
#' @param lheat
#' @param under_prior
#' @return xml code

xml_discretetraitdata <- function(states_dat, taxon_name, discrete_trait_name, date_name = NULL, lheat = 1, under_prior = F) {
  
  states <- sort(as.vector(unique(states_dat[, discrete_trait_name])))
  states <- states[states != "?"]
  states_num <- length(states)
  
  taxa <- ""
  # insert discrete-trait data
  for (i in 1:nrow(states_dat)) {
    
    taxon <- paste0("\t\t<taxon id=\"", states_dat[i, taxon_name], "\">\n")
    if (!is.null(date_name)) {
      taxon <- paste0(taxon, "\t\t\t<date value=\"", states_dat[i, date_name], "\" direction=\"forwards\" units=\"years\"/>\n")
    }
    if (lheat == 1) {
      taxon <- paste0(taxon, "\t\t\t<attr name=\"", discrete_trait_name, "\">", ifelse(under_prior, "?", states_dat[i, discrete_trait_name]), "</attr>\n")
    }
    taxa <- paste0(taxa, taxon, "\t\t</taxon>\n")
  }
  taxa <- paste0("\t<taxa id=\"taxa\">\n", taxa, "\t</taxa>\n\n")
  
  # insert list of discrete trait
  state_lines <- ""
  for (i in 1:states_num) {
    if (lheat == 1) {
      state_line <- paste0("\t\t<state code=\"", states[i], "\"/>\n")
    } else if (lheat > 1) {
      state_line <- paste0("\t\t<state code=\"", c(LETTERS, letters, 0:9)[i], "\"/>\n")
    }
    state_lines <- paste0(state_lines, state_line)
  }
  general_datatype <- paste0("\t<generalDataType id=\"", discrete_trait_name, ".dataType\">\n", 
                             state_lines, "\t</generalDataType>\n")
  
  # insert data pattern chunk for discrete trait
  if (lheat == 1) {
    pattern_alignment <- paste0("\t<attributePatterns id=\"", discrete_trait_name, ".pattern\" attribute=\"", discrete_trait_name, "\">\n",
                                "\t\t<taxa idref=\"taxa\"/>\n","\t\t<generalDataType idref=\"", discrete_trait_name, ".dataType\"/>\n",
                                "\t</attributePatterns>\n")
  } else if (lheat > 1) {
    pattern_alignment <- c(paste0("\n\t<alignment id=\"", discrete_trait_name, "\">"), "\t</alignment>\n")
    pattern_alignment <- append(pattern_alignment, paste0("\t<generalDataType idref=\"", discrete_trait_name, ".dataType\"/>"), after = length(pattern_alignment) - 1)
    
    for (i in 1:nrow(states_dat)) {
      
      if (!(states_dat[i, discrete_trait_name] %in% states)) {
        state_dc <- states_dat[i, discrete_trait_name]
      } else {
        state_dc <- c(LETTERS, letters, 0:9)[match(states_dat[i, discrete_trait_name], states)]
      }
      
      taxon_stateseq <- paste0("\t\t<sequence>\t<taxon idref=\"", states_dat[i, taxon_name], "\"/>\t", 
                               paste(rep(state_dc, lheat), collapse = ""), "\t</sequence>")
      pattern_alignment <- append(pattern_alignment, taxon_stateseq, after = length(pattern_alignment) - 1)
    }
    
    pattern_alignment <- paste(pattern_alignment, collapse = "\n")
    
    # add site compression chunk
    pattern_alignment <- paste0(pattern_alignment, "\t<patterns id=\"patterns\" from=\"1\" strip=\"false\">\n",
                                "\t\t<alignment idref=\"", discrete_trait_name, "\"/>\n", "\t</patterns>\n")
  }
  
  return(paste0(taxa, general_datatype, pattern_alignment))
}


xml_discretetraitmodel <- function(states_dat, discrete_trait_name, symmetry = T, bssvs = T, delta_prior = "Poisson", 
                                   poisson_default = T, poisson_mean = 0, alpha_beta = 0, beta_beta = 0,
                                   rates_proposal_weight = 0, indicators_proposal_weight = 0, 
                                   rootfreq_proposal_weight = 0, indicatorprob_proposal_weight = 0) {
  
  states <- sort(as.vector(unique(states_dat[, discrete_trait_name])))
  states <- states[states != "?"]
  states_num <- length(states)
  
  # insert the chunk for the substitution model of discrete trait
  substitution_model <- paste0("\t<generalSubstitutionModel id=\"", discrete_trait_name, ".model\">\n", 
                               "\t\t<generalDataType idref=\"", discrete_trait_name, ".dataType\"/>\n")
  frequencies <- paste0("\t\t<frequencies>\n",
                        "\t\t\t<frequencyModel id=\"", discrete_trait_name, ".frequencyModel\" normalize=\"true\">\n",
                        "\t\t\t\t<generalDataType idref=\"", discrete_trait_name, ".dataType\"/>\n",
                        "\t\t\t\t<frequencies>\n",
                        "\t\t\t\t\t<parameter id=\"", discrete_trait_name, ".frequencies\" dimension=\"", states_num, "\"/>\n",
                        "\t\t\t\t</frequencies>\n",
                        "\t\t\t</frequencyModel>\n",
                        "\t\t</frequencies>\n")
  
  if (symmetry) {
    edges_max <- choose(states_num, 2)
  } else {
    edges_max <- choose(states_num, 2) * 2
  }
  rates <- paste0("\t\t<rates>\n",
                  "\t\t\t<parameter id=\"", discrete_trait_name, ".rates\" dimension=\"", edges_max, "\" value=\"1.0\"/>\n",
                  "\t\t</rates>\n")
  
  indicators <- nonZeroRates <- ""
  if (bssvs) {
    indicators <- paste0("\t\t<rateIndicator>\n",
                         "\t\t\t<parameter id=\"", discrete_trait_name, ".indicators\" dimension=\"", edges_max, "\" value=\"1.0\"/>\n",
                         "\t\t</rateIndicator>\n")
    # insert the non-zero rates chunk
    nonZeroRates <- paste0("\t<sumStatistic id=\"", discrete_trait_name, ".nonZeroRates\" elementwise=\"true\">\n",
                           "\t\t<parameter idref=\"", discrete_trait_name, ".indicators\"/>\n",
                           "\t</sumStatistic>\n")
  }
  
  # insert the site model chunk
  site_model <- paste0("\t<siteModel id=\"", discrete_trait_name, ".siteModel\">\n",
                       "\t\t<substitutionModel>\n",
                       "\t\t\t<generalSubstitutionModel idref=\"", discrete_trait_name, ".model\"/>\n",
                       "\t\t</substitutionModel>\n",
                       "\t</siteModel>\n")
  
  substitution_model <- paste0(substitution_model, frequencies, rates, indicators, "\t</generalSubstitutionModel>\n", nonZeroRates, site_model)
  
  rootfreq_model <- paste0("\t\t<frequencyModel id=\"root.frequencyModel\" normalize=\"true\">\n",
                           "\t\t\t<generalDataType idref=\"", discrete_trait_name, ".dataType\"/>\n", 
                           "\t\t\t<frequencies>\n", 
                           "\t\t\t\t<parameter id=\"", discrete_trait_name, ".root.frequencies\" dimension=\"", states_num, "\"/>\n",
                           "\t\t\t</frequencies>\n",
                           "\t\t</frequencyModel>\n")
  if (symmetry) {
    rootfreq_model <- ""
  }
  
  # priors
  # prior on r
  rates_prior <- paste0("\t\t<cachedPrior>\n",
                        "\t\t\t<gammaPrior shape=\"1.0\" scale=\"1.0\" offset=\"0.0\">\n",
                        "\t\t\t\t<parameter idref=\"", discrete_trait_name, ".rates\"/>\n",
                        "\t\t\t</gammaPrior>\n",
                        "\t\t\t<parameter idref=\"", discrete_trait_name, ".rates\"/>\n",
                        "\t\t</cachedPrior>\n")
  
  # prior on the number of dispersal routes
  nonZeroRates_prior <- ""
  if (bssvs) {
    if (delta_prior == "Poisson") {
      
      if (poisson_default) {
        if (symmetry) {
          poisson_mean <- 0.6931471805599453
        } else {
          poisson_mean <- states_num - 1
        }
      }
      poisson_offset <- ifelse(symmetry, states_num - 1, 0)
      
      nonZeroRates_prior <- paste0("\t\t<poissonPrior mean=\"", poisson_mean, "\" offset=\"", poisson_offset, "\">\n",
                                   "\t\t\t<statistic idref=\"", discrete_trait_name, ".nonZeroRates\"/>\n",
                                   "\t\t</poissonPrior>\n")
      
    } else if (delta_prior == "Beta-Binomial") {
      nonZeroRates_prior <- paste0("\t\t<binomialLikelihood>\n",
                                   "\t\t\t<trials>\n",
                                   "\t\t\t\t<parameter dimension=\"", edges_max, "\" value=\"1.0\"/>\n",
                                   "\t\t\t</trials>\n",
                                   "\t\t\t<counts>\n",
                                   "\t\t\t\t<parameter idref=\"", discrete_trait_name, ".indicators\"/>\n",
                                   "\t\t\t</counts>\n", 
                                   "\t\t\t<proportion>\n",
                                   "\t\t\t\t<parameter id=\"", discrete_trait_name, ".indicator.probability\" value=\"0.1\"/>\n",
                                   "\t\t\t</proportion>\n",
                                   "\t\t</binomialLikelihood>\n",
                                   "\t\t<betaPrior shape=\"", alpha_beta, "\" shapeB=\"", beta_beta, "\">\n",
                                   "\t\t\t<parameter idref=\"", discrete_trait_name, ".indicator.probability\"/>\n",
                                   "\t\t</betaPrior>\n")
    }
  }
  
  root_frequencies_prior <- paste0("\t\t<uniformPrior lower=\"0.0\" upper=\"1.0\">\n",
                                   "\t\t\t<parameter idref=\"", discrete_trait_name, ".root.frequencies\"/>\n",
                                   "\t\t</uniformPrior>\n")
  if (symmetry) {
    root_frequencies_prior <- ""
  }
  
  substmodel_priors <- paste0(nonZeroRates_prior, root_frequencies_prior, rates_prior,
                              paste0("\t\t<generalSubstitutionModel idref=\"", discrete_trait_name, ".model\"/>\n"))
  
  # proposals
  rates_proposal <- paste0("\t\t<scaleOperator scaleFactor=\"0.75\" weight=\"", rates_proposal_weight, "\" scaleAllIndependently=\"true\">\n",
                           "\t\t\t<parameter idref=\"", discrete_trait_name, ".rates\"/>\n",
                           "\t\t</scaleOperator>\n")
  
  root_frequencies_proposal <- ""
  if (!symmetry) {
    root_frequencies_proposal <- paste0("\t\t<deltaExchange delta=\"0.01\" weight=\"", rootfreq_proposal_weight, "\">\n",
                                        "\t\t\t<parameter idref=\"", discrete_trait_name, ".root.frequencies\"/>\n",
                                        "\t\t</deltaExchange>\n")
  }
  
  indicators_proposal <- indicator_prob_proposal <- ""
  if (bssvs) {
    indicators_proposal <- paste0("\t\t<bitFlipOperator weight=\"", indicators_proposal_weight, "\">\n",
                                  "\t\t\t<parameter idref=\"", discrete_trait_name, ".indicators\"/>\n",
                                  "\t\t</bitFlipOperator>\n")
    
    if (delta_prior == "Beta-Binomial") {
      indicator_prob_proposal <- paste0("\t\t<randomWalkOperator windowSize=\"0.025\" weight=\"", indicatorprob_proposal_weight, "\">\n",
                                        "\t\t\t<parameter idref=\"", discrete_trait_name, ".indicator.probability\"/>\n",
                                        "\t\t</randomWalkOperator>\n")
    }
  }
  
  substmodel_proposals <- paste0(rates_proposal, indicators_proposal, indicator_prob_proposal, root_frequencies_proposal)
  
  
  return(list(substitution_model = substitution_model, rootfreq_model = rootfreq_model,
              substmodel_priors = substmodel_priors, substmodel_proposals = substmodel_proposals))
  
}


xml_treemodel <- function(tree, treefile_name, tree_proposal_weight = 0, empiricaltree_mh = F) {
  
  # tree distribution model chunk
  if ("phylo" %in% class(tree)) { # a single tree
    
    tree$tip.label <- gsub("\'", "", tree$tip.label)
    tree_newick <- ape::write.tree(tree)
    tree_model <- paste0("\t<newick id=\"startingTree\" units=\"years\" usingDates=\"true\">\n",
                         "\t\t", tree_newick, "\n",
                         "\t</newick>\n",
                         "\t<treeModel id=\"treeModel\">\n",
                         "\t\t<tree idref=\"startingTree\"/>\n",
                         "\t\t<rootHeight>\n",
                         "\t\t\t<parameter id=\"treeModel.rootHeight\"/>\n",
                         "\t\t</rootHeight>\n",
                         "\t\t<nodeHeights internalNodes=\"true\">\n",
                         "\t\t\t<parameter id=\"treeModel.internalNodeHeights\"/>\n",
                         "\t\t</nodeHeights>\n",
                         "\t\t<nodeHeights internalNodes=\"true\" rootNode=\"true\">\n",
                         "\t\t\t<parameter id=\"treeModel.allInternalNodeHeights\"/>\n",
                         "\t\t</nodeHeights>\n",
                         "\t</treeModel>\n")
    
  } else { # multiple trees
    
    tree_model <- paste0("\t<empiricalTreeDistributionModel id=\"treeModel\" fileName=\"", treefile_name, "\">\n", 
                         "\t\t<taxa idref=\"taxa\"/>\n",
                         "\t</empiricalTreeDistributionModel>\n",
                         "\t<statistic id=\"treeModel.currentTree\" name=\"Current Tree\">\n",
                         "\t\t<empiricalTreeDistributionModel idref=\"treeModel\"/>\n",
                         "\t</statistic>\n")
  }
  
  # tree proposal chunk
  if ("phylo" %in% class(tree)) {
    tree_proposal <- ""
  } else {
    tree_proposal <- paste0("\t\t<empiricalTreeDistributionOperator weight=\"", tree_proposal_weight, "\" metropolisHastings=\"", 
                            ifelse(empiricaltree_mh, "true", "false"), "\">\n",
                            "\t\t\t<empiricalTreeDistributionModel idref=\"treeModel\"/>\n",
                            "\t\t</empiricalTreeDistributionOperator>\n")
  }
  
  # tree indices output chunk
  if ("phylo" %in% class(tree)) {
    currentTree_output <- ""
  } else {
    currentTree_output <- "\t\t\t<statistic idref=\"treeModel.currentTree\"/>\n"
  }
  
  return(list(tree_model = tree_model, tree_proposal = tree_proposal, currentTree_output = currentTree_output))
}


xml_clockratemodel <- function(discrete_trait_name, ctmc = T, clockrate_mean = 1, clockrate_mean_stochastic = T, 
                               clockrate_proposal_weight = 1, clockratemean_proposal_weight = 1, clockrate_mean_gammashaperate = 0.5) {
  
  discretetrait_clock_rate <- paste0("\t<strictClockBranchRates id=\"", discrete_trait_name, ".branchRates\">\n",
                                     "\t\t<rate>\n",
                                     "\t\t\t<parameter id=\"", discrete_trait_name, ".clock.rate\" value=\"0.01\" lower=\"0.0\"/>\n",
                                     "\t\t</rate>\n",
                                     "\t</strictClockBranchRates>\n")
  
  clock_rate_proposal <- paste0("\t\t<scaleOperator scaleFactor=\"0.75\" weight=\"", clockrate_proposal_weight, "\">\n",
                                "\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate\"/>\n",
                                "\t\t</scaleOperator>\n")
  
  
  if (ctmc) { # the default prior
    clock_rate_prior <- paste0("\t\t<ctmcScalePrior>\n",
                               "\t\t\t<ctmcScale>\n",
                               "\t\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate\"/>\n", 
                               "\t\t\t</ctmcScale>\n", 
                               "\t\t\t<treeModel idref=\"treeModel\"/>\n", 
                               "\t\t</ctmcScalePrior>\n")
    
  } else if (!clockrate_mean_stochastic) { 
    
    clock_rate_prior <- paste0("\t\t<gammaPrior shape=\"1.0\" scale=\"", clockrate_mean, "\" offset=\"0.0\">\n",
                               "\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate\"/>\n", 
                               "\t\t</gammaPrior>\n")
  } else if (clockrate_mean_stochastic) {

    clock_rate_prior <- paste0("\t\t<distributionLikelihood>\n",
                               "\t\t\t<distribution>\n",
                               "\t\t\t\t<exponentialDistributionModel>\n",
                               "\t\t\t\t\t<mean>\n",
                               "\t\t\t\t\t\t<parameter id=\"", discrete_trait_name, ".clock.rate.exp.mean\" value=\"1\"/>\n",
                               "\t\t\t\t\t</mean>\n",
                               "\t\t\t\t</exponentialDistributionModel>\n", 
                               "\t\t\t</distribution>\n",
                               "\t\t\t<data>\n",
                               "\t\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate\"/>\n",
                               "\t\t\t</data>\n",
                               "\t\t</distributionLikelihood>\n",
                               "\t\t<gammaPrior shape=\"", clockrate_mean_gammashaperate, "\" scale=\"", 1/clockrate_mean_gammashaperate, "\" offset=\"0.0\">\n",
                               "\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate.exp.mean\"/>\n", 
                               "\t\t</gammaPrior>\n")
    
    clock_rate_proposal <- paste0(clock_rate_proposal, "\t\t<scaleOperator scaleFactor=\"0.75\" weight=\"", clockratemean_proposal_weight, "\">\n",
                                  "\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate.exp.mean\"/>\n",
                                  "\t\t</scaleOperator>\n")
  }
  
  return((list(discretetrait_clock_rate = discretetrait_clock_rate, clock_rate_prior = clock_rate_prior, clock_rate_proposal = clock_rate_proposal)))
  
}



xml_phyloctmc <- function(states_dat, discrete_trait_name, rootfreq_model, lheat = 1, symmetry = T, complete_history = F, do_totalcount = T, do_pairwisecount = T) {
  
  states <- sort(as.vector(unique(states_dat[, discrete_trait_name])))
  states <- states[states != "?"]
  states_num <- length(states)
  
  if (lheat == 1) {
    markov_jumps <- paste0("\t\t<attributePatterns idref=\"", discrete_trait_name, ".pattern\"/>\n")
  } else if (lheat > 1) {
    markov_jumps <- paste0("\t\t<patterns idref=\"patterns\"/>\n")
  }
  markov_jumps <- paste0(markov_jumps, "\t\t<treeModel idref=\"treeModel\"/>\n\t\t<siteModel idref=\"",
                         discrete_trait_name, ".siteModel\"/>\n",
                         "\t\t<generalSubstitutionModel idref=\"", discrete_trait_name, ".model\"/>\n",
                         "\t\t<strictClockBranchRates idref=\"", discrete_trait_name, ".branchRates\"/>\n")
  
  if (lheat > 1) {
    markov_jumps <- paste0("\t<treeLikelihood id=\"", discrete_trait_name, ".treeLikelihood\">\n", markov_jumps)
    
  } else if (complete_history) {
    markov_jumps <- paste0("\t<markovJumpsTreeLikelihood id=\"", discrete_trait_name,
                           ".treeLikelihood\" useUniformization=\"true\" saveCompleteHistory=\"true\" logCompleteHistory=\"true\" compactHistory=\"true\">\n", markov_jumps)
  } else {
    markov_jumps <- paste0("\t<markovJumpsTreeLikelihood id=\"", discrete_trait_name,
                           ".treeLikelihood\" stateTagName=\"", discrete_trait_name, ".states\">\n", markov_jumps)
  }
  
  # add root frequencies for asymmetric model
  markov_jumps <- paste0(markov_jumps, rootfreq_model)
  
  if ((!complete_history) && lheat == 1 && (do_totalcount + do_pairwisecount > 0)) {
    
    markov_jumps <- paste0(markov_jumps, "\n")
    
    if (do_totalcount) { # total counts
      total_count_name <- paste0("\t\t<parameter id=\"", discrete_trait_name, ".count\" value=\" ")
      total_count <- numeric(length = states_num^2)
      for (i in 1:states_num) {
        for (j in 1:states_num) {
          if (j != i) {
            total_count[(i - 1) * states_num + j] <- 1
          }
        }
      }
      
      total_count_line <- paste0(total_count_name, paste0(total_count, collapse = " "), "\"/>\n")
      markov_jumps <- paste0(markov_jumps, total_count_line)
    }
    
    if (do_pairwisecount) { # pairwise counts
      
      count_lines <- ""
      for (i in 1:states_num) {
        for (j in 1:states_num) {
          if (j != i) {
            count_name <- paste0("\t\t<parameter id=\"", i, "-", j, ".count", "\" value=\" ")
            counts <- numeric(length = states_num^2)
            counts[(i - 1) * states_num + j] <- 1
            count_line <- paste0(count_name, paste0(counts, collapse = " "), "\"/>\n")
            count_lines <- paste0(count_lines, count_line)
          }
        }
      }
      
      markov_jumps <- paste0(markov_jumps, count_lines)
    }
  }
  
  markov_jumps <- paste0(markov_jumps, ifelse(lheat > 1, "\t</treeLikelihood>\n", "\t</markovJumpsTreeLikelihood>\n"))
  
  return(markov_jumps)
}


xml_monitors <- function(discrete_trait_name, currentTree_output, file_name, bssvs = T, complete_history = F, lheat = 1,
                         ctmc = T, clockrate_mean_stochastic = F, symmetry = T, delta_prior = "Poisson", 
                         under_prior = F, mcmc_samplingfreq = 1, ml_numstones = 0, ml_chainlengthperstone = 0, ml_samplingfreq = 0, ml_alphaofbeta = 0.3) {
  
  nonZeroRates_screenlog <- ""
  if (bssvs) {
    nonZeroRates_screenlog <- paste0("\t\t\t<sumStatistic idref=\"", discrete_trait_name, ".nonZeroRates\"/>\n")
  }
  
  log_screen <- paste0("\t\t<log id=\"screenLog\" logEvery=\"", as.integer(mcmc_samplingfreq), "\">\n",
                       "\t\t\t<posterior idref=\"posterior\"/>\n", 
                       "\t\t\t<prior idref=\"prior\"/>\n",
                       "\t\t\t<likelihood idref=\"likelihood\"/>\n",
                       "\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate\"/>\n",
                       nonZeroRates_screenlog, "\t\t</log>\n\n")
  
  log_file <- paste0("\t\t<log id=\"fileLog\" logEvery=\"", as.integer(mcmc_samplingfreq), "\" fileName=\"", file_name, ".log\" overwrite=\"false\">\n",
                     "\t\t\t<posterior idref=\"posterior\"/>\n",
                     "\t\t\t<prior idref=\"prior\"/>\n",
                     "\t\t\t<likelihood idref=\"likelihood\"/>\n")
  
  parameters_log <- paste0("\t\t\t<parameter idref=\"", discrete_trait_name,".clock.rate\"/>\n",
                           "\t\t\t<parameter idref=\"", discrete_trait_name, ".rates\"/>\n")
  if (clockrate_mean_stochastic) {
    parameters_log <- paste0("\t\t\t<parameter idref=\"", discrete_trait_name, ".clock.rate.exp.mean\"/>\n", parameters_log)
  }
  if (!symmetry) {
    parameters_log <- paste0("\t\t\t<parameter idref=\"", discrete_trait_name, ".root.frequencies\"/>\n", parameters_log)
  }
  if (bssvs) {
    parameters_log <- paste0(parameters_log, "\t\t\t<parameter idref=\"", discrete_trait_name, ".indicators\"/>\n")
    if (delta_prior == "Beta-Binomial") {
      parameters_log <- paste0(parameters_log, "\t\t\t<parameter idref=\"", discrete_trait_name, ".indicator.probability\"/>\n")
    }
    parameters_log <- paste0(parameters_log, "\t\t\t<sumStatistic idref=\"", discrete_trait_name, ".nonZeroRates\"/>\n")
  }
  
  log_file <- paste0(log_file, parameters_log, currentTree_output, "\t\t\t<", ifelse(lheat > 1, "treeLikelihood", "markovJumpsTreeLikelihood"),
                     " idref=\"", discrete_trait_name, ".treeLikelihood\"/>\n\t\t</log>\n\n")
  
  if (lheat > 1) {
    log_tree <- paste0("\t\t<logTree id=\"treeFileLog\" logEvery=\"", as.integer(mcmc_samplingfreq), "\" nexusFormat=\"true\" fileName=\"", 
                       file_name, ".trees\" sortTranslationTable=\"true\">\n",
                       "\t\t\t<treeModel idref=\"treeModel\"/>\n",
                       "\t\t\t<posterior idref=\"posterior\"/>\n",
                       "\t\t</logTree>\n")
    log_history <- ""
    
  } else if (complete_history) {
    log_tree <- paste0("\t\t<logTree id=\"treeFileLog\" logEvery=\"", as.integer(mcmc_samplingfreq), "\" nexusFormat=\"true\" fileName=\"", 
                       file_name, ".trees\" sortTranslationTable=\"true\">\n",
                       "\t\t\t<treeModel idref=\"treeModel\"/>\n",
                       "\t\t\t<posterior idref=\"posterior\"/>\n",
                       "\t\t\t<markovJumpstreelikelihood idref=\"", discrete_trait_name, ".treeLikelihood\"/>\n",
                       "\t\t</logTree>\n")
    log_history <- paste0("\t\t<log id=\"historyLogger\" logEvery=\"", as.integer(mcmc_samplingfreq), "\" fileName=\"", file_name, ".txt\">\n",
                          "\t\t\t<completeHistoryLogger>\n",
                          "\t\t\t\t<markovJumpstreelikelihood idref=\"", discrete_trait_name, ".treeLikelihood\"/>\n",
                          "\t\t\t</completeHistoryLogger>\n", "\t\t</log>\n")
  } else {
    log_tree <- paste0("\t\t<logTree id=\"treeFileLog\" logEvery=\"", as.integer(mcmc_samplingfreq), "\" nexusFormat=\"true\" fileName=\"", 
                       file_name, ".trees\" sortTranslationTable=\"true\">\n",
                       "\t\t\t<treeModel idref=\"treeModel\"/>\n",
                       "\t\t\t<posterior idref=\"posterior\"/>\n",
                       "\t\t\t<trait name=\"", discrete_trait_name, ".states\" tag=\"", discrete_trait_name, "\">\n", 
                       "\t\t\t\t<ancestralTreeLikelihood idref=\"", discrete_trait_name, ".treeLikelihood\"/>\n",
                       "\t\t\t</trait>\n",
                       "\t\t</logTree>\n")
    log_history <- ""
  }
  
  log_powerposterior <- ""
  if ((!under_prior) && (lheat == 1) && ml_chainlengthperstone > 0) {
    log_powerposterior <- paste0("\t<marginalLikelihoodEstimator chainLength=\"", as.integer(ml_chainlengthperstone), 
                                     "\" pathSteps=\"", ml_numstones, "\" pathScheme=\"betaquantile\" alpha=\"", as.numeric(ml_alphaofbeta), "\">\n", 
                                     "\t\t<samplers>\n", 
                                     "\t\t\t<mcmc idref=\"mcmc\"/>\n", 
                                     "\t\t</samplers>\n",
                                     "\t\t<pathLikelihood id=\"pathLikelihood\">\n", 
                                     "\t\t\t<source>\n",
                                     "\t\t\t\t<posterior idref=\"posterior\"/>\n",
                                     "\t\t\t</source>\n",
                                     "\t\t\t<destination>\n",
                                     "\t\t\t\t<prior idref=\"prior\"/>\n",
                                     "\t\t\t</destination>\n",
                                     "\t\t</pathLikelihood>\n",
                                     "\t\t<log id=\"MLE\" logEvery=\"", as.integer(ml_samplingfreq), "\" fileName=\"", file_name, "_MLE.log\">\n",
                                     "\t\t\t<pathLikelihood idref=\"pathLikelihood\"/>\n")
    log_powerposterior <- paste0(log_powerposterior, parameters_log, "\t\t</log>\n", "\t</marginalLikelihoodEstimator>\n")
  }
  
  return(list(log_jointposterior = paste0(log_screen, log_file, log_tree, log_history), log_powerposterior = log_powerposterior))
}
