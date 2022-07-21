
#' Simulate history of discrete-character changes over a bifurcation tree conditional or unconditional on the tip states
#' 
#' @param tree A bifurcation tree of class "phylo"
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant geographic model) characterizes the CTMC
#' @param Q_ages Boundaries of time intervals for a piecewise constant geographic model (NULL means a constant model)
#' @param root_freq The vector state frequencies at the root of the tree (that we will draw the root state from); if NULL then it's uniform
#' @param nsim Number of simulations to perform
#' @param conditional Whether condition on the observed states at the tip (i.e., stochastic mapping) or not (i.e., forward simulation)
#' @param trait Name of the discrete trait used in the tree file (e.g., "states")
#' @return A phylo and simmap object (or a multiPhylo and multiSimmap object when nsim > 1) that contains the simulated full history
#' @export
#' @examples
#' tree <- ape::rtree(7)
#' Q <- matrix(c(-1, 1, 1, -1), ncol = 2, byrow = T)
#' root_freq <- setNames(rep(0.5, 2), c("A", "B"))
#' one_history <- sim_history(tree, Q, root_freq = root_freq, conditional = F)
sim_history <- function(tree, Q, Q_ages = NULL, root_freq = NULL, nsim = 1L, conditional = F, trait = NULL) {
  
  # first some sanity checks
  if (!inherits(tree, "phylo")) {
    stop("tree should be an object of class \"phylo\".")
  }
  
  # config Q matrices and the interval time bounds
  if (is.list(Q)) {
    if (is.null(Q_ages) && length(Q) > 1) {
      stop("multiple matrices but no information about how to arrange them chronologically.\n")
    } else if (!is.null(Q_ages)) {
      
      if (all(Q_ages <= 0)) {
        Q_ages <- -Q_ages
      } else if (any(Q_ages < 0)) {
        stop("Q times need to be either all non-negative or non-positive.\n")
      }
      
      if (length(Q) != length(Q_ages[Q_ages != 0]) + 1L) {
        stop("number of matrices does not match the number of epoch time boundaries, which is supposed to be one fewer.\n")
      }
      
      if (all(Q_ages != 0)) {
        if (length(Q_ages) > 1 && identical(Q_ages, sort(Q_ages, decreasing = T))) {
          Q_ages <- c(Q_ages, 0)
        } else {
          Q_ages <- c(0, Q_ages)
        }
      }
      
      Q <- Q[order(Q_ages, decreasing = T)]
      Q_ages <- sort(Q_ages, decreasing = T)
      Q_ages <- Q_ages[-length(Q_ages)]
      
    }
  } else if (!is.matrix(Q)) {
    stop("Q needs to be either a single matrix or a list of matrices.\n")
  } else {
    Q <- list(Q)
  }
  
  nstates <- ncol(Q[[1]])
  if (nstates < 2) {
    stop("there has to be at least two states.\n")
  }
  
  # we assume the state names can be found as either the row or column names of the Q matrix of or the name of the root frequency vector
  states <- rownames(Q[[1]])
  if (is.null(states)) {
    states <- colnames(Q[[1]])
    if (is.null(states)) {
      if (!is.null(root_freq)) {
        states <- names(root_freq)
      }
      if (is.null(states)) {
        stop("state names are not assigned.\n")
      }
    }
  }
  
  # sanity checks for Q matrix
  for (i in 1:length(Q)) {
    if (ncol(Q[[i]]) != nrow(Q[[i]])) {
      stop("all matrices need to be square.\n")
    }
    if (ncol(Q[[i]]) != nstates) {
      stop("all matrices need to have identical dimensions.\n")
    }
    if (!isTRUE(all.equal(as.numeric(apply(Q[[i]], 1, sum)), rep(0, nstates), tolerance = 1e-3))) {
      stop("row of Q needs to sum to 0.\n")
    }
    if (any(Q[[i]][row(Q[[i]]) != col(Q[[i]])] < 0)) {
      stop("off diagonal elements of Q needs to be non-negative.\n")
    }
    if (any(diag(Q[[i]]) > 0)) {
      stop("diagonal elements of Q needs to be non-positive.\n")
    }
    if (all(diag(Q[[i]]) == 0)) {
      stop("some elements in Q need to be positive.\n")
    }
  }
  
  if (conditional) {
    return(sim_history_conditional(tree, Q, Q_ages, states, nsim, trait))
  } else {
    return(sim_history_unconditional(tree, Q, Q_ages, states, root_freq, nsim))
  }
}


#' Simulate discrete-character histories using the inferred posterior distribution and compute posterior-predictive summary statistics using the simulated histories
#' 
#' @param states_dat A data-frame object containing at least two columns: one for the name of each tip (column name specified by \code{taxon_name}), 
#' and the other for the trait state of each tip (column name specified by \code{discrete_trait_name}).
#' @param taxon_name Name of the column containing tip names.
#' @param discrete_trait_name Name of the column containing tip states.
#' @param tree_path File path of the posterior distribution of trees that will be read in to simulated over.
#' @param log_path File path of the posterior distribution of parameters whose values will be used in the simulation.
#' @param simsamples_num Number of histories to simulate.
#' @param simulated_outpath File path to write the simulated data to.
#' @return A data-frame object that contains the simulated and observed values of the two summary statistics
#' @export
#' @examples 
#' 
historySimulator_teststatisticsComputer <- function (states_dat, taxon_name, discrete_trait_name, tree_path, log_path, 
                                                     simsamples_num = 1000L, simulated_outpath) {
  
  # discrete trait information
  observed_tipstates <- states_dat[, discrete_trait_name]
  names(observed_tipstates) <- states_dat[, taxon_name]
  
  states <- sort(unique(observed_tipstates))
  states <- states[states != "?"]
  states_num <- length(states)
  
  observed_tipstates_phyDat <- phangorn::phyDat(t(t(observed_tipstates)), type = "USER", levels = states)
  observed_tipstates_nomissing <- observed_tipstates[observed_tipstates != "?"]
  observed_multinomiallikelihood <- sum(table(observed_tipstates_nomissing) * log(table(observed_tipstates_nomissing))) - length(observed_tipstates_nomissing) * log(length(observed_tipstates_nomissing))

  # read the tree file until reaching the very first tree (to get the header)
  tree_con = file(tree_path, "r")
  tree_line <- readLines(tree_con, n = 1)
  treeheader_lines <- c()
  while (length(tree_line) > 0 && (!grepl("&", tree_line))) {
    treeheader_lines <- c(treeheader_lines, tree_line)
    tree_line <- readLines(tree_con, n = 1)
  }

  # prep for log file parsing
  col_names <- colnames(read.table(log_path, header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
  
  rates_startcolnum <- min(grep(".rates", col_names))
  rates_endcolnum <- max(grep(".rates", col_names))
  symmetry <- T
  if (rates_endcolnum - rates_startcolnum + 1 == states_num * (states_num - 1)) symmetry <- F
  
  bssvs <- T
  if (length(grep(".indicators", col_names)) == 0) {
    bssvs <- F
  } else {
    indicators_startcolnum <- min(grep(".indicators", col_names))
  }
  
  mu_colnum <- grep(".clock.rate$", col_names)[1]
  
  rootfreq_colnum <- 0
  if (any(grepl(".root.frequencies$", col_names))) rootfreq_colnum <- grep(".root.frequencies$", col_names)[1]
  
  # read the log file until the reaching the header
  log_con = file(log_path, "r")
  log_line <- readLines(log_con, n = 1)
  while (length(log_line) > 0 && (log_line != paste(col_names, collapse = "\t"))) {
    log_line <- readLines(log_con, n = 1)
  }
  
  simsamples_id <- 1L
  observed_parsimonyscore <- integer()
  simulated_parsimonyscore <- integer()
  simulated_multinomiallikelihood <- numeric()
  simulated_tipstates_all <- list()
  simulated_sampleidx <- c()
  
  while ((simsamples_num == 0) || (simsamples_id <= simsamples_num)) {
    
    if (length(tree_line) == 0 || grepl("End;", tree_line)) {
      break
    } else if (grepl("&", tree_line)) {
      tmptree_path <- tempfile(fileext = ".tree")
      cat(c(treeheader_lines, tree_line, "End;"), file = tmptree_path, sep = "\n")
      tree <- ape::read.nexus(tmptree_path)
      file.remove(tmptree_path)
      
      tree_line <- readLines(tree_con, n = 1L)
    } else {
      tree_line <- readLines(tree_con, n = 1L)
      next
    }
    
    log_line <- readLines(log_con, n = 1L)
    if (length(log_line) == 0) {
      break
    } else {
      logline_values <- as.numeric(unlist(strsplit(log_line, "\t")))
      
      if (length(logline_values) == length(col_names)) {
        
        # prepare the Q matrix
        # put rates into matrix
        rate_mat <- matrix(0, nrow = states_num, ncol = states_num)
        rate_mat[lower.tri(rate_mat)] <- logline_values[1:(choose(states_num, 2)) + rates_startcolnum - 1]
        rate_mat <- t(rate_mat)
        
        if (symmetry) {
          rate_mat[lower.tri(rate_mat)] <- logline_values[1:(choose(states_num, 2)) + rates_startcolnum - 1]
        } else {
          rate_mat[lower.tri(rate_mat)] <- logline_values[(choose(states_num, 2) + 1):(choose(states_num, 2) * 2) + rates_startcolnum - 1]
        }
        
        # put indicator into matrix
        if (bssvs) {
          indicator_mat <- matrix(0, nrow = states_num, ncol = states_num)
          indicator_mat[lower.tri(indicator_mat)] <- logline_values[1:(choose(states_num, 2)) + indicators_startcolnum - 1]
          indicator_mat <- t(indicator_mat)
          
          if (symmetry) {
            indicator_mat[lower.tri(indicator_mat)] <- logline_values[1:(choose(states_num, 2)) + indicators_startcolnum - 1]
          } else {
            indicator_mat[lower.tri(indicator_mat)] <- logline_values[(choose(states_num, 2) + 1): (choose(states_num, 2) * 2) + indicators_startcolnum - 1]
          }
        }
        
        # overall rate
        clock_rate <- logline_values[mu_colnum]
        
        # construct Q matrix
        Q <- rate_mat
        if (bssvs) {
          Q <- Q * indicator_mat
        }
        # note here the Q matrix would be correctly rescaled if it's symmetric
        # but would be incorret if it's asymmetric (as pi won't be flat)
        # however, this is how BEAST has been rescaling internally so far, so we follow it for now
        mu <- sum(apply(Q, 1, sum) / nrow(Q))
        Q <- (Q / mu) * clock_rate
        
        diag(Q) <- -apply(Q, 1, sum)
        dimnames(Q) <- list(states, states)
        
        # using the estimated root freq vector to weight the conditional likelihoods
        # following the default behavior of beast
        
        root_freqs <- NULL
        if (rootfreq_colnum > 0) {
          root_freqs <- logline_values[rootfreq_colnum]
          names(root_freqs) <- states
        }
        
      } else {
        next
      }
    }

    history <- sim_history(tree = tree, Q = Q, root_freq = root_freqs, nsim = 1L, conditional = F)
    
    # write tip state to a file so it can be reused
    # need to delete it as the end
    simulated_tipstates <- history$states
    simulated_tipstates[names(observed_tipstates)[observed_tipstates == "?"]] <- "?"
    simulated_tipstates_all <- c(simulated_tipstates_all, list(simulated_tipstates[sort(names(simulated_tipstates))]))
    simulated_tipstates_phyDat <- phangorn::phyDat(t(t(simulated_tipstates)), type = "USER", levels = states)
    
    observed_parsimonyscore <- c(observed_parsimonyscore, phangorn::parsimony(tree = tree, data = observed_tipstates_phyDat, method = "fitch"))
    simulated_parsimonyscore <- c(simulated_parsimonyscore, phangorn::parsimony(tree = tree, data = simulated_tipstates_phyDat, method = "fitch"))
    
    simulated_tipstates <- simulated_tipstates[simulated_tipstates != "?"]
    simulated_multinomiallikelihood <- c(simulated_multinomiallikelihood, 
                                         sum(table(simulated_tipstates) * log(table(simulated_tipstates))) - length(simulated_tipstates) * log(length(simulated_tipstates)))
    
    simulated_sampleidx <- c(simulated_sampleidx, simsamples_id)
    simsamples_id <- simsamples_id + 1L
  }
  
  close(tree_con)
  close(log_con)
  
  tip_names <- names(simulated_tipstates_all[[1]])
  simulated_tipstates_all <- do.call(rbind, simulated_tipstates_all)
  simulated_dataset <- data.frame(cbind(simulated_sampleidx, simulated_tipstates_all), stringsAsFactors = F)
  colnames(simulated_dataset) <- c("sample_index", tip_names)

  write.table(simulated_dataset, file = simulated_outpath, quote = F, sep = "\t", row.names = F)
  
  teststatistics_df <- data.frame(cbind(simulated_sampleidx, observed_parsimonyscore, simulated_parsimonyscore, 
                                        rep(observed_multinomiallikelihood, length(simulated_multinomiallikelihood)), simulated_multinomiallikelihood), stringsAsFactors = F)
  colnames(teststatistics_df) <- c("sample_index", "observed_parsimonyscore", "simulated_parsimonyscore", "observed_multinomiallikelihood", "simulated_multinomiallikelihood")
  return(teststatistics_df)
}
