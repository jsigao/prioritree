
sim.history2 <- function (tree, Q, anc = NULL, nsim = 1, ...) 
{
  if (!inherits(tree, "phylo")) 
    stop("tree should be an object of class \"phylo\".")
  if (hasArg(message)) 
    message <- list(...)$message
  else message <- TRUE
  tree <- ape::reorder.phylo(tree, "cladewise")
  if (!isSymmetric(Q)) 
    if (message) 
      cat("Note - the rate of substitution from i->j should be given by Q[j,i].\n")
  if (!all(round(colSums(Q), 10) == 0)) {
    if (all(round(rowSums(Q), 10) == 0) && !isSymmetric(Q)) {
      if (message) {
        cat("Detecting that rows, not columns, of Q sum to zero :\n")
        cat("Transposing Q for internal calculations.\n")
      }
      Q <- t(Q)
    }
    else {
      if (message) 
        cat("Some columns (or rows) of Q don't sum to 0.0. Fixing.\n")
      diag(Q) <- 0
      diag(Q) <- -colSums(Q, na.rm = TRUE)
    }
  }
  if (is.null(dimnames(Q))) 
    dimnames(Q) <- list(1:nrow(Q), 1:ncol(Q))
  mtrees <- vector(mode = "list", length = nsim)
  class(mtrees) <- c("multiSimmap", "multiPhylo")
  if (is.null(anc)) 
    anc <- setNames(rep(1/ncol(Q), ncol(Q)), colnames(Q))
  if (is.character(anc)) {
    anc <- colSums(to.matrix(anc, colnames(Q)))
    anc <- anc/sum(anc)
  }
  for (i in 1:nsim) {
    a <- phytools::rstate(anc)
    mtree <- tree
    mtree$maps <- vector(mode = "list", length = nrow(tree$edge))
    node.states <- matrix(NA, nrow(tree$edge), ncol(tree$edge))
    node.states[which(tree$edge[, 1] == (length(tree$tip) + 1)), 1] <- a
    for (j in 1:nrow(tree$edge)) {
      if (tree$edge.length[j] == 0) {
        map <- vector()
        map[1] <- tree$edge.length[j]
        names(map)[1] <- node.states[which(tree$edge[, 1] == tree$edge[j, 2]), 1] <- node.states[j,2] <- node.states[j, 1]
      }
      else {
        time = 0
        state <- node.states[j, 1]
        new.state <- state
        dt <- vector()
        map <- vector()
        k <- 1
        while (time < tree$edge.length[j]) {
          dt[1] <- time
          
          # customized edits from the original phytools version of sim.history to allow the matrix to be reducible
          # i.e., Q matrix can have an entire row with zero implying the corresponding state to be an absorbing state
          # this type of asymmetric Q matrix may have positive likelihood giving rise the data
          if (Q[state, state] == 0) {
            dt[2] <- Inf
          } else {
            dt[2] <- dt[1] + rexp(n = 1, rate = -Q[state, state])
          }
          
          if (dt[2] < tree$edge.length[j]) new.state <- phytools::rstate(Q[, state][-match(state, rownames(Q))]/sum(Q[, state][-match(state, rownames(Q))]))
          dt[2] <- min(dt[2], tree$edge.length[j])
          map[k] <- dt[2] - dt[1]
          names(map)[k] <- state
          k <- k + 1
          state <- new.state
          time <- dt[2]
        }
        node.states[which(tree$edge[, 1] == tree$edge[j, 2]), 1] <- node.states[j, 2] <- names(map)[length(map)]
      }
      mtree$maps[[j]] <- map
    }
    mtree$node.states <- node.states
    tip.states <- node.states[tree$edge[, 2] <= length(tree$tip), 2]
    tip.states <- tip.states[order(tree$edge[tree$edge[, 2] <= length(tree$tip), 2])]
    names(tip.states) <- tree$tip.label
    mtree$states <- tip.states
    allstates <- vector()
    for (j in 1:nrow(mtree$edge)) allstates <- c(allstates, names(mtree$maps[[j]]))
    allstates <- unique(allstates)
    mtree$mapped.edge <- matrix(data = 0, length(mtree$edge.length), 
                                length(allstates), dimnames = list(apply(mtree$edge, 1, function(x) paste(x, collapse = ",")), state = allstates))
    for (j in 1:length(mtree$maps)) for (k in 1:length(mtree$maps[[j]])) mtree$mapped.edge[j, names(mtree$maps[[j]])[k]] <- mtree$mapped.edge[j, names(mtree$maps[[j]])[k]] + mtree$maps[[j]][k]
    class(mtree) <- c("simmap", setdiff(class(mtree), "simmap"))
    mtrees[[i]] <- mtree
  }
  if (nsim == 1) 
    mtrees <- mtrees[[1]]
  if (message) 
    cat("Done simulation(s).\n")
  mtrees
}


# given one tree file path and one log file path
historySimulator_teststatisticsComputer <- function (states_dat, taxon_name, discrete_trait_name, tree_path, log_path, 
                                                     simsamples_num = 1000L, simulated_tmppath) {
  
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

    history <- sim.history2(tree = tree, Q = Q, anc = root_freqs, nsim = 1, message = F)
    
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

  write.table(simulated_dataset, file = simulated_tmppath, quote = F, sep = "\t", row.names = F)
  
  teststatistics_df <- data.frame(cbind(simulated_sampleidx, observed_parsimonyscore, simulated_parsimonyscore, 
                                        rep(observed_multinomiallikelihood, length(simulated_multinomiallikelihood)), simulated_multinomiallikelihood), stringsAsFactors = F)
  colnames(teststatistics_df) <- c("sample_index", "observed_parsimonyscore", "simulated_parsimonyscore", "observed_multinomiallikelihood", "simulated_multinomiallikelihood")
  return(teststatistics_df)
}
