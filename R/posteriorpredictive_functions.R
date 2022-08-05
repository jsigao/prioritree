#' Simulate history of discrete-character changes over a bifurcation tree without conditioning on the tip states
#' @param tree A bifurcation tree of class "phylo"
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant geographic model) characterizes the CTMC
#' @param Q_ages A vector containing boundaries of time intervals for a piecewise constant geographic model (NULL means a constant model);
#' the length of this vector should be one shorter than the Q-matrix list, sorted from oldest to youngest.
#' @param states States of the discrete character
#' @param root_freq A vector of frequencies at the root of the tree (that we will draw the root state from); if NULL then it's uniform
#' @param nsim Number of simulations to perform
#' @return A phylo and simmap object (or a multiPhylo and multiSimmap object when nsim > 1) that contains the simulated full history
#' @export
#' @examples
#' tree <- ape::rtree(7)
#' Q <- list(matrix(c(-1, 1, 1, -1), ncol = 2, byrow = TRUE))
#' states <- c("A", "B")
#' root_freq <- setNames(rep(0.5, 2), c("A", "B"))
#' one_history <- sim_history_unconditional(tree, Q, states = states, root_freq = root_freq)
sim_history_unconditional <- function(tree, Q, Q_ages = NULL, states, root_freq = NULL, nsim = 1L) {
  
  nstates <- length(states)
  
  # sanity checks for root state freq
  if (is.null(root_freq)) { # if not provided then assume it to be uniform
    root_freq <- rep(1 / nstates, nstates)
    names(root_freq) <- states
  } else {
    
    if (any(root_freq < 0)) {
      stop("root frequencies need to be non-negative.\n")
    } else if (all(root_freq == 0)) {
      stop("at least one root frequency needs to be positive.\n")
    }
    
    if (!is.null(names(root_freq))) {
      if (!identical(states, names(root_freq))) {
        if (!identical(sort(states), sort(names(root_freq)))) {
          stop("names of root frequencies are not identical to Q's.\n")
        } else {
          root_freq <- root_freq[states]
        }
      }
    }
    # normalize the vector
    root_freq <- root_freq / sum(root_freq)
  } # end sanity checks
  
  # tree meta info
  nedges <- nrow(tree$edge)
  nnodes <- nedges + 1L
  ntips <- length(tree$tip.label)
  if (2 * ntips - 1L != nnodes) {
    stop("we assume the tree is fully bifurcating")
  }
  
  root <- unique(tree$edge[, 1][!tree$edge[, 1] %in% tree$edge[, 2]])
  if (length(root) != 1) {
    stop("we should have only one root.\n")
  }
  
  trees <- vector("list", nsim)
  class(trees) <- c("multiSimmap", "multiPhylo")
  
  # start simulation
  for (i in 1:nsim) {
    
    node.states <- matrix(NA, nrow = nedges, ncol = 2, byrow = T)
    maps <- vector("list", nedges)
    
    # helper variables
    node_visited <- rep(F, nnodes)
    node_times <- ape::node.depth.edgelength(tree)
    node_ages <- max(node_times) - node_times
    
    Q_times <- NULL
    if (length(Q) > 1) {
      Q_times <- max(node_ages) - Q_ages
    }
    
    # sample root state first
    node <- root
    state_current <- states[sample.int(nstates, size = 1L, prob = root_freq)]
    node.states[tree$edge[, 1] == node, 1] <- state_current
    
    # now we need to traverse the tree to simulate the history
    while (any(!node_visited)) {
      
      # traversal
      if (node %in% tree$edge[, 1]) { # not tip, so must have two descendants
        descendants <- tree$edge[tree$edge[, 1] == node, 2]
        if (!node_visited[descendants[1]]) { # first visit
          node_next <- descendants[1] # move forward
        } else if (!node_visited[descendants[2]]) { # second visit
          node <- descendants[2] # move forward
          next
        } else { # third visit
          node_visited[node] <- T
          if (node == root) {
            break
          } else {
            ancestor <- tree$edge[tree$edge[, 2] == node, 1]
            node <- ancestor # move backward
            next
          }
        }
      } else { # is a tip
        node_visited[node] <- T
        ancestor <- tree$edge[tree$edge[, 2] == node, 1]
        node_next <- ancestor # move backward
      }
      
      if (node != root) { # do simulation on the subtending branch of this node
        
        map <- numeric()
        edge_idx <- which(tree$edge[, 2] == node)
        if (length(edge_idx) != 1) {
          stop("there should be one and only one subtending branch of each non-root node.\n")
        }
        ancestor <- tree$edge[edge_idx, 1]
        edge_length <- tree$edge.length[edge_idx]
        
        time_last <- node_times[ancestor]
        time_current <- node_times[ancestor]
        time_end <- node_times[node]
        
        state_current <- node.states[edge_idx, 1]
        stateidx_current <- match(state_current, states)
        
        while (time_current < time_end) {
          
          # next matrix change time
          if (length(Q) > 1 && any(Q_times > time_current)) {
            time_wall <- Q_times[Q_times > time_current][1]
          } else {
            time_wall <- time_end
          }
          if (time_wall > time_end) {
            time_wall <- time_end
          }
          
          # fetch the Q matrix in this interval
          if (length(Q) == 1) {
            Q_current <- Q[[1]]
          } else {
            Q_current <- Q[[findInterval(time_current, Q_times) + 1L]]
          }
          
          # draw the event (if any) in this interval
          rate_dominant <- -Q_current[stateidx_current, stateidx_current]
          if (rate_dominant == 0) { # this state is absorbing
            time_current <- time_wall
          } else {
            piece_duration <- rexp(1, rate_dominant)
            time_current <- time_current + piece_duration
            
            if (time_current < time_wall) {
              map_states <- names(map)
              map <- c(map, time_current - time_last)
              names(map) <- c(map_states, state_current)
              time_last <- time_current
              
              prob_vec <- Q_current[stateidx_current, ]
              prob_vec[stateidx_current] <- 0
              state_current <- states[sample.int(nstates, size = 1L, prob = prob_vec)]
              stateidx_current <- match(state_current, states)
              
            } else {
              time_current <- time_wall
            }
          }
          
        } # end while loop
        
        node.states[edge_idx, 2] <- state_current
        if (node %in% tree$edge[, 1]) { # assign start state of the descendant branch (if haven't reached tip)
          node.states[tree$edge[, 1] == node, 1] <- state_current
        }
        
        map_states <- names(map)
        map <- c(map, time_end - time_last)
        names(map) <- c(map_states, state_current)
        maps[[edge_idx]] <- map
      } # end simulation on this branch
      
      node <- node_next
    } # end tree traversal
    
    # format the tree as a simmap object
    # tip states
    tip.states <- node.states[match(1:ntips, tree$edge[, 2]), 2]
    names(tip.states) <- tree$tip.label
    
    # mapped.edge
    mapped.edge <- matrix(data = 0, nrow = nedges, ncol = nstates, 
                          dimnames = list(apply(tree$edge, 1, function(x) paste(x, collapse = ",")), state = states))
    for (j in 1:nedges) {
      for (k in 1:length(maps[[j]])) {
        mapped.edge[j, names(maps[[j]])[k]] <- mapped.edge[j, names(maps[[j]])[k]] + maps[[j]][k]
      }
    }
    
    tree$maps <- maps
    tree$node.states <- node.states
    tree$states <- tip.states
    tree$mapped.edge <- mapped.edge
    class(tree) <- c("simmap", class(tree)[class(tree) != "simmap"])
    
    trees[[i]] <- tree
  } # end simulation
  
  # (following the convention established by sim.history of phytools) simplify data structure when there is only one resulted history
  if (nsim == 1) {
    trees <- trees[[1]]
  }
  
  return(trees)
}

#' Simulate history of discrete-character changes over a bifurcation tree conditioning on the tip states (currently on all node states)
#' we can relax this to allow only conditioning on the tip states, which would require an implementation of pruning algorithm (including transition-probablity matrix computation)
#' 
#' @param tree A bifurcation tree of class "phylo" 
#' (here we assume it either has a node.states, node.data, or maps component so that we can fetch the state of each node, both internal and tip, in the tree to condition on)
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant geographic model) characterizes the CTMC
#' @param Q_ages A vector containing boundaries of time intervals for a piecewise constant geographic model (NULL means a constant model);
#' the length of this vector should be one shorter than the Q-matrix list, sorted from oldest to youngest.
#' @param states States of the discrete character
#' @param nsim Number of simulations to perform
#' @param trait Name of the discrete trait used in the tree file (e.g., "states")
#' @return A phylo and simmap object (or a multiPhylo and multiSimmap object when nsim > 1) that contains the simulated full history
#' @export
#' @examples 
#' \dontrun{
#' tree <- ape::rtree(7)
#' Q <- list(matrix(c(-1, 1, 1, -1), ncol = 2, byrow = TRUE))
#' states <- c("A", "B")
#' one_history <- sim_history_conditional(tree, Q, states = states)
#' # todo: add a tree to data file
#' }

sim_history_conditional <- function(tree, Q, Q_ages = NULL, states, nsim = 1L, trait = NULL) {
  
  nstates <- length(states)
  
  # tree meta info
  nedges <- nrow(tree$edge)
  nnodes <- nedges + 1L
  ntips <- length(tree$tip.label)
  if (2 * ntips - 1L != nnodes) {
    stop("we assume the tree is fully bifurcating")
  }
  
  # we need to fetch the node states when there is not already node.states
  if (!("node.states" %in% names(tree))) {
    
    if ("maps" %in% names(tree)) {
      
      node.state <- character(nnodes)
      node.state[tree$edge[, 2]] <- sapply(tree$maps, function(x) names(x)[length(x)])
      
      root <- unique(tree$edge[, 1][!tree$edge[, 1] %in% tree$edge[, 2]])
      if (length(root) != 1 || node.state[root] != "") {
        stop ("root ill-formed.\n")
      }
      
      node.state[root] <- names(tree$maps[[which(tree$edge[, 1] == root)[1]]])[1]
      
      if (any(!node.state %in% states)) { # currently we assume internal node states are all resolved
        stop ("node state info ill-formed.\n")
      }
      
    } else if ("node.data" %in% names(tree)) {
      
      newickext_names <- gsub("=\\{\\{(.*?)\\}\\}", "", tree$node.data)
      newickext_names <- gsub("=\\{(.*?)\\}", "", newickext_names)
      newickext_names <- lapply(strsplit(newickext_names, ","), function(x) gsub("=.*$", "", x))
      newickext_name <- unique(unlist(newickext_names))
      
      if (is.null(trait)) {
        trait_nothistory <- grep("history|rate|\\.prob$|\\.set$|length|height|range|posterior|HPD|median", newickext_name, invert = T, value = T)
        if (length(trait_nothistory) == 1 && all(sapply(newickext_names, function(x) trait_nothistory %in% x))) {
          trait <- trait_nothistory
        } else {
          stop("Cannot figure out tag name for the trait (i.e., node state).")
        }
      } else if (any(!sapply(newickext_names, function(x) trait %in% x))) {
        stop("trait not found in (at least) some of the nodes")
      }
      
      node.state <- str_match(tree$node.data, paste0(trait, "=\"(.*?)\""))[, 2]
      if (length(node.state) != nnodes || any(!node.state %in% states)) { # currently we assume internal node states are all resolved
        stop ("node state info ill-formed.\n")
      }
      
    } else {
      # todo
      stop("currently this function assumes the internal node states are known.\n")
    }
    
    tree$node.states <- matrix(node.state[tree$edge], ncol = 2)
  } # end fetching
  
  nodes_times <- ape::node.depth.edgelength(tree)
  nodes_ages <- max(nodes_times) - nodes_times
  
  # create mu and the uniformized R matrix as global variable as they need to be updated by the fill_DTMCmats function as needed
  assign("mu", sapply(Q, function(x) max(-diag(x))), envir = .pkg_env)
  assign("R", vector("list", length(Q)), envir = .pkg_env)
  fill_DTMCmats(Q, 1)
  
  trees <- vector("list", nsim)
  # start simulation
  for (i in 1:nsim) {
    maps <- vector("list", nedges)
    for (j in 1:nedges) { # loop over branches
      node_ages <- nodes_ages[tree$edge[j, ]]
      node_state_indices <- match(tree$node.states[j, ], states)
      maps[[j]] <- sim_history_branch_conditional(node_ages, Q, Q_ages, node_state_indices, states)
    }
    
    tree$maps_posthoc <- maps
    trees[[i]] <- tree
  }
  
  if (nsim == 1) {
    trees <- trees[[1]]
  }
  
  return(trees)
  
  # todo: when internal state are not available
  # 1: write pruning algorithm ourselves to obtain the partial likelihoods for each node (based on the precomputed P matrix of each branch)
  # 2: reconstruct the joint ancestral states by doing the up-pass using the computed partial likelihoods, root frequency (if provided), and P matrices
  # 3: stochastic mapping (same as what we do when we have internal state estimates already)
}


#' Simulate history of discrete-character changes over a branch (which may comprise one or more pieces delimited by time interval bounds) 
#' conditioning on the start and end states
#' @param node_ages Ages of the two nodes defining this branch
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant geographic model) characterizes the CTMC
#' @param Q_ages A vector containing boundaries of time intervals for a piecewise constant geographic model (NULL means a constant model);
#' the length of this vector should be one shorter than the Q-matrix list, sorted from oldest to youngest.
#' @param node_state_indices State indices (i.e., the indices to use to fecth the states from the states vector) of the start and end nodes
#' @param states States of the discrete character
#' @return A map object (as in the simmap data structure) that contains the simulated history (as a series of events) over this branch
#' @export
sim_history_branch_conditional <- function(node_ages, Q, Q_ages = NULL, node_state_indices, states) {
  
  # we first compute P matrix for each piece (defined by time intervals) of the branch
  # then if there are more than one pieces on this branch, we need to compute the convoluted P matrix vector
  # then if there are more than one pieces on this branch, we draw state at each intermediate node
  # then we sample history conditioning on the end state of each piece using uniformization
  
  # fetch the time intervals this branch overlap with
  indices <- 1L
  if (length(Q) > 1) {
    indices <- sapply(node_ages, function(x) findInterval(-x, -Q_ages, left.open = T)) + 1L
    indices <- indices[1]:indices[2]
  }
  
  nstates <- nrow(Q[[indices[1]]])
  
  npieces <- length(indices)
  if (npieces > 1) {
    node_ages <- append(node_ages, Q_ages[indices[-length(indices)]], after = 1)
  }
  t <- abs(diff(node_ages))
  
  # compute P matrix of each piece using matrix exponentiation
  # todo: we can also use uniformization to compute the P matrices, which should be a faster 
  # as we have already done some part of the computation other where in this function
  P <- lapply(1:npieces, function(x) expm::expm(Q[[indices[x]]] * t[x]))
  
  if (npieces > 1) { # when there are more than one intervals overlapping with this branch
    
    # convolved matrix: first element is P along the entire branch
    # second element is P along the branch without the first piece, so on,
    # last element is P along the last two pieces, so number of elements equals npieces - 1
    # for instance, when there are two pieces, this list should only contain one P 
    # as the P along the entire branch (which should = P1 %*% P2 in this case)
    
    # to save computation, we compute the last elements of this list: P_conv[[npieces - 1]] = P[[npieces - 1]] %*% P[[npieces]]
    # then P_conv[[npieces - 2]] = P[[npieces - 2]] %*% P_conv[[npieces - 1]]
    
    P_conv <- vector("list", npieces - 1)
    P_conv[[npieces - 1]] <- P[[npieces - 1]] %*% P[[npieces]]
    
    if (npieces > 2) {
      for (j in (npieces - 2):1) { # loop over the pieces backwards
        P_conv[[j]] <- P[[j]] %*% P_conv[[npieces - 1]]
      }
    }
    
    # loop over the pieces forwards to draw state of each intermediate node 
    # (defined by the overlap of the boundary of time intervals with this branch) 
    # conditioning on the state of start state of the branch (for the first intermediate node) or state of the previous intermediate node
    # and the end state of this branch
    state_idx_end <- node_state_indices[length(node_state_indices)]
    for (j in 1:(npieces - 1)) { 
      
      if (j == npieces - 1) {
        P_complement <- P[[j + 1]]
      } else {
        P_complement <- P_conv[[j + 1]]
      }
      
      state_idx_start <- node_state_indices[j]
      # compute the probability vector of this intermediate node
      state_probs_inter <- numeric(nstates)
      for (k in 1:nstates) {
        # here prob_i,j = P(this piece)[i, k] * P(complement; i.e., along the remaining pieces of the branch)[k, j] / P(along this + remaining pieces)[i, j]
        # but as the denominator is identical to all ks, so we can omit it
        state_probs_inter[k] <- P[[j]][state_idx_start, k] * P_complement[k, state_idx_end]
      }
      # draw a state of this intermediate node according to the probability vector
      state_idx_inter <- sample.int(nstates, 1, prob = state_probs_inter)
      
      node_state_indices <- append(node_state_indices, state_idx_inter, after = j)
    } # end loop to draw state of each intermediate node (now node_state_indices is a vector of state indices 
    # where the ones between the first and last elements correspond to the intermediate nodes)
  }
  
  # loop over the pieces again to simulate history over each piece conditioning on the associated start and end states 
  to_ages <- lapply(1:npieces, function(i) sim_history_piece_conditional(node_ages[c(i, i + 1)], Q, Q_ages, P = P[[i]], node_state_indices[c(i, i + 1)]))
  to <- unlist(lapply(to_ages, function(x) x$to)) # end state of each event over the branch (we don't need to store the corresponding start state as it must be the previous end state)
  age <- unlist(lapply(to_ages, function(x) x$age)) # age of each event over the branch
  
  # format the return as the map object of the simmap data structure
  ages <- c(node_ages[1], age, node_ages[length(node_ages)])
  map <- abs(diff(ages))
  names(map) <- states[c(node_state_indices[1], to)]
  
  return(map)
}

#' Simulate history of discrete-character changes over a duration of time (i.e., a piece of a branch that the character evolved under a constant model)
#' conditioning on the start and end states (implementing algorithm #5 of Hobolth and Stone, 2009)
#' @param node_ages Start and end ages of this duration
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant geographic model) characterizes the CTMC
#' @param Q_ages A vector containing boundaries of time intervals for a piecewise constant geographic model (NULL means a constant model);
#' the length of this vector should be one shorter than the Q-matrix list, sorted from oldest to youngest.
#' @param P The transition probability matrix computed from the Q matrix (if not provided then it will be computed in this function)
#' @param node_state_indices State indices (i.e., the indices to use to fecth the states from the states vector) pf the start and end nodes
#' @return A list containing two vectors, one for the end state of each event over this duration and other for the age of each event
#' @keywords internal
sim_history_piece_conditional <- function(node_ages, Q, Q_ages = NULL, P = NULL, node_state_indices) {
  
  # first find interval
  idx <- 1L
  if (length(Q) > 1) {
    idx <- findInterval(-node_ages[2], -Q_ages, left.open = T) + 1L
  }
  
  nstates <- nrow(Q[[idx]])
  
  # compute the P matrix using matrix exponentiation (if it's not provided)
  t <- abs(diff(node_ages))
  if (is.null(P)) {
    P <- expm::expm(Q[[idx]] * t)
  }
  
  # draw number of changes conditioning on the start and end states
  state_idx_start <- node_state_indices[1]
  state_idx_end <- node_state_indices[length(node_state_indices)]
  n <- draw_nevents_conditional(node_ages, Q, Q_ages, node_state_indices, P[state_idx_start, state_idx_end])
  
  to <- integer()
  age <- numeric()
  
  # for each event, draw the time it occurs and the state the character changes to
  if (n > 1 || (n == 1 && node_state_indices[1] != node_state_indices[2])) {
    
    # simulate event times
    ts <- sort(runif(n)) * t
    
    # simulate change per event
    if (n > 1) { # for the first to the second but last event (when there are more than one events)
      for (i in 1:(n - 1)) { # loop over each event (but cannot be parallelized as the next start state is the previous end state)
        
        state_idx_start <- node_state_indices[i]
        state_probs_inter <- numeric(nstates)
        for (j in 1:nstates) {
          state_probs_inter[j] <- .pkg_env$R[[idx]][[2]][state_idx_start, j] * .pkg_env$R[[idx]][[n - i + 1]][j, state_idx_end] # Remark 7 from Hobolth and Stone, 2009
        }
        state_idx_inter <- sample.int(nstates, 1, prob = state_probs_inter)
        
        node_state_indices <- append(node_state_indices, state_idx_inter, after = i)
        if (state_idx_inter != node_state_indices[i]) {
          to <- c(to, state_idx_inter)
          age <- c(age, node_ages[1] - ts[i])
        }
      }
    }
    
    if (node_state_indices[n] != node_state_indices[n + 1]) { # for the last event (if there is any)
      to <- c(to, node_state_indices[n + 1])
      age <- c(age, node_ages[1] - ts[n])
    }
  }
  
  return(list(to = to, age = age))
}

#' Compute the uniformized matrix (R) of Q and its exponentiations (R^2, R^3, ..., R^n)
#' Here the vector of uniformized matrices in stored globally so we check if the provided n (number of events)
#' exceeds the maximum exponent minus 1 (as the first element is an identity matrix)
#' and only append the additional exponentiation(s) when it exceeds
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant geographic model) characterizes the CTMC
#' @param n Number of events (which is also the largest exponent to compute in this step)
#' @param indices Which interval we need to update (if not provided then update all)
#' 
#' @keywords internal
fill_DTMCmats <- function(Q, n, indices = NULL) {
  
  nstates <- nrow(Q[[1]])
  R_this <- .pkg_env$R
  
  if (is.null(indices)) {
    indices <- seq_along(.pkg_env$R)
  }
  
  for (i in indices) {
    if (length(R_this[[i]]) <= n) {
      
      for (j in length(R_this[[i]]):n) {
        if (j == 0) { # the first element of the vector is an identity matrix as R^0 = I
          R_this[[i]] <- list(diag(nrow = nstates, ncol = nstates))
        } else if (j == 1) {
          
          R_tmp <- Q[[i]] / .pkg_env$mu[i] + R_this[[i]][[1]]
          R_this[[i]] <- c(R_this[[i]], list(R_tmp))
          
        } else {
          R_this[[i]] <- c(R_this[[i]], list(R_this[[i]][[j]] %*% R_this[[i]][[2]]))
        }
      }
      
    }
  }
  
  # R is a global variable that is a list (each element corresponds to an interval) of 
  # list (each element corresponds to an exponents) of uniformized matrices
  assign("R", R_this, envir = .pkg_env)
}

#' Draw the number of events conditioning on the start and end state given the underlying CTMC 
#' (implementing equation 2.9 from Hobolth and Stone, 2009)
#' @param node_ages A vector containing the start (first element) and end ages (second element) of this duration
#' @param Q An instantaneous-rate matrix (or a list of matrices for a piecewise constant discrete-geographic model) characterizes the CTMC
#' @param Q_ages A vector containing boundaries of time intervals for a piecewise constant geographic model (NULL means a constant model);
#' the length of this vector should be one shorter than the Q-matrix list, sorted from oldest to youngest.
#' @param node_state_indices State indices (i.e., the indices to use to fecth the states from the states vector) pf the start and end nodes
#' @param Pab The transition probability between the start and end states
#' @return An integer that is the sampled number of events
#' 
#' @keywords internal
draw_nevents_conditional <- function(node_ages, Q, Q_ages = NULL, node_state_indices, Pab) {
  
  # first find the time interval we are at
  idx <- 1L
  if (length(Q) > 1) {
    idx <- findInterval(-node_ages[2], -Q_ages, left.open = T) + 1L
  }
  
  t <- abs(diff(node_ages))
  
  n <- 0L
  u <- runif(1)
  cum_prob <- 0
  
  repeat {
    fill_DTMCmats(Q, n, indices = idx)
    cum_prob <- cum_prob + dpois(n, .pkg_env$mu[idx] * t) * .pkg_env$R[[idx]][[n + 1]][node_state_indices[1], node_state_indices[2]] / Pab
    if (cum_prob >= u) break
    n <- n + 1L
  }
  
  return(n)
}

