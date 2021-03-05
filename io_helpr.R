
library(ape)
library(phangorn)

readtreefile_linebyline <- function(filepath, earlystop = T) {
  con = file(filepath, "r")
  lines <- NULL
  while ( T ) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    } else if (earlystop && grepl("&", line) && (length(lines) > 1 && any(grepl("&", lines)))) {
      lines <- c(lines, line)
      break
    }
    
    lines <- c(lines, line)
  }
  
  close(con)
  return(lines)
}


parsimonyscore_treebytree <- function(filepath, tree = NULL, tipstates, states, firsttree_only = T) {
  
  if (firsttree_only && (!is.null(tree))) {
    parsimony_score <- phangorn::parsimony(tree = tree, data = phyDat(t(t(tipstates)), type = "USER", levels = states), method = "fitch")
  } else if (!firsttree_only) {
    con = file(filepath, "r")
    lines <- NULL
    parsimony_score <- integer()
    while ( T ) {
      line <- readLines(con, n = 1)
      if (length(line) == 0 || grepl("End;", line)) {
        break
      } else if (!grepl("&", line)) {
        lines <- c(lines, line)
      } else if (grepl("&", line)) {
        tmptree_path <- tempfile(fileext = ".tree")
        cat(c(lines, line, "End;"), file = tmptree_path, sep = "\n")
        tree <- ape::read.nexus(tmptree_path)
        
        parsimony_score <- c(parsimony_score, phangorn::parsimony(tree = tree, data = phyDat(t(t(tipstates)), type = "USER", levels = states), method = "fitch"))
      }
    }
    
    close(con)
  }
  
  return(ceiling(mean(parsimony_score)))
}


getDifferedLines <- function(last_txt = NULL, current_txt = NULL, file_ext = ".txt") {
  
  dataline_str <- ""
  
  if (!is.null(last_txt)) {
    lastxml_tmppath <- tempfile(fileext = file_ext)
    currentxml_tmppath <- tempfile(fileext = file_ext)
    
    writeLines(last_txt, lastxml_tmppath)
    writeLines(current_txt, currentxml_tmppath)
    
    changedoradded_linenum <- as.integer(unlist(strsplit(system(paste("diff -a --unchanged-line-format='' --old-line-format='' --new-line-format='%dn '", 
                                                                      lastxml_tmppath, currentxml_tmppath), intern = T, ignore.stderr = T), " ")))
    # if ((length(changedoradded_linenum) > 0) && (length(unlist(strsplit(current_txt, "\n"))) != length(changedoradded_linenum))) {
    #   dataline_str <- paste0("data-line='", paste(changedoradded_linenum, collapse = ", "), "'")
    # }
    if ((length(changedoradded_linenum) > 0)) {
      dataline_str <- paste0("data-line='", paste(changedoradded_linenum, collapse = ", "), "'")
    }
  }
  
  return(dataline_str)
}

