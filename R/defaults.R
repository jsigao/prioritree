
.pkg_env = new.env(parent = emptyenv())

assign("tex_helprs", list(section_wrapper = list(c("<h5>", "</h5>"), c("\\section*{", "}\n"), c("#### ", "\n")),
                          subsection_wrapper = list(c("<h6><i><b>", "</b></i></h6>"), c("\\subsection*{", "}\n"), c("##### ", "\n")),
                          section_ending = c("<br>", "\n", "\n"),
                          italic_wrapper = list(c("<em>", "</em>"), c("\\textit{", "}"), c("*", "*")),
                          texttt_wrapper = list(c("<tt>", "</tt>"), c("\\texttt{", "}"), c("`", "`")),
                          equation_wrapper = list(c("\\begin{align*}\n", "\\end{align*}\n"), c("\\begin{align*}\n", "\\end{align*}\n"), c("\\[", "\\]")),
                          citep_wrapper = list(c("[@", "]"), c("\\citep{", "}"), c("[@", "]")),
                          citet_wrapper = list(c("@", ""), c("\\citet{", "}"), c("@", "")),
                          cite_sep = c("; @", ",", "; @"),
                          all_format = c("HTML", "Latex", "Markdown")), envir = .pkg_env)

# customize colors
assign("mycolorpalette", c("#46a2f2", "#ff7f00", RColorBrewer::brewer.pal(9, "Set1")[c(1, 3:4, 6:8)], 
                           RColorBrewer::brewer.pal(7, "Dark2")[c(1:4, 6:7)], "darkgreen", "darkred", "darkcyan", "wheat4", 
                           "darksalmon", "#5874c6", "#d3436c", "#457956", "#d54c35", "orange3", "blue4"), envir = .pkg_env)

assign("inputdefault_init", list(model_symmetry = "symmetric", with_bssvs = T, empiricaltree_mh = "Metropolis-Hastings algorithm (recommended)",
                                 delta_prior = "Poisson", poisson_default = F, poisson_lambda = 0.693,
                                 mu_prior = "Hierarchical Exponential", alphaofgamma_mumean = 0.5,
                                 proposalweight_r = 10, proposalweight_delta = 30, proposalweight_pdeltaij = 5, proposalweight_rootfreq = 1,
                                 proposalweight_mu = 8, proposalweight_mumean = 4, proposalweight_tree = 30,
                                 mcmc_chainlength = 50000000, mcmc_samplingfreq = 10000, mcmc_numreplicates = 2), envir = .pkg_env)