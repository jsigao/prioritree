library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyjqui)
library(shinythemes)
library(scales)
library(shinyFiles)
library(rmutil)
library(markdown)
library(rmarkdown)
library(DT)
source("io_helpr.R")
source("xml_helpr.R")
source("tex_helpr.R")
source("plot_helpr.R")
source("posteriorpredictive_helpr.R")
file.copy("literature.bib", paste0(tempdir(), "/literature.bib"))

options(warn = -1)
options(shiny.maxRequestSize = 16384*1024^2)
options(shiny.reactlog = T)

inputdefault_init <- list(model_symmetry = "symmetric", with_bssvs = T, empiricaltree_mh = "Metropolis–Hastings algorithm (recommended)",
                          delta_prior = "Poisson", poisson_default = F, poisson_lambda = 0.693,
                          mu_prior = "Hierarchical Exponential", alphaofgamma_mumean = 0.5,
                          proposalweight_r = 10, proposalweight_delta = 30, proposalweight_pdeltaij = 5, proposalweight_rootfreq = 1,
                          proposalweight_mu = 8, proposalweight_mumean = 4, proposalweight_tree = 30,
                          mcmc_chainlength = 50000000, mcmc_samplingfreq = 10000, mcmc_numreplicates = 2)


prismDependencies <- tags$head(
  tags$script(src = "prism.js"),
  tags$link(rel = "stylesheet", type = "text/css", href = "prism-okaidia.css")
)


# Define UI for app
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(".divdisabled {pointer-events: none; opacity: 0.4;}"),
  shinyjs::inlineCSS(".disappeared {visibility:hidden;}"),
  # shinyjs::inlineCSS(".nav-tabs li.fixedwidth{width:200%;}"),
  # shinyjs::inlineCSS(".tabformat {padding-right: 30px}"),
  prismDependencies,
  # tags$script(HTML('Shiny.onInputChange("priorspecification_class", document.getElementById("prior_specification").className);')),
  tags$style(type = 'text/css', ".nav-tabs {font-size: 90%;}"),
  tags$style(type = 'text/css', ".nav-pills {font-size: 85%;}"),
  # tags$style(type = 'text/css', ".panel {border-width: 10;}"),
  
  # App title ----
  # titlePanel("PrioriTree"),
  
  tags$script(HTML("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});"), type = "text/x-mathjax-config"),
  
  navbarPage(title = "PrioriTree", id = "main_menu",
             
             tabPanel(title = "Analysis Setup", value = "analysis_setup",
                      fluidRow(
                        column(width = 4, style = "padding-left: 10px; padding-right: 10px;",
                               wellPanel(style = "background-color: #F2F3F4;",
                                         
                                         navlistPanel(id = "main_tabs", well = F, widths = c(4, 8), 
                                                      
                                                      tabPanel(title = "Step 1: Import Data", value = "data_input",
                                                               
                                                               tabsetPanel(type = "tabs", id = "datainput_tabs",
                                                                           # upload the discrete-trait data
                                                                           tabPanel(title = "Discrete-Geography File", value = "trait",
                                                                                    h6(""),
                                                                                    checkboxInput(inputId = "geography_file_defaultupload", label = "Load example discrete-geography file", value = F),
                                                                                    
                                                                                    div(id = "geography_file_div",
                                                                                        fileInput(inputId = "geography_file", label = "Choose discrete-geography file", 
                                                                                                  multiple = F, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt"))
                                                                                    ),
                                                                                    
                                                                                    div(id = "geographyfile_attributes",
                                                                                        checkboxInput(inputId = "geographyfile_header", label = "File header", value = T),
                                                                                        selectInput(inputId = "taxoncolumn_name", label = "Taxon column name", choices = "", selectize = F),
                                                                                        selectInput(inputId = "traitcolumn_name", label = "Discrete-geography column name", choices = "", selectize = F)
                                                                                        # uiOutput("columnname_ui")
                                                                                    )
                                                                           ),
                                                                           
                                                                           # upload either a posterior distribution of trees or a single (MCC) tree
                                                                           tabPanel(title = "Tree(s) File", value = "tree",
                                                                                    h6(""),
                                                                                    checkboxInput(inputId = "tree_file_defaultupload", label = "Load example tree(s) file", value = F),
                                                                                    
                                                                                    div(id = "tree_num_defaultupload_div",
                                                                                        radioButtons(inputId = "tree_num_defaultupload", label = "Type of example tree to load", 
                                                                                                     choices = c("distribution (multiple)", "single"), inline = T),
                                                                                    ),
                                                                                    
                                                                                    div(id = "tree_file_div",
                                                                                        fileInput("tree_file", "Choose tree(s) file", multiple = F, accept = c(".tree", ".trees", ".tre"))
                                                                                    )
                                                                           )
                                                                           
                                                               )
                                                      ),
                                                      
                                                      tabPanel(title = "Step 2: Specify Model", value = "model_specification",
                                                               
                                                               tabsetPanel(type = "tabs", 
                                                                           tabPanel(title = "Geographic Model",
                                                                                    h6(""),
                                                                                    radioButtons(inputId = "model_symmetry", label = "Rate-Matrix Symmetry", choices = c("symmetric", "asymmetric"), 
                                                                                                 selected = inputdefault_init$model_symmetry, inline = T),
                                                                                    
                                                                                    h3(""),
                                                                                    checkboxInput(inputId = "with_bssvs", label = "Infer dispersal routes using BSSVS", value = inputdefault_init$with_bssvs)),
                                                                           tabPanel(title = "Tree Model",
                                                                                    h6(""),
                                                                                    uiOutput("treemodel_ui")
                                                                           )
                                                               ),
                                                               
                                                               h4(""),
                                                               actionButton(inputId = "enablestep3_button", label = "Proceed to next step")
                                                      ),
                                                      
                                                      tabPanel(title = "Step 3: Configure Analysis (Basic)", value = "analysis_configBasic",
                                                               
                                                               tabsetPanel(type = "tabs", id = "analysisconfigBasic_tabs",
                                                                           
                                                                           tabPanel(title = "MCMC Sampling", value = "mcmcsampling",
                                                                                    h6(""),
                                                                                    numericInput(inputId = "mcmc_chainlength", label = "MCMC chain length", 
                                                                                                 value = inputdefault_init$mcmc_chainlength, min = 1),
                                                                                    
                                                                                    h5(""),
                                                                                    numericInput(inputId = "mcmc_samplingfreq", label = "Sampling frequency of the MCMC chain", 
                                                                                                 value = inputdefault_init$mcmc_samplingfreq, min = 1),
                                                                                    
                                                                                    h4(""),
                                                                                    numericInput(inputId = "mcmc_numreplicates", label = "Number of analysis replicates", 
                                                                                                 value = inputdefault_init$mcmc_numreplicates, min = 1)
                                                                           ),
                                                                           
                                                                           tabPanel(title = "Proposal Weights", value = "proposalweights",
                                                                                    h6(""),
                                                                                    uiOutput("proposalweight_ui")
                                                                           )
                                                               ),
                                                               
                                                               h4(""),
                                                               actionButton(inputId = "enablestep4_button", label = "Proceed to next step")
                                                      ),
                                                      
                                                      tabPanel(title = "Step 4: Configure Analysis (Advanced)", value = "analysis_configAdvanced",
                                                               
                                                               tabsetPanel(type = "tabs", id = "analysisconfigAdvanced_tabs",
                                                                           
                                                                           tabPanel(title = "Summary Statistics",
                                                                                    h6(""),
                                                                                    selectInput(inputId = "do_stochasticmapping", label = "Estimate number of dispersal events using",
                                                                                                choices = c(Choose = '', "Fast stochastic mapping (incomplete history, simulation-free)", 
                                                                                                            "Stochastic mapping (complete history, simulation-based)"), 
                                                                                                selected = "", multiple = F, selectize = F),
                                                                                    div(id = "markovjumps_totalpairwise", style = "margin-left:15px",
                                                                                        checkboxInput(inputId = "markovjumps_total", label = "overall number of dispersal events", value = T),
                                                                                        checkboxInput(inputId = "markovjumps_pairwise", label = "number of dispersal events between each pair of areas", value = T)
                                                                                    )
                                                                           ),
                                                                           tabPanel(title = "Model Exploration",
                                                                                    h6(""),
                                                                                    selectInput(inputId = "further_analysis", label = "Setting up further analysis for model exploration", 
                                                                                                choices = c(Choose = '', "Marginal likehood estimation", "Under prior", "Data cloning"), 
                                                                                                multiple = F, selectize = F),
                                                                                    uiOutput("furtheranalysis_ui")
                                                                           )
                                                               ),
                                                               
                                                               h4(""),
                                                               actionButton(inputId = "enablestep5_button", label = "Proceed to next step")
                                                      ),
                                                      
                                                      tabPanel(title = "Step 5: Download BEAST XML and Methods Template", value = "src_export",
                                                               
                                                               tabsetPanel(type = "tabs", id = "download_tabs",
                                                                           
                                                                           tabPanel(title = "BEAST XML", style = "font-size:90%;",
                                                                                    
                                                                                    h6(''),
                                                                                    textInput(inputId = "xml_name", label = "Name of the XML file (without .xml as it will be appended automatically)", value = "geo"),
                                                                                    actionButton(inputId = "downloadxml_fake", label = "Download", icon = icon("download")),
                                                                                    downloadButton(outputId = "downloadxml_real", label = "Download")
                                                                           ),
                                                                           
                                                                           tabPanel(title = "Methods Template", style = "font-size:90%;",
                                                                                    
                                                                                    h6(''),
                                                                                    radioButtons("downloadmethods_format", label = "Format of the Methods template file", choices = c("Word", "LaTex", "PDF", "HTML", "Markdown"), selected = "Word", inline = T),
                                                                                    textInput(inputId = "methods_name", label = "Name of the Methods template file (without filename extension, e.g., '.doc', as it will be appended automatically)", value = "methods"),
                                                                                    actionButton(inputId = "downloadmethods_fake", label = "Download", icon = icon("download")),
                                                                                    downloadButton(outputId = "downloadmethods_real", label = "Download"))
                                                               )
                                                      )
                                                      
                                         )
                               )
                               
                        ),
                        
                        column(width = 8, offset = 0, style = "padding: 0px 15px 0px 15px; margin: 0%;",
                               
                               fluidRow(id = "analysis_specification_prior", style = "padding: 2px; margin: 0%;",
                                        
                                        HTML('<div id = "prior_specification_panel" class="panel-heading" style = "background: none; border: 0;"><h4 class="panel-title"><a data-toggle="collapse" href="#prior_specification"><span class="glyphicon glyphicon-chevron-down" aria-hidden="true"></span>Prior Specification</a></h4></div>'),
                                        div(id = 'prior_specification', class = "panel-collapse collapse", 
                                            column(width = 3, offset = 0, style = "padding: 7px 10px 0px 10px; margin: 0px 5px 0px 0px; background-color: #D6EAF8;",
                                                   tabsetPanel(type = "tabs", id = "prior_tabs",
                                                               
                                                               tabPanel(title = HTML("Number of Dispersal Routes, &Delta;"), value = "delta",
                                                                        h6(""),
                                                                        
                                                                        selectInput(inputId = "delta_prior", label = HTML("Prior on &Delta;"), 
                                                                                    choices = c("Poisson", "Beta-Binomial", "Uniform"),
                                                                                    multiple = F, selectize = T),
                                                                        uiOutput("deltaprior_ui")),
                                                               tabPanel(title = HTML("Average Dispersal Rate, &mu;"), value = "mu", 
                                                                        h6(""),
                                                                        
                                                                        selectInput(inputId = "mu_prior", label = HTML("Prior on &mu;"), 
                                                                                    choices = c("Hierarchical Exponential", "CTMC rate-ref (BEAST default)", "Empirical-Informed Exponential"),
                                                                                    multiple = F, selectize = T),
                                                                        uiOutput("muprior_ui"),
                                                                        checkboxInput(inputId = "parsimonyscore_alltree", label = "Recompute parsimony score using all trees (to make the app more efficient, the default value is computed using the first two trees)", value = F)
                                                               )
                                                               
                                                   )
                                            ),
                                            
                                            column(width = 4, offset = 0, style = "padding: 7px 10px 0px 10px; margin: 0px 12px 0px 5px; background-color: #F4F6F6;",
                                                   tabsetPanel(type = "tabs", id = "priornote_tabs",
                                                               
                                                               tabPanel(title = "Brief Description", value = "text",
                                                                        h6(''),
                                                                        uiOutput(outputId = "priordescription_ui")
                                                               ),
                                                               
                                                               tabPanel(title = "Math Notation", value = "math",
                                                                        h6(''),
                                                                        uiOutput(outputId = "priornotation_ui")
                                                               )
                                                               
                                                               # tabPanel(title = "Graphical Model", value = "graph")
                                                   )
                                            ),
                                            
                                            column(width = 4, offset = 0, style = "padding: 0.2px; margin: 0%; background-color: #ffffff;",
                                                   uiOutput(outputId = "priorplot_ui")
                                            )
                                        )
                                        
                               ),
                               
                               div(id = "methods_template_div",
                                   h3(""),
                                   
                                   HTML('<div id = "methods_template_panel" class = "panel-heading"><h4 class="panel-title"><a data-toggle="collapse" href="#methods_template"><span class="glyphicon glyphicon-chevron-down" aria-hidden="true"></span>Methods Template Viewer</a></h4></div>'),
                                   div(id = 'methods_template', class = "panel-collapse collapse",
                                       tabsetPanel(type = "tabs", id = "methods_tabs",
                                                   
                                                   tabPanel(title = "Data", value = "data",
                                                            h6(""),
                                                            fluidRow(style = "height:200px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;",
                                                                     uiOutput("methods_data")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Model", value = "model",
                                                            h6(""),
                                                            fluidRow(style = "height:200px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;",
                                                                     uiOutput("methods_model")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Bayesian Inference (Prior)", value = "prior",
                                                            h6(""),
                                                            fluidRow(style = "height:200px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;",
                                                                     uiOutput("methods_prior")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Analysis", value = "analysis",
                                                            h6(""),
                                                            fluidRow(style = "height:200px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;",
                                                                     uiOutput("methods_analysis")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Full", value = "full",
                                                            h6(""),
                                                            fluidRow(style = "height:200px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;",
                                                                     uiOutput("methods_full")
                                                            )
                                                   )
                                       )
                                       
                                   )
                               ),
                               
                               div(id = "xml_viewer_div",
                                   h3(""),
                                   
                                   HTML('<div id = "xml_viewer_panel" class = "panel-heading"><h4 class="panel-title"><a data-toggle="collapse" href="#xml_viewer"><span class="glyphicon glyphicon-chevron-down" aria-hidden="true"></span>BEAST XML Viewer</a></h4></div>'),
                                   div(id = 'xml_viewer', class = "panel-collapse collapse",
                                       
                                       tabsetPanel(type = "tabs", id = "xmlviewer_tabs",
                                                   
                                                   tabPanel(title = "Data", value = "xmldata",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_data")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Model and Prior", value = "xmlmodelprior",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_modelprior")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "MCMC Sampling", value = "xmlmcmc",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_mcmc")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Proposal", value = "xmlproposal",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_proposal")
                                                            )
                                                   ),

                                                   tabPanel(title = "Phylogenetic Likelihood and Stochastic Mapping", value = "xmlphyloctmc",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_phyloctmc")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Marginal Likelihood Estimation", value = "xmlpowerposterior",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_powerposterior")
                                                            )
                                                   ),
                                                   
                                                   tabPanel(title = "Full", value = "xmlfull",
                                                            h6(""),
                                                            fluidRow(style = "height: 200px; overflow: auto;",
                                                                     uiOutput("beastxml_full")
                                                            )
                                                   )
                                       )
                                       
                                   )
                               )
                        )
                      )
             ),
             
             navbarMenu(title = "Model-Exploration Post-Processing", menuName = "post_processing",
                        
                        tabPanel(title = "Data Cloning", value = "focalparam_summary",
                                 fluidRow(
                                   column(width = 4, style = "padding-left: 10px; padding-right: 10px; overflow-y:auto; max-height: 800px;",
                                          
                                          wellPanel(style = "background-color: #F2F3F4;",
                                                    
                                                    navlistPanel(id = "focalparamsummary_tabs", well = F, widths = c(4, 8), 
                                                                
                                                                tabPanel(title = "Step 1: Upload Log File(s)", value = "log_input",
                                                                         h6(""),
                                                                         
                                                                         checkboxInput(inputId = "focalparamsummary_logfile_defaultupload", label = "Load example log files", value = F),
                                                                         
                                                                         div(id = paste0("focalparamsummary_logfile_div", 1),
                                                                             fileInput(inputId = paste0("focalparamsummary_logfile", 1), 
                                                                                       label = paste0("Estimate log file(s) under prior No. ", 1), 
                                                                                       multiple = T, accept = ".log")
                                                                         ),
                                                                         
                                                                         div(id = "focalparamsummary_loginput_addremove_div",
                                                                             p("Add or remove prior model", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                             actionButton(inputId = "focalparamsummary_loginput_add", label = "", icon = icon("plus", lib = "glyphicon")),
                                                                             actionButton(inputId = "focalparamsummary_loginput_remove", label = "", icon = icon("minus", lib = "glyphicon"))
                                                                         ),

                                                                         uiOutput(outputId = "focalparamsummary_paramname_ui"),
                                                                         actionButton(inputId = "focalparamsummary_startprocessing", label = "Read in the log files and start initial processing")
                                                                ),
                                                                
                                                                tabPanel(title = "Step 2: Configure Post-Processing Settings", value = "processing_settings",
                                                                         
                                                                         tabsetPanel(type = "tabs", id = "focalparamsummary_processingsettings_tabs",
                                                                                     
                                                                                     tabPanel(title = "Post-processing settings", value = "log_processing",
                                                                                              h6(""),
                                                                                              
                                                                                              checkboxInput(inputId = "focalparamsummary_logcombinereplicates", label = "Combine replicates for all log files", value = F),
                                                                                              
                                                                                              uiOutput(outputId = "focalparamsummary_logprocessing_ui")
                                                                                     ),
                                                                                     
                                                                                     tabPanel(title = "Edit figure or table", value = "figuresettings_specific",
                                                                                              
                                                                                              h6(''),
                                                                                              uiOutput(outputId = "focalparamsummary_priormodelname_ui"),
                                                                                              
                                                                                              HTML('<div id = "focalparamsummary_par_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparamsummary_par_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Margin edits</a></p></div>'),
                                                                                              div(id = 'focalparamsummary_par_edits', class = "panel-collapse collapse in",
                                                                                                  
                                                                                                  numericInput(inputId = "focalparamsummary_plot_margin_bottom", label = "Bottom margin (in inches) of the plot", value = 0.4, min = 0),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_margin_left", label = "Left margin  (in inches) of the plot", value = 0.45, min = 0),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_margin_top", label = "Top margin (in inches) of the plot", value = 0.15, min = 0),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_margin_right", label = "Right margin (in inches) of the plot", value = 0, min = 0)
                                                                                              ),
 
                                                                                              HTML('<div id = "focalparamsummary_xaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparamsummary_xaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>X-axis edits</a></p></div>'),
                                                                                              div(id = 'focalparamsummary_xaxis_edits', class = "panel-collapse collapse in",
                                                                                                  
                                                                                                  numericInput(inputId = "focalparamsummary_plot_xaxis_lab_cex", label = "Cex of prior model name label", value = 1.35, min = 0.1),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_xaxis_lab_line", label = "Line of prior model name label", value = -2),
                                                                                                  
                                                                                                  textInput(inputId = "focalparamsummary_plot_x_lab", label = "X-axis name", value = "number of data clones"),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_x_lab_cex", label = "Cex of X-axis name", value = 1.6, min = 0.1),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_x_lab_line", label = "Line of X-axis name", value = 0.75),
                                                                                                  
                                                                                                  numericInput(inputId = "focalparamsummary_plot_xaxis_boxgroup_labcex", label = "Cex of clone number label", value = 1.1, min = 0.1),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_xaxis_boxgroup_labline", label = "Line of clone number label", value = -2)

                                                                                              ),
                                                                                              
                                                                                              HTML('<div id = "focalparamsummary_yaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparamsummary_yaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Y-axis edits</a></p></div>'),
                                                                                              div(id = 'focalparamsummary_yaxis_edits', class = "panel-collapse collapse in",
                                                                                                  
                                                                                                  textInput(inputId = "focalparamsummary_plot_y_lab", label = "Y-axis name", value = ""),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_y_lab_cex", label = "Cex of Y-axis name", value = 1.6, min = 0.1),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_y_lab_line", label = "Line of Y-axis name", value = 0.9),
                                                                                                  
                                                                                                  checkboxInput(inputId = "focalparamsummary_plot_yaxis_log", label = "Y-axis on the log scale", value = F),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_yaxis_lab_cex", label = "Cex of Y-axis label", value = 0.75, min = 0.1),
                                                                                                  numericInput(inputId = "focalparamsummary_plot_yaxis_lab_line", label = "Line of Y-axis label", value = -1.5)
                                                                                              )
                                                                                     )

                                                                         )
                                                                         
                                                                ),
                                                                
                                                                tabPanel(title = "Step 3: Download Output", value = "download_output",
                                                                         h6(''),
                                                                         p("Download figure", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                         radioButtons("focalparamsummary_plot_downloadformat", label = "Format of the figure", choices = c("PDF", "EPS", "PNG", "JPEG", "TIFF"), selected = "PDF", inline = T),
                                                                         textInput(inputId = "focalparamsummary_plot_downloadname", label = "Name of the figure (without filename extension, e.g., '.pdf', as it will be appended automatically)", value = "figure"),
                                                                         downloadButton(outputId = "focalparamsummary_plot_download", label = "Download"),
                                                                         
                                                                         h4(''),
                                                                         p("Download table", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                         radioButtons("focalparamsummary_table_downloadformat", label = "Format of the table", choices = c("TSV", "CSV"), selected = "TSV", inline = T),
                                                                         textInput(inputId = "focalparamsummary_table_downloadname", label = "Name of the table (without filename extension, e.g., '.tsv', as it will be appended automatically)", value = "table"),
                                                                         downloadButton(outputId = "focalparamsummary_table_download", label = "Download")
                                                                )
                                                                
                                                    )
                                          )
                                   ),
                                   
                                   column(id = "focalparamsummary_result_column", width = 8, offset = 0, 
                                          style = "padding: 0px 15px 0px 15px; margin: 0%;", 
                                          
                                          fluidRow(id = "focalparamsummary_result_div",
                                                   
                                                   tabsetPanel(type = "tabs", id = "focalparamsummary_result_tabs",
                                                               
                                                               tabPanel(title = "Figure", value = "figure",
                                                                        plotOutput(outputId = "focalparamsummary_plot", height = "auto")
                                                               ),
                                                               
                                                               tabPanel(title = "Table", value = "table",
                                                                        h6(''),
                                                                        DT::dataTableOutput(outputId = "focalparamsummary_table")
                                                                        
                                                               )
                                                               
                                                   )
                                          )
                                   )
                              )
                        ),
                        
                        
                        tabPanel(title = "Robust Bayesian", value = "focalparam2_summary",
                                 fluidRow(
                                   column(width = 4, style = "padding-left: 10px; padding-right: 10px; overflow-y:auto; max-height: 800px;",
                                          
                                          wellPanel(style = "background-color: #F2F3F4;",
                                                    
                                                    navlistPanel(id = "focalparam2summary_tabs", well = F, widths = c(4, 8), 
                                                                 
                                                                 tabPanel(title = "Step 1: Upload Log File(s)", value = "log_input",
                                                                          h6(""),
                                                                          
                                                                          checkboxInput(inputId = "focalparam2summary_logfile_defaultupload", label = "Load example log files", value = F),
                                                                          
                                                                          div(id = paste0("focalparam2summary_logfile_div", 1),
                                                                              fileInput(inputId = paste0("focalparam2summary_logfile", 1), 
                                                                                        label = paste0("Estimate log file(s) under prior No. ", 1), 
                                                                                        multiple = T, accept = ".log")
                                                                          ),
                                                                          
                                                                          div(id = "focalparam2summary_loginput_addremove_div",
                                                                              p("Add or remove prior model", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                              actionButton(inputId = "focalparam2summary_loginput_add", label = "", icon = icon("plus", lib = "glyphicon")),
                                                                              actionButton(inputId = "focalparam2summary_loginput_remove", label = "", icon = icon("minus", lib = "glyphicon"))
                                                                          ),

                                                                          uiOutput(outputId = "focalparam2summary_paramname_ui"),
                                                                          actionButton(inputId = "focalparam2summary_startprocessing", label = "Read in the log files and start initial processing")
                                                                 ),
                                                                 
                                                                 tabPanel(title = "Step 2: Configure Post-Processing Settings", value = "processing_settings",
                                                                          
                                                                          tabsetPanel(type = "tabs", id = "focalparam2summary_processingsettings_tabs",
                                                                                      
                                                                                      tabPanel(title = "Post-processing settings", value = "log_processing",
                                                                                               h6(""),
                                                                                               
                                                                                               checkboxInput(inputId = "focalparam2summary_logcombinereplicates", label = "Combine replicates for all log files", value = F),
                                                                                               
                                                                                               uiOutput(outputId = "focalparam2summary_logprocessing_ui")
                                                                                      ),
                                                                                      
                                                                                      tabPanel(title = "Edit figure or table", value = "figuresettings_specific",
                                                                                               
                                                                                               h6(''),
                                                                                               uiOutput(outputId = "focalparam2summary_priormodelname_ui"),
                                                                                               
                                                                                               HTML('<div id = "focalparam2summary_par_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparam2summary_par_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Margin edits</a></p></div>'),
                                                                                               div(id = 'focalparam2summary_par_edits', class = "panel-collapse collapse in",
                                                                                                   
                                                                                                   numericInput(inputId = "focalparam2summary_plot_margin_bottom", label = "Bottom margin (in inches) of the plot", value = 0.25, min = 0),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_margin_left", label = "Left margin  (in inches) of the plot", value = 0.45, min = 0),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_margin_top", label = "Top margin (in inches) of the plot", value = 0.15, min = 0),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_margin_right", label = "Right margin (in inches) of the plot", value = 0, min = 0)
                                                                                               ),
                                                                                               
                                                                                               HTML('<div id = "focalparam2summary_xaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparam2summary_xaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>X-axis edits</a></p></div>'),
                                                                                               div(id = 'focalparam2summary_xaxis_edits', class = "panel-collapse collapse in",

                                                                                                   numericInput(inputId = "focalparam2summary_plot_xaxis_lab_cex", label = "Cex of prior model name label", value = 1.35, min = 0.1),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_xaxis_lab_line", label = "Line of prior model name label", value = -2),
                                                                                                   
                                                                                                   numericInput(inputId = "focalparam2summary_plot_xaxis_boxgroup_labcex", label = "Cex of prior or posterior label", value = 1.1, min = 0.1),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_xaxis_boxgroup_labline", label = "Line of prior or posterior label", value = -2)
                                                                                                   
                                                                                               ),
                                                                                               
                                                                                               HTML('<div id = "focalparam2summary_yaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparam2summary_yaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Y-axis edits</a></p></div>'),
                                                                                               div(id = 'focalparam2summary_yaxis_edits', class = "panel-collapse collapse in",
                                                                                                   
                                                                                                   textInput(inputId = "focalparam2summary_plot_y_lab", label = "Y-axis name", value = ""),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_y_lab_cex", label = "Cex of Y-axis name", value = 1.6, min = 0.1),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_y_lab_line", label = "Line of Y-axis name", value = 0.9),
                                                                                                   
                                                                                                   checkboxInput(inputId = "focalparam2summary_plot_yaxis_log", label = "Y-axis on the log scale", value = F),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_yaxis_lab_cex", label = "Cex of Y-axis label", value = 0.75, min = 0.1),
                                                                                                   numericInput(inputId = "focalparam2summary_plot_yaxis_lab_line", label = "Line of Y-axis label", value = -1.5)
                                                                                               )
                                                                                      )
                                                                          )
                                                                          
                                                                 ),
                                                                 
                                                                 tabPanel(title = "Step 3: Download Output", value = "download_output",
                                                                          h6(''),
                                                                          p("Download figure", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                          radioButtons("focalparam2summary_plot_downloadformat", label = "Format of the figure", choices = c("PDF", "EPS", "PNG", "JPEG", "TIFF"), selected = "PDF", inline = T),
                                                                          textInput(inputId = "focalparam2summary_plot_downloadname", label = "Name of the figure (without filename extension, e.g., '.pdf', as it will be appended automatically)", value = "figure"),
                                                                          downloadButton(outputId = "focalparam2summary_plot_download", label = "Download"),
                                                                          
                                                                          h4(''),
                                                                          p("Download table", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                          radioButtons("focalparam2summary_table_downloadformat", label = "Format of the table", choices = c("TSV", "CSV"), selected = "TSV", inline = T),
                                                                          textInput(inputId = "focalparam2summary_table_downloadname", label = "Name of the table (without filename extension, e.g., '.tsv', as it will be appended automatically)", value = "table"),
                                                                          downloadButton(outputId = "focalparam2summary_table_download", label = "Download")
                                                                 )
                                                                 
                                                    )
                                          )
                                   ),
                                   
                                   column(id = "focalparam2summary_result_column", width = 8, offset = 0, 
                                          style = "padding: 0px 15px 0px 15px; margin: 0%;", 
                                          
                                          fluidRow(id = "focalparam2summary_result_div",
                                                   
                                                   tabsetPanel(type = "tabs", id = "focalparam2summary_result_tabs",
                                                               
                                                               tabPanel(title = "Figure", value = "figure",
                                                                        plotOutput(outputId = "focalparam2summary_plot", height = "auto")
                                                               ),
                                                               
                                                               tabPanel(title = "Table", value = "table",
                                                                        h6(''),
                                                                        DT::dataTableOutput(outputId = "focalparam2summary_table")
                                                                        
                                                               )
                                                               
                                                   )
                                          )
                                   )
                                 )
                        ),
                        
                        
                        tabPanel(title = "Posterior-Predictive Checking", value = "posterior_predictive",
                                 fluidRow(
                                   column(width = 4, style = "padding-left: 10px; padding-right: 10px; overflow-y:auto; max-height: 800px;",
                                   
                                          wellPanel(style = "background-color: #F2F3F4;",
                                                    
                                                    navlistPanel(id = "posteriorpredictive_tabs", well = F, widths = c(4, 8), 
                                                                 
                                                                 tabPanel(title = "Step 1: Upload Files and Setup Simulations", value = "log_input",
                                                                          
                                                                          tabsetPanel(type = "tabs", id = "posteriorpredictive_input_tabs",
                                                                                      
                                                                                      tabPanel(title = "Discrete-Geography File", value = "trait",
                                                                                               h6(''),
                                                                                               
                                                                                               checkboxInput(inputId = "posteriorpredictive_geographyfile_defaultupload", label = "Load example discrete-geography file", value = F),
                                                                                               
                                                                                               div(id = "posteriorpredictive_geographyfile_div", 
                                                                                                   fileInput("posteriorpredictive_geographyfile", "Choose discrete-geography file", multiple = F, 
                                                                                                             accept = c("text/csv","text/comma-separated-values,text/plain",".csv", ".txt"))
                                                                                               ),
                                                                                               div(id = "posteriorpredictive_geographyfile_attributes",
                                                                                                   checkboxInput(inputId = "posteriorpredictive_geographyfile_header", label = "header", value = T),
                                                                                                   selectInput(inputId = "posteriorpredictive_geographyfile_taxoncolumn_name", label = "Taxon column name", choices = "", selectize = F),
                                                                                                   selectInput(inputId = "posteriorpredictive_geographyfile_traitcolumn_name", label = "Discrete-geography column name", choices = "", selectize = F)
                                                                                               ),
                                                                                      ),
                                                                                      
                                                                                      tabPanel(title = "Log and Tree Files", value = "logtree",
                                                                                               
                                                                                               checkboxInput(inputId = "posteriorpredictive_logfile_defaultupload", label = "Load example log and tree files", value = F),
                                                                                               
                                                                                               div(id = paste0("posteriorpredictive_logfile_div", 1),
                                                                                                   h6(''),
                                                                                                   p(paste0("Upload estimate files under prior No. ", 1), align = "center", 
                                                                                                     style = "font-size: 95%; font-weight: bold;"),
                                                                                                   fileInput(inputId = paste0("posteriorpredictive_logfile", 1), 
                                                                                                             label = "log file(s)", 
                                                                                                             multiple = T, accept = ".log"),
                                                                                                   fileInput(inputId = paste0("posteriorpredictive_treefile", 1), 
                                                                                                             label = "tree file(s)",
                                                                                                             multiple = T, accept = c(".trees", ".tree", ".tre"))
                                                                                               ),
                                                                                               
                                                                                               div(id = "posteriorpredictive_loginput_addremove_div",
                                                                                                   p("Add or remove prior model", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                   actionButton(inputId = "posteriorpredictive_loginput_add", label = "", icon = icon("plus", lib = "glyphicon")),
                                                                                                   actionButton(inputId = "posteriorpredictive_loginput_remove", label = "", icon = icon("minus", lib = "glyphicon"))
                                                                                               )
                                                                                      ),
                                                                                      
                                                                                      tabPanel(title = "Perform Simulations", value = "startsimulations",
                                                                                               checkboxInput(inputId = "posteriorpredictive_simulateall",
                                                                                                             label = "Perform one simulation for each sample", value = T),
                                                                                               div(id = "posteriorpredictive_simulatenumber_div",
                                                                                                   numericInput(inputId = "posteriorpredictive_simulatenumber",
                                                                                                                label = "Number of simulations",
                                                                                                                value = 100, min = 1, step = 1),
                                                                                                   p("*sampling with replacement so this number can be greater than the number of samples", style = "font-size:90%; margin-top:-1em;")
                                                                                               ),
                                                                                               
                                                                                               h5(''),
                                                                                               actionButton(inputId = "posteriorpredictive_startprocessing", label = "Start posterior-predictive simulation")
                                                                                      )
                                                                          )
                                                                 ),
                                                                 
                                                                 tabPanel(title = "Step 2: Summarize Posterior-Predictive Statistics", value = "processing_settings",
                                                                          
                                                                          tabsetPanel(type = "tabs", id = "posteriorpredictive_processingsettings_tabs",
                                                                                      
                                                                                      tabPanel(title = "Post-processing settings", value = "log_processing",
                                                                                               h6(''),
                                                                                               
                                                                                               radioButtons(inputId = "posteriorpredictive_teststatistic", label = "Choose the summary statistic to plot", 
                                                                                                            choices = c("Parsimony", "Tip-wise multinomial"), selected = "Parsimony", inline = F),
                                                                                               
                                                                                               h6(''),
                                                                                               checkboxInput(inputId = "posteriorpredictive_logcombinereplicates", label = "Combine replicates for all log files", value = F),
                                                                                               
                                                                                               uiOutput(outputId = "posteriorpredictive_logprocessing_ui")
                                                                                      ),
                                                                                      
                                                                                      tabPanel(title = "Edit figure or table", value = "figuresettings_specific",
                                                                                               
                                                                                               h6(''),
                                                                                               uiOutput(outputId = "posteriorpredictive_priormodelname_ui"),
                                                                                               
                                                                                               HTML('<div id = "posteriorpredictive_par_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#posteriorpredictive_par_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Margin edits</a></p></div>'),
                                                                                               div(id = 'posteriorpredictive_par_edits', class = "panel-collapse collapse in",
                                                                                                   
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_margin_bottom", label = "Bottom margin (in inches) of the plot", value = 0.05, min = 0),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_margin_left", label = "Left margin  (in inches) of the plot", value = 0.45, min = 0),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_margin_top", label = "Top margin (in inches) of the plot", value = 0.15, min = 0),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_margin_right", label = "Right margin (in inches) of the plot", value = 0, min = 0)
                                                                                               ),
                                                                                               
                                                                                               HTML('<div id = "posteriorpredictive_xaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#posteriorpredictive_xaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>X-axis edits</a></p></div>'),
                                                                                               div(id = 'posteriorpredictive_xaxis_edits', class = "panel-collapse collapse in",
                                                                                                   
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_xaxis_lab_cex", label = "Cex of prior model name label", value = 1.35, min = 0.1),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_xaxis_lab_line", label = "Line of prior model name label", value = -2)
                                                                                               ),
                                                                                               
                                                                                               HTML('<div id = "posteriorpredictive_yaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#posteriorpredictive_yaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Y-axis edits</a></p></div>'),
                                                                                               div(id = 'posteriorpredictive_yaxis_edits', class = "panel-collapse collapse in",
                                                                                                   
                                                                                                   textInput(inputId = "posteriorpredictive_plot_y_lab", label = "Y-axis name", value = ""),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_y_lab_cex", label = "Cex of Y-axis name", value = 1.6, min = 0.1),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_y_lab_line", label = "Line of Y-axis name", value = 0.9),
                                                                                                   
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_yaxis_lab_cex", label = "Cex of Y-axis label", value = 0.75, min = 0.1),
                                                                                                   numericInput(inputId = "posteriorpredictive_plot_yaxis_lab_line", label = "Line of Y-axis label", value = -1.5)
                                                                                               )
                                                                                      )
                                                                                      
                                                                          )
                                                                          
                                                                 ),
                                                                 
                                                                 tabPanel(title = "Step 3: Download Output", value = "download_output",
                                                                          
                                                                          tabsetPanel(type = "tabs", id = "posteriorpredictive_downloadoutput_tabs",
                                                                                      
                                                                                      tabPanel(title = "Figure and table", value = "figuretable",
                                                                                               h6(''),
                                                                                               p("Download figure", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                               radioButtons("posteriorpredictive_plot_downloadformat", label = "Format of the figure", choices = c("PDF", "EPS", "PNG", "JPEG", "TIFF"), selected = "PDF", inline = T),
                                                                                               textInput(inputId = "posteriorpredictive_plot_downloadname", label = "Name of the figure (without filename extension, e.g., '.pdf', as it will be appended automatically)", value = "figure"),
                                                                                               downloadButton(outputId = "posteriorpredictive_plot_download", label = "Download"),
                                                                                               
                                                                                               h4(''),
                                                                                               p("Download table", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                               radioButtons("posteriorpredictive_table_downloadformat", label = "Format of the table", choices = c("TSV", "CSV"), selected = "TSV", inline = T),
                                                                                               textInput(inputId = "posteriorpredictive_table_downloadname", label = "Name of the table (without filename extension, e.g., '.tsv', as it will be appended automatically)", value = "table"),
                                                                                               downloadButton(outputId = "posteriorpredictive_table_download", label = "Download")
                                                                                      ),
                                                                                      
                                                                                      tabPanel(title = "Simulated dataset(s)", value = "simdata",
                                                                                               h6(''),
                                                                                               p("Download simulated dataset(s)", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                               textInput(inputId = "posteriorpredictive_simdata_downloadname", label = "Name of the simulated data file (without filename extension, e.g., '.tsv' or '.zip', as it will be appended automatically)", value = "simulated"),
                                                                                               downloadButton(outputId = "posteriorpredictive_simdata_download", label = "Download")
                                                                                      )
                                                                         )
                                                                 )
                                                    )
                                          )
                                   ),
                                   
                                   column(id = "posteriorpredictive_result_column", width = 8, offset = 0, 
                                          style = "padding: 0px 15px 0px 15px; margin: 0%;", 
                                          
                                          fluidRow(id = "posteriorpredictive_result_div",
                                                   
                                                   tabsetPanel(type = "tabs", id = "posteriorpredictive_result_tabs",
                                                               
                                                               tabPanel(title = "Figure", value = "figure",
                                                                        plotOutput(outputId = "posteriorpredictive_plot", height = "auto")
                                                               ),
                                                               
                                                               tabPanel(title = "Table", value = "table",
                                                                        h6(''),
                                                                        DT::dataTableOutput(outputId = "posteriorpredictive_table")
                                                                        
                                                               )
                                                               
                                                   )
                                          )
                                   )
                                 )
                        )
             ),
             
             tabPanel(title = "About", value = "about",
                      fluidRow(
                        includeMarkdown("./about.md")
                      ))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # setting default input values
  input_default <- reactiveValues(model_symmetry = inputdefault_init$model_symmetry, with_bssvs = inputdefault_init$with_bssvs, empiricaltree_mh = inputdefault_init$empiricaltree_mh,
                                  delta_prior = inputdefault_init$delta_prior, poisson_default = inputdefault_init$poisson_default, poisson_lambda = inputdefault_init$poisson_lambda,
                                  mu_prior = inputdefault_init$mu_prior, alphaofgamma_mumean = inputdefault_init$alphaofgamma_mumean,
                                  proposalweight_r = inputdefault_init$proposalweight_r, proposalweight_delta = inputdefault_init$proposalweight_delta, 
                                  proposalweight_rootfreq = inputdefault_init$proposalweight_rootfreq, 
                                  proposalweight_pdeltaij = inputdefault_init$proposalweight_pdeltaij, proposalweight_mu = inputdefault_init$proposalweight_mu, 
                                  proposalweight_mumean = inputdefault_init$proposalweight_mumean, proposalweight_tree = inputdefault_init$proposalweight_tree,
                                  mcmc_chainlength = inputdefault_init$mcmc_chainlength, mcmc_samplingfreq = inputdefault_init$mcmc_samplingfreq, 
                                  mcmc_numreplicates = inputdefault_init$mcmc_numreplicates)
  
  # the default value of the poisson lambda depends on the number of states and the symmetry of the matrix
  observe({
    # req(input$geography_file)
    req(states_dat())
    req(input$traitcolumn_name)
    req(input$model_symmetry)
    
    states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
    states <- states[states != "?"]
    states_num <- length(states)
    
    if (input$model_symmetry == "symmetric") {
      poisson_mean <- ceiling(choose(states_num, 2) / 2) - (states_num - 1)
    } else if (input$model_symmetry == "asymmetric") {
      poisson_mean <- choose(states_num, 2)
    }
    if (poisson_mean < 1) {
      poisson_mean <- 0.6931471805599453
    }
    
    input_default$poisson_lambda <- poisson_mean
  })
  
  # store the analysis specification main tab clicked status
  maintabs_status <- reactiveValues(step2tab_clicked = F, step3tab_clicked = F, step4tab_clicked = F, step5tab_clicked = F)
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "model_specification") {
      maintabs_status$step2tab_clicked <- T
    } else if (input$main_tabs == "analysis_configBasic") {
      maintabs_status$step3tab_clicked <- T
    } else if (input$main_tabs == "analysis_configAdvanced") {
      maintabs_status$step4tab_clicked <- T
    } else if (input$main_tabs == "src_export") {
      maintabs_status$step5tab_clicked <- T
    }
  })

  # only enable the model specification step when all the input files are in place and valid
  observe({
    shinyjs::toggleClass(selector = "#main_tabs li a[data-value=model_specification]", class = "divdisabled", 
                         condition = is.null(states_dat()) || is.null(tree_values$tree) || input_condition$trait_invalid || input_condition$tree_invalid || input_condition$treetrait_invalid)
  })
  
  # only enable the prior specification panel when all the input files are in place and valid
  observe({
    shinyjs::toggleClass(id = "analysis_specification_prior", class = "divdisabled", 
                         condition = is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step2tab_clicked))
  })
  
  # store the analysis specification panel expansion status
  panel_status <- reactiveValues(priorspecification_expanded = F, methodstemplate_expanded = F, xmlviewer_expanded = F)
  
  # expand the prior specification panel when all the input files are in place and valid
  observe({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    if (maintabs_status$step2tab_clicked && (!panel_status$priorspecification_expanded)) {
      panel_status$priorspecification_expanded <- T
      shinyjs::runjs("$('#prior_specification_panel a')[0].click();")
    }
  })
  
  # only enable the methods template viewer panel when all the input files are in place
  observe({
    shinyjs::toggleClass(id = "methods_template_div", class = "divdisabled", 
                         condition = is.null(states_dat()) || is.null(tree_values$tree))
  })
  
  # expand the methods template viewer panel when all the input files are in place and valid
  observe({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    if (!panel_status$methodstemplate_expanded) {
      panel_status$methodstemplate_expanded <- T
      shinyjs::runjs("$('#methods_template_panel a')[0].click();")
    }
  })
  
  # only enable the xml viewer panel when all the input files are in place
  observe({
    shinyjs::toggle(id = "xml_viewer_div", condition = !(is.null(states_dat()) || is.null(tree_values$tree)))
  })

  # link the up down triangle with the collapsed status of the corresponding panel
  shinyjs::runjs("$('#prior_specification').on('shown.bs.collapse', function() {
    $('#prior_specification_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#prior_specification').on('hidden.bs.collapse', function() {
    $('#prior_specification_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#methods_template').on('shown.bs.collapse', function() {
    $('#methods_template_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#methods_template').on('hidden.bs.collapse', function() {
    $('#methods_template_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#xml_viewer').on('shown.bs.collapse', function() {
    $('#xml_viewer_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#xml_viewer').on('hidden.bs.collapse', function() {
    $('#xml_viewer_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  

  # observe({
  #   req(input$geography_file)
  #   req(input$tree_file)
  #   if (!panel_status$xmlviewer_expanded) {
  #     panel_status$xmlviewer_expanded <- T
  #     shinyjs::runjs("$('#xml_viewer_panel a')[0].click();")
  #   }
  # })
  
  # navigate through the tab of xml viewer and methods template viewer according to the status of the main input panel
  # enable and jump to model and prior tab once the second input panel tab is clicked
  observe({
    xmlviewer_modelprior_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step2tab_clicked))
    shinyjs::toggleClass(selector = "#xmlviewer_tabs li a[data-value=xmlmodelprior]", class = "divdisabled", 
                         condition = xmlviewer_modelprior_disabled)
    if (!xmlviewer_modelprior_disabled) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlmodelprior")
    }
  })
  
  observe({
    methods_modelprior_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step2tab_clicked))
    shinyjs::toggleClass(selector = "#methods_tabs li a[data-value=model]", class = "divdisabled", 
                         condition = methods_modelprior_disabled)
    shinyjs::toggleClass(selector = "#methods_tabs li a[data-value=prior]", class = "divdisabled", 
                         condition = methods_modelprior_disabled)
    
    if (!methods_modelprior_disabled) {
      updateTabsetPanel(session, inputId = "methods_tabs", selected = "model")
    }
  })
  
  # only enable the analysis main input panel when the enable button is clicked (when there is some default changes)
  # or the confirm enable button is clicked (when there is no default changes), i.e., 'not overlooked'
  observe({
    step3_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || input$enablestep3_button == 0 || steps_setting$step2_overlooked || steps_setting$prior_overlooked)
    
    shinyjs::toggleClass(selector = "#main_tabs li a[data-value=analysis_configBasic]", class = "divdisabled", 
                         condition = step3_disabled)
    if (!step3_disabled) {
      updateNavlistPanel(session, inputId = "main_tabs", selected = "analysis_configBasic")
    }
  })
  
  # enable and jump to analysis tab once the third input panel tab is clicked
  observe({
    methods_analysis_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step3tab_clicked))
    shinyjs::toggleClass(selector = "#methods_tabs li a[data-value=analysis]", class = "divdisabled", 
                         condition = methods_analysis_disabled)
    
    if (!methods_analysis_disabled) {
      updateTabsetPanel(session, inputId = "methods_tabs", selected = "analysis")
    }
  })
  
  observe({
    req(tree_values$tree)
    maintabs_status$step3tab_clicked
    
    # if ((tree_values$tree_num > 1) && (!is.null(input$empiricaltree_mh)) && (input$empiricaltree_mh == "empirical sampling (not recommended)")) {
    #   xmlviewer_mcmc_disabled <- is.null(input$geography_file) || is.null(input$tree_file)
    # } else {
    #   xmlviewer_mcmc_disabled <- (is.null(input$geography_file) || is.null(input$tree_file) || (!maintabs_status$step3tab_clicked))
    # }
    xmlviewer_mcmc_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step3tab_clicked))
    
    shinyjs::toggleClass(selector = "#xmlviewer_tabs li a[data-value=xmlmcmc]", class = "divdisabled", 
                         condition = xmlviewer_mcmc_disabled)
    if (!xmlviewer_mcmc_disabled) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlmcmc")
    }
  })
  
  # enable and jump to proposal tab once the proposal input panel tab is clicked
  analysisconfigBasictabs_status <- reactiveValues(proposalweightstab_clicked = F)
  observeEvent(input$analysisconfigBasic_tabs, {
    if (input$analysisconfigBasic_tabs == "proposalweights") {
      analysisconfigBasictabs_status$proposalweightstab_clicked <- T
    }
  })
  
  observe({
    xmlviewer_proposal_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step3tab_clicked))
    shinyjs::toggleClass(selector = "#xmlviewer_tabs li a[data-value=xmlproposal]", class = "divdisabled", 
                         condition = xmlviewer_proposal_disabled)
    
    if ((!xmlviewer_proposal_disabled) && analysisconfigBasictabs_status$proposalweightstab_clicked) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlproposal")
    }
  })

  # only enable the analysis advanced main input panel when the enable button is clicked (when there is some default changes)
  # or the confirm enable button is clicked (when there is no default changes), i.e., 'not overlooked'
  observe({
    step4_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || input$enablestep4_button == 0 || steps_setting$step3_overlooked)
    
    shinyjs::toggleClass(selector = "#main_tabs li a[data-value=analysis_configAdvanced]", class = "divdisabled", 
                         condition = step4_disabled)
    if (!step4_disabled) {
      updateNavlistPanel(session, inputId = "main_tabs", selected = "analysis_configAdvanced")
    }
  })
  
  observe({
    xmlviewer_phyloctmc_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step4tab_clicked))
    shinyjs::toggleClass(selector = "#xmlviewer_tabs li a[data-value=xmlphyloctmc]", class = "divdisabled", 
                         condition = xmlviewer_phyloctmc_disabled)
    if (!xmlviewer_phyloctmc_disabled) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlphyloctmc")
    }
  })
  
  # only show the power posterior panel when marginal likelihood analysis is selected 
  xmlviewertabs_status <- reactiveValues(powerposteriortab_clicked = F)
  observeEvent(input$further_analysis, {
    xmlviewer_powerposterior_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || 
                                            (!maintabs_status$step4tab_clicked) || ((!is.null(input$further_analysis)) && input$further_analysis != "Marginal likehood estimation"))
    shinyjs::toggle(selector = "#xmlviewer_tabs li a[data-value=xmlpowerposterior]", condition = !xmlviewer_powerposterior_disabled)
    if (!xmlviewer_powerposterior_disabled) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlpowerposterior")
      xmlviewertabs_status$powerposteriortab_clicked <- T
      # showTab("xmlviewer_tabs", target = "xmlpowerposterior", select = T)
    }
    # else {
    #   hideTab("xmlviewer_tabs", target = "xmlpowerposterior")
    # }
  })
  
  observeEvent(input$further_analysis, {
    if ((!is.null(input$further_analysis)) && (input$further_analysis != "Marginal likehood estimation") && (input$xmlviewer_tabs == "xmlpowerposterior")) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlphyloctmc")
      xmlviewertabs_status$powerposteriortab_clicked <- F
    }
  })
  
  
  # only enable the download main input panel when the enable button is clicked (when there is some default changes)
  # or the confirm enable button is clicked (when there is no default changes), i.e., 'not overlooked'
  observe({
    step5_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || input$enablestep5_button == 0 || steps_setting$step4_overlooked)
    
    shinyjs::toggleClass(selector = "#main_tabs li a[data-value=src_export]", class = "divdisabled", 
                         condition = step5_disabled)
    if (!step5_disabled) {
      updateNavlistPanel(session, inputId = "main_tabs", selected = "src_export")
    }
  })
  
  # enable and jump to full tab once the download input panel tab is clicked
  observe({
    xmlviewer_full_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step5tab_clicked))
    shinyjs::toggleClass(selector = "#xmlviewer_tabs li a[data-value=xmlfull]", class = "divdisabled", 
                         condition = xmlviewer_full_disabled)
    if (!xmlviewer_full_disabled) {
      updateTabsetPanel(session, inputId = "xmlviewer_tabs", selected = "xmlfull")
    }
  })
  
  observe({
    methods_full_disabled <- (is.null(states_dat()) || is.null(tree_values$tree) || (!maintabs_status$step5tab_clicked))
    shinyjs::toggleClass(selector = "#methods_tabs li a[data-value=full]", class = "divdisabled", 
                         condition = methods_full_disabled)
    
    if (!methods_full_disabled) {
      updateTabsetPanel(session, inputId = "methods_tabs", selected = "full")
    }
  })
  
  # helper variables for determining whether any default input values have changed and if the confirm button is clicked 
  # so that it will be overlooked
  steps_setting <- reactiveValues(step2_overlooked = T, step2_default = T, 
                                  step3_overlooked = T, step3_default = T, 
                                  step4_overlooked = T, step4_default = T,
                                  prior_overlooked = T, prior_default = T,
                                  all_overlooked = T, all_default = T)
  
  # step 2 button, to enable step 3
  observeEvent(input$enablestep3_button, {
    
    # req(input$tree_file)
    # req(input$geography_file)
    req(tree_values$tree)
    req(input$model_symmetry)
    req(input$delta_prior)
    req(input$mu_prior)
    req(tree_values$tree)
    req(states_dat())
    req(!input_condition$trait_invalid)
    req(!input_condition$tree_invalid)
    req(!input_condition$treetrait_invalid)
    
    # compare to default
    if ((input$model_symmetry != input_default$model_symmetry) || (input$with_bssvs != input_default$with_bssvs)) {
      steps_setting$step2_default <- F
    } else if ((tree_values$tree_num > 1) && (!is.null(input$empiricaltree_mh)) && (input$empiricaltree_mh != input_default$empiricaltree_mh)) {
      steps_setting$step2_default <- F
    }
    if (!steps_setting$step2_default) steps_setting$step2_overlooked <- F
    
    if (input$delta_prior != input_default$delta_prior || input$mu_prior != input_default$mu_prior) {
      steps_setting$prior_default <- F
    } else if ((input$delta_prior == "Poisson") && (input$poisson_default != input_default$poisson_default || input$poisson_lambda != input_default$poisson_lambda)) {
      steps_setting$prior_default <- F
    } else if ((input$mu_prior == "Hierarchical Exponential") && (input$alphaofgamma_mumean != input_default$alphaofgamma_mumean)) {
      steps_setting$prior_default <- F
    }
    if (!steps_setting$prior_default) steps_setting$prior_overlooked <- F
    
    if (steps_setting$step2_default + steps_setting$prior_default < 2) steps_setting$all_default <- F
    
    modalspec_filler <- c("model specification is", "prior specification is", "model and prior specifications are both")
    names(modalspec_filler) <- c(paste0(T, F), paste0(F, T), paste0(T, T))
    
    if (steps_setting$step2_overlooked || steps_setting$prior_overlooked) {
      showModal(modalDialog(paste0("The current ", modalspec_filler[paste0(steps_setting$step2_overlooked, steps_setting$prior_overlooked)],
                                   " identical to the default setting. Still Proceeding to next step?"),
                            title = "Warning: no default value changed",
                            footer = tagList(actionButton(inputId = "enablestep3_confirm_button", label = "Yes"),
                                             modalButton("No")),
                            easyClose = F,
                            size = "m"
      ))
    }
    
  })
  
  # click the confirm button then that means they are not overlooked
  observeEvent(input$enablestep3_confirm_button, {
    steps_setting$step2_overlooked <- F
    steps_setting$prior_overlooked <- F
    removeModal()
  })
  
  # step 3 button, to enable step 4
  observeEvent(input$enablestep4_button, {
    
    # req(input$tree_file)
    # req(input$geography_file)
    req(input$model_symmetry)
    req(input$delta_prior)
    req(input$mu_prior)
    req(input$proposalweight_r)
    req(input$proposalweight_mu)
    req(tree_values$tree)
    req(states_dat())
    req(!input_condition$trait_invalid)
    req(!input_condition$tree_invalid)
    req(!input_condition$treetrait_invalid)
    
    # compare to default
    if ((input$mcmc_chainlength != input_default$mcmc_chainlength) || (input$mcmc_samplingfreq != input_default$mcmc_samplingfreq) || (input$mcmc_numreplicates != input_default$mcmc_numreplicates)) {
      steps_setting$step3_default <- F
    } else if ((input$proposalweight_r != input_default$proposalweight_r) || (input$proposalweight_mu != input_default$proposalweight_mu)) {
      steps_setting$step3_default <- F
    } else if ((input$model_symmetry == "asymmetric") && (input$proposalweight_rootfreq != input_default$proposalweight_rootfreq)) {
      steps_setting$step3_default <- F
    } else if (input$with_bssvs && (input$proposalweight_delta != input_default$proposalweight_delta)) {
      steps_setting$step3_default <- F
    } else if ((input$mu_prior == "Hierarchical Exponential") && (input$proposalweight_mumean != input_default$proposalweight_mumean)) {
      steps_setting$step3_default <- F
    } else if ((tree_values$tree_num > 1) && (input$proposalweight_tree != input_default$proposalweight_tree)) {
      steps_setting$step3_default <- F
    } else if (input$with_bssvs && (input$delta_prior == "Beta-Binomial") && (input$proposalweight_pdeltaij != input_default$proposalweight_pdeltaij)) {
      steps_setting$step3_default <- F
    }
    if (!steps_setting$step3_default) {
      steps_setting$step3_overlooked <- steps_setting$all_default <- F
    }
    
    if (steps_setting$step3_overlooked) {
      showModal(modalDialog("The current basic analysis configuration is identical to the default setting. Still Proceeding to next step?",
                            title = "Warning: no default value changed",
                            footer = tagList(actionButton(inputId = "enablestep4_confirm_button", label = "Yes"),
                                             modalButton("No")),
                            easyClose = F,
                            size = "m"
      ))
    }
    
  })
  
  # click the confirm button then that means they are not overlooked
  observeEvent(input$enablestep4_confirm_button, {
    steps_setting$step3_overlooked <- F
    removeModal()
  })
  
  # step 4 button, to enable step 5
  observeEvent(input$enablestep5_button, {
    
    # req(input$tree_file)
    req(tree_values$tree)
    # req(input$geography_file)
    req(states_dat())
    
    if (input$do_stochasticmapping != "" || input$further_analysis != "") {
      steps_setting$step4_overlooked <- steps_setting$step4_default <- steps_setting$all_default <- F
    }
    
    if (steps_setting$step4_overlooked) {
      showModal(modalDialog("The current advanced analysis configuration is identical to the default setting. Still Proceeding to next step?",
                            title = "Warning: no default value changed",
                            footer = tagList(actionButton(inputId = "enablestep5_confirm_button", label = "Yes"),
                                             modalButton("No")),
                            easyClose = F,
                            size = "m"
      ))
    }
    
  })
  
  # click the confirm button then that means they are not overlooked
  observeEvent(input$enablestep5_confirm_button, {
    steps_setting$step4_overlooked <- F
    removeModal()
  })
  
  
  # remove/hide those buttons once we have proceeded to the corresponding tab
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "analysis_configBasic") {
      shinyjs::hide(id = "enablestep3_button")
    } else if (input$main_tabs == "analysis_configAdvanced") {
      shinyjs::hide(id = "enablestep4_button")
    } else if (input$main_tabs == "src_export") {
      shinyjs::hide(id = "enablestep5_button")
    }
  })
  
  # compare to default for all (check before download)
  observe({
    
    # req(input$tree_file)
    # req(input$geography_file)
    req(input$model_symmetry)
    req(input$delta_prior)
    req(input$mu_prior)
    req(input$proposalweight_r)
    req(input$proposalweight_mu)
    req(tree_values$tree)
    req(states_dat())
    req(!input_condition$trait_invalid)
    req(!input_condition$tree_invalid)
    req(!input_condition$treetrait_invalid)
    req(input$enablestep5_button)
    
    if (steps_setting$all_default) {
      
      if ((input$model_symmetry != input_default$model_symmetry) || (input$with_bssvs != input_default$with_bssvs)) {
        steps_setting$all_default <- F
      } else if ((tree_values$tree_num > 1) && (!is.null(input$empiricaltree_mh)) && (input$empiricaltree_mh != input_default$empiricaltree_mh)) {
        steps_setting$all_default <- F
      } else if (input$delta_prior != input_default$delta_prior || input$mu_prior != input_default$mu_prior) {
        steps_setting$all_default <- F
      } else if ((input$delta_prior == "Poisson") && (input$poisson_default != input_default$poisson_default || input$poisson_lambda != input_default$poisson_lambda)) {
        steps_setting$all_default <- F
      } else if ((input$mu_prior == "Hierarchical Exponential") && (input$alphaofgamma_mumean != input_default$alphaofgamma_mumean)) {
        steps_setting$all_default <- F
      } else if ((input$mcmc_chainlength != input_default$mcmc_chainlength) || (input$mcmc_samplingfreq != input_default$mcmc_samplingfreq) || (input$mcmc_numreplicates != input_default$mcmc_numreplicates)) {
        steps_setting$all_default <- F
      } else if ((input$proposalweight_r != input_default$proposalweight_r) || (input$proposalweight_mu != input_default$proposalweight_mu)) {
        steps_setting$all_default <- F
      } else if ((input$model_symmetry == "asymmetric") && (input$proposalweight_rootfreq != input_default$proposalweight_rootfreq)) {
        steps_setting$all_default <- F
      } else if (input$with_bssvs && (input$proposalweight_delta != input_default$proposalweight_delta)) {
        steps_setting$all_default <- F
      } else if ((input$mu_prior == "Hierarchical Exponential") && (input$proposalweight_mumean != input_default$proposalweight_mumean)) {
        steps_setting$all_default <- F
      } else if ((tree_values$tree_num > 1) && (input$proposalweight_tree != input_default$proposalweight_tree)) {
        steps_setting$all_default <- F
      } else if (input$with_bssvs && (input$delta_prior == "Beta-Binomial") && (input$proposalweight_pdeltaij != input_default$proposalweight_pdeltaij)) {
        steps_setting$all_default <- F
      } else if (input$do_stochasticmapping != "" || input$further_analysis != "") {
        steps_setting$all_default <- F
      }
    }
    
    if (!steps_setting$all_default) steps_setting$all_overlooked <- F
  })
  
  # if nothing changed, poping up the warning message
  # the fake button is identical to the corresponding real button except that it would produce the message
  observe({
    shinyjs::toggle(id = "downloadxml_fake", condition = steps_setting$all_overlooked)
    shinyjs::toggle(id = "downloadxml_real", condition = !steps_setting$all_overlooked)
  })
  
  observeEvent(input$downloadxml_fake, {
    showModal(modalDialog("Current settings are all identical to the default.",
                          title = "Warning: no default value changed",
                          footer = tagList(downloadButton(outputId = "downloadxml_faketoreal", label = "Still download"),
                                           modalButton("Close this")),
                          easyClose = F,
                          size = "m"
    ))
  })
  
  observe({
    shinyjs::toggle(id = "downloadmethods_fake", condition = steps_setting$all_overlooked)
    shinyjs::toggle(id = "downloadmethods_real", condition = !steps_setting$all_overlooked)
  })
  
  observeEvent(input$downloadmethods_fake, {
    showModal(modalDialog("Current settings are all identical to the default.",
                          title = "Warning: no default value changed",
                          footer = tagList(downloadButton(outputId = "downloadmethods_faketoreal", label = "Still download"),
                                           modalButton("Close this")),
                          easyClose = F,
                          size = "m"
    ))
  })
  
  # some sanity check to ensure the input values are valid
  observe({
    
    if ((!is.integer(input$mcmc_chainlength)) || input$mcmc_chainlength <= 0) {
      showModal(modalDialog("MCMC chain length has to be a postive integer.",
                            title = "Invalid input",
                            easyClose = F,
                            size = "s"
                            
      ))
    }
    
    if ((!is.integer(input$mcmc_samplingfreq)) || input$mcmc_samplingfreq <= 0) {
      showModal(modalDialog("MCMC sampling frequency has to be a positive integer.",
                            title = "Invalid input",
                            easyClose = F,
                            size = "s"
                            
      ))
    }
    
    if ((!is.integer(input$mcmc_numreplicates))  || input$mcmc_numreplicates <= 0) {
      showModal(modalDialog("Number of analysis replicates has to be a positive integer.",
                            title = "Invalid input",
                            easyClose = F,
                            size = "s"
                            
      ))
    }
  })
  
  # enable the geography attributes input once the geography input file is uploaded
  # as some of the options of the input fields depend on the input file
  observe({
    shinyjs::toggle(id = "geographyfile_attributes", condition = !is.null(states_dat()))
  })
  
  # enable the markov jump options (between each pair and/or total) only when the user choose to do stochastic mapping
  observe({
    shinyjs::toggle(id = "markovjumps_totalpairwise", condition = (input$do_stochasticmapping != ""))
  })
  
  observe({
    if ((!is.null(input$do_stochasticmapping)) && (input$do_stochasticmapping != "") && 
        (input$do_stochasticmapping == "Fast stochastic mapping (incomplete history, simulation-free)") && (input$markovjumps_total + input$markovjumps_pairwise == 0)) {
      
      showModal(modalDialog("If neither total number of dispersal events between all areas nor the number of dispersal events between each pair of areas is estimated, then the fast stochastic mapping algorithm won't be performed",
                            title = "Warning",
                            easyClose = F,
                            size = "m"
                            
      ))
    }
  })
  
  # render the further analysis panel according to what kind of further analysis user wants to do
  output$furtheranalysis_ui <- renderUI({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    furtheranalysis_ui_list <- tagList()
    if (input$further_analysis == "Data cloning") {
      
      furtheranalysis_ui_list <- tagAppendChildren(furtheranalysis_ui_list, 
                                                   h6(""),
                                                   numericInput(inputId = paste0("lheats", 1), label = paste0("Clone number for data-cloning analysis No. ", 1), 
                                                                value = 5, min = 1),
                                                   
                                                   div(id = "lheats_addremove_div",
                                                       p("Add different number of clones", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                       actionButton(inputId = "lheats_add", label = "", icon = icon("plus", lib = "glyphicon")),
                                                       actionButton(inputId = "lheats_remove", label = "", icon = icon("minus", lib = "glyphicon")))
                                                   )
                                                   
    
      } else if (input$further_analysis == "Marginal likehood estimation") {
      furtheranalysis_ui_list <- tagAppendChildren(furtheranalysis_ui_list, 
                                                   numericInput(inputId = "ml_chainlengthperstone", label = "Chain length per power posterior step", value = 250000, min = 1),
                                                   numericInput(inputId = "ml_samplingfreq", label = "Sampling frequency of the power posterior chains", value = 5000, min = 1),
                                                   numericInput(inputId = "ml_numstones", label = "Number of power posterior steps", value = 100, min = 1),
                                                   numericInput(inputId = "ml_alphaofbeta", label = "Alpha of the Beta distribution", value = 0.3, min = 0),
                                                   plotOutput(outputId = "powerbeta_plot", width = "100%", height = 100))
    }
    
    furtheranalysis_ui_list
  })
  
  # dynamically rendering the fields for lheat so that user can specify a sequence of clones
  lheats <- reactiveValues(num = 1)
  observeEvent(input$lheats_add, {
    lheats$num <- lheats$num + 1
    insertUI(selector = "#lheats_addremove_div", where = "beforeBegin", 
             ui = div(id = paste0("lheats_div", lheats$num),
                      numericInput(inputId = paste0("lheats", lheats$num), 
                                   label = paste0("Clone number for data-cloning analysis No. ", lheats$num), value = 5, min = 1)))
  })
  observeEvent(input$lheats_remove, {
    if (lheats$num > 1) {
      removeUI(selector = paste0("#lheats_div", lheats$num))
      lheats$num <- lheats$num - 1
    }
  })

  # powers (of the power posterior distributions) plot
  output$powerbeta_plot <- renderPlot({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$further_analysis)
    
    ml_numstones <- 100
    if (!is.null(input$ml_numstones)) ml_numstones <- input$ml_numstones
    
    ml_alphaofbeta <- 0.3
    if (!is.null(input$ml_alphaofbeta)) ml_alphaofbeta <- input$ml_alphaofbeta
    
    powers <- qbeta(c(seq(from = 1, to = 0, length.out = ml_numstones + 1)), shape1 = ml_alphaofbeta, shape2 = 1)
    
    par(lend = 2, mai = c(0.4, 0.01, 0.01, 0.01))
    plot(NA, xlim = rev(c(0, 1)), ylim = c(-0.01, 0.01), xaxt = "n", yaxt = "n", type = "n", xlab = NA, ylab = NA, bty = "n")
    points(x = powers, y = rep(0, length(powers)), col = mycolorpalette[5], pch = 4, cex = 0.65)
    
    axis(side = 1, labels = NA, lwd = 1, lwd.ticks = 1, tcl = -0.25)
    axis(side = 1, lwd = 0, lwd.ticks = 0, cex.axis = 1, line = -1)
    # axis(side = 2, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
    # axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = 1, line = -0.75)
    
    mtext(expression(paste("power (the likelihood is raised to), ", beta[k])), side = 1, line = 1.1, cex = 1)
    # mtext("power index, k", side = 1, line = 0.9, cex = 1)
    
  })
  
  
  # input sanity check
  observe({
    if (input$further_analysis == "Data cloning") {
      
      for (i in 1:lheats$num) {
        if ((paste0("lheats", i) %in% names(input)) && (!is.null(input[[paste0("lheats", i)]])) && 
            ((!is.integer(input[[paste0("lheats", i)]])) || input[[paste0("lheats", i)]] <= 1)) {
          showModal(modalDialog("Number of clones needs to be an integer greater than 1.",
                                title = "Invalid input",
                                easyClose = F,
                                size = "m"
                                
          ))
          break
        }
      }
    }
  })
  
  observe({
    
    if ("Marginal likehood estimation" %in% input$further_analysis) {
      
      req(input$ml_numstones)
      req(input$ml_chainlengthperstone)
      req(input$ml_samplingfreq)
      req(input$ml_alphaofbeta)
      
      if ((!is.integer(input$ml_numstones)) || input$ml_numstones < 1) {
        showModal(modalDialog("Number of power posterior steps needs to be an integer greater than 1.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
      }
      if ((!is.integer(input$ml_chainlengthperstone)) || input$ml_chainlengthperstone < 1) {
        showModal(modalDialog("Chain length per power posterior step needs to be an integer greater than 1.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
      }
      if ((!is.integer(input$ml_samplingfreq)) || input$ml_samplingfreq < 1) {
        showModal(modalDialog("Sampling frequency of the power posterior chains needs to be an integer greater than 1.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
      }
      if (input$ml_alphaofbeta <= 0) {
        showModal(modalDialog("Alpha of the beta distribution needs to be positive.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
      }
    }
  })
  
  # enable the tree input tab only after user uploads the discrete-trait file (and it's valid)
  observe({
    shinyjs::toggleClass(id = "geography_file_div", class = "divdisabled", 
                         condition = input$geography_file_defaultupload)
  })
  
  # reading in discrete-trait data
  # assume there is only one discrete trait for now, could be easily relaxed
  states_dat <- reactiveVal()
  # observeEvent(input$geography_file, {
  #   req(input$geography_file)
  #   states_dat(read.table(text = gsub("\t", ",", readLines(input$geography_file$datapath)), header = T, sep = ",", stringsAsFactors = F))
  # })
  observe({
    if (input$geography_file_defaultupload) {
      states_dat(read.table(text = gsub("\t", ",", readLines("./data/analyses_setup/discrete_trait.txt")), header = T, sep = ",", stringsAsFactors = F))
    } else if (!is.null(input$geography_file)) {
      req(input$geography_file)
      states_dat(read.table(text = gsub("\t", ",", readLines(input$geography_file$datapath)), header = T, sep = ",", stringsAsFactors = F))
    } else {
      states_dat(NULL)
    }
  })
  
  # output$columnname_ui <- renderUI({
  #   req(input$geography_file)
  #   
  #   columnname_ui_list <- tagList()
  #   
  #   if (input$geographyfile_header) {
  #     columnname_ui_list <- tagAppendChildren(columnname_ui_list, 
  #                                             selectInput(inputId = "taxoncolumn_name", label = "taxon column name", 
  #                                                         choices = colnames(states_dat()), selected = colnames(states_dat())[1], selectize = F),
  #                                             selectInput(inputId = "traitcolumn_name", label = "discrete-trait column name", 
  #                                                         choices = colnames(states_dat()), selected = colnames(states_dat())[2], selectize = F))
  #   } else {
  #     showModal(modalDialog("The discrete-trait file doesn't have a header row so you need to provide name for the taxon column and the trait column, respectively. The trait column name will be used to name this trait in the xml file. Also in this case we assume the first column is the taxon column and the second column is the trait column.",
  #                           title = "Warning",
  #                           easyClose = F,
  #                           size = "l"
  #                           
  #     ))
  #   }
  # })
  
  # check for valid input
  input_condition <- reactiveValues(tree_invalid = T, trait_invalid = T, treetrait_invalid = T)
  
  # make sure the discrete-trait input file has header, and then update the column name input fields accordingly
  observe({
    req(states_dat())
    
    if (!input$geographyfile_header) {
      showModal(modalDialog("Please edit the discrete-geography file so that the first row indicates column names.",
                            title = "Invalid input",
                            easyClose = F,
                            size = "m"
                            
      ))
      input_condition$trait_invalid <- T
    } else {
      input_condition$trait_invalid <- F
    }
    
    updateSelectInput(session = session, inputId = "taxoncolumn_name", choices = colnames(states_dat()), selected = colnames(states_dat())[1])
    updateSelectInput(session = session, inputId = "traitcolumn_name", choices = colnames(states_dat()), selected = colnames(states_dat())[2])
  })
  
  # enable the tree input tab only after user uploads the discrete-trait file (and it's valid)
  observe({
    shinyjs::toggleClass(selector = "#datainput_tabs li a[data-value=tree]", class = "divdisabled", 
                         condition = is.null(states_dat()) || (!input$geographyfile_header) || 
                           is.null(input$taxoncolumn_name) || is.null(input$traitcolumn_name)  || input_condition$trait_invalid)
  })
  
  observe({
    shinyjs::toggle(id = "tree_num_defaultupload_div", condition = input$tree_file_defaultupload)
  })
  
  observe({
    shinyjs::toggleClass(id = "tree_file_div", class = "divdisabled", condition = input$tree_file_defaultupload)
  })
  
  # read in the tree or the first two trees (if there are multiple trees)
  tree_values <- reactiveValues(tree = NULL, tree_num = 0, tree_length = 0)
  tree_paths_defaultupload <- c("./data/analyses_setup/HIV_datasetA_sample.trees", "./data/analyses_setup/HIV_datasetA_MCC.tree")
  observe({
    
    input_condition$tree_invalid <- F
    
    if (input$tree_file_defaultupload) {
      if (input$tree_num_defaultupload == "distribution (multiple)") {
        tree_values$tree <- ape::read.nexus(tree_paths_defaultupload[1])
        tree_values$tree_num <- length(tree_values$tree)
        tree_values$tree_length <- sum(tree_values$tree[[1]]$edge.length)
      } else {
        tree_values$tree <- ape::read.nexus(tree_paths_defaultupload[2])
        tree_values$tree_num <- 1L
        tree_values$tree_length <- sum(tree_values$tree$edge.length)
      }
      
    } else {
      req(input$tree_file)
      
      tree_tmp <- readtreefile_linebyline(filepath = input$tree_file$datapath)
      tree_values$tree_num <- max(grep("&", tree_tmp)) - min(grep("&", tree_tmp)) + 1L
      
      if (tree_values$tree_num > 1) {
        tmptree_path <- tempfile(fileext = ".tree")
        cat(c(tree_tmp[1:(min(grep("&", tree_tmp)) + 1)], "End;"), file = tmptree_path, sep = "\n")
        tree_values$tree <- ape::read.nexus(tmptree_path)
        tree_values$tree_length <- sum(tree_values$tree[[1]]$edge.length)
        
      } else if (tree_values$tree_num == 1) {
        tree_values$tree <- ape::read.nexus(input$tree_file$datapath)
        tree_values$tree_length <- sum(tree_values$tree$edge.length)
      } else {
        input_condition$tree_invalid <- T
        
        showModal(modalDialog("No tree in the uploaded tree file.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "s"
        ))
      }
    }
  })
  
  # sanity check: tree and discrete data match in terms of taxon list
  observe({
    
    # req(input$geography_file)
    # req(input$tree_file)
    req(input$taxoncolumn_name)
    req(tree_values$tree)
    req(states_dat())
    
    if (tree_values$tree_num > 1) {
      tip_lab <- sort(tree_values$tree[[1]]$tip.label)
    } else if (tree_values$tree_num == 1) {
      tip_lab <- sort(tree_values$tree$tip.label)
    }
    
    if ((input$taxoncolumn_name != "") && (!is.null(states_dat()))) {
      taxa <- sort(as.vector(states_dat()[, input$taxoncolumn_name]))
      
      if (!identical(tip_lab, taxa)) {
        showModal(modalDialog("Taxa list in the tree file doesn't match the discrete-geography file.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
        input_condition$treetrait_invalid <- T
      } else {
        input_condition$treetrait_invalid <- F
      }
    }
  })

  # update the output files name according to the input file
  observe({
    # req(input$geography_file)
    # req(input$tree_file)
    req(input$traitcolumn_name)
    req(tree_values$tree)
    req(states_dat())
    
    if (input$tree_file_defaultupload) {
      req(input$tree_num_defaultupload)
      file_name <- basename(ifelse(input$tree_num_defaultupload == "distribution (multiple)", tree_paths_defaultupload[1], tree_paths_defaultupload[2]))
    } else {
      req(input$tree_file)
      file_name <- input$tree_file$name
    }
    file_name <- paste0(unlist(strsplit(file_name, "\\."))[1], "_", input$traitcolumn_name)
    
    updateTextInput(session = session, inputId = "xml_name", value = file_name)
    updateTextInput(session = session, inputId = "methods_name", value = file_name)
  })
  
  # generate the default name for output xml and method template
  output_file <- reactiveValues(file_name = NULL)
  observe({
    
    # req(input$geography_file)
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$traitcolumn_name)
    req(!is.null(input$further_analysis))
    req(lheats$num)
    req(states_dat())
    
    file_name <- input$xml_name
    
    if (input$further_analysis != "Data cloning") {
      if (input$do_stochasticmapping == "Fast stochastic mapping (incomplete history, simulation-free)") {
        file_name <- paste0(file_name, "_incompleteHistory")
      } else if (input$do_stochasticmapping == "Stochastic mapping (complete history, simulation-based)") {
        file_name <- paste0(file_name, "_completeHistory")
      }
    }
    
    if (input$further_analysis == "") {
      file_name <- paste0(file_name, "_posterior")
    } else if (input$further_analysis == "Marginal likehood estimation") {
      file_name <- paste0(file_name, "_mlafterposterior")
    } else if (input$further_analysis == "Under prior") {
      file_name <- paste0(file_name, "_underprior")
    } else if (input$further_analysis == "Data cloning") {
      lheat <- 1
      if (!is.null(input$lheats1)) {
        lheat <- sort(unique(as.integer(unlist(sapply(1:lheats$num, function(i) input[[paste0("lheats", i)]])))))
      }
      file_name <- paste0(file_name, "_datacloning", lheat)
    }
    
    filenames <- c()
    for (i in 1:lheats$num) {
      for (j in 1:input$mcmc_numreplicates) {
        filenames <- c(filenames, paste0(file_name[i], "_run", j))
      }
    }
    
    output_file$file_name <- filenames
  })
  
  # tree model ui: only gives user the option to choose whether to do sequential Bayesian (i.e., sampling trees) correctly 
  # or not when the uploaded tree file contains multiple trees
  output$treemodel_ui <- renderUI({
    
    # req(input$tree_file)
    req(tree_values$tree)
    
    if (tree_values$tree_num > 1) {
      
      selectInput(inputId = "empiricaltree_mh", label = "Averaging over imported trees using", 
                  choices = c(input_default$empiricaltree_mh, "empirical sampling (not recommended)"),
                  selected = input_default$empiricaltree_mh, multiple = F, selectize = F)
    } else {
      p("A single tree is imported, so the phylogenetic random variable is fixed to be this tree.")
    }
  })
  
  observe({
    
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$empiricaltree_mh)
    
    if (input$empiricaltree_mh == "empirical sampling (not recommended)") {
      showModal(modalDialog("Averaging over trees by always accepting the proposed tree during the MCMC (i.e., without computing the acceptance ratio) is not recommended. For more details, see Supplementary Material of Gao et al. 2021",
                            title = "Warning: unrecommended option",
                            easyClose = F,
                            size = "m"
                            
      ))
    }
  })
  
  # compute the parsimony score (only use the first tree for now, just to be efficient)
  parsimony_score <- reactiveValues(mean = 0)
  observe({
    
    # req(input$tree_file)
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    req(tree_values$tree)
    req(states_dat())
    req(!input_condition$tree_invalid)
    req(!input_condition$trait_invalid)
    req(!input_condition$treetrait_invalid)
    
    observed_tipstates <- as.vector(states_dat()[, input$traitcolumn_name])
    states <- sort(unique(observed_tipstates))
    states <- states[states != "?"]
    names(observed_tipstates) <- as.vector(states_dat()[, input$taxoncolumn_name])
    
    firsttree_only <- T
    # if (!is.null(input$parsimonyscore_alltree_confirm) && (input$parsimonyscore_alltree_confirm > 0)) firsttree_only <- F
    
    tree <- tree_values$tree
    if (tree_values$tree_num > 1) tree <- tree[[1]]
    
    if (input$tree_file_defaultupload) {
      if (input$tree_num_defaultupload == "distribution (multiple)") {
        filepath <- tree_paths_defaultupload[1]
      } else {
        filepath <- tree_paths_defaultupload[2]
      }
    } else {
      req(input$tree_file)
      filepath <- input$tree_file$datapath
    }
    parsimony_score$mean <- parsimonyscore_treebytree(filepath = filepath, tree = tree, 
                                                      tipstates = observed_tipstates, states = states, firsttree_only = firsttree_only)
  })
  
  # not doing this now as it gets complicated what to do with other parts that may involve the difference between using the first tree 
  # versus using all the trees
  # observeEvent(input$parsimonyscore_alltree, {
  #   
  #   if (input$parsimonyscore_alltree) {
  #     showModal(modalDialog("Computing a mean parsimony score over all trees can take some time. Still proceed?",
  #                           title = "Warning",
  #                           footer = tagList(actionButton(inputId = "parsimonyscore_alltree_confirm", label = "Yes"),
  #                                            modalButton("No")),
  #                           easyClose = F,
  #                           size = "m"
  #                           
  #     ))
  #   }
  # })
  
  # only generate the delta prior panel when user choose to do bssvs
  observe({
    if (input$with_bssvs) {
      showTab(inputId = "prior_tabs", target = "delta", select = T)
    } else {
      hideTab(inputId = "prior_tabs", target = "delta")
    }
  })
  
  # render delta prior ui according to the selected prior
  output$deltaprior_ui <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$model_symmetry)
    req(input$taxoncolumn_name)
    
    states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
    states <- states[states != "?"]
    states_num <- length(states)
    
    deltaprior_ui_list <- tagList()
    
    if (input$delta_prior == "Poisson") {
      
      if (input$model_symmetry == "symmetric") {
        poisson_mean <- ceiling(choose(states_num, 2)/2) - (states_num - 1)
      } else if (input$model_symmetry == "asymmetric") {
        poisson_mean <- choose(states_num, 2)
      }
      
      if (poisson_mean < 1) {
        poisson_mean <- 0.6931471805599453
      }
      
      deltaprior_ui_list <- tagAppendChildren(deltaprior_ui_list, checkboxInput(inputId = "poisson_default", label = "BEAST default", value = F),
                                              div(id = "poisson_alternative",
                                                  numericInput(inputId = "poisson_lambda", label = HTML("&lambda; of the offset Poisson distribution"), value = poisson_mean, min = 0)
                                                  # sliderInput(inputId = "poisson_offset", label = "offset of Poisson", min = 0, 
                                                  #             max = ifelse(input$model_symmetry, choose(states_num, 2), choose(states_num, 2) * 2), 
                                                  #             value = ifelse(input$model_symmetry, states_num - 1, 0))
                                              ))
    } else if (input$delta_prior == "Beta-Binomial") {
      deltaprior_ui_list <- tagAppendChildren(deltaprior_ui_list, numericInput(inputId = "alpha_beta", label = "alpha of Beta", value = 1),
                                              numericInput(inputId = "beta_beta", label = "beta of Beta", value = 1, min = 0))
    }
    
    deltaprior_ui_list
  })
  
  # update the default poisson lambda
  observe({
    
    # req(input$geography_file)
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$delta_prior)
    req(states_dat())
    req(input$traitcolumn_name)
    req(input$model_symmetry)
    
    if (input$delta_prior == "Poisson" && (!is.null(input$poisson_default))) {
      shinyjs::toggleClass(id = "poisson_alternative", class = "divdisabled", condition = input$poisson_default)
      
      states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
      states <- states[states != "?"]
      states_num <- length(states)
      
      if (input$poisson_default) {
        
        if (input$model_symmetry == "symmetric") {
          poisson_lambda <- log(2)
        } else {
          poisson_lambda <- states_num - 1
        }
        updateNumericInput(session = session, inputId = "poisson_lambda", value = round(poisson_lambda, digits = 3))
        
      } else {
        
        if (input$model_symmetry == "symmetric") {
          poisson_lambda <- ceiling(choose(states_num, 2)/2) - (states_num - 1)
        } else if (input$model_symmetry == "asymmetric") {
          poisson_lambda <- choose(states_num, 2)
        }
        
        if (poisson_lambda < 1) {
          poisson_lambda <- 0.6931471805599453
        }
        
        updateNumericInput(session = session, inputId = "poisson_lambda", value = round(poisson_lambda, digits = 3))
      }
    }
  })
  
  # sanity check for the input poisson lambda value
  observe({
    
    # req(input$geography_file)
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$poisson_lambda)
    # req(input$poisson_offset)
    
    states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
    states <- states[states != "?"]
    states_num <- length(states)
    delta_max <- ((input$model_symmetry == "asymmetric") + 1) * choose(states_num, 2)
    
    if (input$poisson_lambda > delta_max || input$poisson_lambda <= 0) {
      showModal(modalDialog("Lambda of the Poisson prior needs to be within the possible range.",
                            title = "Invalid input",
                            easyClose = T,
                            size = "s"
                            
      ))
    }
    
    # if (input$poisson_offset > delta_max || input$poisson_offset < 0) {
    #   showModal(modalDialog("Offset of the Poisson prior needs to be within the possible range.",
    #                         title = "Invalid input",
    #                         easyClose = T,
    #                         size = "s"
    #                         
    #   ))
    # }
    # 
    # if (input$poisson_lambda + input$poisson_offset > delta_max) {
    #   showModal(modalDialog("Sum of lambda and offset of the Poisson prior shall not exceed the total possible number of dispersal routes.",
    #                         title = "Invalid input",
    #                         easyClose = T,
    #                         size = "s"
    #                         
    #   ))
    # }
    
  })
  
  # render mu prior ui according to the selected prior
  output$muprior_ui <- renderUI({
    # req(input$geography_file)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(parsimony_score$mean != 0)
    
    muprior_ui_list <- tagList()
    asterisk_num <- 1
    
    if (input$mu_prior == "Hierarchical Exponential") {
      muprior_ui_list <- tagAppendChildren(muprior_ui_list, numericInput(inputId = "alphaofgamma_mumean", 
                                                                         label = HTML("alpha of Gamma on the mean of &mu;"), value = 0.5, min = 0))
      
    } else if (input$mu_prior == "Empirical-Informed Exponential") {
      
      alpha <- 1 # effectively an exponential
      lower_bound <- 0.25
      # figuring out what the parameter(s) of the negative binomial distribution given the parsimony score to be a specified quantile
      fpnbinom <- function(x, parsimony_score, alpha, lower_bound) pnbinom(parsimony_score, size = alpha, prob = x) - lower_bound
      p_nbinom <- uniroot(fpnbinom, parsimony_score = parsimony_score$mean, alpha = alpha, lower_bound = lower_bound, 
                          lower = 1e-8, upper = 1 - 1e-8, tol = .Machine$double.eps, maxiter = 1e4)$root
      mean_nbinom <- (1 - p_nbinom) / p_nbinom * alpha
      
      muprior_ui_list <- tagAppendChildren(muprior_ui_list, 
                                           numericInput(inputId = "dispersaleventnum_NBmean", 
                                                        label = paste0("Expected number of dispersal events over the entire biogeographic history (as a reference, the parsimony score is ", 
                                                                       ceiling(parsimony_score$mean), " for this dataset)"), value = ceiling(mean_nbinom), min = 1),
                                           p(paste0(c(rep("*", asterisk_num), "The default value shown here puts the parsimony score at the lower quartile of the prior distribution."), collapse = ""), style = "font-size:90%;"))
      asterisk_num <- asterisk_num + 1
    }
    
    if (tree_values$tree_num > 1) {
      muprior_ui_list <- tagAppendChildren(muprior_ui_list, p(paste0(c(rep("*", asterisk_num), "To make the app more efficient, here the parsimony score is computed using the first tree of the imported distribution of trees."), collapse = ""), style = "font-size:90%;"))
    }
    
    muprior_ui_list
  })
  
  # don't show the option for computing the average parsimony score across all trees for now
  observe({
    # req(input$tree_file)
    req(tree_values$tree)
    # shinyjs::toggle(id = "parsimonyscore_alltree", condition = tree_values$tree_num > 1)
    shinyjs::toggle(id = "parsimonyscore_alltree", condition = F)
  })
  
  # sanity check for the mu prior associated input values
  observe({
    
    # req(input$geography_file)
    # req(input$tree_file)
    req(input$mu_prior)
    req(tree_values$tree)
    req(states_dat())
    req(parsimony_score$mean != 0)
    
    if (input$mu_prior == "Hierarchical Exponential" && (!is.null(input$alphaofgamma_mumean)) && (input$alphaofgamma_mumean <= 0)) {
      
      req(input$alphaofgamma_mumean) 
      
      if (input$alphaofgamma_mumean <= 0) {
        showModal(modalDialog("Shape and rate parameter of the Gamma distribution have to be positive.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
      }
    }
    
    if (input$mu_prior == "Empirical-Informed Exponential") {
      
      req(input$dispersaleventnum_NBmean)
      
      if (input$dispersaleventnum_NBmean <= 0) {
        showModal(modalDialog("Expected number of dispersal events has to be strictly positive.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
      }
    }
  })
  
  # render the proposal ui according to the model specification
  output$proposalweight_ui <- renderUI({
    # req(input$tree_file)
    req(tree_values$tree)
    # req(input$geography_file)
    req(states_dat())
    
    proposalweight_ui_list <- tagList()
    proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_r", label = "Weight of proposal on pairwise dispersal rates, r", 
                                                                                  value = input_default$proposalweight_r, min = 0))
    
    if (input$with_bssvs) {
      proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_delta", label = HTML("Weight of proposal on dispersal route indicators, &delta;"), 
                                                                                    value = input_default$proposalweight_delta, min = 0))
    }
    
    if (input$with_bssvs && input$delta_prior == "Beta-Binomial") {
      proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_pdeltaij", label = "Weight of proposal on individual dispersal route probability", 
                                                                                    value = input_default$proposalweight_pdeltaij, min = 0))
    }
    
    if (input$model_symmetry == "asymmetric") {
      proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_rootfreq", label = "Weight of proposal on root frequencies", 
                                                                                    value = input_default$proposalweight_rootfreq, min = 0))
    }
    
    proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_mu", label = HTML("Weight of proposal on average dispersal rate, &mu;"), 
                                                                                  value = input_default$proposalweight_mu, min = 0))
    if (input$mu_prior == "Hierarchical Exponential") {
      proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_mumean", label = HTML("Weight of proposal on mean of &mu;"), 
                                                                                    value = input_default$proposalweight_mumean, min = 0))
    } 
    
    if (tree_values$tree_num > 1) {
      proposalweight_ui_list <- tagAppendChild(proposalweight_ui_list, numericInput(inputId = "proposalweight_tree", label = "Weight of proposal on trees", 
                                                                                    value = input_default$proposalweight_tree, min = 0))
    }
    
    proposalweight_ui_list
  })
  
  # warning message for BEAST default priors
  observe({
    # req(input$tree_file)
    req(tree_values$tree)
    # req(input$geography_file)
    req(states_dat())
    req(input$delta_prior)
    
    if (input$delta_prior == "Poisson" && (!is.null(input$poisson_default)) && (input$poisson_default)) {
      showModal(modalDialog(paste0("Note that this prior is not recommended since it expresses an explicit and very informative preference for biogeographic models with the minimal number of dispersal routes. ",
                                   "For more details, see Gao et al. 2021."),
                            title = "Warning: unrecommended option",
                            easyClose = F,
                            size = "l"
                            
      ))
    }
  })
  
  observe({
    # req(input$tree_file)
    req(tree_values$tree)
    # req(input$geography_file)
    req(states_dat())
    req(input$mu_prior)
    
    if (input$mu_prior == "CTMC rate-ref (BEAST default)") {
      showModal(modalDialog(paste0("Note that this prior assumes the expected number of dispersal events across the entire tree is only 0.5 and the 95% prior interval is (0, 3), ",
                                   "regardless of the number of tips, the number of geographic areas, or the duration of the biogeographic history; ",
                                   "it is very informative (i.e., it may have strong impact on the posterior estimates) and unlikely to be biologically realistic, ",
                                   "so it's not recommended. For more details, see Gao et al. 2021"),
                            title = "Warning: unrecommended option",
                            easyClose = F,
                            size = "l"
      ))
    }
  })
  
  # discrete trait data xml chunk
  discretetraitdata_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    req(input$taxoncolumn_name)
    req(input$traitcolumn_name)
    req(lheats$num)
    
    lheat <- 1
    if ((!is.null(input$further_analysis)) && (input$further_analysis == "Data cloning") && (!is.null(input$lheats1))) {
      lheat <- sort(unique(as.integer(unlist(sapply(1:lheats$num, function(i) input[[paste0("lheats", i)]])))))
    }
    
    under_prior <- F
    if ((!is.null(input$further_analysis)) && (input$further_analysis == "Under prior")) under_prior <- T
    
    sapply(lheat, function(power) 
      xml_discretetraitdata(states_dat = states_dat(), taxon_name = input$taxoncolumn_name, discrete_trait_name = input$traitcolumn_name, 
                          lheat = power, under_prior = under_prior))
  })
  
  # store the current and previous version for comparison (so that we can highlight the changed part)
  discretetraitdataxml <- reactiveValues(last = NULL, current = NULL)
  observe({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    isolate(discretetraitdataxml$last <- discretetraitdataxml$current)
    discretetraitdataxml$current <- discretetraitdata_xml()[1]
    if (!maintabs_status$step2tab_clicked) {
      isolate(discretetraitdataxml$last <- discretetraitdataxml$current)
    }
  })
  
  # discrete trait model xml chunk
  discretetraitmodel_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    
    rates_proposal_weight <- 0
    if (!is.null(input$proposalweight_r)) rates_proposal_weight <- input$proposalweight_r
    
    rootfreq_proposal_weight <- 0
    if (input$model_symmetry == "asymmetric" && (!is.null(input$proposalweight_rootfreq))) rootfreq_proposal_weight <- input$proposalweight_rootfreq
    
    indicators_proposal_weight <- 0
    if (input$with_bssvs && (!is.null(input$proposalweight_delta))) indicators_proposal_weight <- input$proposalweight_delta
    
    delta_prior <- "Poisson"
    if (!is.null(input$delta_prior)) delta_prior <- input$delta_prior
    
    poisson_default <- F
    poisson_lambda <- 0
    if (input$with_bssvs && (!is.null(input$delta_prior)) && input$delta_prior == "Poisson" && (!is.null(input$poisson_default))) {
      poisson_default <- input$poisson_default
      if (!poisson_default && (!is.null(input$poisson_lambda))) poisson_lambda <- input$poisson_lambda
    }
    
    alpha_beta <- beta_beta <- indicatorprob_proposal_weight <- 0
    if (input$with_bssvs && input$delta_prior == "Beta-Binomial" && (!is.null(input$alpha_beta)) && (!is.null(input$beta_beta)) && (!is.null(input$proposalweight_pdeltaij))) {
      alpha_beta <- input$alpha_beta
      beta_beta <- input$beta_beta
      indicatorprob_proposal_weight <- input$proposalweight_pdeltaij
    }
    
    xml_discretetraitmodel(states_dat = states_dat(), discrete_trait_name = input$traitcolumn_name,
                           symmetry = (input$model_symmetry == "symmetric"), bssvs = input$with_bssvs, 
                           delta_prior = delta_prior, poisson_default = poisson_default, poisson_mean = poisson_lambda, 
                           alpha_beta = alpha_beta, beta_beta = beta_beta,
                           rates_proposal_weight = rates_proposal_weight, indicators_proposal_weight = indicators_proposal_weight, 
                           indicatorprob_proposal_weight = indicatorprob_proposal_weight, rootfreq_proposal_weight = rootfreq_proposal_weight)
  })
  
  # mu model xml chunk
  clockratemodel_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$mu_prior)
    
    clockratemean_proposal_weight <- 0
    clockrate_mean_gammashaperate <- 0.5
    if (input$mu_prior == "Hierarchical Exponential") {
      clockratemean_proposal_weight <- input$proposalweight_mumean
      clockrate_mean_gammashaperate <- input$alphaofgamma_mumean
    }
    
    clockrate_mean <- 1
    if (input$mu_prior == "Empirical-Informed Exponential" && (!is.null(input$dispersaleventnum_NBmean))) {
      clockrate_mean <- input$dispersaleventnum_NBmean/tree_values$tree_length
    }
    
    xml_clockratemodel(discrete_trait_name = input$traitcolumn_name, ctmc = (input$mu_prior == "CTMC rate-ref (BEAST default)"), 
                       clockrate_mean_stochastic = (input$mu_prior == "Hierarchical Exponential"), clockrate_mean_gammashaperate = clockrate_mean_gammashaperate,
                       clockrate_proposal_weight = input$proposalweight_mu, clockratemean_proposal_weight = clockratemean_proposal_weight, clockrate_mean = clockrate_mean)
  })
  
  # tree model xml chunk
  treemodel_xml <- reactive({
    
    # req(input$tree_file)
    req(tree_values$tree)
    if (tree_values$tree_num > 1) {
      req(input$proposalweight_tree)
      req(input$empiricaltree_mh)
    }

    tree_proposal_weight <- 0
    empiricaltree_mh <- F
    if (tree_values$tree_num > 1) {
      tree_proposal_weight <- input$proposalweight_tree
      empiricaltree_mh <- (input$empiricaltree_mh == input_default$empiricaltree_mh)
    }
    
    if (input$tree_file_defaultupload) {
      tree_name <- basename(tree_paths_defaultupload[1])
    } else {
      req(input$tree_file)
      tree_name <- input$tree_file$name
    }
    xml_treemodel(tree = tree_values$tree, treefile_name = tree_name,
                  tree_proposal_weight = tree_proposal_weight, empiricaltree_mh = empiricaltree_mh)
  })
  
  # put the model xml chunks together
  model_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    paste(treemodel_xml()$tree_model, clockratemodel_xml()$discretetrait_clock_rate, discretetraitmodel_xml()$substitution_model, sep = "\n")
  })
  
  # put the prior xml chunks together
  prior_xml <- reactive({
    paste0("\t<prior id=\"prior\">\n", 
           clockratemodel_xml()$clock_rate_prior, discretetraitmodel_xml()$substmodel_priors, 
           "\t</prior>\n")
  })
  
  modelprior_xml <- reactive({
    paste(model_xml(), prior_xml(), sep = "\n")
  })
  
  modelprior_xml_d <- debounce(modelprior_xml, millis = 1000)
  modelpriorxml <- reactiveValues(last = NULL, current = NULL)
  observeEvent(modelprior_xml_d(), {
    
    modelpriorxml$last <- modelpriorxml$current
    modelpriorxml$current <- modelprior_xml_d()
    # if (!maintabs_status$step2tab_clicked) {
    #   isolate(modelpriorxml$last <- modelpriorxml$current)
    # }
  })
  
  # put the proposal xml chunks together
  moves_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    paste0("\t<operators id=\"operators\" optimizationSchedule=\"log\">\n", 
           treemodel_xml()$tree_proposal, clockratemodel_xml()$clock_rate_proposal, discretetraitmodel_xml()$substmodel_proposals, 
           "\t</operators>\n")
  })
  
  moves_xml_d <- debounce(moves_xml, millis = 1000)
  movesxml <- reactiveValues(last = NULL, current = NULL)
  observeEvent(moves_xml_d(), {
    
    movesxml$last <- movesxml$current
    movesxml$current <- moves_xml_d()
    # if (!maintabs_status$step3tab_clicked) {
    #   isolate(movesxml$last <- movesxml$current)
    # }
  })
  
  # phyloctmc chunk (phylogenetic likelihood and stochastic mapping)
  phyloctmc_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$traitcolumn_name)
    
    lheat <- 1
    if ((!is.null(input$further_analysis)) && (input$further_analysis == "Data cloning") && (!is.null(input$lheats1))) lheat <- input$lheats1
    
    do_totalcount <- do_pairwisecount <- F
    if ((!is.null(input$do_stochasticmapping)) && input$do_stochasticmapping != "") {
      do_totalcount <- input$markovjumps_total
      do_pairwisecount <- input$markovjumps_pairwise
    }
    
    xml_phyloctmc(states_dat = states_dat(), discrete_trait_name = input$traitcolumn_name, rootfreq_model = discretetraitmodel_xml()$rootfreq_model,
                  lheat = lheat, symmetry = (input$model_symmetry == "symmetric"),
                  complete_history = (input$do_stochasticmapping == "Stochastic mapping (complete history, simulation-based)"),
                  do_totalcount = do_totalcount, do_pairwisecount = do_pairwisecount)
  })
  
  phyloctmcxml <- reactiveValues(last = NULL, current = NULL)
  observe({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    isolate(phyloctmcxml$last <- phyloctmcxml$current)
    phyloctmcxml$current <- phyloctmc_xml()
    if (!maintabs_status$step4tab_clicked) {
      isolate(phyloctmcxml$last <- phyloctmcxml$current)
    }
  })
  
  # posterior xml chunk
  posterior_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$traitcolumn_name)
    
    prior <- "\t\t<prior idref=\"prior\">\n"
    likelihood <- paste0("\t\t\t<likelihood id=\"likelihood\">\n", 
                         "\t\t\t\t<markovJumpsTreeLikelihood idref=\"", input$traitcolumn_name, ".treeLikelihood\"/>\n",
                         "\t\t\t</likelihood>\n")
    paste0("\t\t<posterior id=\"posterior\">\n", prior, likelihood, "\t\t</posterior>\n")
  })
  
  # note that this will be a list of xml chunks as there could be multiple replicates or data cloning analyses
  monitors_xml <- reactive({
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    ml_numstones <- ml_chainlengthperstone <- ml_samplingfreq <- ml_alphaofbeta <- 0
    if ((!is.null(input$further_analysis)) && ("Marginal likehood estimation" %in% input$further_analysis)) {
      if (!is.null(input$ml_numstones)) ml_numstones <- input$ml_numstones
      if (!is.null(input$ml_chainlengthperstone)) ml_chainlengthperstone <- input$ml_chainlengthperstone
      if (!is.null(input$ml_samplingfreq)) ml_samplingfreq <- input$ml_samplingfreq
      if (!is.null(input$ml_alphaofbeta)) ml_alphaofbeta <- input$ml_alphaofbeta
    }
    
    lheat <- 1
    if ((!is.null(input$further_analysis)) && (input$further_analysis == "Data cloning") && (!is.null(input$lheats1))) lheat <- input$lheats1
    
    delta_prior <- "Poisson"
    if (!is.null(input$delta_prior)) delta_prior <- input$delta_prior
    
    lapply(output_file$file_name, function(file_name)
      xml_monitors(discrete_trait_name = input$traitcolumn_name, file_name = gsub("_mlafterposterior", "_posterior", file_name),
                 currentTree_output = treemodel_xml()$currentTree_output, bssvs = input$with_bssvs, lheat = lheat,
                 complete_history = (input$do_stochasticmapping == "Stochastic mapping (complete history, simulation-based)"), 
                 ctmc = (input$mu_prior == "CTMC rate-ref (BEAST default)"), clockrate_mean_stochastic = (input$mu_prior == "Hierarchical Exponential"),
                 symmetry = (input$model_symmetry == "symmetric"), 
                 delta_prior = delta_prior, mcmc_samplingfreq = input$mcmc_samplingfreq,
                 ml_numstones = ml_numstones, ml_chainlengthperstone = ml_chainlengthperstone, ml_samplingfreq = ml_samplingfreq, ml_alphaofbeta = ml_alphaofbeta))
    
  })
  
  powerposteriorxml <- reactiveValues(last = NULL, current = NULL)
  observe({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    isolate(powerposteriorxml$last <- powerposteriorxml$current)
    powerposteriorxml$current <- monitors_xml()[[1]]$log_powerposterior
    
    if (input$further_analysis != "Marginal likehood estimation" || (!xmlviewertabs_status$powerposteriortab_clicked)) {
      isolate(powerposteriorxml$last <- powerposteriorxml$current <- "")
    }
    
    # if ((input$further_analysis != "Marginal likehood estimation") || (input$xmlviewer_tabs != "xmlpowerposterior") || (!xmlviewertabs_status$powerposteriortab_clicked)) {
    #   isolate(powerposteriorxml$last <- powerposteriorxml$current)
    # }
  })
  
  # mcmc xml chunk
  # note that this can be a vector of mcmc xml chunks as there could be multiple replicates
  mcmc_xml <- reactive({
    sapply(monitors_xml(), function(x) 
      paste0("\t<mcmc id=\"mcmc\" chainLength=\"", as.integer(input$mcmc_chainlength), "\" autoOptimize=\"true\">\n",
                                              posterior_xml(), "\t\t<operators idref=\"operators\">\n\n", 
                                              x$log_jointposterior, "\t</mcmc>\n"))
  })
  
  mcmcxml <- reactiveValues(last = NULL, current = NULL)
  observe({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    isolate(mcmcxml$last <- mcmcxml$current)
    mcmcxml$current <- mcmc_xml()[1]
    if (!maintabs_status$step3tab_clicked) {
      isolate(mcmcxml$last <- mcmcxml$current)
    }
  })
  
  
  # put all the xml chunk together
  # note that this can be a vector as there could be muliple data cloning analyses and replicates
  full_xml <- reactive({
    
    fullxml_reps <- c()
    for (i in 1:lheats$num) {
      for (j in 1:input$mcmc_numreplicates) {
        fullxml_reps <- c(fullxml_reps, paste("<?xml version=\"1.0\" standalone=\"yes\"?>", "<beast>",
                                              discretetraitdata_xml()[i], model_xml(), prior_xml(), moves_xml(), phyloctmc_xml(), mcmc_xml()[j], 
                                              monitors_xml()[[j]]$log_powerposterior,
                                              "\t<report>\n\t\t<property name=\"timer\">\n\t\t\t<mcmc idref=\"mcmc\"/>\n\t\t</property>\n\t</report>", 
                                              "</beast>", sep = "\n"))
      }
    }
    
    return(fullxml_reps)
  })
  
  fullxml <- reactiveValues(last = NULL, current = NULL)
  observe({

    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)

    isolate(fullxml$last <- fullxml$current)
    fullxml$current <- full_xml()[1]
    if (!maintabs_status$step2tab_clicked) {
      isolate(fullxml$last <- fullxml$current)
    }
  })
  
  
  # render data xml ui
  output$beastxml_data <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = discretetraitdataxml$last, current_txt = discretetraitdataxml$current, file_ext = "_data.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_data' class='language-markup'>", gsub("<", "&lt;", discretetraitdata_xml()[1]), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_data')[0], true);")
    )
  })
  
  # render model and prior xml ui
  output$beastxml_modelprior <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = modelpriorxml$last, current_txt = modelpriorxml$current, file_ext = "_modelprior.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_modelprior' class='language-markup'>", gsub("<", "&lt;", modelprior_xml()), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_modelprior')[0], true);")
    )
  })
  
  # render proposal/operators xml ui
  output$beastxml_proposal <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = movesxml$last, current_txt = movesxml$current, file_ext = "_moves.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_proposal' class='language-markup'>", gsub("<", "&lt;", moves_xml()), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_proposal')[0], true);")
    )
  })
  
  # render mcmc xml ui
  output$beastxml_mcmc <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = mcmcxml$last, current_txt = mcmcxml$current, file_ext = "_mcmc.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_mcmc' class='language-markup'>", gsub("<", "&lt;", mcmc_xml()[1]), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_mcmc')[0], true);")
    )
  })
  
  # render phyloctmc/treelikelihood xml ui
  output$beastxml_phyloctmc <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = phyloctmcxml$last, current_txt = phyloctmcxml$current, file_ext = "_phyloctmc.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_phyloctmc' class='language-markup'>", gsub("<", "&lt;", phyloctmc_xml()), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_phyloctmc')[0], true);")
    )
  })
  
  # render power posterior/marginal likelihood xml ui
  output$beastxml_powerposterior <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = powerposteriorxml$last, current_txt = powerposteriorxml$current, file_ext = "_powerposterior.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_powerposterior' class='language-markup'>", gsub("<", "&lt;", monitors_xml()[[1]]$log_powerposterior), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_powerposterior')[0], true);")
    )
  })
  
  # render full xml ui
  output$beastxml_full <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    
    dataline_str <- getDifferedLines(last_txt = fullxml$last, current_txt = fullxml$current, file_ext = "_full.txt")
    
    tagList(
      HTML(paste0("<pre class='line-numbers'", dataline_str, "><code id='xml_full' class='language-markup'>", gsub("<", "&lt;", full_xml()[1]), "</code></pre>")),
      tags$script("Prism.highlightElement($('#xml_full')[0], true);")
    )
  })
  
  
  # Download XML
  output$downloadxml_real <- downloadHandler(
    
    filename = function() {
      if (length(output_file$file_name) == 1) {
        paste0(output_file$file_name, ".xml")
      } else {
        paste0(input$xml_name, ".zip")
      }
    },
    
    content = function(file) {
      # add text methods description at the very beginning
      methods <- paste0("<!--Generated by PrioriTree-->\n<!--\n<h2>Materials and Methods</h2>", full_text$html, "\n-->\n\n")
      
      if (length(full_xml()) == 1) {
        writeLines(paste0(methods, full_xml()[1]), file)
      } else {
        
        tmppaths <- c()
        for (i in 1:length(full_xml())) {
          tmppaths <- c(tmppaths, tempfile(fileext = ".xml"))
          writeLines(paste0(methods, full_xml()[i]), tmppaths[i])
          file.rename(tmppaths[i], paste0(output_file$file_name[i], ".xml"))
        }
        
        zip(zipfile = file, files = paste0(output_file$file_name, ".xml"), flags = "-j")
        file.remove(paste0(output_file$file_name, ".xml"))
      }
    }
  )
  
  # Download XML (the confirm button that effectively triggers the same downloading effect)
  output$downloadxml_faketoreal <- downloadHandler(
    
    filename = function() {
      if (length(output_file$file_name) == 1) {
        paste0(output_file$file_name, ".xml")
      } else {
        paste0(input$xml_name, ".zip")
      }
    },
    
    content = function(file) {
      
      # dismiss the warning sign first
      removeModal()
      # add text methods description at the very beginning
      methods <- paste0("<!--Generated by PrioriTree-->\n<!--\n<h2>Materials and Methods</h2>", full_text$html, "\n-->\n\n")
      
      if (length(full_xml()) == 1) {
        writeLines(paste0(methods, full_xml()[1]), file)
      } else {
        
        tmppaths <- c()
        for (i in 1:length(full_xml())) {
          tmppaths <- c(tmppaths, tempfile(fileext = ".xml"))
          writeLines(paste0(methods, full_xml()[i]), tmppaths[i])
          file.rename(tmppaths[i], paste0(output_file$file_name[i], ".xml"))
        }
        
        zip(zipfile = file, files = paste0(output_file$file_name, ".xml"), flags = "-j")
        file.remove(paste0(output_file$file_name, ".xml"))
      }
    }
  )
  
  
  # methods text: data
  data_text <- reactiveValues(html = "", tex = "", md = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
    states <- states[states != "?"]
    states_num <- length(states)
    taxa_num <- nrow(states_dat())
    
    data_text$html <- tex_data(states_num = states_num, taxa_num = taxa_num, format = "HTML")
    data_text$tex <- tex_data(states_num = states_num, taxa_num = taxa_num, format = "Latex")
    data_text$md <- tex_data(states_num = states_num, taxa_num = taxa_num, format = "Markdown")
  })

  output$methods_data <- renderUI({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    withMathJax(HTML(paste0("<div style = 'font-size: 90%;'>", data_text$html, "</div>")))
  })
  
  # methods text: model
  model_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "", tmppath = NULL)
  observe({

    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$model_symmetry)

    model_text$html <- tex_model(bssvs = input$with_bssvs, symmetry = (input$model_symmetry == "symmetric"), format = "HTML")
    model_text$tex <- tex_model(bssvs = input$with_bssvs, symmetry = (input$model_symmetry == "symmetric"), format = "Latex")
    model_text$md <- tex_model(bssvs = input$with_bssvs, symmetry = (input$model_symmetry == "symmetric"), format = "Markdown")
    model_text$htmlviewer <- tex_model(bssvs = input$with_bssvs, symmetry = (input$model_symmetry == "symmetric"), format = "HTML", render_citation = F)
  })

  output$methods_model <- renderUI({

    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())

    withMathJax(HTML(paste0("<div style = 'font-size: 90%;'>", model_text$htmlviewer, "</div>")))
    
    # if (is.null(model_text$tmppath)) {
    #   model_text$tmppath <- tempfile(fileext = ".md")
    # }
    # tmppath <- model_text$tmppath
    # 
    # out_tmp <- paste0("---\ntitle: ''\noutput: md_document\nbibliography: literature.bib\n---\n", model_text$md, "\n\n")
    # writeLines(out_tmp, tmppath)
    # 
    # out <- rmarkdown::render(tmppath, output_format = "md_document")
    # div(style = 'font-size: 90%;', includeMarkdown(out))
  })
  
  # methods text: prior
  prior_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "", tmppath = NULL)
  observe({

    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$model_symmetry)
    req(input$mu_prior)
    
    states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
    states <- states[states != "?"]
    states_num <- length(states)
    
    deltaprior_text <- list(html = "", tex = "", md = "", htmlviewer = "")
    if (input$with_bssvs) {
      
      poisson_default <- F
      poisson_lambda <- log(2)
      if ((input$delta_prior == "Poisson") && (!is.null(input$poisson_default))) {
        poisson_default <- input$poisson_default
        
        if (poisson_default) {
          if (input$model_symmetry == "symmetric") {
            poisson_lambda <- log(2)
          } else {
            poisson_lambda <- states_num - 1
          }
        } else if (!poisson_default && (!is.null(input$poisson_lambda))) {
          poisson_lambda <- input$poisson_lambda
        }
      }

      alpha_beta <- beta_beta <- 0
      if (input$with_bssvs && input$delta_prior == "Beta-Binomial" && (!is.null(input$alpha_beta)) && (!is.null(input$beta_beta))) {
        alpha_beta <- input$alpha_beta
        beta_beta <- input$beta_beta
      }
      
      states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
      states <- states[states != "?"]
      states_num <- length(states)
      
      deltaprior_text$html <- tex_deltaprior(states_num = states_num, symmetry = (input$model_symmetry == "symmetric"), 
                                        delta_prior = input$delta_prior, poisson_default = poisson_default, poisson_lambda = poisson_lambda,
                                        alpha_beta = alpha_beta, beta_beta = beta_beta, format = "HTML")
      deltaprior_text$tex <- tex_deltaprior(states_num = states_num, symmetry = (input$model_symmetry == "symmetric"), 
                                       delta_prior = input$delta_prior, poisson_default = poisson_default, poisson_lambda = poisson_lambda,
                                       alpha_beta = alpha_beta, beta_beta = beta_beta, format = "Latex")
      deltaprior_text$md <- tex_deltaprior(states_num = states_num, symmetry = (input$model_symmetry == "symmetric"), 
                                      delta_prior = input$delta_prior, poisson_default = poisson_default, poisson_lambda = poisson_lambda,
                                      alpha_beta = alpha_beta, beta_beta = beta_beta, format = "Markdown")
      deltaprior_text$htmlviewer <- tex_deltaprior(states_num = states_num, symmetry = (input$model_symmetry == "symmetric"), 
                                             delta_prior = input$delta_prior, poisson_default = poisson_default, poisson_lambda = poisson_lambda,
                                             alpha_beta = alpha_beta, beta_beta = beta_beta, format = "HTML", render_citation = F)
    }
    
    hierachexp_alphaofgamma <- 0.5
    hierachexp_dispersaleventsnummean <- 1
    if (input$mu_prior == "Hierarchical Exponential") {
      hierachexp_alphaofgamma <- input$alphaofgamma_mumean
      hierachexp_dispersaleventsnummean <- hierarchexp$dispersaleventsnum_mean
    }

    empinformed_dispersaleventsnummean <- 1
    parsimonyscore <- 1
    if (input$mu_prior == "Empirical-Informed Exponential" && (!is.null(input$dispersaleventnum_NBmean))) {
      empinformed_dispersaleventsnummean <- input$dispersaleventnum_NBmean
      parsimonyscore <- parsimony_score$mean
    }
    
    muprior_text <- list(html = "", tex = "", md = "", htmlviewer = "")
    muprior_text$html <- tex_muprior(mu_prior = input$mu_prior, tree_length = tree_values$tree_length, 
                                hierachexp_alphaofgamma = hierachexp_alphaofgamma, hierachexp_dispersaleventsnummean = hierachexp_dispersaleventsnummean, 
                                empinformed_dispersaleventsnummean = empinformed_dispersaleventsnummean, parsimony_score = parsimonyscore, format = "HTML")
    muprior_text$tex <- tex_muprior(mu_prior = input$mu_prior, tree_length = tree_values$tree_length, 
                               hierachexp_alphaofgamma = hierachexp_alphaofgamma, hierachexp_dispersaleventsnummean = hierachexp_dispersaleventsnummean, 
                               empinformed_dispersaleventsnummean = empinformed_dispersaleventsnummean, parsimony_score = parsimonyscore, format = "Latex")
    muprior_text$md <- tex_muprior(mu_prior = input$mu_prior, tree_length = tree_values$tree_length,
                              hierachexp_alphaofgamma = hierachexp_alphaofgamma, hierachexp_dispersaleventsnummean = hierachexp_dispersaleventsnummean, 
                              empinformed_dispersaleventsnummean = empinformed_dispersaleventsnummean, parsimony_score = parsimonyscore, format = "Markdown")
    muprior_text$htmlviewer <- tex_muprior(mu_prior = input$mu_prior, tree_length = tree_values$tree_length, 
                                     hierachexp_alphaofgamma = hierachexp_alphaofgamma, hierachexp_dispersaleventsnummean = hierachexp_dispersaleventsnummean, 
                                     empinformed_dispersaleventsnummean = empinformed_dispersaleventsnummean, parsimony_score = parsimonyscore, format = "HTML", render_citation = F)
    
    prior_text$html <- tex_prior(deltaprior_text = deltaprior_text$html, muprior_text = muprior_text$html, format = "HTML")
    prior_text$tex <- tex_prior(deltaprior_text = deltaprior_text$tex, muprior_text = muprior_text$tex, format = "Latex")
    prior_text$md <- tex_prior(deltaprior_text = deltaprior_text$md, muprior_text = muprior_text$md, format = "Markdown")
    prior_text$htmlviewer <- tex_prior(deltaprior_text = deltaprior_text$htmlviewer, muprior_text = muprior_text$htmlviewer, format = "HTML")
  })
  
  # methods text: bayesian inference
  bayesianinference_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$empiricaltree_mh)
    
    empiricaltree_mh <- F
    if (tree_values$tree_num > 1) {
      empiricaltree_mh <- (input$empiricaltree_mh == input_default$empiricaltree_mh)
    }
    
    bayesianinference_text$html <- tex_bayesianinference(empiricaltree_mh = empiricaltree_mh, tree_num = tree_values$tree_num,
                                                         bssvs = input$with_bssvs, format = "HTML")
    bayesianinference_text$tex <- tex_bayesianinference(empiricaltree_mh = empiricaltree_mh, tree_num = tree_values$tree_num,
                                                         bssvs = input$with_bssvs, format = "Latex")
    bayesianinference_text$md <- tex_bayesianinference(empiricaltree_mh = empiricaltree_mh, tree_num = tree_values$tree_num,
                                                        bssvs = input$with_bssvs, format = "Markdown")
    bayesianinference_text$htmlviewer <- tex_bayesianinference(empiricaltree_mh = empiricaltree_mh, tree_num = tree_values$tree_num,
                                                         bssvs = input$with_bssvs, format = "HTML", render_citation = F)
  })
  
  # prior and bayesian inference
  output$methods_prior <- renderUI({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    withMathJax(HTML(paste0("<div style = 'font-size: 90%;'>", prior_text$htmlviewer, "<br>", bayesianinference_text$htmlviewer, "</div>")))
    
    # if (is.null(prior_text$tmppath)) {
    #   prior_text$tmppath <- tempfile(fileext = ".md")
    # }
    # tmppath <- prior_text$tmppath
    # 
    # out_tmp <- paste0("---\ntitle: ''\noutput: md_document\nbibliography: literature.bib\n---\n", 
    #                    prior_text$md, "\n", bayesianinference_text$md, "\n\n")
    # writeLines(out_tmp, tmppath)
    # 
    # out <- rmarkdown::render(tmppath, output_format = "md_document")
    # div(style = 'font-size: 90%;', includeMarkdown(out))
  })
  
  # methods text: analysis
  posterioranalysis_text <- reactiveValues(html = "", tex = "", md = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$mcmc_numreplicates)
    
    posterioranalysis_text$html <- tex_posterioranalysis(mcmc_chainlength = as.integer(input$mcmc_chainlength),
                                                         mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                                         mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "HTML")
    posterioranalysis_text$tex <- tex_posterioranalysis(mcmc_chainlength = as.integer(input$mcmc_chainlength),
                                                         mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                                        mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Latex")
    posterioranalysis_text$md <- tex_posterioranalysis(mcmc_chainlength = as.integer(input$mcmc_chainlength),
                                                       mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                                       mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Markdown")
  })

  summarystats_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    do_totalcount <- do_pairwisecount <- F
    if ((!is.null(input$do_stochasticmapping)) && input$do_stochasticmapping != "") {
      do_totalcount <- input$markovjumps_total
      do_pairwisecount <- input$markovjumps_pairwise
      
      summarystats_text$html <- tex_summarystats(do_stochasticmapping = input$do_stochasticmapping,
                                            markovjumps_total = do_totalcount, markovjumps_pairwise = do_pairwisecount, format = "HTML")
      summarystats_text$tex <- tex_summarystats(do_stochasticmapping = input$do_stochasticmapping,
                                            markovjumps_total = do_totalcount, markovjumps_pairwise = do_pairwisecount, format = "Latex")
      summarystats_text$md <- tex_summarystats(do_stochasticmapping = input$do_stochasticmapping,
                                           markovjumps_total = do_totalcount, markovjumps_pairwise = do_pairwisecount, format = "Markdown")
      summarystats_text$htmlviewer <- tex_summarystats(do_stochasticmapping = input$do_stochasticmapping,
                                                 markovjumps_total = do_totalcount, markovjumps_pairwise = do_pairwisecount, format = "HTML", render_citation = F)
    }
  })
  
  powerposterior_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$mcmc_numreplicates)
    
    ml_numstones <- ml_chainlengthperstone <- ml_samplingfreq <- ml_alphaofbeta <- 0
    if ((!is.null(input$further_analysis)) && ("Marginal likehood estimation" %in% input$further_analysis)) {
      if (!is.null(input$ml_numstones)) ml_numstones <- input$ml_numstones
      if (!is.null(input$ml_chainlengthperstone)) ml_chainlengthperstone <- input$ml_chainlengthperstone
      if (!is.null(input$ml_samplingfreq)) ml_samplingfreq <- input$ml_samplingfreq
      if (!is.null(input$ml_alphaofbeta)) ml_alphaofbeta <- input$ml_alphaofbeta
      
      powerposterior_text$html <- tex_powerposterior(ml_numstones = ml_numstones, ml_chainlengthperstone = ml_chainlengthperstone,
                                                     ml_samplingfreq = ml_samplingfreq, ml_alphaofbeta = ml_alphaofbeta, 
                                                     mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "HTML")
      powerposterior_text$tex <- tex_powerposterior(ml_numstones = ml_numstones, ml_chainlengthperstone = ml_chainlengthperstone,
                                                     ml_samplingfreq = ml_samplingfreq, ml_alphaofbeta = ml_alphaofbeta, 
                                                    mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Latex")
      powerposterior_text$md <- tex_powerposterior(ml_numstones = ml_numstones, ml_chainlengthperstone = ml_chainlengthperstone,
                                                    ml_samplingfreq = ml_samplingfreq, ml_alphaofbeta = ml_alphaofbeta, 
                                                   mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Markdown")
      powerposterior_text$htmlviewer <- tex_powerposterior(ml_numstones = ml_numstones, ml_chainlengthperstone = ml_chainlengthperstone,
                                                     ml_samplingfreq = ml_samplingfreq, ml_alphaofbeta = ml_alphaofbeta, 
                                                     mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "HTML", render_citation = F)
    }
  })
  
  prioranalysis_text <- reactiveValues(html = "", tex = "", md = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$mcmc_numreplicates)
    
    prioranalysis_text$html <- tex_prioranalysis(mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                                 mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                                 mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "HTML")
    prioranalysis_text$tex <- tex_prioranalysis(mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                                mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                                mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Latex")
    prioranalysis_text$md <- tex_prioranalysis(mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                               mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                               mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Markdown")
  })
  
  dcanalysis_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "")
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    req(input$mcmc_numreplicates)
    
    lheat <- 1
    if ((!is.null(input$further_analysis)) && (input$further_analysis == "Data cloning") && (!is.null(input$lheats1))) {
      lheat <- sort(unique(as.integer(unlist(sapply(1:lheats$num, function(i) input[[paste0("lheats", i)]])))))
      
      dcanalysis_text$html <- tex_dcanalysis(lheat = lheat, mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                             mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                             mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "HTML")
      dcanalysis_text$tex <- tex_dcanalysis(lheat = lheat, mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                             mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                            mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Latex")
      dcanalysis_text$md <- tex_dcanalysis(lheat = lheat, mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                           mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                           mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "Markdown")
      dcanalysis_text$htmlviewer <- tex_dcanalysis(lheat = lheat, mcmc_chainlength = as.integer(input$mcmc_chainlength), 
                                             mcmc_samplingfreq = as.integer(input$mcmc_samplingfreq), 
                                             mcmc_numreplicates = as.integer(input$mcmc_numreplicates), format = "HTML", render_citation = F)
    }
  })
  
  analysis_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "", tmppath = NULL)
  observe({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    analysis_text$html <- tex_analysis(further_analysis = input$further_analysis, posterioranalysis_text = posterioranalysis_text$html, 
                                       summarystats_text = summarystats_text$html, powerposterior_text = powerposterior_text$html, 
                                       prioranalysis_text = prioranalysis_text$html, dcanalysis_text = dcanalysis_text$html, format = "HTML")
    analysis_text$tex <- tex_analysis(further_analysis = input$further_analysis, posterioranalysis_text = posterioranalysis_text$tex, 
                                       summarystats_text = summarystats_text$tex, powerposterior_text = powerposterior_text$tex, 
                                       prioranalysis_text = prioranalysis_text$tex, dcanalysis_text = dcanalysis_text$tex, format = "Latex")
    analysis_text$md <- tex_analysis(further_analysis = input$further_analysis, posterioranalysis_text = posterioranalysis_text$md, 
                                      summarystats_text = summarystats_text$md, powerposterior_text = powerposterior_text$md, 
                                      prioranalysis_text = prioranalysis_text$md, dcanalysis_text = dcanalysis_text$md, format = "Markdown")
    analysis_text$htmlviewer <- tex_analysis(further_analysis = input$further_analysis, posterioranalysis_text = posterioranalysis_text$html, 
                                       summarystats_text = summarystats_text$htmlviewer, powerposterior_text = powerposterior_text$htmlviewer, 
                                       prioranalysis_text = prioranalysis_text$html, dcanalysis_text = dcanalysis_text$htmlviewer, format = "HTML")
  })
  
  # render analysis ui
  output$methods_analysis <- renderUI({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    withMathJax(HTML(paste0("<div style = 'font-size: 90%;'>", analysis_text$htmlviewer, "</div>")))
    
    # if (is.null(analysis_text$tmppath)) {
    #   analysis_text$tmppath <- tempfile(fileext = ".md")
    # }
    # tmppath <- analysis_text$tmppath
    # 
    # out_tmp <- paste0("---\ntitle: ''\noutput: md_document\nbibliography: literature.bib\n---\n",
    #                    analysis_text$md, "\n\n")
    # writeLines(out_tmp, tmppath)
    # 
    # out <- rmarkdown::render(tmppath, output_format = "md_document")
    # div(style = 'font-size: 90%;', includeMarkdown(out))
  })
  
  # methods text: full
  full_text <- reactiveValues(html = "", tex = "", md = "", htmlviewer = "", tmppath = NULL)
  observe({
    full_text$html <- paste(data_text$html, model_text$html, prior_text$html, bayesianinference_text$html, analysis_text$html, sep = "<br>")
    full_text$tex <- paste(data_text$tex, model_text$tex, prior_text$tex, bayesianinference_text$tex, analysis_text$tex, sep = "\n")
    full_text$md <- paste(data_text$md, model_text$md, prior_text$md, bayesianinference_text$md, analysis_text$md, sep = "\n")
    full_text$htmlviewer <- paste(data_text$html, model_text$htmlviewer, prior_text$htmlviewer, bayesianinference_text$htmlviewer, analysis_text$htmlviewer, sep = "<br>")
  })
  
  output$methods_full <- renderUI({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    req(input$taxoncolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(states_dat())
    
    withMathJax(HTML(paste0("<div style = 'font-size: 90%;'>", full_text$htmlviewer, "</div>")))
    
    # if (is.null(full_text$tmppath)) {
    #   full_text$tmppath <- tempfile(fileext = ".html")
    # }
    # html_tmppath <- full_text$tmppath
    # 
    # html_tmp <- paste0("---\ntitle: ''\noutput: html_document\nbibliography: literature.bib\n---\n", 
    #                    full_text$html, "<br><h5>References</h5>")
    # writeLines(html_tmp, html_tmppath)
    # 
    # out <- rmarkdown::render(html_tmppath, output_format = "md_document")
    # div(style = 'font-size: 90%;', includeMarkdown(out))
  })
  
  # methods text download
  # single file when the output format is not latex and no replicate or multiple clone numbers
  # otherwise would be zipped folder
  output$downloadmethods_real <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$downloadmethods_format, "LaTex" = ".zip", "PDF" = ".pdf", "Word" = ".docx", "HTML" = ".html", "Markdown" = ".md")
      paste0(input$methods_name, file_extension)
    },
    content = function(file) {
      
      template_title <- "Materials and Methods"
      md_css <- "<style type='text/css'>\nh1.title {font-size: 20px;}\n</style>\n"
      
      if (input$downloadmethods_format == "LaTex") {
        
        tex_begin <- paste0("\\documentclass[11pt, onecolumn]{article}\n", "\\usepackage[top=0.9in, bottom=0.9in, left=0.9in, right=0.9in]{geometry}\n",
                            "\\usepackage{amsmath}\n", "\\usepackage{amsmath}\n", "\\usepackage{amssymb}\n", "\\usepackage{natbib}\n\n", "\\begin{document}\n\n")
        tex_end <- paste0("\n\\bibliographystyle{apalike}\n\\bibliography{literature.bib}\n\n\\end{document}\n")

        tex_tmppath <- tempfile(fileext = ".tex")
        writeLines(paste0(tex_begin, full_text$tex, tex_end), tex_tmppath)
        file.rename(tex_tmppath, paste0(input$methods_name, ".tex"))
        
        zip(zipfile = file, files = c("literature.bib", paste0(input$methods_name, ".tex")), flags = "-j")
        file.remove(paste0(input$methods_name, ".tex"))
        
      } else if (input$downloadmethods_format == "HTML") {
        
        html_tmp <- paste0("---\ntitle: '", template_title, "'\noutput: html_document\nbibliography: literature.bib\n---\n", 
                           md_css, full_text$html, "<br><br><h5>References</h5>")
        html_tmppath <- tempfile(fileext = ".html")
        writeLines(html_tmp, html_tmppath)
        
        out <- rmarkdown::render(html_tmppath, output_format = "html_document")
        file.rename(out, file)
        
      } else if (input$downloadmethods_format %in% c("Markdown", "PDF", "Word")) {
        
        md_tmp <- paste0("---\ntitle: '", template_title, "'\noutput: md_document\nbibliography: literature.bib\n---\n", 
                         md_css, full_text$md, "\n\n#### References\n")
        md_tmppath <- tempfile(fileext = ".md")
        writeLines(md_tmp, md_tmppath)
        
        out_format <- switch(input$downloadmethods_format, "PDF" = "pdf_document", "Word" = "word_document", "Markdown" = "md_document")
        out <- rmarkdown::render(md_tmppath, output_format = out_format)
        file.rename(out, file)
      }
    }
  )
  
  output$downloadmethods_faketoreal <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$downloadmethods_format, "LaTex" = ".zip", "PDF" = ".pdf", "Word" = ".docx", "HTML" = ".html", "Markdown" = ".md")
      paste0(input$methods_name, file_extension)
    },
    content = function(file) {
      
      removeModal()
      template_title <- "Materials and Methods"
      md_css <- "<style type='text/css'>\nh1.title {font-size: 20px;}\n</style>\n"
      
      if (input$downloadmethods_format == "LaTex") {
        
        tex_begin <- paste0("\\documentclass[11pt, onecolumn]{article}\n", "\\usepackage[top=0.9in, bottom=0.9in, left=0.9in, right=0.9in]{geometry}\n",
                            "\\usepackage{amsmath}\n", "\\usepackage{amsmath}\n", "\\usepackage{amssymb}\n", "\\usepackage{natbib}\n\n", "\\begin{document}\n\n")
        tex_end <- paste0("\n\\bibliographystyle{apalike}\n\\bibliography{literature.bib}\n\n\\end{document}\n")
        
        tex_tmppath <- tempfile(fileext = ".tex")
        writeLines(paste0(tex_begin, full_text$tex, tex_end), tex_tmppath)
        file.rename(tex_tmppath, paste0(input$methods_name, ".tex"))
        
        zip(zipfile = file, files = c("literature.bib", paste0(input$methods_name, ".tex")), flags = "-j")
        file.remove(paste0(input$methods_name, ".tex"))
        
      } else if (input$downloadmethods_format == "HTML") {
        
        html_tmp <- paste0("---\ntitle: '", template_title, "'\noutput: html_document\nbibliography: literature.bib\n---\n", 
                           md_css, full_text$html, "<br><br><h5>References</h5>")
        html_tmppath <- tempfile(fileext = ".html")
        writeLines(html_tmp, html_tmppath)
        
        out <- rmarkdown::render(html_tmppath, output_format = "html_document")
        file.rename(out, file)
        
      } else if (input$downloadmethods_format %in% c("Markdown", "PDF", "Word")) {
        
        md_tmp <- paste0("---\ntitle: '", template_title, "'\noutput: md_document\nbibliography: literature.bib\n---\n", 
                         md_css, full_text$md, "\n\n#### References\n")
        md_tmppath <- tempfile(fileext = ".md")
        writeLines(md_tmp, md_tmppath)
        
        out_format <- switch(input$downloadmethods_format, "PDF" = "pdf_document", "Word" = "word_document", "Markdown" = "md_document")
        out <- rmarkdown::render(md_tmppath, output_format = out_format)
        file.rename(out, file)
      }
    }
  )
  

  # render the prior description ui
  output$priordescription_ui <- renderUI({
    
    # req(input$geography_file)
    req(input$traitcolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$prior_tabs)
    req(input$delta_prior)
    req(input$mu_prior)
    req(input$model_symmetry)
    req(states_dat())
    req(parsimony_score$mean != 0)
    
    deltaprior_description <- ""
    muprior_description <- ""
    
    if (input$with_bssvs) {
      
      states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
      states <- states[states != "?"]
      states_num <- length(states)

      poisson_default <- T
      if (!is.null(input$poisson_default)) {
        poisson_default <- input$poisson_default
      }
      
      poisson_lambda <- log(2)
      if (poisson_default) {
        if (input$model_symmetry == "symmetric") {
          poisson_lambda <- log(2)
        } else {
          poisson_lambda <- states_num - 1
        }
      } else if (!poisson_default && (!is.null(input$poisson_lambda))) {
        poisson_lambda <- input$poisson_lambda
      }
      
      alpha_beta <- beta_beta <- 1
      if (!is.null(input$alpha_beta)) alpha_beta <- input$alpha_beta
      if (!is.null(input$beta_beta)) beta_beta <- input$beta_beta
      
      deltaprior_description <- description_deltaprior(states_num = states_num, symmetry = (input$model_symmetry == "symmetric"), 
                                                       delta_prior = input$delta_prior, poisson_default = poisson_default, poisson_lambda = poisson_lambda, 
                                                       alpha_beta = alpha_beta, beta_beta = beta_beta)
      
    }
    
    hierachexp_mu95interval <- NULL
    if ((input$mu_prior == "Hierarchical Exponential") && (!is.null(hierarchexp$mu_x95interval))) {
      hierachexp_mu95interval <- hierarchexp$mu_x95interval
    }
    
    hierachexp_alphaofgamma <- 0.5
    if ((input$mu_prior == "Hierarchical Exponential") && (!is.null(input$alphaofgamma_mumean))) {
      hierachexp_alphaofgamma <- input$alphaofgamma_mumean
    }
    
    hierachexp_dispersaleventsnummean <- 1
    if ((input$mu_prior == "Hierarchical Exponential") && (!is.null(hierarchexp$dispersaleventsnum_mean))) {
      hierachexp_dispersaleventsnummean <- hierarchexp$dispersaleventsnum_mean
    }
    
    hierachexp_dispersaleventsnum95interval <- NULL
    if ((input$mu_prior == "Hierarchical Exponential") && (!is.null(hierarchexp$dispersaleventsnum_x95interval))) {
      hierachexp_dispersaleventsnum95interval <- hierarchexp$dispersaleventsnum_x95interval
    }

    empinformed_dispersaleventsnummean <- 1
    if ((input$mu_prior == "Empirical-Informed Exponential") && (!is.null(input$dispersaleventnum_NBmean))) {
      empinformed_dispersaleventsnummean <- input$dispersaleventnum_NBmean
    }
    
    parsimonyscore_quantile <- 0.5
    if (input$mu_prior == "CTMC rate-ref (BEAST default)") {
      parsimonyscore_quantile <- pnbinom(parsimony_score$mean, size = 0.5, prob = 0.5)
    } else if (input$mu_prior == "Hierarchical Exponential" && (!is.null(hierarchexp$dispersaleventsnum_parsimonyscore_quantile))) {
      parsimonyscore_quantile <- hierarchexp$dispersaleventsnum_parsimonyscore_quantile
    } else if (input$mu_prior == "Empirical-Informed Exponential" && (!is.null(input$dispersaleventnum_NBmean)) && (input$dispersaleventnum_NBmean != "")) {
      parsimonyscore_quantile <- pnbinom(parsimony_score$mean, size = 1, mu = input$dispersaleventnum_NBmean)
    }
    
    muprior_description <- description_muprior(mu_prior = input$mu_prior, tree_length = tree_values$tree_length, hierachexp_mu95interval = hierarchexp$mu_x95interval, 
                                               hierachexp_alphaofgamma = hierachexp_alphaofgamma, hierachexp_dispersaleventsnummean = hierachexp_dispersaleventsnummean, 
                                               hierachexp_dispersaleventsnum95interval = hierachexp_dispersaleventsnum95interval, 
                                               empinformed_dispersaleventsnummean = empinformed_dispersaleventsnummean, parsimonyscore_quantile = parsimonyscore_quantile)
    
    if ((input$prior_tabs == "delta") && input$with_bssvs) {
      withMathJax(HTML(paste0("<div style = 'font-size: 95%;'>", deltaprior_description, "</div>")))
    } else if (input$prior_tabs == "mu") {
      withMathJax(HTML(paste0("<div style = 'font-size: 95%;'>", muprior_description, "</div>")))
    }
  })
  
  # render the prior notation ui
  output$priornotation_ui <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    req(input$traitcolumn_name)
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$prior_tabs)
    req(input$delta_prior)
    req(input$mu_prior)
    req(input$model_symmetry)
    
    math_tex <- NULL
    
    states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
    states <- states[states != "?"]
    states_num <- length(states)
    delta_max <- ((input$model_symmetry == "asymmetric") + 1) * choose(states_num, 2)
    
    if (input$with_bssvs) {
      math_tex <- c(math_tex, "Prior on $\\Delta$:")
      
      if (input$delta_prior == "Poisson") {
        
        poisson_lambda <- log(2)
        poisson_default <- T
        if (!is.null(input$poisson_default)) {
          poisson_default <- input$poisson_default
        }
        
        if (poisson_default) {
          if (input$model_symmetry == "symmetric") {
            poisson_lambda <- 0.6931471805599453
          } else {
            poisson_lambda <- states_num - 1
          }
        } else if (!poisson_default && (!is.null(input$poisson_lambda))) {
          poisson_lambda <- input$poisson_lambda
        }
        poisson_offset <- ifelse(input$model_symmetry == "symmetric", states_num - 1, 0)
        
        if (input$model_symmetry == "symmetric") {
          math_tex <- c(math_tex, paste0("$$\\Delta - ", poisson_offset, "\\sim Pois(", formatC(poisson_lambda, digits = 3, format = "fg"), ")$$"))
        } else {
          math_tex <- c(math_tex, paste0("$$\\Delta \\sim Pois(", formatC(poisson_lambda, digits = 3, format = "fg"), ")$$"))
        }
        
      } else if (input$delta_prior == "Beta-Binomial") {
        a <- b <- 1
        if (!is.null(input$alpha_beta)) a <- input$alpha_beta
        if (!is.null(input$beta_beta)) b <- input$beta_beta
        
        math_tex <- c(math_tex, paste0("$$p \\sim Beta(", a, ", ", b, ")$$"), paste0("$$\\Delta \\sim Bin(", delta_max, ", p)$$"))
        
      } else if (input$delta_prior == "Uniform") {
        math_tex <- c(math_tex, paste0("$$\\Delta \\sim U(0, ", delta_max, ")$$"))
      }
    }

    math_tex <- c(math_tex, "Prior on $\\mu$:")
    if (input$mu_prior == "CTMC rate-ref (BEAST default)") {
      math_tex <- c(math_tex, paste0("$$\\mu \\sim Gamma(0.5, ", formatC(tree_values$tree_length, digits = 3, format = "fg"), ")$$"))
    } else if (input$mu_prior == "Hierarchical Exponential") {
      
      v <- 0.5 # shape of hyper gamma
      if (!is.null(input$alphaofgamma_mumean)) {
        v <- input$alphaofgamma_mumean
      }
      
      math_tex <- c(math_tex, paste0("$$\\theta \\sim Gamma(", v, ", ", v, ")$$"))
      math_tex <- c(math_tex, paste0("$$\\mu \\sim Exp(\\frac{1}{\\theta})$$"))
      
    } else if (input$mu_prior == "Empirical-Informed Exponential") {
      
      clockrate_mean <- 1
      if (!is.null(input$dispersaleventnum_NBmean)) {
        clockrate_mean <- input$dispersaleventnum_NBmean/tree_values$tree_length
      }
      math_tex <- c(math_tex, paste0("$$\\mu \\sim Exp(", formatC(clockrate_mean, digits = 3, format = "fg"), ")$$"))
    }
    
    withMathJax(paste(math_tex, collapse = "\n"))
  })
  
  # render the prior plot ui
  output$priorplot_ui <- renderUI({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$prior_tabs)
    
    plot_height <- 300
    if (input$prior_tabs == "delta") {
      plot_height <- 240
    } else if (input$prior_tabs == "mu") {
      plot_height <- 450 # for mu prior, there would be two plots vertically stacked, so larger height
    }
    
    plotOutput(outputId = "prior_plot", width = "100%", height = plot_height)
  })
  
  # store some computational expensive hierachical exponential prior plotting settings
  hierarchexp <- reactiveValues()
  observe({
    req(input$alphaofgamma_mumean)
    req(parsimony_score$mean != 0)
    
    clockrate_mean_gammashaperate <- input$alphaofgamma_mumean
    
    rns_num <- 1e6
    mus <- rgamma(rns_num, shape = 1, scale = rgamma(rns_num, shape = clockrate_mean_gammashaperate, rate = clockrate_mean_gammashaperate))
    hierarchexp$mu_x95interval <- quantile(mus, c(0.025, 0.975))
    
    rns_binom <- rpois(rns_num, lambda = tree_values$tree_length * mus)
    hierarchexp$dispersaleventsnum_mean <- mean(rns_binom)
    hierarchexp$dispersaleventsnum_parsimonyscore_quantile <- mean(rns_binom <= parsimony_score$mean)
    rns_binom_tab <- table(rns_binom)/rns_num
    
    x_lim <- ceiling(range(c(quantile(rns_binom, c(0.005, 0.995)), parsimony_score$mean)))
    if (x_lim[1] == 0) x_lim[1] <- 1
    x_seq <- x_lim[1]:x_lim[2]
    
    y_seq <- numeric(length(x_seq))
    names(y_seq) <- x_seq

    y_seq[names(rns_binom_tab)[names(rns_binom_tab) %in% as.character(x_seq)]] <- rns_binom_tab[names(rns_binom_tab) %in% as.character(x_seq)]
    
    x95interval_lim <- ceiling(quantile(rns_binom, c(0.025, 0.975)))
    hierarchexp$dispersaleventsnum_x95interval <- x95interval_lim
    
    if (x95interval_lim[1] == 0) x95interval_lim[1] <- 1
    x95interval_seq <- x95interval_lim[1]:x95interval_lim[2]
    y95interval_seq <- y_seq[as.character(x95interval_seq)]
    
    hierarchexp$dispersaleventsnum_yseq <- y_seq
    hierarchexp$dispersaleventsnum_y95intervalseq <- y95interval_seq
  })
  
  # this may not be needed, just leaving this blocking mechanism here for future reference
  # hierarchexp_dispersaleventsnum_coords_d <- debounce(hierarchexp_dispersaleventsnum_coords, 10)
  
  # plot the prior distribution (and the induced priors)
  output$prior_plot <- renderPlot({
    
    # req(input$geography_file)
    req(states_dat())
    # req(input$tree_file)
    req(tree_values$tree)
    req(input$taxoncolumn_name)
    req(input$traitcolumn_name)
    req(input$prior_tabs)
    req(input$model_symmetry)
    
    cols <- seq_gradient_pal(low = "blue", high = "orange")(c(0, 1))
    
    if (input$prior_tabs == "delta") {
      
      col <- cols[1]
      
      states <- sort(as.vector(unique(states_dat()[, input$traitcolumn_name])))
      states <- states[states != "?"]
      states_num <- length(states)
      delta_max <- ((input$model_symmetry == "asymmetric") + 1) * choose(states_num, 2)
      
      par(lend = 2, mai = c(0.4, 0.45, 0.05, 0.05))
      x_lim <- c(0, delta_max)
      x_seq <- x_lim[1]:x_lim[2]
      
      if (input$delta_prior == "Poisson") {
        
        poisson_lambda <- log(2)
        poisson_default <- T
        if (!is.null(input$poisson_default)) {
          poisson_default <- input$poisson_default
        }
        
        if (poisson_default) {
          col <- cols[2]
          if (input$model_symmetry == "symmetric") {
            poisson_lambda <- 0.6931471805599453
          } else {
            poisson_lambda <- states_num - 1
          }
        } else if (!poisson_default && (!is.null(input$poisson_lambda))) {
          poisson_lambda <- input$poisson_lambda
        }
        poisson_offset <- ifelse(input$model_symmetry == "symmetric", states_num - 1, 0)
        
        y_zero <- NULL
        if (poisson_offset > x_lim[1]) y_zero <- rep(0, poisson_offset - x_lim[1])
        y_seq <- c(y_zero, dpois(0:(x_lim[2] - poisson_offset), lambda = poisson_lambda))
        
        x95interval_lim <- qpois(c(0.025, 0.975), lambda = poisson_lambda) + poisson_offset
        x95interval_seq <- x95interval_lim[1]:x95interval_lim[2]
        y95interval_seq <- dpois(x95interval_seq - poisson_offset, lambda = poisson_lambda)
        
      } else if (input$delta_prior == "Beta-Binomial") {
        a <- b <- 1
        if (!is.null(input$alpha_beta)) a <- input$alpha_beta
        if (!is.null(input$beta_beta)) b <- input$beta_beta
        
        # y_seq <- choose(delta_max, x_seq) * beta(a = a + x_seq, b = delta_max - x_seq + b)/beta(a = a, b = b)
        y_seq <- rmutil::dbetabinom(x_seq, size = delta_max, m = a/(a + b), s = a + b)
        
        x95interval_lim <- rmutil::qbetabinom(c(0.025, 0.975), size = delta_max, m = a/(a + b), s = a + b)
        x95interval_seq <- x95interval_lim[1]:x95interval_lim[2]
        y95interval_seq <- rmutil::dbetabinom(x95interval_seq, size = delta_max, m = a/(a + b), s = a + b)
        
      } else if (input$delta_prior == "Uniform") {
        y_seq <- rep(1/(delta_max + 1), delta_max + 1)
        
        x95interval_lim <- ceiling(qunif(c(0.025, 0.975), min = 0, max = delta_max))
        x95interval_seq <- x95interval_lim[1]:x95interval_lim[2]
        y95interval_seq <- rep(1/(delta_max + 1), length(x95interval_seq))
      }
      
      y_lim <- c(0, max(y_seq))
      
      plot(NA, xlim = x_lim, ylim = y_lim, xaxt = "n", yaxt = "n", type = "n", xlab = NA, ylab = NA)
      polygon(x = c(x95interval_seq[1], x95interval_seq, x95interval_seq[length(x95interval_seq)]),
              y = c(0, y95interval_seq, 0),
              col = paste0(col, "40"), border = NA)
      lines(x = x_seq, y = y_seq, col = col, lwd = 1.5, lty = 1)
      
      legend("topright", legend = "95% prior interval", pch = 15, bty = "n", cex = 1, pt.cex = 1.4, col = paste0(col, "40"))
      
      axis(side = 1, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
      axis(side = 1, lwd = 0, lwd.ticks = 0, cex.axis = 0.9, line = -1)
      axis(side = 2, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
      axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = 0.9, line = -0.75)
      
      mtext("prior probability", side = 2, line = 1.3, cex = 1.25)
      mtext(expression(paste("number of dispersal routes, ", Delta)), side = 1, line = 1.1, cex = 1.25)
      
    } else if (input$prior_tabs == "mu") {
      
      col <- ifelse(input$mu_prior == "CTMC rate-ref (BEAST default)", cols[2], cols[1])
      
      layout(cbind(1:3), heights = c(1, 0.2, 1))
      par(lend = 2, mai = c(0, 0, 0, 0), omi = c(0.375, 0.4, 0.01, 0.01))
      
      # dispersal rate plot
      xlim_upper <- 2.5
      xlim_lower <- 1e-5
      x_seq <- exp(seq(from = log(xlim_lower), to = log(xlim_upper), length.out = 1e5))
      
      if (input$mu_prior == "CTMC rate-ref (BEAST default)") {
        
        y_seq <- dgamma(x_seq, shape = 0.5, rate = tree_values$tree_length)
        
        x95interval_lim <- findInterval(range(c(qgamma(c(0.025, 0.975), shape = 0.5, rate = tree_values$tree_length), xlim_lower, xlim_upper)), x_seq)
        x95interval_seq <- x_seq[x95interval_lim[1]:x95interval_lim[2]]
        y95interval_seq <- dgamma(x95interval_seq, shape = 0.5, rate = tree_values$tree_length)
        
      } else if (input$mu_prior == "Hierarchical Exponential") {
        
        req(hierarchexp$dispersaleventsnum_yseq)
        
        dK <- function(x, L, v, mu) { # K distribution
          return(2 * (v/mu)^((v + L)/2) * x^((v + L - 2)/2) * besselK(2 * sqrt(x * v/mu), nu = v - L)/(gamma(v) * gamma(L)))
        }
        
        v <- 0.5 # shape of hyper gamma
        if ((!is.null(input$alphaofgamma_mumean)) && (input$alphaofgamma_mumean > 0)) {
          v <- input$alphaofgamma_mumean
        }
        mu <- 1 # mean of hyper gamma
        L <- 1 # shape of the exponential (apparently 1, but just to be generic)
        
        y_seq <- dK(x_seq, L = L, v = v, mu = mu)
        
        x95interval_lim <- findInterval(range(c(hierarchexp$mu_x95interval, xlim_lower, xlim_upper)), x_seq)
        x95interval_seq <- x_seq[x95interval_lim[1]:x95interval_lim[2]]
        y95interval_seq <- dK(x95interval_seq, L = L, v = v, mu = mu)
        
      } else if (input$mu_prior == "Empirical-Informed Exponential") {
        
        clockrate_mean <- 1
        if (!is.null(input$dispersaleventnum_NBmean)) {
          clockrate_mean <- input$dispersaleventnum_NBmean/tree_values$tree_length
        }
        
        y_seq <- dgamma(x_seq, shape = 1, scale = clockrate_mean)
        
        x95interval_lim <- findInterval(range(c(qgamma(c(0.025, 0.975), shape = 1, scale = clockrate_mean), xlim_lower, xlim_upper)), x_seq)
        x95interval_seq <- x_seq[x95interval_lim[1]:x95interval_lim[2]]
        y95interval_seq <- dgamma(x95interval_seq, shape = 1, scale = clockrate_mean)
      }

      plot(NA, xlim = range(x_seq), ylim = c(0, ifelse(max(y_seq) < 5, max(y_seq), 5)), xaxt = "n", yaxt = "n", type = "n", 
           xlab = NA, ylab = NA)
      polygon(x = c(x95interval_seq[1], x95interval_seq, x95interval_seq[length(x95interval_seq)]),
              y = c(0, y95interval_seq, 0),
              col = paste0(col, "40"), border = NA)
      lines(x = x_seq, y = y_seq, col = col, lwd = 1.5, lty = 1)
      
      legend("topright", legend = "95% prior interval", pch = 15, bty = "n", cex = 1.4, pt.cex = 1.8, col = paste0(col, "40"))

      axis(side = 1, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
      axis(side = 1, lwd = 0, lwd.ticks = 0, cex.axis = 1.15, line = -1)
      axis(side = 2, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
      axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = 1.15, line = -0.75)
      
      mtext("prior probability", side = 2, line = 1.6, cex = 1.25)
      mtext(expression(paste("average dispersal rate, ", mu)), side = 1, line = 1.6, cex = 1.25)
      
      # white space between upper and lower panel
      plot.new()
      
      # number of dispersal events plot
      if (input$mu_prior == "CTMC rate-ref (BEAST default)") {
        
        # ctmc rate-ref param
        # Gamma with shape = 0.5 = size;
        # scale * T = 1, so prob = 1/(1 + scale * T) = 0.5, mu = size * (1 - prob)/prob = 0.5
        
        x_lim <- range(c(qnbinom(c(0.005, 0.995), size = 0.5, prob = 0.5), parsimony_score$mean))
        if (x_lim[1] == 0) x_lim[1] <- 1
        x_seq <- x_lim[1]:x_lim[2]
        y_seq <- dnbinom(x_seq, size = 0.5, prob = 0.5)
        y_lim <- c(0, max(y_seq))
        
        x95interval_lim <- qnbinom(c(0.025, 0.975), size = 0.5, prob = 0.5)
        if (x95interval_lim[1] == 0) x95interval_lim[1] <- 1
        x95interval_seq <- x95interval_lim[1]:x95interval_lim[2]
        y95interval_seq <- dnbinom(x95interval_seq, size = 0.5, prob = 0.5)
        
      } else if (input$mu_prior == "Hierarchical Exponential") {
        
        x_seq <- as.integer(names(hierarchexp$dispersaleventsnum_yseq))
        y_seq <- as.numeric(hierarchexp$dispersaleventsnum_yseq)
        
        x_lim <- range(x_seq)
        y_lim <- c(0, max(y_seq))
        
        x95interval_seq <- as.integer(names(hierarchexp$dispersaleventsnum_y95intervalseq))
        y95interval_seq <- as.numeric(hierarchexp$dispersaleventsnum_y95intervalseq)
        
        sparseridx <- unique(floor(exp(seq(from = 0, to = log(length(x95interval_seq)), length.out = 2e3))))
        x95interval_seq <- x95interval_seq[sparseridx]
        y95interval_seq <- y95interval_seq[sparseridx]
        
      } else if (input$mu_prior == "Empirical-Informed Exponential") {
        
        dispersaleventnum_NBmean <- parsimony_score$mean
        if (!is.null(input$dispersaleventnum_NBmean)) {
          dispersaleventnum_NBmean <- input$dispersaleventnum_NBmean
        }
        
        x_lim <- range(c(qnbinom(c(0.005, 0.995), size = 1, mu = dispersaleventnum_NBmean), parsimony_score$mean))
        if (x_lim[1] == 0) x_lim[1] <- 1
        x_seq <- x_lim[1]:x_lim[2]
        y_seq <- dnbinom(x_seq, size = 1, mu = dispersaleventnum_NBmean)
        y_lim <- c(0, max(y_seq))
        
        x95interval_lim <- qnbinom(c(0.025, 0.975), size = 1, mu = dispersaleventnum_NBmean)
        if (x95interval_lim[1] == 0) x95interval_lim[1] <- 1
        x95interval_seq <- x95interval_lim[1]:x95interval_lim[2]
        y95interval_seq <- dnbinom(x95interval_seq, size = 1, mu = dispersaleventnum_NBmean)
      }
      
      plot(NA, xlim = x_lim, ylim = y_lim, xaxt = "n", yaxt = "n", type = "n", xlab = NA, ylab = NA, log = "x")
      
      polygon(x = c(x95interval_seq[1], x95interval_seq, x95interval_seq[length(x95interval_seq)]),
              y = c(0, y95interval_seq, 0),
              col = paste0(col, "40"), border = NA)
      
      lines(x = x_seq, y = y_seq, col = col, lwd = 1.5, lty = 1)
      abline(v = parsimony_score$mean, lwd = 2, lty = 2)
      
      legend("topright", legend = c("95% prior interval", "parsimony score"), pch = c(15, NA), lty = c(NA, 2), lwd = 1.5, 
             bty = "n", cex = 1.4, pt.cex = 1.8, col = c(paste0(col, "40"), "black"))
      
      axis(side = 1, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
      axis(side = 1, lwd = 0, lwd.ticks = 0, cex.axis = 1.15, line = -1)
      axis(side = 2, labels = NA, lwd = 0, lwd.ticks = 1, tcl = -0.25)
      axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = 1.15, line = -0.75)
      
      mtext("prior probability", side = 2, line = 1.6, cex = 1.25)
      mtext("number of dispersal events", side = 1, line = 1.6, cex = 1.25)
      
    }
  })
  
  # force uis to be rendered even when they are hidden
  outputOptions(output, "furtheranalysis_ui", suspendWhenHidden = F)
  outputOptions(output, "treemodel_ui", suspendWhenHidden = F)
  outputOptions(output, "deltaprior_ui", suspendWhenHidden = F)
  outputOptions(output, "muprior_ui", suspendWhenHidden = F)
  outputOptions(output, "proposalweight_ui", suspendWhenHidden = F)
  
  outputOptions(output, "methods_data", suspendWhenHidden = F)
  outputOptions(output, "methods_model", suspendWhenHidden = F)
  outputOptions(output, "methods_prior", suspendWhenHidden = F)
  outputOptions(output, "methods_analysis", suspendWhenHidden = F)
  outputOptions(output, "methods_full", suspendWhenHidden = F)
  
  outputOptions(output, "beastxml_data", suspendWhenHidden = F)
  outputOptions(output, "beastxml_modelprior", suspendWhenHidden = F)
  outputOptions(output, "beastxml_proposal", suspendWhenHidden = F)
  outputOptions(output, "beastxml_phyloctmc", suspendWhenHidden = F)
  outputOptions(output, "beastxml_mcmc", suspendWhenHidden = F)
  outputOptions(output, "beastxml_full", suspendWhenHidden = F)
  

  #######################################################
  # here the second main panel (post processing) starts #
  #######################################################
  
  focalparamsummary_logfile_defaultupload_paths <- list(c("./data/post_processing/ctmc/HIV_datasetA_underprior_run1.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_underprior_run2.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_posterior_run1.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_posterior_run2.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_datacloning5_run1.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_datacloning5_run2.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_datacloning20_run1.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_datacloning20_run2.log"),
                                                        c("./data/post_processing/exphyper/HIV_datasetA_underprior_run1.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_underprior_run2.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_posterior_run1.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_posterior_run2.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_datacloning5_run1.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_datacloning5_run2.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_datacloning20_run1.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_datacloning20_run2.log"))
  
  input_default_static_focalparamsummary <- lapply(focalparamsummary_logfile_defaultupload_paths, function(x) {
    data.frame(name = basename(x), datapath = x, check.names = F, stringsAsFactors = F)
  })
  
  observeEvent(input$focalparamsummary_logfile_defaultupload, {
    if (input$focalparamsummary_logfile_defaultupload) {
      if (focalparamsummary_loginputset$num == 1) {
        shinyjs::runjs("$('#focalparamsummary_loginput_add').click();")
      } else if (focalparamsummary_loginputset$num > 2) {
        focalparamsummary_loginputset$num <- 2L
      }
    } else {
      if (focalparamsummary_loginputset$num_inputui == 2 && (is.null(input[[paste0("focalparamsummary_logfile", 2)]]))) {
        shinyjs::runjs("$('#focalparamsummary_loginput_remove').click();")
      }
    }
  })
  
  # dynamically rendering input fields according to user specification
  focalparamsummary_loginputset <- reactiveValues(num = 1L, num_inputui = 1L)
  # add
  observeEvent(input$focalparamsummary_loginput_add, {
    focalparamsummary_loginputset$num <- focalparamsummary_loginputset$num + 1L
    focalparamsummary_loginputset$num_inputui <- focalparamsummary_loginputset$num_inputui + 1L
    
    insertUI(selector = "#focalparamsummary_loginput_addremove_div", where = "beforeBegin", immediate = T,
             ui = div(id = paste0("focalparamsummary_logfile_div", focalparamsummary_loginputset$num),
                      class = ifelse(input$focalparamsummary_logfile_defaultupload, "divdisabled", ""),
                      fileInput(inputId = paste0("focalparamsummary_logfile", focalparamsummary_loginputset$num), 
                            label = paste0("Estimate log file(s) under prior No. ", focalparamsummary_loginputset$num), 
                            multiple = T, accept = ".log")))
  })
  # remove
  observeEvent(input$focalparamsummary_loginput_remove, {
    if (focalparamsummary_loginputset$num > 1) {
      removeUI(selector = paste0("#focalparamsummary_logfile_div", focalparamsummary_loginputset$num))
      focalparamsummary_loginputset$num <- focalparamsummary_loginputset$num - 1L
      focalparamsummary_loginputset$num_inputui <- focalparamsummary_loginputset$num_inputui - 1L
    }
  })
  
  observe({
    for (i in 1:focalparamsummary_loginputset$num_inputui) {
      shinyjs::toggleClass(id = paste0("focalparamsummary_logfile_div", i), class = "divdisabled", 
                           condition = input$focalparamsummary_logfile_defaultupload)
    }
  })
  observe({
    shinyjs::toggleClass(id = "focalparamsummary_loginput_addremove_div", class = "divdisabled", 
                         condition = input$focalparamsummary_logfile_defaultupload)
  })
  
  focalparamsummary_logfileset <- reactiveValues(file = NULL)
  
  observe({
    isolate(focalparamsummary_logfileset$file <- vector("list", focalparamsummary_loginputset$num))
    for (i in 1:focalparamsummary_loginputset$num) {
      if (input$focalparamsummary_logfile_defaultupload) {
        req(focalparamsummary_loginputset$num == length(input_default_static_focalparamsummary))
        isolate(focalparamsummary_logfileset$file[[i]] <- input_default_static_focalparamsummary[[i]])
      } else {
        if (!is.null(input[[paste0("focalparamsummary_logfile", i)]])) {
          isolate(focalparamsummary_logfileset$file[[i]] <- input[[paste0("focalparamsummary_logfile", i)]])
        }
      }
    }
  })
  
  # dynamically rendering clone number, burnin, and include or not
  output$focalparamsummary_logprocessing_ui <- renderUI({
    
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    
    focalparamsummary_logprocessing_ui_list <- tagList()
    
    logprocessing_perpanel_height <- 540 / focalparamsummary_loginputset$num
    if (logprocessing_perpanel_height < 200) logprocessing_perpanel_height <- 200
    
    for (i in 1:focalparamsummary_loginputset$num) {

      if (!is.null(focalparamsummary_logfileset$file[[i]])) {
        
        powers <- numeric(nrow(focalparamsummary_logfileset$file[[i]]))
        for (j in 1:nrow(focalparamsummary_logfileset$file[[i]])) {
          if (grepl("_underprior", focalparamsummary_logfileset$file[[i]]$name[j])) {
            powers[j] <- 0
          } else if (grepl("posterior", focalparamsummary_logfileset$file[[i]]$name[j])) {
            powers[j] <- 1
          } else if (grepl("_datacloning", focalparamsummary_logfileset$file[[i]]$name[j])) {
            powers[j] <- as.numeric(gsub("datacloning", "", grep("datacloning", unlist(strsplit(focalparamsummary_logfileset$file[[i]]$name[j], "_|\\.")), value = T)[1]))
          } else {
            col_names <- colnames(read.table(focalparamsummary_logfileset$file[[i]]$datapath[j], header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
            if ("pathLikelihood.theta" %in% col_names) powers[j] <- 0.5
          }
        }
        
        focalparamsummary_logprocessing_ui_list <- tagAppendChildren(focalparamsummary_logprocessing_ui_list,
                                                                     HTML(paste0("<div id = \"focalparamsummary_logprocessing_panel", i, 
                                                                                 "\" class = \"panel-heading\"><p class=\"panel-title\" style=\"font-size: 95%; font-weight: bold;\">", 
                                                                                 "<a data-toggle=\"collapse\" href=\"#focalparamsummary_logprocessing", i,
                                                                                 "\"><span class=\"glyphicon glyphicon-chevron-up\" aria-hidden=\"true\"></span>", 
                                                                                 "Settings for estimate log file(s) under prior model No. ", i, 
                                                                                 "</a></p></div>")),
                                                                     div(id = paste0("focalparamsummary_logprocessing", i), 
                                                                         class = "panel-collapse collapse in", 
                                                                         fluidRow(style = paste0("max-height: ", logprocessing_perpanel_height, "px; overflow: auto; background-color: #EBF5FB; padding: 2px 10px 5px 10px;"),
                                                                                  lapply(1:nrow(focalparamsummary_logfileset$file[[i]]), function(j) {
                                                                                    tagList(
                                                                                      p(focalparamsummary_logfileset$file[[i]]$name[j], align = "center", 
                                                                                        style = "font-size: 95%; font-weight: bold;"),
                                                                                      numericInput(inputId = paste0("focalparamsummary_logfile", i, "_log", j, "_likelihoodpower"), 
                                                                                                   label = "Number of copies of the data",
                                                                                                   value = powers[j], min = 0),
                                                                                      checkboxInput(inputId = paste0("focalparamsummary_logfile", i, "_log", j, "_include"),
                                                                                                    label = "include this log file", value = T),
                                                                                      sliderInput(inputId = paste0("focalparamsummary_logfile", i, "_log", j, "_burnin"),
                                                                                                  label = "burn-in proportion",
                                                                                                  min = 0, max = 99, value = 10),
                                                                                      p("", align = "center", style = "font-size: 85%;"))
                                                                                  })[order(powers)])
                                                                         
                                                                     ))
        if (i != focalparamsummary_loginputset$num) {
          focalparamsummary_logprocessing_ui_list <- tagAppendChildren(focalparamsummary_logprocessing_ui_list, h6(""))
        }
      }
    }
    
    focalparamsummary_logprocessing_ui_list
  })
  
  # sanity check to make sure that at least one log file exist under each prior model
  observe({
    
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    
    for (i in 1:focalparamsummary_loginputset$num) {
      req(!is.null(focalparamsummary_logfileset$file[[i]]))
      sum <- 0
      
      for (j in 1:nrow(focalparamsummary_logfileset$file[[i]])) {
        req(!is.null(input[[paste0("focalparamsummary_logfile", i, "_log", j, "_include")]]))
        sum <- sum + input[[paste0("focalparamsummary_logfile", i, "_log", j, "_include")]]
      }
      
      if (sum == 0) {
        showModal(modalDialog(paste0("No log file is included for prior model ", i,
                                     ". At least one log file needs to be included under each prior model."),
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
        ))
        break
      }
    }
  })
  
  # function below not working, couldn't figure out why
  observe({
    lapply(1:focalparamsummary_loginputset$num, function(i) {
      shinyjs::runjs(HTML(paste0("$('#focalparamsummary_logprocessing", i, "').on('shown.bs.collapse', function() {
                              $('#focalparamsummary_logprocessing_panel", i, "').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
                            });
                            $('#focalparamsummary_logprocessing", i, "').on('hidden.bs.collapse', function() {
                              $('#focalparamsummary_logprocessing_panel", i, "').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
                            });")))
    })
  })

  
  # render prior model name ui
  output$focalparamsummary_priormodelname_ui <- renderUI({
    
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    focalparamsummary_priormodelname_ui_list <- tagList()
    
    for (i in 1:focalparamsummary_loginputset$num) {
      req((!is.null(focalparamsummary_logfileset$file[[i]])) && (nrow(focalparamsummary_logfileset$file[[i]]) >= 1))
      
      focalparamsummary_priormodelname_ui_list <- tagAppendChildren(focalparamsummary_priormodelname_ui_list,
                                                                    textInput(inputId = paste0("focalparamsummary_priormodelname", i),
                                                                              label = paste0("Name of prior No. ", i),
                                                                              value = paste0("prior", i)))
    }
    
    focalparamsummary_priormodelname_ui_list
  })


  # store reactive values
  focalparamsummary_values <- reactiveValues(paramnames_shared_focal = NULL, paramnames_shared_nonfocal = NULL, 
                                             paramname_selected = NULL, 
                                             focalparam_alldf_raw = NULL, focalparam_alldf_processed = NULL)
  
  # get the parameter list first
  observe({
    
    focalparamsummary_values$paramnames_shared_focal <- NULL
    focalparamsummary_values$paramnames_shared_nonfocal <- NULL
    
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    for (i in 1:focalparamsummary_loginputset$num) {
      req(focalparamsummary_logfileset$file[[i]])
    }

    paramnames <- list()
    for (i in 1:focalparamsummary_loginputset$num) {
      req((!is.null(focalparamsummary_logfileset$file[[i]])) && (nrow(focalparamsummary_logfileset$file[[i]]) >= 1))
      
      for (j in 1:nrow(focalparamsummary_logfileset$file[[i]])) {
        paramnames <- c(paramnames, list(colnames(read.table(focalparamsummary_logfileset$file[[i]]$datapath[j], 
                                                             header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))))
      }
    }

    paramnames_shared <- names(table(unlist(paramnames)))[table(unlist(paramnames)) == length(paramnames)]
    paramnames_shared <- paramnames_shared[paramnames_shared != "state"]
    
    paramnames_shared_focal <- c()
    if (any(grepl(".clock.rate$", paramnames_shared))) { # mu
      paramnames_shared_focal <- c(paramnames_shared_focal, grep(".clock.rate$", paramnames_shared, value = T)[1])
    }
    if (any(grepl(".nonZeroRates$", paramnames_shared))) { # Delta
      paramnames_shared_focal <- c(paramnames_shared_focal, grep(".nonZeroRates$", paramnames_shared, value = T)[1])
    }
    
    if (length(grep(".count", paramnames_shared)) == 1) { # total number of dispersal events
      paramnames_shared_focal <- c(paramnames_shared_focal, grep(".count", paramnames_shared, value = T))
    } else if ((length(grep(".count", paramnames_shared)) > 1) && (!any(grepl(".count\\[2", paramnames_shared)))) {
      nrow_tmp <- 10
      logdat_tmp <- read.table(focalparamsummary_logfileset$file[[1]]$datapath[1],
                               header = T, sep = "\t", nrows = nrow_tmp, stringsAsFactors = F, check.names = F)
      totalcount_id <- apply(logdat_tmp[, grep(".count", paramnames_shared, value = T)], MARGIN = 1, function(x) which(x == max(x)))
      if (length(unique(totalcount_id)) == 1) {
        totalcount_id <- unique(totalcount_id)
        paramnames_shared_focal <- c(paramnames_shared_focal, grep(".count", paramnames_shared, value = T)[totalcount_id])
      } else if (length(unique(totalcount_id)) > 1) {
        totalcount_id <- as.integer(names(table(totalcount_id)[table(totalcount_id) == nrow_tmp]))[1]
        paramnames_shared_focal <- c(paramnames_shared_focal, grep(".count", paramnames_shared, value = T)[totalcount_id])
      }

    }
    if (length(paramnames_shared_focal) == 0) paramnames_shared_focal <- NULL
    
    paramnames_shared_nonfocal <- paramnames_shared[!(paramnames_shared %in% paramnames_shared_focal)]
    paramnames_shared_nonfocal_putfirst <- paramnames_shared_nonfocal[paramnames_shared_nonfocal %in% c("posterior", "prior", "likelihood")]
    paramnames_shared_nonfocal <- c(paramnames_shared_nonfocal_putfirst, paramnames_shared_nonfocal[!(paramnames_shared_nonfocal %in% paramnames_shared_nonfocal_putfirst)])
    if (length(paramnames_shared_nonfocal) == 0) paramnames_shared_nonfocal <- NULL
    
    focalparamsummary_values$paramnames_shared_focal <- paramnames_shared_focal
    focalparamsummary_values$paramnames_shared_nonfocal <- paramnames_shared_nonfocal
  })
  
  # reading in the header first and then find out the intersect columns
  # generate a drop down menu whose fields are the column names (except for the iteration)
  output$focalparamsummary_paramname_ui <- renderUI({
    
    req(is.null(focalparamsummary_values$paramnames_shared_focal) + is.null(focalparamsummary_values$paramnames_shared_nonfocal) < 2)
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    
    for (i in 1:focalparamsummary_loginputset$num) {
      req(focalparamsummary_logfileset$file[[i]])
    }
    
    paramnames_shared_focal <- focalparamsummary_values$paramnames_shared_focal
    paramnames_shared_nonfocal <- focalparamsummary_values$paramnames_shared_nonfocal
    
    focalparamsummary_paramname_ui_list <- tagList()
    focalparamsummary_paramname_ui_list <- tagAppendChildren(focalparamsummary_paramname_ui_list, 
                                                             h2(''),
                                                             p("Select the parameter to summarize", align = "center", 
                                                               style = "font-size: 95%; font-weight: bold;"))
    
    if (!is.null(paramnames_shared_focal)) {
      paramnames_radiobutton_list <- paramnames_shared_focal
      
      if (!is.null(paramnames_shared_nonfocal)) {
        paramnames_radiobutton_list <- c(paramnames_radiobutton_list, "show others")
      }
      
      focalparamsummary_paramname_ui_list <- tagAppendChildren(focalparamsummary_paramname_ui_list,
                                                               radioButtons(inputId = "focalparamsummary_paramnameshared_focallist",
                                                                            label = NULL, choices = paramnames_radiobutton_list))
    }
    
    if (!is.null(paramnames_shared_nonfocal)) {
      focalparamsummary_paramname_ui_list <- tagAppendChildren(focalparamsummary_paramname_ui_list,
                                                               div(id = "focalparamsummary_paramnameshared_nonfocallist_div", style = "display: none;", 
                                                                   selectInput(inputId = "focalparamsummary_paramnameshared_nonfocallist", label = NULL, 
                                                                                                        choices = paramnames_shared_nonfocal, selectize = F)))
    }
    
    focalparamsummary_paramname_ui_list
  })
  
  # only show the drop down menu when the focal parameter is not in the parameter list or users choose to show other
  observe({
    focalparamsummary_loginputset$num
    nonfocallist_enabled <- ((is.null(focalparamsummary_values$paramnames_shared_focal)) || (input$focalparamsummary_paramnameshared_focallist == "show others")) && (!is.null(focalparamsummary_values$paramnames_shared_nonfocal)) 
    shinyjs::toggle(id = "focalparamsummary_paramnameshared_nonfocallist_div", condition = nonfocallist_enabled)
  })
  
  # only show the start processing button when the upstream settings are completer and it's not been clicked 
  # or when any of the upstream settings were invalidated so that user need to re-read in the log files
  focalparamsummary_startprocessing_status <- reactiveValues(enabled = F, clicked = F)
  observe({
    shinyjs::toggle(id = "focalparamsummary_startprocessing", condition = focalparamsummary_startprocessing_status$enabled)
  })
  
  observe({
    
    focalparamsummary_startprocessing_enabled <- T
    focalparamsummary_startprocessing_clicked <- focalparamsummary_startprocessing_status$clicked
    
    if (is.null(focalparamsummary_values$paramnames_shared_focal) + is.null(focalparamsummary_values$paramnames_shared_nonfocal) == 2) {
      focalparamsummary_startprocessing_enabled <- F
      focalparamsummary_startprocessing_clicked <- F
    } else if (is.null(input$focalparamsummary_paramnameshared_focallist) + is.null(input$focalparamsummary_paramnameshared_nonfocallist) == 2) {
      focalparamsummary_startprocessing_enabled <- F
      focalparamsummary_startprocessing_clicked <- F
    } else {
      req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
      for (i in 1:focalparamsummary_loginputset$num) {
        if (is.null(focalparamsummary_logfileset$file[[i]]) || focalparamsummary_logfileset$file[[i]] == "" || nrow(focalparamsummary_logfileset$file[[i]]) < 1) {
          focalparamsummary_startprocessing_enabled <- F
          focalparamsummary_startprocessing_clicked <- F
        }
      }
    }
    
    focalparamsummary_startprocessing_status$enabled <- focalparamsummary_startprocessing_enabled
    focalparamsummary_startprocessing_status$clicked <- focalparamsummary_startprocessing_clicked
  })

  # store the selected parameter name
  observe({
    if ((!is.null(input$focalparamsummary_paramnameshared_focallist)) && (input$focalparamsummary_paramnameshared_focallist != "show others")) {
      focalparamsummary_values$paramname_selected <- input$focalparamsummary_paramnameshared_focallist
    } else if (!is.null(input$focalparamsummary_paramnameshared_nonfocallist)) {
      focalparamsummary_values$paramname_selected <- input$focalparamsummary_paramnameshared_nonfocallist
    } else {
      focalparamsummary_values$paramname_selected <- NULL
      focalparamsummary_startprocessing_status$enabled <- F
    }
  })
  
  # enable the second tab (processing setting) only when all the initial processing is complete
  observe({
    focalparamsummary_tab2_disabled <- (input$focalparamsummary_startprocessing == 0) || (!focalparamsummary_startprocessing_status$clicked) || is.null(focalparamsummary_values$focalparam_alldf_processed)
    
    shinyjs::toggleClass(selector = "#focalparamsummary_tabs li a[data-value=processing_settings]", class = "divdisabled", 
                         condition = focalparamsummary_tab2_disabled)
    if (!focalparamsummary_tab2_disabled) {
      updateNavlistPanel(session, inputId = "focalparamsummary_tabs", selected = "processing_settings")
    }
  })
  
  # enable the third tab (processing setting) only when all the initial processing is complete
  observe({
    focalparamsummary_tab3_disabled <- (input$focalparamsummary_startprocessing == 0) || (!focalparamsummary_startprocessing_status$clicked) || is.null(focalparamsummary_values$focalparam_alldf_processed)
    
    shinyjs::toggleClass(selector = "#focalparamsummary_tabs li a[data-value=download_output]", class = "divdisabled", 
                         condition = focalparamsummary_tab3_disabled)
  })
  
  # enable the results visualization panel only when all the initial processing is complete
  observe({
    focalparamsummary_tab2_disabled <- (input$focalparamsummary_startprocessing == 0) || (!focalparamsummary_startprocessing_status$clicked) || is.null(focalparamsummary_values$focalparam_alldf_processed)
    # shinyjs::toggleClass(id = "focalparamsummary_result_div", class = "divdisabled", condition = focalparamsummary_tab2_disabled)
    # shinyjs::toggleClass(id = "focalparamsummary_result_div", class = "disappeared", condition = focalparamsummary_tab2_disabled)
    shinyjs::toggle(id = "focalparamsummary_result_div", condition = !focalparamsummary_tab2_disabled)
  })
  
  # reading in the posterior log files
  # computational expensive step (so only will be executed when the start processing button is clicked)
  observeEvent(input$focalparamsummary_startprocessing, {
    
    req(focalparamsummary_loginputset$num >= 1)
    req(focalparamsummary_values$paramname_selected)
    focalparam_alldf_raw <- vector("list", focalparamsummary_loginputset$num)
    
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    for (i in 1:focalparamsummary_loginputset$num) {
      req(focalparamsummary_logfileset$file[[i]])
    }
    
    lognum_all <- sum(sapply(1:focalparamsummary_loginputset$num, function(i) nrow(focalparamsummary_logfileset$file[[i]])))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Reading in log files", value = 0)
    for (i in 1:focalparamsummary_loginputset$num) {
      
      focalparam_alldf <- vector("list", nrow(focalparamsummary_logfileset$file[[i]]))
      powers_repnum <- c()
      
      for (j in 1:nrow(focalparamsummary_logfileset$file[[i]])) {
        
        progress$inc(1/lognum_all, detail = paste0("Processing estimate log file No.", j , " under prior model No.", i))
        
        col_names <- colnames(read.table(focalparamsummary_logfileset$file[[i]]$datapath[j], header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
        
        col_classes <- rep("NULL", length(col_names))
        col_classes[col_names == focalparamsummary_values$paramname_selected | col_names == "pathLikelihood.theta"] <- "numeric"
        
        focalparam_df <- read.table(focalparamsummary_logfileset$file[[i]]$datapath[j], header = T, sep = "\t", colClasses = col_classes, stringsAsFactors = F)
        
        if ("pathLikelihood.theta" %in% col_names) {
          focalparam_df <- focalparam_df[, c("pathLikelihood.theta", sort(colnames(focalparam_df)[colnames(focalparam_df) != "pathLikelihood.theta"]))]
          
          powers_current <- sort(unique(focalparam_df$pathLikelihood.theta))
          powers_repnum_current <- rep(1, length(powers_current))
          names(powers_repnum_current) <- powers_current
          for (k in 1:length(powers_repnum_current)) {
            if (as.numeric(names(powers_repnum_current)[k]) %in% as.numeric(names(powers_repnum))) {
              powers_repnum_current[k] <- powers_repnum[names(powers_repnum_current)[k]] + 1
              powers_repnum[names(powers_repnum_current)[k]] <- powers_repnum[names(powers_repnum_current)[k]] + 1
            } else {
              powers_repnum <- c(powers_repnum, 1)
              names(powers_repnum)[length(powers_repnum)] <- names(powers_repnum_current)[k]
            }
          }
          
          focalparam_df <- data.frame(cbind(focalparam_df$pathLikelihood.theta, as.integer(powers_repnum_current[as.character(focalparam_df$pathLikelihood.theta)]), 
                                            focalparam_df[, colnames(focalparam_df) != "pathLikelihood.theta"]))
          
        } else {
          
          power <- 1
          if (grepl("_underprior", focalparamsummary_logfileset$file[[i]]$name[j])) {
            power <- 0
          } else if (grepl("_posterior", focalparamsummary_logfileset$file[[i]]$name[j])) {
            power <- 1
          } else if (grepl("_datacloning", focalparamsummary_logfileset$file[[i]]$name[j])) {
            power <- as.numeric(gsub("datacloning", "", grep("datacloning", unlist(strsplit(focalparamsummary_logfileset$file[[i]]$name[j], "_|\\.")), value = T)[1]))
          }
          
          repnum <- 1
          if (power %in% as.numeric(names(powers_repnum))) {
            repnum <- as.integer(powers_repnum[as.character(power)] + 1)
            powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
          } else {
            powers_repnum <- c(powers_repnum, 1)
            names(powers_repnum)[length(powers_repnum)] <- power
          }
          
          focalparam_df <- data.frame(cbind(rep(power, nrow(focalparam_df)), rep(repnum, nrow(focalparam_df)), focalparam_df), stringsAsFactors = F)
        }
        
        colnames(focalparam_df) <- c("likelihoodpower", "repid", focalparamsummary_values$paramname_selected)
        focalparam_alldf[[j]] <- focalparam_df
      }
      
      focalparam_alldf_raw[[i]] <- focalparam_alldf
    }
    
    focalparamsummary_values$focalparam_alldf_raw <- focalparam_alldf_raw
    focalparamsummary_startprocessing_status$enabled <- F
    focalparamsummary_startprocessing_status$clicked <- T
  })
  
  # additional processing step (after the log files have been read in)
  # computational inexpensive step (will be invalidated thus re-executed frequently)
  observe({
    
    req(focalparamsummary_values$focalparam_alldf_raw)
    req(focalparamsummary_loginputset$num == length(focalparamsummary_logfileset$file))
    for (i in 1:focalparamsummary_loginputset$num) {
      req(focalparamsummary_logfileset$file[[i]])
    }
    
    focalparam_alldf_raw <- focalparamsummary_values$focalparam_alldf_raw
    focalparam_alldf_processed <- vector("list", length(focalparam_alldf_raw))
    
    req(length(focalparam_alldf_raw) == focalparamsummary_loginputset$num)
    
    for (i in 1:focalparamsummary_loginputset$num) {
      
      focalparam_alldf <- focalparam_alldf_raw[[i]]
      req(length(focalparam_alldf) == nrow(focalparamsummary_logfileset$file[[i]]))
      
      posterior_notinpowerposterior_exist <- prior_notinpowerposterior_exist <- F
      
      # figuring out if posterior and prior is already there and if so we won't include them from the power posterior analyses
      for (j in 1:nrow(focalparamsummary_logfileset$file[[i]])) {
        if ((!is.null(input[[paste0("focalparamsummary_logfile", i, "_log", j, "_include")]])) && input[[paste0("focalparamsummary_logfile", i, "_log", j, "_include")]]) {
          
          powers <- unique(focalparam_alldf[[j]]$likelihoodpower)
          if (length(powers) == 1) {
            if (powers == 1) {
              posterior_notinpowerposterior_exist <- T
            } else if (powers == 0) {
              prior_notinpowerposterior_exist <- T
            }
          }
          
        }
      }
      
      powers_repnum <- c()
      k <- 1
      for (j in 1:nrow(focalparamsummary_logfileset$file[[i]])) {
        if ((!is.null(input[[paste0("focalparamsummary_logfile", i, "_log", j, "_include")]])) && input[[paste0("focalparamsummary_logfile", i, "_log", j, "_include")]]) {
          
          focalparam_df <- focalparam_alldf[[j]]
          burnin_prop <- 0
          if ((!is.null(input[[paste0("focalparamsummary_logfile", i, "_log", j, "_burnin")]])) && input[[paste0("focalparamsummary_logfile", i, "_log", j, "_burnin")]] > 0) {
            burnin_prop <- input[[paste0("focalparamsummary_logfile", i, "_log", j, "_burnin")]] / 100
          }
          
          if (length(unique(focalparam_df$likelihoodpower)) >= 2) {
            
            powers <- sort(unique(focalparam_df$likelihoodpower))
            if (any(powers == 0) && prior_notinpowerposterior_exist) powers <- powers[powers != 0]
            if (any(powers == 1) && posterior_notinpowerposterior_exist) powers <- powers[powers != 1]
            
            if (length(powers) == 0) {
              focalparam_df <- focalparam_df[-(1:nrow(focalparam_df)),]
            } else {
              focalparam_powerdf <- vector("list", length(powers))
              
              for (l in 1:length(powers)) {
                focalparam_powerdf[[l]] <- focalparam_df[focalparam_df$likelihoodpower == powers[l], ]
                
                power <- powers[l]
                repnum <- 1
                if (power %in% as.numeric(names(powers_repnum))) {
                  repnum <- as.integer(powers_repnum[as.character(power)] + 1)
                  powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
                } else {
                  powers_repnum <- c(powers_repnum, 1)
                  names(powers_repnum)[length(powers_repnum)] <- power
                }
                focalparam_powerdf[[l]]$repid <- rep(repnum, nrow(focalparam_powerdf[[l]]))
                
                burnin <- ceiling(nrow(focalparam_powerdf[[l]]) * burnin_prop)
                
                if (burnin == nrow(focalparam_powerdf[[l]])) {
                  focalparam_powerdf[[l]] <- focalparam_powerdf[[l]][nrow(focalparam_powerdf[[l]]), ]
                } else if (burnin > 0) {
                  focalparam_powerdf[[l]] <- focalparam_powerdf[[l]][-(1:burnin), ]
                }
                
                focalparam_powerdf[[l]] <- data.frame(focalparam_powerdf[[l]])
              }
              
              focalparam_df <- do.call(rbind, focalparam_powerdf)
            }
            
          } else {
            
            if (!is.null(input[[paste0("focalparamsummary_logfile", i, "_log", j, "_likelihoodpower")]])) {
              power <- as.numeric(input[[paste0("focalparamsummary_logfile", i, "_log", j, "_likelihoodpower")]])
              focalparam_df$likelihoodpower <- rep(power, nrow(focalparam_df))
            }
            power <- unique(focalparam_df$likelihoodpower)[1]
            
            repnum <- 1
            if (power %in% as.numeric(names(powers_repnum))) {
              repnum <- as.integer(powers_repnum[as.character(power)] + 1)
              powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
            } else {
              powers_repnum <- c(powers_repnum, 1)
              names(powers_repnum)[length(powers_repnum)] <- power
            }
            focalparam_df$repid <- rep(repnum, nrow(focalparam_df))
            
            burnin <- ceiling(nrow(focalparam_df) * burnin_prop)
            if (burnin == nrow(focalparam_df)) {
              focalparam_df <- focalparam_df[nrow(focalparam_df), ]
            } else if (burnin > 0) {
              focalparam_df <- focalparam_df[-(1:burnin), ]
            }
            
            focalparam_df <- data.frame(focalparam_df)
          }
          
          focalparam_alldf[[k]] <- focalparam_df
          k <- k + 1
        }
      }

      focalparam_alldf <- do.call(rbind, focalparam_alldf[1:(k - 1)])
      focalparam_alldf <- focalparam_alldf[order(focalparam_alldf$likelihoodpower, focalparam_alldf$repid), ]
      focalparam_alldf_processed[[i]] <- focalparam_alldf
    }
    
    focalparamsummary_values$focalparam_alldf_processed <- focalparam_alldf_processed
  })
  
  # allow the dropdown icon to change according to collapsed or expanded
  shinyjs::runjs("$('#focalparamsummary_par_edits').on('shown.bs.collapse', function() {
    $('#focalparamsummary_par_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#focalparamsummary_par_edits').on('hidden.bs.collapse', function() {
    $('#focalparamsummary_par_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#focalparamsummary_xaxis_edits').on('shown.bs.collapse', function() {
    $('#focalparamsummary_xaxis_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#focalparamsummary_xaxis_edits').on('hidden.bs.collapse', function() {
    $('#focalparamsummary_xaxis_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#focalparamsummary_yaxis_edits').on('shown.bs.collapse', function() {
    $('#focalparamsummary_yaxis_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#focalparamsummary_yaxis_edits').on('hidden.bs.collapse', function() {
    $('#focalparamsummary_yaxis_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  
  # only allow y-axis to be on the log scale (and also set to be the default) when all values of the selected parameter are strictly positive
  observe({
    req(focalparamsummary_values$focalparam_alldf_processed)
    req(focalparamsummary_values$paramname_selected)
    
    yaxis_log <- F
    if (focalparamsummary_values$paramname_selected %in% colnames(focalparamsummary_values$focalparam_alldf_processed[[1]])) {
      param_values <- unlist(lapply(focalparamsummary_values$focalparam_alldf_processed, function(x) x[, focalparamsummary_values$paramname_selected]))
      if (all(param_values > 0)) yaxis_log <- T
    }
    
    shinyjs::toggle(id = "focalparamsummary_plot_yaxis_log", condition = yaxis_log)
    if (yaxis_log) {
      updateCheckboxInput(session, inputId = "focalparamsummary_plot_yaxis_log", value = T)
    }
  })
  
  # use the selecte parameter name (as the column name of the log file) as the y-axis name
  observe({
    req(focalparamsummary_values$paramname_selected)
    updateTextInput(session = session, inputId = "focalparamsummary_plot_y_lab", value = focalparamsummary_values$paramname_selected)
  })
  
  # get the default value for the plot name
  observe({
    req(focalparamsummary_logfileset$file[[1]])
    req(focalparamsummary_values$paramname_selected)

    file_name <- paste0(unlist(strsplit(focalparamsummary_logfileset$file[[1]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1], 
                        "_", focalparamsummary_values$paramname_selected, "_datacloning")
    updateTextInput(session = session, inputId = "focalparamsummary_plot_downloadname", value = file_name)
  })
  
  # check the device plotting region width (so that we can set the height accordingly)
  observe({
    req(focalparamsummary_values$focalparam_alldf_processed)
    req(focalparamsummary_values$paramname_selected)
    
    shinyjs::runjs('Shiny.onInputChange("focalparamsummary_plot_width", document.getElementById("focalparamsummary_plot").offsetWidth);')
  })

  # plot the series of box plots
  observe({
    output$focalparamsummary_plot <- renderPlot({
      
      req(focalparamsummary_values$focalparam_alldf_processed)
      focalparam_alldf_processed <- focalparamsummary_values$focalparam_alldf_processed
      
      value_list <- list()
      group_at<- c()
      rep_id <- c()
      boxplot_col <- c()
      priormodel_name <- character(length(focalparam_alldf_processed))
      powers_all <- c()
      
      for (i in 1:length(focalparam_alldf_processed)) {
        
        focalparam_alldf <- focalparam_alldf_processed[[i]]
        
        powers <- sort(unique(focalparam_alldf$likelihoodpower))
        numpowerbetween01 <- 0
        # numpowerbetween01 <- input$focalparamsummary_numpowerbetween01
        
        if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
          
          if (numpowerbetween01 <= 0) {
            powers <- sort(powers[powers <= 0 | powers >= 1])
          } else {
            powers_between01 <- powers[powers > 0 & powers < 1]
            powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
            powers_rep <- powers_rep[-c(1, length(powers_rep))]
            powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
            powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
          }
        }
        
        powers_all <- c(powers_all, powers)
        
        for (j in 1:length(powers)) {
          
          if (powers[j] == 1) {
            col <- mycolorpalette[5]
          } else if (powers[j] == 0) {
            col <- mycolorpalette[4]
          } else {
            col <- "#A6ACAF"
          }
          
          repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
            
            if (k == 1 || (!input$focalparamsummary_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              group_at <- c(group_at, i)
              rep_id <- c(rep_id, k)
              boxplot_col <- c(boxplot_col, col)
            } else if (input$focalparamsummary_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
        
        priormodel_name[i] <- paste0("prior", i)
        if (!is.null(input[[paste0("focalparamsummary_priormodelname", i)]])) priormodel_name[i] <- input[[paste0("focalparamsummary_priormodelname", i)]]
      }
      
      axis_log <- NULL
      if ((all(unlist(value_list)) > 0) && (!is.null(input$focalparamsummary_plot_yaxis_log)) && input$focalparamsummary_plot_yaxis_log) axis_log <- "y"
      
      xaxis_lab_cex <- 1.35
      if (!is.null(input$focalparamsummary_plot_xaxis_lab_cex)) xaxis_lab_cex <- input$focalparamsummary_plot_xaxis_lab_cex
      
      xaxis_lab_line <- -1.5
      if (!is.null(input$focalparamsummary_plot_xaxis_lab_line)) xaxis_lab_line <- input$focalparamsummary_plot_xaxis_lab_line
      
      xaxis_boxgroup_labcex <- 1
      if (!is.null(input$focalparamsummary_plot_xaxis_boxgroup_labcex)) xaxis_boxgroup_labcex <- input$focalparamsummary_plot_xaxis_boxgroup_labcex
      
      xaxis_boxgroup_labline <- -2
      if (!is.null(input$focalparamsummary_plot_xaxis_boxgroup_labline)) xaxis_boxgroup_labline <- input$focalparamsummary_plot_xaxis_boxgroup_labline
      
      yaxis_lab_cex <- 0.75
      if (!is.null(input$focalparamsummary_plot_yaxis_lab_cex)) yaxis_lab_cex <- input$focalparamsummary_plot_yaxis_lab_cex
      
      yaxis_lab_line <- -1.5
      if (!is.null(input$focalparamsummary_plot_yaxis_lab_line)) yaxis_lab_line <- input$focalparamsummary_plot_yaxis_lab_line
      
      x_lab <- "number of data clones"
      if (!is.null(input$focalparamsummary_plot_x_lab)) x_lab <- input$focalparamsummary_plot_x_lab
      
      x_lab_cex <- 1.6
      if (!is.null(input$focalparamsummary_plot_x_lab_cex)) x_lab_cex <- input$focalparamsummary_plot_x_lab_cex
      
      x_lab_line <- 0.75
      if (!is.null(input$focalparamsummary_plot_x_lab_line)) x_lab_line <- input$focalparamsummary_plot_x_lab_line
      
      y_lab <- focalparamsummary_values$paramname_selected
      if (!is.null(input$focalparamsummary_plot_y_lab)) y_lab <- input$focalparamsummary_plot_y_lab
      
      y_lab_cex <- 1.6
      if (!is.null(input$focalparamsummary_plot_y_lab_cex)) y_lab_cex <- input$focalparamsummary_plot_y_lab_cex
      
      y_lab_line <- 0.9
      if (!is.null(input$focalparamsummary_plot_y_lab_line)) y_lab_line <- input$focalparamsummary_plot_y_lab_line
      
      mai_bottom <- 0.4
      if (!is.null(input$focalparamsummary_plot_margin_bottom)) mai_bottom <- input$focalparamsummary_plot_margin_bottom
      
      mai_left <- 0.45
      if (!is.null(input$focalparamsummary_plot_margin_left)) mai_left <- input$focalparamsummary_plot_margin_left
      
      mai_top <- 0.15
      if (!is.null(input$focalparamsummary_plot_margin_top)) mai_top <- input$focalparamsummary_plot_margin_top
      
      mai_right <- 0
      if (!is.null(input$focalparamsummary_plot_margin_right)) mai_right <- input$focalparamsummary_plot_margin_right
      
      par(lend = 2, mai = c(mai_bottom, mai_left, mai_top, mai_right), xpd = F)
      multigroup_boxplot(value_list = value_list, group_at = group_at, rep_id = rep_id,
                         box_col = boxplot_col, whiskcol = boxplot_col, staplecol = boxplot_col, axis_log = axis_log, 
                         plot_xaxis = T, xaxis_lab = priormodel_name, xaxis_lab_cex = xaxis_lab_cex, xaxis_lab_line = xaxis_lab_line, xaxis_side = 3, 
                         plot_xaxis_boxgroup = T, xaxis_boxgroup_lab = powers_all, xaxis_boxgroup_labcex = xaxis_boxgroup_labcex, xaxis_boxgroup_labline = xaxis_boxgroup_labline, xaxis_boxgroup_side = 1,
                         plot_yaxis = F)

      if (is.null(axis_log)) {
        axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      } else if (axis_log == "y") {
        
        axis_lab <- c(min(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.025))))),
                      max(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.975))))))
        if (ceiling(log10(axis_lab[1])) <= floor(log10(axis_lab[2]))) {
          axis_lab <- c(axis_lab[1], 10^(ceiling(log10(axis_lab[1])):floor(log10(axis_lab[2]))), axis_lab[2])
        }
        axis(side = 2, at = axis_lab,
             labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, at = axis_lab,
             labels = formatC(axis_lab, digits = 1, format = "e"), lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      }

      mtext(x_lab, side = 1, line = x_lab_line, cex = x_lab_cex)
      mtext(y_lab, side = 2, line = y_lab_line, cex = y_lab_cex)
    }, height = ifelse(!is.null(input$focalparamsummary_plot_width) && as.numeric(input$focalparamsummary_plot_width) > 0, 
                       as.numeric(input$focalparamsummary_plot_width) * 0.55, 500))
  })

  # parameter summary plot download
  output$focalparamsummary_plot_download <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$focalparamsummary_plot_downloadformat, "PDF" = ".pdf", "EPS" = ".eps", "PNG" = ".png", "JPEG" = ".jpeg", "TIFF" = ".tiff")
      paste0(input$focalparamsummary_plot_downloadname, file_extension)
    },
    
    content = function(file) {
      
      focalparam_alldf_processed <- focalparamsummary_values$focalparam_alldf_processed
      
      value_list <- list()
      group_at<- c()
      rep_id <- c()
      boxplot_col <- c()
      priormodel_name <- character(length(focalparam_alldf_processed))
      powers_all <- c()
      
      for (i in 1:length(focalparam_alldf_processed)) {
        
        focalparam_alldf <- focalparam_alldf_processed[[i]]
        
        powers <- sort(unique(focalparam_alldf$likelihoodpower))
        numpowerbetween01 <- 0
        # numpowerbetween01 <- input$focalparamsummary_numpowerbetween01
        
        if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
          
          if (numpowerbetween01 <= 0) {
            powers <- sort(powers[powers <= 0 | powers >= 1])
          } else {
            powers_between01 <- powers[powers > 0 & powers < 1]
            powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
            powers_rep <- powers_rep[-c(1, length(powers_rep))]
            powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
            powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
          }
        }
        
        powers_all <- c(powers_all, powers)
        
        for (j in 1:length(powers)) {
          
          if (powers[j] == 1) {
            col <- mycolorpalette[5]
          } else if (powers[j] == 0) {
            col <- mycolorpalette[4]
          } else {
            col <- "#A6ACAF"
          }
          
          repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
            
            if (k == 1 || (!input$focalparamsummary_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              group_at <- c(group_at, i)
              rep_id <- c(rep_id, k)
              boxplot_col <- c(boxplot_col, col)
            } else if (input$focalparamsummary_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
        
        priormodel_name[i] <- paste0("prior", i)
        if (!is.null(input[[paste0("focalparamsummary_priormodelname", i)]])) priormodel_name[i] <- input[[paste0("focalparamsummary_priormodelname", i)]]
      }
      
      axis_log <- NULL
      if ((all(unlist(value_list)) > 0) && (!is.null(input$focalparamsummary_plot_yaxis_log)) && input$focalparamsummary_plot_yaxis_log) axis_log <- "y"
      
      xaxis_lab_cex <- 1.35
      if (!is.null(input$focalparamsummary_plot_xaxis_lab_cex)) xaxis_lab_cex <- input$focalparamsummary_plot_xaxis_lab_cex
      
      xaxis_lab_line <- -2
      if (!is.null(input$focalparamsummary_plot_xaxis_lab_line)) xaxis_lab_line <- input$focalparamsummary_plot_xaxis_lab_line
      
      xaxis_boxgroup_labcex <- 1.1
      if (!is.null(input$focalparamsummary_plot_xaxis_boxgroup_labcex)) xaxis_boxgroup_labcex <- input$focalparamsummary_plot_xaxis_boxgroup_labcex
      
      xaxis_boxgroup_labline <- -2
      if (!is.null(input$focalparamsummary_plot_xaxis_boxgroup_labline)) xaxis_boxgroup_labline <- input$focalparamsummary_plot_xaxis_boxgroup_labline
      
      yaxis_lab_cex <- 0.75
      if (!is.null(input$focalparamsummary_plot_yaxis_lab_cex)) yaxis_lab_cex <- input$focalparamsummary_plot_yaxis_lab_cex
      
      yaxis_lab_line <- -1.5
      if (!is.null(input$focalparamsummary_plot_yaxis_lab_line)) yaxis_lab_line <- input$focalparamsummary_plot_yaxis_lab_line
      
      x_lab <- "number of data clones"
      if (!is.null(input$focalparamsummary_plot_x_lab)) x_lab <- input$focalparamsummary_plot_x_lab
      
      x_lab_cex <- 1.6
      if (!is.null(input$focalparamsummary_plot_x_lab_cex)) x_lab_cex <- input$focalparamsummary_plot_x_lab_cex
      
      x_lab_line <- 0.75
      if (!is.null(input$focalparamsummary_plot_x_lab_line)) x_lab_line <- input$focalparamsummary_plot_x_lab_line
      
      y_lab <- focalparamsummary_values$paramname_selected
      if (!is.null(input$focalparamsummary_plot_y_lab)) y_lab <- input$focalparamsummary_plot_y_lab
      
      y_lab_cex <- 1.6
      if (!is.null(input$focalparamsummary_plot_y_lab_cex)) y_lab_cex <- input$focalparamsummary_plot_y_lab_cex
      
      y_lab_line <- 0.9
      if (!is.null(input$focalparamsummary_plot_y_lab_line)) y_lab_line <- input$focalparamsummary_plot_y_lab_line
      
      plot_width <- 500
      if (!is.null(input$focalparamsummary_plot_width) && (as.numeric(input$focalparamsummary_plot_width) > 0)) plot_width <- as.numeric(input$focalparamsummary_plot_width)
      plot_height <- plot_width * 0.55
      res <- 300
      if (input$focalparamsummary_plot_downloadformat == "PDF") {
        pdf(file = file, width = plot_width/72, height = plot_height/72)
      } else if (input$focalparamsummary_plot_downloadformat == "EPS") {
        setEPS()
        postscript(file = file, width = plot_width/72, height = plot_height/72)
      } else if (input$focalparamsummary_plot_downloadformat == "PNG") {
        png(file = file, width = plot_width/72*res, height = plot_height/72*res, res = res)
      } else if (input$focalparamsummary_plot_downloadformat == "JPEG") {
        jpeg(file = file, width = plot_width/72*res, height = plot_height/72*res, res = res)
      } else if (input$focalparamsummary_plot_downloadformat == "TIFF") {
        tiff(file = file, width =  plot_width/72*res, height = plot_height/72*res, res = res)
      }
      
      mai_bottom <- 0.4
      if (!is.null(input$focalparamsummary_plot_margin_bottom)) mai_bottom <- input$focalparamsummary_plot_margin_bottom
      
      mai_left <- 0.45
      if (!is.null(input$focalparamsummary_plot_margin_left)) mai_left <- input$focalparamsummary_plot_margin_left
      
      mai_top <- 0.15
      if (!is.null(input$focalparamsummary_plot_margin_top)) mai_top <- input$focalparamsummary_plot_margin_top
      
      mai_right <- 0
      if (!is.null(input$focalparamsummary_plot_margin_right)) mai_right <- input$focalparamsummary_plot_margin_right
      
      par(lend = 2, mai = c(mai_bottom, mai_left, mai_top, mai_right), xpd = F)
      multigroup_boxplot(value_list = value_list, group_at = group_at, rep_id = rep_id,
                         box_col = boxplot_col, whiskcol = boxplot_col, staplecol = boxplot_col, axis_log = axis_log, 
                         plot_xaxis = T, xaxis_lab = priormodel_name, xaxis_lab_cex = xaxis_lab_cex, xaxis_lab_line = xaxis_lab_line, xaxis_side = 3, 
                         plot_xaxis_boxgroup = T, xaxis_boxgroup_lab = powers_all, xaxis_boxgroup_labcex = xaxis_boxgroup_labcex, xaxis_boxgroup_labline = xaxis_boxgroup_labline, xaxis_boxgroup_side = 1,
                         plot_yaxis = F)
      
      if (is.null(axis_log)) {
        axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      } else if (axis_log == "y") {
        
        axis_lab <- c(min(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.025))))),
                      max(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.975))))))
        if (ceiling(log10(axis_lab[1])) <= floor(log10(axis_lab[2]))) {
          axis_lab <- c(axis_lab[1], 10^(ceiling(log10(axis_lab[1])):floor(log10(axis_lab[2]))), axis_lab[2])
        }
        axis(side = 2, at = axis_lab,
             labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, at = axis_lab,
             labels = formatC(axis_lab, digits = 1, format = "e"), lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      }
      
      mtext(x_lab, side = 1, line = x_lab_line, cex = x_lab_cex)
      mtext(y_lab, side = 2, line = y_lab_line, cex = y_lab_cex)
      
      dev.off()
    }
  )
  
  # render the parameter summary table
  output$focalparamsummary_table <- DT::renderDataTable({
    
    req(focalparamsummary_values$focalparam_alldf_processed)
    focalparam_alldf_processed <- focalparamsummary_values$focalparam_alldf_processed
    
    value_list <- list()
    rep_id <- c()
    priormodelname_all <- c()
    powers_all <- c()
    
    for (i in 1:length(focalparam_alldf_processed)) {
      
      priormodel_name <- paste0("prior", i)
      if (!is.null(input[[paste0("focalparamsummary_priormodelname", i)]])) priormodel_name <- input[[paste0("focalparamsummary_priormodelname", i)]]
      
      focalparam_alldf <- focalparam_alldf_processed[[i]]
      
      powers <- sort(unique(focalparam_alldf$likelihoodpower))
      numpowerbetween01 <- 0
      # numpowerbetween01 <- input$focalparamsummary_numpowerbetween01
      
      if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
        
        if (numpowerbetween01 <= 0) {
          powers <- sort(powers[powers <= 0 | powers >= 1])
        } else {
          powers_between01 <- powers[powers > 0 & powers < 1]
          powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
          powers_rep <- powers_rep[-c(1, length(powers_rep))]
          powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
          powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
        }
      }
      
      for (j in 1:length(powers)) {
        
        repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
        for (k in 1:length(repids)) {
          
          values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
          
          if (k == 1 || (!input$focalparamsummary_logcombinereplicates)) {
            value_list <- c(value_list, list(values))
            rep_id <- c(rep_id, k)
            powers_all <- c(powers_all, powers[j])
            priormodelname_all <- c(priormodelname_all, priormodel_name)
          } else if (input$focalparamsummary_logcombinereplicates && k > 1) {
            value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
          }
          
        }
      }
    }
    
    mean_all <- sapply(value_list, mean)
    CI95lower_all <- sapply(value_list, function(x) quantile(x, probs = 0.025, names = F))
    CI95upper_all <- sapply(value_list, function(x) quantile(x, probs = 0.975, names = F))
    
    focalparamsummary_df <- data.frame(cbind(priormodelname_all, powers_all, rep_id, mean_all, CI95lower_all, CI95upper_all), stringsAsFactors = F)
    colnames(focalparamsummary_df) <- c("Prior model", "Number of data clones", "Replicate", "Mean", "Lower 95% CI", "Upper 95% CI")
    
    focalparamsummary_df
  })
  
  observe({
    req(focalparamsummary_logfileset$file[[1]])
    req(focalparamsummary_values$paramname_selected)

    file_name <- paste0(unlist(strsplit(focalparamsummary_logfileset$file[[1]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1],
                        "_", focalparamsummary_values$paramname_selected, "_datacloning")
    updateTextInput(session = session, inputId = "focalparamsummary_table_downloadname", value = file_name)
  })
  
  # parameter summary table download
  output$focalparamsummary_table_download <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$focalparamsummary_table_downloadformat, "TSV" = ".tsv", "CSV" = ".csv")
      paste0(input$focalparamsummary_table_downloadname, file_extension)
    },
    
    content = function(file) {
      
      focalparam_alldf_processed <- focalparamsummary_values$focalparam_alldf_processed
      
      value_list <- list()
      rep_id <- c()
      priormodelname_all <- c()
      powers_all <- c()
      
      for (i in 1:length(focalparam_alldf_processed)) {
        
        priormodel_name <- paste0("prior", i)
        if (!is.null(input[[paste0("focalparamsummary_priormodelname", i)]])) priormodel_name <- input[[paste0("focalparamsummary_priormodelname", i)]]
        
        focalparam_alldf <- focalparam_alldf_processed[[i]]
        
        powers <- sort(unique(focalparam_alldf$likelihoodpower))
        numpowerbetween01 <- 0
        # numpowerbetween01 <- input$focalparamsummary_numpowerbetween01
        
        if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
          
          if (numpowerbetween01 <= 0) {
            powers <- sort(powers[powers <= 0 | powers >= 1])
          } else {
            powers_between01 <- powers[powers > 0 & powers < 1]
            powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
            powers_rep <- powers_rep[-c(1, length(powers_rep))]
            powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
            powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
          }
        }
        
        for (j in 1:length(powers)) {
          
          repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
            
            if (k == 1 || (!input$focalparamsummary_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              rep_id <- c(rep_id, k)
              powers_all <- c(powers_all, powers[j])
              priormodelname_all <- c(priormodelname_all, priormodel_name)
            } else if (input$focalparamsummary_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
      }
      
      mean_all <- sapply(value_list, mean)
      CI95lower_all <- sapply(value_list, function(x) quantile(x, probs = 0.025, names = F))
      CI95upper_all <- sapply(value_list, function(x) quantile(x, probs = 0.975, names = F))
      
      focalparamsummary_df <- data.frame(cbind(priormodelname_all, powers_all, rep_id, mean_all, CI95lower_all, CI95upper_all), stringsAsFactors = F)
      colnames(focalparamsummary_df) <- c("Prior model", "Number of data clones", "Replicate", "Mean", "Lower 95% CI", "Upper 95% CI")
      
      write.table(focalparamsummary_df, file = file, sep = switch(input$focalparamsummary_table_downloadformat, "TSV" = "\t", "CSV" = ","),
                  row.names = F)
      
    }
  )

  # force dynamic ui rendering to be performed even when they are hidden
  outputOptions(output, "focalparamsummary_logprocessing_ui", suspendWhenHidden = F)
  outputOptions(output, "focalparamsummary_priormodelname_ui", suspendWhenHidden = F)
  outputOptions(output, "focalparamsummary_paramname_ui", suspendWhenHidden = F)
  
  ###################
  # robust bayesian #
  ###################
  
  focalparam2summary_logfile_defaultupload_paths <- list(c("./data/post_processing/ctmc/HIV_datasetA_underprior_run1.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_underprior_run2.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_posterior_run1.log",
                                                          "./data/post_processing/ctmc/HIV_datasetA_posterior_run2.log"),
                                                        c("./data/post_processing/exphyper/HIV_datasetA_underprior_run1.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_underprior_run2.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_posterior_run1.log",
                                                          "./data/post_processing/exphyper/HIV_datasetA_posterior_run2.log"))
  
  input_default_static_focalparam2summary <- lapply(focalparam2summary_logfile_defaultupload_paths, function(x) {
    data.frame(name = basename(x), datapath = x, check.names = F, stringsAsFactors = F)
  })
  
  observeEvent(input$focalparam2summary_logfile_defaultupload, {
    if (input$focalparam2summary_logfile_defaultupload) {
      if (focalparam2summary_loginputset$num == 1) {
        shinyjs::runjs("$('#focalparam2summary_loginput_add').click();")
      } else if (focalparam2summary_loginputset$num > 2) {
        focalparam2summary_loginputset$num <- 2L
      }
    } else {
      if (focalparam2summary_loginputset$num_inputui == 2 && (is.null(input[[paste0("focalparam2summary_logfile", 2)]]))) {
        shinyjs::runjs("$('#focalparam2summary_loginput_remove').click();")
      }
    }
  })
  
  # dynamically rendering input fields according to user specification
  focalparam2summary_loginputset <- reactiveValues(num = 1L, num_inputui = 1L)
  # add
  observeEvent(input$focalparam2summary_loginput_add, {
    focalparam2summary_loginputset$num <- focalparam2summary_loginputset$num + 1L
    focalparam2summary_loginputset$num_inputui <- focalparam2summary_loginputset$num_inputui + 1L
    
    insertUI(selector = "#focalparam2summary_loginput_addremove_div", where = "beforeBegin", 
             ui = div(id = paste0("focalparam2summary_logfile_div", focalparam2summary_loginputset$num_inputui),
                      class = ifelse(input$focalparam2summary_logfile_defaultupload, "divdisabled", ""),
                      fileInput(inputId = paste0("focalparam2summary_logfile", focalparam2summary_loginputset$num_inputui), 
                                label = paste0("Estimate log file(s) under prior No. ", focalparam2summary_loginputset$num_inputui), 
                                multiple = T, accept = ".log")))
  })
  # remove
  observeEvent(input$focalparam2summary_loginput_remove, {
    if (focalparam2summary_loginputset$num > 1) {
      removeUI(selector = paste0("#focalparam2summary_logfile_div", focalparam2summary_loginputset$num_inputui))
      focalparam2summary_loginputset$num <- focalparam2summary_loginputset$num - 1L
      focalparam2summary_loginputset$num_inputui <- focalparam2summary_loginputset$num_inputui - 1L
    }
  })
  
  observe({
    for (i in 1:focalparam2summary_loginputset$num_inputui) {
      shinyjs::toggleClass(id = paste0("focalparam2summary_logfile_div", i), class = "divdisabled", 
                           condition = input$focalparam2summary_logfile_defaultupload)
    }
  })
  observeEvent(input$focalparam2summary_logfile_defaultupload, {
    shinyjs::toggleClass(id = "focalparam2summary_loginput_addremove_div", class = "divdisabled", 
                         condition = input$focalparam2summary_logfile_defaultupload)
  })
  
  focalparam2summary_logfileset <- reactiveValues(file = NULL)
  
  observe({
    isolate(focalparam2summary_logfileset$file <- vector("list", focalparam2summary_loginputset$num))
    for (i in 1:focalparam2summary_loginputset$num) {
      if (input$focalparam2summary_logfile_defaultupload) {
        req(focalparam2summary_loginputset$num == length(input_default_static_focalparam2summary))
        isolate(focalparam2summary_logfileset$file[[i]] <- input_default_static_focalparam2summary[[i]])
      } else {
        if (!is.null(input[[paste0("focalparam2summary_logfile", i)]])) {
          isolate(focalparam2summary_logfileset$file[[i]] <- input[[paste0("focalparam2summary_logfile", i)]])
        }
      }
    }
  })
  
  input_processed <- reactiveValues()
  observe({
    
    req(focalparam2summary_loginputset$num == length(focalparam2summary_logfileset$file))
    for (i in 1:focalparam2summary_loginputset$num) {
      req(!is.null(focalparam2summary_logfileset$file[[i]]))
    }
    
    input_tmp <- lapply(1:focalparam2summary_loginputset$num, function(i) focalparam2summary_logfileset$file[[i]])
    input_tmp <- lapply(input_tmp, function(x) x[grep("_underprior|_posterior", x$name), ])
    names(input_tmp) <- paste0("focalparam2summary_logfile", 1:focalparam2summary_loginputset$num)
    
    input_processed$focalparam2summary_logfile <- input_tmp
  })
  
  
  output$focalparam2summary_logprocessing_ui <- renderUI({
    
    req(focalparam2summary_loginputset$num == length(focalparam2summary_logfileset$file))
    focalparam2summary_logprocessing_ui_list <- tagList()
    
    logprocessing_perpanel_height <- 540 / focalparam2summary_loginputset$num
    if (logprocessing_perpanel_height < 200) logprocessing_perpanel_height <- 200
    
    for (i in 1:focalparam2summary_loginputset$num) {
      
      if (!is.null(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
        
        powers <- numeric(nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]))
        for (j in 1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
          if (grepl("_underprior", input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j])) {
            powers[j] <- 0
          } else if (grepl("_posterior", input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j])) {
            powers[j] <- 1
          } else if (grepl("_datacloning", input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j])) {
            powers[j] <- as.numeric(gsub("datacloning", "", grep("datacloning", unlist(strsplit(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j], "_|\\.")), value = T)[1]))
          } else {
            col_names <- colnames(read.table(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$datapath[j], header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
            if ("pathLikelihood.theta" %in% col_names) powers[j] <- 0.5
          }
        }
        
        focalparam2summary_logprocessing_ui_list <- tagAppendChildren(focalparam2summary_logprocessing_ui_list,
                                                                      HTML(paste0("<div id = \"focalparam2summary_logprocessing_panel", i, 
                                                                                  "\" class = \"panel-heading\"><p class=\"panel-title\" style=\"font-size: 95%; font-weight: bold;\">", 
                                                                                  "<a data-toggle=\"collapse\" href=\"#focalparam2summary_logprocessing", i,
                                                                                  "\"><span class=\"glyphicon glyphicon-chevron-up\" aria-hidden=\"true\"></span>", 
                                                                                  "Settings for estimate log file(s) under prior model No. ", i, 
                                                                                  "</a></p></div>")),
                                                                      div(id = paste0("focalparam2summary_logprocessing", i), 
                                                                          class = "panel-collapse collapse in", 
                                                                          fluidRow(style = paste0("max-height: ", logprocessing_perpanel_height, "px; overflow: auto; background-color: #EBF5FB; padding: 2px 10px 5px 10px;"),
                                                                                   lapply(1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]), function(j) {
                                                                                     tagList(
                                                                                       p(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j], align = "center", 
                                                                                         style = "font-size: 95%; font-weight: bold;"),
                                                                                       # numericInput(inputId = paste0("focalparam2summary_logfile", i, "_log", j, "_likelihoodpower"), 
                                                                                       #              label = "Number of copies of the data",
                                                                                       #              value = powers[j], min = 0),
                                                                                       radioButtons(inputId = paste0("focalparam2summary_logfile", i, "_log", j, "_likelihoodpower"), 
                                                                                                    label = "Posterior (with data) or Prior (without data)", 
                                                                                                    choices = c("posterior", "prior"), 
                                                                                                    selected = ifelse(powers[j] == 0, "prior", "posterior"), inline = T),
                                                                                       checkboxInput(inputId = paste0("focalparam2summary_logfile", i, "_log", j, "_include"),
                                                                                                     label = "include this log file", value = T),
                                                                                       sliderInput(inputId = paste0("focalparam2summary_logfile", i, "_log", j, "_burnin"),
                                                                                                   label = "burn-in proportion",
                                                                                                   min = 0, max = 99, value = 10),
                                                                                       p("", align = "center", style = "font-size: 85%;"))
                                                                                   })[order(powers)])
                                                                          
                                                                      ))
        if (i != focalparam2summary_loginputset$num) {
          focalparam2summary_logprocessing_ui_list <- tagAppendChildren(focalparam2summary_logprocessing_ui_list, h6(""))
        }
        
      }
    }
    
    focalparam2summary_logprocessing_ui_list
  })
  
  # sanity check to make sure that at least one log file exist under each prior model
  observe({
    
    for (i in 1:focalparam2summary_loginputset$num) {
      req(!is.null(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]))
      sum <- 0
      
      for (j in 1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
        req(!is.null(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_include")]]))
        sum <- sum + input[[paste0("focalparam2summary_logfile", i, "_log", j, "_include")]]
      }
      
      if (sum == 0) {
        showModal(modalDialog(paste0("No log file is included for prior model ", i,
                                     ". At least one log file needs to be included under each prior model."),
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
        ))
        break
      }
    }
  })
  
  observe({
    lapply(1:focalparam2summary_loginputset$num, function(i) {
      shinyjs::runjs(HTML(paste0("$('#focalparam2summary_logprocessing", i, "').on('shown.bs.collapse', function() {
                              $('#focalparam2summary_logprocessing_panel", i, "').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
                            });
                            $('#focalparam2summary_logprocessing", i, "').on('hidden.bs.collapse', function() {
                              $('#focalparam2summary_logprocessing_panel", i, "').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
                            });")))
    })
  })
  
  output$focalparam2summary_priormodelname_ui <- renderUI({
    
    req(focalparam2summary_loginputset$num >= 1)
    focalparam2summary_priormodelname_ui_list <- tagList()
    
    for (i in 1:focalparam2summary_loginputset$num) {
      req((!is.null(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) && (nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]) >= 1))
      
      focalparam2summary_priormodelname_ui_list <- tagAppendChildren(focalparam2summary_priormodelname_ui_list,
                                                                     textInput(inputId = paste0("focalparam2summary_priormodelname", i),
                                                                               label = paste0("Name of prior No. ", i),
                                                                               value = paste0("prior", i)))
    }
    
    focalparam2summary_priormodelname_ui_list
  })
  
  # obtain the parameter list and the selected parameter
  focalparam2summary_values <- reactiveValues(paramnames_shared_focal = NULL, paramnames_shared_nonfocal = NULL, 
                                              paramname_selected = NULL, 
                                              focalparam_alldf_raw = NULL, focalparam_alldf_processed = NULL)
  observe({
    
    focalparam2summary_values$paramnames_shared_focal <- NULL
    focalparam2summary_values$paramnames_shared_nonfocal <- NULL
    
    for (i in 1:focalparam2summary_loginputset$num) {
      req(focalparam2summary_logfileset$file[[i]])
    }
    
    paramnames <- list()
    for (i in 1:focalparam2summary_loginputset$num) {
      req(nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]) >= 1)
      
      for (j in 1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
        paramnames <- c(paramnames, list(colnames(read.table(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$datapath[j], 
                                                             header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))))
      }
    }
    
    paramnames_shared <- names(table(unlist(paramnames)))[table(unlist(paramnames)) == length(paramnames)]
    paramnames_shared <- paramnames_shared[paramnames_shared != "state"]
    
    paramnames_shared_focal <- c()
    if (any(grepl(".clock.rate$", paramnames_shared))) {
      paramnames_shared_focal <- c(paramnames_shared_focal, grep(".clock.rate$", paramnames_shared, value = T)[1])
    }
    if (any(grepl(".nonZeroRates$", paramnames_shared))) {
      paramnames_shared_focal <- c(paramnames_shared_focal, grep(".nonZeroRates$", paramnames_shared, value = T)[1])
    }
    
    if (length(grep(".count", paramnames_shared)) == 1) {
      paramnames_shared_focal <- c(paramnames_shared_focal, grep(".count", paramnames_shared, value = T))
    } else if ((length(grep(".count", paramnames_shared)) > 1) && (!any(grepl(".count\\[2", paramnames_shared)))) {
      nrow_tmp <- 10
      logdat_tmp <- read.table(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", 1)]]$datapath[1],
                               header = T, sep = "\t", nrows = nrow_tmp, stringsAsFactors = F, check.names = F)
      totalcount_id <- apply(logdat_tmp[, grep(".count", paramnames_shared, value = T)], MARGIN = 1, function(x) which(x == max(x)))
      if (length(unique(totalcount_id)) == 1) {
        totalcount_id <- unique(totalcount_id)
        paramnames_shared_focal <- c(paramnames_shared_focal, grep(".count", paramnames_shared, value = T)[totalcount_id])
      } else if (length(unique(totalcount_id)) > 1) {
        totalcount_id <- as.integer(names(table(totalcount_id)[table(totalcount_id) == nrow_tmp]))[1]
        paramnames_shared_focal <- c(paramnames_shared_focal, grep(".count", paramnames_shared, value = T)[totalcount_id])
      }
      
    }
    if (length(paramnames_shared_focal) == 0) paramnames_shared_focal <- NULL
    
    paramnames_shared_nonfocal <- paramnames_shared[!(paramnames_shared %in% paramnames_shared_focal)]
    paramnames_shared_nonfocal_putfirst <- paramnames_shared_nonfocal[paramnames_shared_nonfocal %in% c("posterior", "prior", "likelihood")]
    paramnames_shared_nonfocal <- c(paramnames_shared_nonfocal_putfirst, paramnames_shared_nonfocal[!(paramnames_shared_nonfocal %in% paramnames_shared_nonfocal_putfirst)])
    if (length(paramnames_shared_nonfocal) == 0) paramnames_shared_nonfocal <- NULL
    
    focalparam2summary_values$paramnames_shared_focal <- paramnames_shared_focal
    focalparam2summary_values$paramnames_shared_nonfocal <- paramnames_shared_nonfocal
  })
  
  
  # reading in the header first and then find out the intersect columns
  # generate a drop down menu whose fields are the column names (except for the iteration)
  output$focalparam2summary_paramname_ui <- renderUI({
    
    req(is.null(focalparam2summary_values$paramnames_shared_focal) + is.null(focalparam2summary_values$paramnames_shared_nonfocal) < 2)
    
    req(focalparam2summary_loginputset$num == length(focalparam2summary_logfileset$file))
    for (i in 1:focalparam2summary_loginputset$num) {
      req(focalparam2summary_logfileset$file[[i]])
    }
    
    paramnames_shared_focal <- focalparam2summary_values$paramnames_shared_focal
    paramnames_shared_nonfocal <- focalparam2summary_values$paramnames_shared_nonfocal
    
    focalparam2summary_paramname_ui_list <- tagList()
    focalparam2summary_paramname_ui_list <- tagAppendChildren(focalparam2summary_paramname_ui_list, 
                                                              h2(''),
                                                              p("Select the parameter to summarize", align = "center", 
                                                                style = "font-size: 95%; font-weight: bold;"))
    
    if (!is.null(paramnames_shared_focal)) {
      paramnames_radiobutton_list <- paramnames_shared_focal
      
      if (!is.null(paramnames_shared_nonfocal)) {
        paramnames_radiobutton_list <- c(paramnames_radiobutton_list, "show others")
      }
      
      focalparam2summary_paramname_ui_list <- tagAppendChildren(focalparam2summary_paramname_ui_list,
                                                                radioButtons(inputId = "focalparam2summary_paramnameshared_focallist",
                                                                             label = NULL, choices = paramnames_radiobutton_list))
    }
    
    if (!is.null(paramnames_shared_nonfocal)) {
      focalparam2summary_paramname_ui_list <- tagAppendChildren(focalparam2summary_paramname_ui_list,
                                                                div(id = "focalparam2summary_paramnameshared_nonfocallist_div", style = "display: none;", 
                                                                    selectInput(inputId = "focalparam2summary_paramnameshared_nonfocallist", label = NULL, 
                                                                                choices = paramnames_shared_nonfocal, selectize = F)))
    }
    
    focalparam2summary_paramname_ui_list
  })
  
  observe({
    focalparam2summary_loginputset$num
    nonfocallist_enabled <- ((is.null(focalparam2summary_values$paramnames_shared_focal)) || (input$focalparam2summary_paramnameshared_focallist == "show others")) && (!is.null(focalparam2summary_values$paramnames_shared_nonfocal)) 
    shinyjs::toggle(id = "focalparam2summary_paramnameshared_nonfocallist_div", condition = nonfocallist_enabled)
  })
  
  focalparam2summary_startprocessing_status <- reactiveValues(enabled = F, clicked = F)
  observe({
    shinyjs::toggle(id = "focalparam2summary_startprocessing", condition = focalparam2summary_startprocessing_status$enabled)
  })
  
  observe({
    
    focalparam2summary_startprocessing_enabled <- T
    focalparam2summary_startprocessing_clicked <- focalparam2summary_startprocessing_status$clicked
    
    if (is.null(focalparam2summary_values$paramnames_shared_focal) + is.null(focalparam2summary_values$paramnames_shared_nonfocal) == 2) {
      focalparam2summary_startprocessing_enabled <- F
      focalparam2summary_startprocessing_clicked <- F
    } else if (is.null(input$focalparam2summary_paramnameshared_focallist) + is.null(input$focalparam2summary_paramnameshared_nonfocallist) == 2) {
      focalparam2summary_startprocessing_enabled <- F
      focalparam2summary_startprocessing_clicked <- F
    } else {
      for (i in 1:focalparam2summary_loginputset$num) {
        if (is.null(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]) || input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]] == "" || nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]) < 1) {
          focalparam2summary_startprocessing_enabled <- F
          focalparam2summary_startprocessing_clicked <- F
        }
      }
    }
    
    focalparam2summary_startprocessing_status$enabled <- focalparam2summary_startprocessing_enabled
    focalparam2summary_startprocessing_status$clicked <- focalparam2summary_startprocessing_clicked
  })
  
  observe({
    if ((!is.null(input$focalparam2summary_paramnameshared_focallist)) && (input$focalparam2summary_paramnameshared_focallist != "show others")) {
      focalparam2summary_values$paramname_selected <- input$focalparam2summary_paramnameshared_focallist
    } else if (!is.null(input$focalparam2summary_paramnameshared_nonfocallist)) {
      focalparam2summary_values$paramname_selected <- input$focalparam2summary_paramnameshared_nonfocallist
    } else {
      focalparam2summary_values$paramname_selected <- NULL
      focalparam2summary_startprocessing_status$enabled <- F
    }
  })
  
  observe({
    focalparam2summary_tab2_disabled <- (input$focalparam2summary_startprocessing == 0) || (!focalparam2summary_startprocessing_status$clicked) || is.null(focalparam2summary_values$focalparam_alldf_processed)
    
    shinyjs::toggleClass(selector = "#focalparam2summary_tabs li a[data-value=processing_settings]", class = "divdisabled", 
                         condition = focalparam2summary_tab2_disabled)
    if (!focalparam2summary_tab2_disabled) {
      updateNavlistPanel(session, inputId = "focalparam2summary_tabs", selected = "processing_settings")
    }
  })
  
  observe({
    focalparam2summary_tab3_disabled <- (input$focalparam2summary_startprocessing == 0) || (!focalparam2summary_startprocessing_status$clicked) || is.null(focalparam2summary_values$focalparam_alldf_processed)
    
    shinyjs::toggleClass(selector = "#focalparam2summary_tabs li a[data-value=download_output]", class = "divdisabled", 
                         condition = focalparam2summary_tab3_disabled)
  })
  
  observe({
    focalparam2summary_tab2_disabled <- (input$focalparam2summary_startprocessing == 0) || (!focalparam2summary_startprocessing_status$clicked) || is.null(focalparam2summary_values$focalparam_alldf_processed)
    shinyjs::toggle(id = "focalparam2summary_result_div", condition = !focalparam2summary_tab2_disabled)
  })
  
  
  # reading in the posterior log files
  observeEvent(input$focalparam2summary_startprocessing, {
    
    req(focalparam2summary_loginputset$num >= 1)
    req(focalparam2summary_values$paramname_selected)
    focalparam_alldf_raw <- vector("list", focalparam2summary_loginputset$num)
    
    req(focalparam2summary_loginputset$num == length(focalparam2summary_logfileset$file))
    for (i in 1:focalparam2summary_loginputset$num) {
      req(focalparam2summary_logfileset$file[[i]])
    }
    
    lognum_all <- sum(sapply(1:focalparam2summary_loginputset$num, function(i) nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Reading in log files", value = 0)
    for (i in 1:focalparam2summary_loginputset$num) {
      
      focalparam_alldf <- list()
      powers_repnum <- c()
      
      for (j in 1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
        
        progress$inc(1/lognum_all, detail = paste0("Processing estimate log file No.", j , " under prior model No.", i))
        
        col_names <- colnames(read.table(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$datapath[j], header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
        
        col_classes <- rep("NULL", length(col_names))
        col_classes[col_names == focalparam2summary_values$paramname_selected | col_names == "pathLikelihood.theta"] <- "numeric"
        
        focalparam_df <- read.table(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$datapath[j], header = T, sep = "\t", colClasses = col_classes, stringsAsFactors = F)
        
        if ("pathLikelihood.theta" %in% col_names) {
          focalparam_df <- focalparam_df[, c("pathLikelihood.theta", sort(colnames(focalparam_df)[colnames(focalparam_df) != "pathLikelihood.theta"]))]
          
          powers_current <- sort(unique(focalparam_df$pathLikelihood.theta))
          powers_repnum_current <- rep(1, length(powers_current))
          names(powers_repnum_current) <- powers_current
          for (k in 1:length(powers_repnum_current)) {
            if (as.numeric(names(powers_repnum_current)[k]) %in% as.numeric(names(powers_repnum))) {
              powers_repnum_current[k] <- powers_repnum[names(powers_repnum_current)[k]] + 1
              powers_repnum[names(powers_repnum_current)[k]] <- powers_repnum[names(powers_repnum_current)[k]] + 1
            } else {
              powers_repnum <- c(powers_repnum, 1)
              names(powers_repnum)[length(powers_repnum)] <- names(powers_repnum_current)[k]
            }
          }
          
          focalparam_df <- data.frame(cbind(focalparam_df$pathLikelihood.theta, as.integer(powers_repnum_current[as.character(focalparam_df$pathLikelihood.theta)]), 
                                            focalparam_df[, colnames(focalparam_df) != "pathLikelihood.theta"]))
          
        } else {
          
          power <- 1
          if (grepl("_underprior", input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j])) {
            power <- 0
          } else if (grepl("_posterior", input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j])) {
            power <- 1
          } else if (grepl("_datacloning", input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j])) {
            power <- as.numeric(gsub("datacloning", "", grep("datacloning", unlist(strsplit(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]$name[j], "_|\\.")), value = T)[1]))
          }
          
          repnum <- 1
          if (power %in% as.numeric(names(powers_repnum))) {
            repnum <- as.integer(powers_repnum[as.character(power)] + 1)
            powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
          } else {
            powers_repnum <- c(powers_repnum, 1)
            names(powers_repnum)[length(powers_repnum)] <- power
          }
          
          focalparam_df <- cbind(rep(power, nrow(focalparam_df)), rep(repnum, nrow(focalparam_df)), focalparam_df)
        }
        
        colnames(focalparam_df) <- c("likelihoodpower", "repid", focalparam2summary_values$paramname_selected)
        focalparam_alldf <- c(focalparam_alldf, list(focalparam_df))
      }
      
      focalparam_alldf_raw[[i]] <- focalparam_alldf
    }
    
    focalparam2summary_values$focalparam_alldf_raw <- focalparam_alldf_raw
    focalparam2summary_startprocessing_status$enabled <- F
    focalparam2summary_startprocessing_status$clicked <- T
  })
  
  observe({
    
    req(focalparam2summary_values$focalparam_alldf_raw)
    req(focalparam2summary_loginputset$num == length(focalparam2summary_logfileset$file))
    for (i in 1:focalparam2summary_loginputset$num) {
      req(focalparam2summary_logfileset$file[[i]])
    }
    
    focalparam_alldf_raw <- focalparam2summary_values$focalparam_alldf_raw
    focalparam_alldf_processed <- vector("list", length(focalparam_alldf_raw))
    
    req(length(focalparam_alldf_raw) == focalparam2summary_loginputset$num)
    
    for (i in 1:focalparam2summary_loginputset$num) {
      
      focalparam_alldf <- focalparam_alldf_raw[[i]]
      req(length(focalparam_alldf) == nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]]))
      
      posterior_notinpowerposterior_exist <- prior_notinpowerposterior_exist <- F
      
      # figuring out if posterior and prior is already there and if so we won't include them from the power posterior analyses
      for (j in 1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
        if ((!is.null(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_include")]])) && input[[paste0("focalparam2summary_logfile", i, "_log", j, "_include")]]) {
          
          powers <- unique(focalparam_alldf[[j]]$likelihoodpower)
          if (length(powers) == 1) {
            if (powers == 1) {
              posterior_notinpowerposterior_exist <- T
            } else if (powers == 0) {
              prior_notinpowerposterior_exist <- T
            }
          }
          
        }
      }
      
      powers_repnum <- c()
      k <- 1
      for (j in 1:nrow(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", i)]])) {
        if ((!is.null(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_include")]])) && input[[paste0("focalparam2summary_logfile", i, "_log", j, "_include")]]) {
          
          focalparam_df <- focalparam_alldf[[j]]
          burnin_prop <- 0
          if ((!is.null(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_burnin")]])) && input[[paste0("focalparam2summary_logfile", i, "_log", j, "_burnin")]] > 0) {
            burnin_prop <- input[[paste0("focalparam2summary_logfile", i, "_log", j, "_burnin")]]/100
          }
          
          if (length(unique(focalparam_df$likelihoodpower)) >= 2) {
            
            powers <- sort(unique(focalparam_df$likelihoodpower))
            if (any(powers == 0) && prior_notinpowerposterior_exist) powers <- powers[powers != 0]
            if (any(powers == 1) && posterior_notinpowerposterior_exist) powers <- powers[powers != 1]
            
            if (length(powers) == 0) {
              focalparam_df <- focalparam_df[-(1:nrow(focalparam_df)),]
            } else {
              focalparam_powerdf <- vector("list", length(powers))
              
              for (l in 1:length(powers)) {
                focalparam_powerdf[[l]] <- focalparam_df[focalparam_df$likelihoodpower == powers[l], ]
                
                power <- powers[l]
                repnum <- 1
                if (power %in% as.numeric(names(powers_repnum))) {
                  repnum <- as.integer(powers_repnum[as.character(power)] + 1)
                  powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
                } else {
                  powers_repnum <- c(powers_repnum, 1)
                  names(powers_repnum)[length(powers_repnum)] <- power
                }
                focalparam_powerdf[[l]]$repid <- rep(repnum, nrow(focalparam_powerdf[[l]]))
                
                burnin <- ceiling(nrow(focalparam_powerdf[[l]]) * burnin_prop)
                
                if (burnin == nrow(focalparam_powerdf[[l]])) {
                  focalparam_powerdf[[l]] <- focalparam_powerdf[[l]][nrow(focalparam_powerdf[[l]]), ]
                } else if (burnin > 0) {
                  focalparam_powerdf[[l]] <- focalparam_powerdf[[l]][-(1:burnin), ]
                }
                
                focalparam_powerdf[[l]] <- data.frame(focalparam_powerdf[[l]])
              }
              
              focalparam_df <- do.call(rbind, focalparam_powerdf)
            }
            
          } else {
            
            if (!is.null(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_likelihoodpower")]])) {
              # power <- as.numeric(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_likelihoodpower")]])
              power <- ifelse(input[[paste0("focalparam2summary_logfile", i, "_log", j, "_likelihoodpower")]] == "prior", 0, 1)
              focalparam_df$likelihoodpower <- rep(power, nrow(focalparam_df))
            }
            
            power <- unique(focalparam_df$likelihoodpower)[1]
            
            repnum <- 1
            if (power %in% as.numeric(names(powers_repnum))) {
              repnum <- as.integer(powers_repnum[as.character(power)] + 1)
              powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
            } else {
              powers_repnum <- c(powers_repnum, 1)
              names(powers_repnum)[length(powers_repnum)] <- power
            }
            focalparam_df$repid <- rep(repnum, nrow(focalparam_df))
            
            burnin <- ceiling(nrow(focalparam_df) * burnin_prop)
            if (burnin == nrow(focalparam_df)) {
              focalparam_df <- focalparam_df[nrow(focalparam_df), ]
            } else if (burnin > 0) {
              focalparam_df <- focalparam_df[-(1:burnin), ]
            }
            
            focalparam_df <- data.frame(focalparam_df)
          }
          
          focalparam_alldf[[k]] <- focalparam_df
          k <- k + 1
        }
      }
      
      focalparam_alldf <- do.call(rbind, focalparam_alldf[1:(k - 1)])
      focalparam_alldf <- focalparam_alldf[order(focalparam_alldf$likelihoodpower, focalparam_alldf$repid), ]
      focalparam_alldf_processed[[i]] <- focalparam_alldf
    }
    
    focalparam2summary_values$focalparam_alldf_processed <- focalparam_alldf_processed
  })
  
  shinyjs::runjs("$('#focalparam2summary_par_edits').on('shown.bs.collapse', function() {
    $('#focalparam2summary_par_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#focalparam2summary_par_edits').on('hidden.bs.collapse', function() {
    $('#focalparam2summary_par_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#focalparam2summary_xaxis_edits').on('shown.bs.collapse', function() {
    $('#focalparam2summary_xaxis_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#focalparam2summary_xaxis_edits').on('hidden.bs.collapse', function() {
    $('#focalparam2summary_xaxis_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#focalparam2summary_yaxis_edits').on('shown.bs.collapse', function() {
    $('#focalparam2summary_yaxis_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#focalparam2summary_yaxis_edits').on('hidden.bs.collapse', function() {
    $('#focalparam2summary_yaxis_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  
  observe({
    req(focalparam2summary_values$focalparam_alldf_processed)
    req(focalparam2summary_values$paramname_selected)
    
    yaxis_log <- F
    if (focalparam2summary_values$paramname_selected %in% colnames(focalparam2summary_values$focalparam_alldf_processed[[1]])) {
      param_values <- unlist(lapply(focalparam2summary_values$focalparam_alldf_processed, function(x) x[, focalparam2summary_values$paramname_selected]))
      if (all(param_values > 0)) yaxis_log <- T
    }
    
    shinyjs::toggle(id = "focalparam2summary_plot_yaxis_log", condition = yaxis_log)
    if (yaxis_log) {
      updateCheckboxInput(session, inputId = "focalparam2summary_plot_yaxis_log", value = T)
    }
  })
  
  observe({
    req(focalparam2summary_values$paramname_selected)
    updateTextInput(session = session, inputId = "focalparam2summary_plot_y_lab", value = focalparam2summary_values$paramname_selected)
  })
  
  observe({
    req(focalparam2summary_logfileset$file[[1]])
    req(focalparam2summary_values$paramname_selected)
    
    file_name <- paste0(unlist(strsplit(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", 1)]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1], 
                        "_", focalparam2summary_values$paramname_selected, "_robustbayesian")
    updateTextInput(session = session, inputId = "focalparam2summary_plot_downloadname", value = file_name)
  })
  
  observe({
    req(focalparam2summary_values$focalparam_alldf_processed)
    req(focalparam2summary_values$paramname_selected)
    
    shinyjs::runjs('Shiny.onInputChange("focalparam2summary_plot_width", document.getElementById("focalparam2summary_plot").offsetWidth);')
  })
  
  
  observe({
    output$focalparam2summary_plot <- renderPlot({
      
      req(focalparam2summary_values$focalparam_alldf_processed)
      focalparam_alldf_processed <- focalparam2summary_values$focalparam_alldf_processed
      
      value_list <- list()
      group_at<- c()
      rep_id <- c()
      boxplot_col <- c()
      priormodel_name <- character(length(focalparam_alldf_processed))
      powers_all <- c()
      
      for (i in 1:length(focalparam_alldf_processed)) {
        
        focalparam_alldf <- focalparam_alldf_processed[[i]]
        
        powers <- sort(unique(focalparam_alldf$likelihoodpower))
        powers <- powers[powers == 0 | powers == 1]
        powers_all <- c(powers_all, powers)
        
        for (j in 1:length(powers)) {
          
          if (powers[j] == 1) {
            col <- mycolorpalette[5]
          } else if (powers[j] == 0) {
            col <- mycolorpalette[4]
          } else {
            col <- "#A6ACAF"
          }
          
          repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
            
            if (k == 1 || (!input$focalparam2summary_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              group_at <- c(group_at, i)
              rep_id <- c(rep_id, k)
              boxplot_col <- c(boxplot_col, col)
            } else if (input$focalparam2summary_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
        
        priormodel_name[i] <- paste0("prior", i)
        if (!is.null(input[[paste0("focalparam2summary_priormodelname", i)]])) priormodel_name[i] <- input[[paste0("focalparam2summary_priormodelname", i)]]
      }
      
      axis_log <- NULL
      if ((all(unlist(value_list)) > 0) && (!is.null(input$focalparam2summary_plot_yaxis_log)) && input$focalparam2summary_plot_yaxis_log) axis_log <- "y"
      
      xaxis_lab_cex <- 1.35
      if (!is.null(input$focalparam2summary_plot_xaxis_lab_cex)) xaxis_lab_cex <- input$focalparam2summary_plot_xaxis_lab_cex
      
      xaxis_lab_line <- -1.5
      if (!is.null(input$focalparam2summary_plot_xaxis_lab_line)) xaxis_lab_line <- input$focalparam2summary_plot_xaxis_lab_line
      
      xaxis_boxgroup_labcex <- 1
      if (!is.null(input$focalparam2summary_plot_xaxis_boxgroup_labcex)) xaxis_boxgroup_labcex <- input$focalparam2summary_plot_xaxis_boxgroup_labcex
      
      xaxis_boxgroup_labline <- -2
      if (!is.null(input$focalparam2summary_plot_xaxis_boxgroup_labline)) xaxis_boxgroup_labline <- input$focalparam2summary_plot_xaxis_boxgroup_labline
      
      yaxis_lab_cex <- 0.75
      if (!is.null(input$focalparam2summary_plot_yaxis_lab_cex)) yaxis_lab_cex <- input$focalparam2summary_plot_yaxis_lab_cex
      
      yaxis_lab_line <- -1.5
      if (!is.null(input$focalparam2summary_plot_yaxis_lab_line)) yaxis_lab_line <- input$focalparam2summary_plot_yaxis_lab_line
      
      y_lab <- focalparam2summary_values$paramname_selected
      if (!is.null(input$focalparam2summary_plot_y_lab)) y_lab <- input$focalparam2summary_plot_y_lab
      
      y_lab_cex <- 1.6
      if (!is.null(input$focalparam2summary_plot_y_lab_cex)) y_lab_cex <- input$focalparam2summary_plot_y_lab_cex
      
      y_lab_line <- 0.9
      if (!is.null(input$focalparam2summary_plot_y_lab_line)) y_lab_line <- input$focalparam2summary_plot_y_lab_line
      
      mai_bottom <- 0.25
      if (!is.null(input$focalparam2summary_plot_margin_bottom)) mai_bottom <- input$focalparam2summary_plot_margin_bottom
      
      mai_left <- 0.45
      if (!is.null(input$focalparam2summary_plot_margin_left)) mai_left <- input$focalparam2summary_plot_margin_left
      
      mai_top <- 0.15
      if (!is.null(input$focalparam2summary_plot_margin_top)) mai_top <- input$focalparam2summary_plot_margin_top
      
      mai_right <- 0
      if (!is.null(input$focalparam2summary_plot_margin_right)) mai_right <- input$focalparam2summary_plot_margin_right
      
      par(lend = 2, mai = c(mai_bottom, mai_left, mai_top, mai_right), xpd = F)
      multigroup_boxplot(value_list = value_list, group_at = group_at, rep_id = rep_id,
                         box_col = boxplot_col, whiskcol = boxplot_col, staplecol = boxplot_col, axis_log = axis_log, 
                         plot_xaxis = T, xaxis_lab = priormodel_name, xaxis_lab_cex = xaxis_lab_cex, xaxis_lab_line = xaxis_lab_line, xaxis_side = 3, 
                         plot_xaxis_boxgroup = T, xaxis_boxgroup_lab = c("prior", "posterior")[powers_all + 1], 
                         xaxis_boxgroup_labcex = xaxis_boxgroup_labcex, xaxis_boxgroup_labline = xaxis_boxgroup_labline, xaxis_boxgroup_side = 1,
                         plot_yaxis = F)
      
      if (is.null(axis_log)) {
        axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      } else if (axis_log == "y") {
        
        axis_lab <- c(min(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.025))))),
                      max(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.975))))))
        if (ceiling(log10(axis_lab[1])) <= floor(log10(axis_lab[2]))) {
          axis_lab <- c(axis_lab[1], 10^(ceiling(log10(axis_lab[1])):floor(log10(axis_lab[2]))), axis_lab[2])
        }
        axis(side = 2, at = axis_lab,
             labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, at = axis_lab,
             labels = formatC(axis_lab, digits = 1, format = "e"), lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      }
      
      mtext(y_lab, side = 2, line = y_lab_line, cex = y_lab_cex)
    }, height = ifelse(!is.null(input$focalparam2summary_plot_width) && as.numeric(input$focalparam2summary_plot_width) > 0, 
                       as.numeric(input$focalparam2summary_plot_width) * 0.55, 500))
  })
  

  output$focalparam2summary_plot_download <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$focalparam2summary_plot_downloadformat, "PDF" = ".pdf", "EPS" = ".eps", "PNG" = ".png", "JPEG" = ".jpeg", "TIFF" = ".tiff")
      paste0(input$focalparam2summary_plot_downloadname, file_extension)
    },
    
    content = function(file) {
      
      focalparam_alldf_processed <- focalparam2summary_values$focalparam_alldf_processed
      
      value_list <- list()
      group_at<- c()
      rep_id <- c()
      boxplot_col <- c()
      priormodel_name <- character(length(focalparam_alldf_processed))
      powers_all <- c()
      
      for (i in 1:length(focalparam_alldf_processed)) {
        
        focalparam_alldf <- focalparam_alldf_processed[[i]]
        
        powers <- sort(unique(focalparam_alldf$likelihoodpower))
        powers <- powers[powers == 0 | powers == 1]
        powers_all <- c(powers_all, powers)
        
        for (j in 1:length(powers)) {
          
          if (powers[j] == 1) {
            col <- mycolorpalette[5]
          } else if (powers[j] == 0) {
            col <- mycolorpalette[4]
          } else {
            col <- "#A6ACAF"
          }
          
          repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
            
            if (k == 1 || (!input$focalparam2summary_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              group_at <- c(group_at, i)
              rep_id <- c(rep_id, k)
              boxplot_col <- c(boxplot_col, col)
            } else if (input$focalparam2summary_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
        
        priormodel_name[i] <- paste0("prior", i)
        if (!is.null(input[[paste0("focalparam2summary_priormodelname", i)]])) priormodel_name[i] <- input[[paste0("focalparam2summary_priormodelname", i)]]
      }
      
      axis_log <- NULL
      if ((all(unlist(value_list)) > 0) && (!is.null(input$focalparam2summary_plot_yaxis_log)) && input$focalparam2summary_plot_yaxis_log) axis_log <- "y"
      
      xaxis_lab_cex <- 1.35
      if (!is.null(input$focalparam2summary_plot_xaxis_lab_cex)) xaxis_lab_cex <- input$focalparam2summary_plot_xaxis_lab_cex
      
      xaxis_lab_line <- -2
      if (!is.null(input$focalparam2summary_plot_xaxis_lab_line)) xaxis_lab_line <- input$focalparam2summary_plot_xaxis_lab_line
      
      xaxis_boxgroup_labcex <- 1.1
      if (!is.null(input$focalparam2summary_plot_xaxis_boxgroup_labcex)) xaxis_boxgroup_labcex <- input$focalparam2summary_plot_xaxis_boxgroup_labcex
      
      xaxis_boxgroup_labline <- -2
      if (!is.null(input$focalparam2summary_plot_xaxis_boxgroup_labline)) xaxis_boxgroup_labline <- input$focalparam2summary_plot_xaxis_boxgroup_labline
      
      yaxis_lab_cex <- 0.75
      if (!is.null(input$focalparam2summary_plot_yaxis_lab_cex)) yaxis_lab_cex <- input$focalparam2summary_plot_yaxis_lab_cex
      
      yaxis_lab_line <- -1.5
      if (!is.null(input$focalparam2summary_plot_yaxis_lab_line)) yaxis_lab_line <- input$focalparam2summary_plot_yaxis_lab_line
      
      y_lab <- focalparam2summary_values$paramname_selected
      if (!is.null(input$focalparam2summary_plot_y_lab)) y_lab <- input$focalparam2summary_plot_y_lab
      
      y_lab_cex <- 1.6
      if (!is.null(input$focalparam2summary_plot_y_lab_cex)) y_lab_cex <- input$focalparam2summary_plot_y_lab_cex
      
      y_lab_line <- 0.9
      if (!is.null(input$focalparam2summary_plot_y_lab_line)) y_lab_line <- input$focalparam2summary_plot_y_lab_line
      
      plot_width <- 500
      if (!is.null(input$focalparam2summary_plot_width) && (as.numeric(input$focalparam2summary_plot_width) > 0)) plot_width <- as.numeric(input$focalparam2summary_plot_width)
      plot_height <- plot_width * 0.55
      res <- 300
      if (input$focalparam2summary_plot_downloadformat == "PDF") {
        pdf(file = file, width = plot_width/72, height = plot_height/72)
      } else if (input$focalparam2summary_plot_downloadformat == "EPS") {
        setEPS()
        postscript(file = file, width = plot_width/72, height = plot_height/72)
      } else if (input$focalparam2summary_plot_downloadformat == "PNG") {
        png(file = file, width = plot_width/72*res, height = plot_height/72 * res, res = res)
      } else if (input$focalparam2summary_plot_downloadformat == "JPEG") {
        jpeg(file = file, width = plot_width/72*res, height = plot_height/72 * res, res = res)
      } else if (input$focalparam2summary_plot_downloadformat == "TIFF") {
        tiff(file = file, width =  plot_width/72*res, height = plot_height/72 * res, res = res)
      }
      
      mai_bottom <- 0.25
      if (!is.null(input$focalparam2summary_plot_margin_bottom)) mai_bottom <- input$focalparam2summary_plot_margin_bottom
      
      mai_left <- 0.45
      if (!is.null(input$focalparam2summary_plot_margin_left)) mai_left <- input$focalparam2summary_plot_margin_left
      
      mai_top <- 0.15
      if (!is.null(input$focalparam2summary_plot_margin_top)) mai_top <- input$focalparam2summary_plot_margin_top
      
      mai_right <- 0
      if (!is.null(input$focalparam2summary_plot_margin_right)) mai_right <- input$focalparam2summary_plot_margin_right
      
      par(lend = 2, mai = c(mai_bottom, mai_left, mai_top, mai_right), xpd = F)
      multigroup_boxplot(value_list = value_list, group_at = group_at, rep_id = rep_id,
                         box_col = boxplot_col, whiskcol = boxplot_col, staplecol = boxplot_col, axis_log = axis_log, 
                         plot_xaxis = T, xaxis_lab = priormodel_name, xaxis_lab_cex = xaxis_lab_cex, xaxis_lab_line = xaxis_lab_line, xaxis_side = 3, 
                         plot_xaxis_boxgroup = T, xaxis_boxgroup_lab = c("prior", "posterior")[powers_all + 1], 
                         xaxis_boxgroup_labcex = xaxis_boxgroup_labcex, xaxis_boxgroup_labline = xaxis_boxgroup_labline, xaxis_boxgroup_side = 1,
                         plot_yaxis = F)
      
      if (is.null(axis_log)) {
        axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      } else if (axis_log == "y") {
        
        axis_lab <- c(min(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.025))))),
                      max(unlist(sapply(value_list, function(x) quantile(x, probs = c(0.975))))))
        if (ceiling(log10(axis_lab[1])) <= floor(log10(axis_lab[2]))) {
          axis_lab <- c(axis_lab[1], 10^(ceiling(log10(axis_lab[1])):floor(log10(axis_lab[2]))), axis_lab[2])
        }
        axis(side = 2, at = axis_lab,
             labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
        axis(side = 2, at = axis_lab,
             labels = formatC(axis_lab, digits = 1, format = "e"), lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      }
      
      mtext(y_lab, side = 2, line = y_lab_line, cex = y_lab_cex)
      
      dev.off()
    }
  )
  
  
  output$focalparam2summary_table <- DT::renderDataTable({
    
    req(focalparam2summary_values$focalparam_alldf_processed)
    focalparam_alldf_processed <- focalparam2summary_values$focalparam_alldf_processed
    
    value_list <- list()
    rep_id <- c()
    priormodelname_all <- c()
    powers_all <- c()
    
    for (i in 1:length(focalparam_alldf_processed)) {
      
      priormodel_name <- paste0("prior", i)
      if (!is.null(input[[paste0("focalparam2summary_priormodelname", i)]])) priormodel_name <- input[[paste0("focalparam2summary_priormodelname", i)]]
      
      focalparam_alldf <- focalparam_alldf_processed[[i]]
      
      powers <- sort(unique(focalparam_alldf$likelihoodpower))
      powers <- powers[powers == 0 | powers == 1]
      
      for (j in 1:length(powers)) {
        
        repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
        for (k in 1:length(repids)) {
          
          values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
          
          if (k == 1 || (!input$focalparam2summary_logcombinereplicates)) {
            value_list <- c(value_list, list(values))
            rep_id <- c(rep_id, k)
            powers_all <- c(powers_all, powers[j])
            priormodelname_all <- c(priormodelname_all, priormodel_name)
          } else if (input$focalparam2summary_logcombinereplicates && k > 1) {
            value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
          }
          
        }
      }
    }
    
    mean_all <- sapply(value_list, mean)
    CI95lower_all <- sapply(value_list, function(x) quantile(x, probs = 0.025, names = F))
    CI95upper_all <- sapply(value_list, function(x) quantile(x, probs = 0.975, names = F))
    
    focalparam2summary_df <- data.frame(cbind(priormodelname_all, c("prior", "posterior")[powers_all + 1], rep_id, mean_all, CI95lower_all, CI95upper_all), stringsAsFactors = F)
    colnames(focalparam2summary_df) <- c("Prior model", "Posterior or prior", "Replicate", "Mean", "Lower 95% CI", "Upper 95% CI")
    
    focalparam2summary_df
  })
  
  observe({
    req(focalparam2summary_logfileset$file[[1]])
    req(focalparam2summary_values$paramname_selected)
    
    file_name <- paste0(unlist(strsplit(input_processed$focalparam2summary_logfile[[paste0("focalparam2summary_logfile", 1)]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1],
                        "_", focalparam2summary_values$paramname_selected, "_robustbayesian")
    updateTextInput(session = session, inputId = "focalparam2summary_table_downloadname", value = file_name)
  })
  
  output$focalparam2summary_table_download <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$focalparam2summary_table_downloadformat, "TSV" = ".tsv", "CSV" = ".csv")
      paste0(input$focalparam2summary_table_downloadname, file_extension)
    },
    
    content = function(file) {
      
      focalparam_alldf_processed <- focalparam2summary_values$focalparam_alldf_processed
      
      value_list <- list()
      rep_id <- c()
      priormodelname_all <- c()
      powers_all <- c()
      
      for (i in 1:length(focalparam_alldf_processed)) {
        
        priormodel_name <- paste0("prior", i)
        if (!is.null(input[[paste0("focalparam2summary_priormodelname", i)]])) priormodel_name <- input[[paste0("focalparam2summary_priormodelname", i)]]
        
        focalparam_alldf <- focalparam_alldf_processed[[i]]
        
        powers <- sort(unique(focalparam_alldf$likelihoodpower))
        powers <- powers[powers == 0 | powers == 1]
        
        for (j in 1:length(powers)) {
          
          repids <- sort(unique(focalparam_alldf$repid[focalparam_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            values <- focalparam_alldf[focalparam_alldf$likelihoodpower == powers[j] & focalparam_alldf$repid == repids[k], 3]
            
            if (k == 1 || (!input$focalparam2summary_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              rep_id <- c(rep_id, k)
              powers_all <- c(powers_all, powers[j])
              priormodelname_all <- c(priormodelname_all, priormodel_name)
            } else if (input$focalparam2summary_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
      }
      
      mean_all <- sapply(value_list, mean)
      CI95lower_all <- sapply(value_list, function(x) quantile(x, probs = 0.025, names = F))
      CI95upper_all <- sapply(value_list, function(x) quantile(x, probs = 0.975, names = F))
      
      focalparam2summary_df <- data.frame(cbind(priormodelname_all, c("prior", "posterior")[powers_all + 1], rep_id, mean_all, CI95lower_all, CI95upper_all), stringsAsFactors = F)
      colnames(focalparam2summary_df) <- c("Prior model", "Posterior or prior", "Replicate", "Mean", "Lower 95% CI", "Upper 95% CI")
      
      write.table(focalparam2summary_df, file = file, sep = switch(input$focalparam2summary_table_downloadformat, "TSV" = "\t", "CSV" = ","),
                  row.names = F)
      
    }
  )
  
  outputOptions(output, "focalparam2summary_logprocessing_ui", suspendWhenHidden = F)
  outputOptions(output, "focalparam2summary_priormodelname_ui", suspendWhenHidden = F)
  outputOptions(output, "focalparam2summary_paramname_ui", suspendWhenHidden = F)
  
  ########################
  # posterior predictive #
  ########################
  
  # enable the tree input tab only after user uploads the discrete-trait file (and it's valid)
  observe({
    shinyjs::toggleClass(id = "posteriorpredictive_geographyfile_div", class = "divdisabled", 
                         condition = input$posteriorpredictive_geographyfile_defaultupload)
  })
  
  # check for valid input
  posteriorpredictive_input_condition <- reactiveValues(tree_invalid = T, trait_invalid = T, log_invalid = T, 
                                                        treetrait_invalid = T, logtree_invalid = T)
  posteriorpredictive_tree_values <- reactiveValues(tree = NULL)
  posteriorpredictive_states_dat <- reactiveVal()
  # posteriorpredictive_states_dat <- reactive({
  #   req(input$posteriorpredictive_geographyfile)
  #   # reading in discrete-trait data
  #   # assume there is only one discrete trait for now, could be easily relaxed
  #   read.table(text = gsub("\t", ",", readLines(input$posteriorpredictive_geographyfile$datapath)), header = T, sep = ",", stringsAsFactors = F)
  # })
  
  observe({
    if (input$posteriorpredictive_geographyfile_defaultupload) {
      posteriorpredictive_states_dat(read.table(text = gsub("\t", ",", readLines("./data/analyses_setup/discrete_trait.txt")), header = T, sep = ",", stringsAsFactors = F))
    } else if (!is.null(input$geography_file)) {
      req(input$posteriorpredictive_geographyfile)
      posteriorpredictive_states_dat(read.table(text = gsub("\t", ",", readLines(input$posteriorpredictive_geographyfile$datapath)), header = T, sep = ",", stringsAsFactors = F))
    } else {
      posteriorpredictive_states_dat(NULL)
    }
  })
  
  observe({
    shinyjs::toggle(id = "posteriorpredictive_geographyfile_attributes", condition = !is.null(posteriorpredictive_states_dat()))
  })
  
  observe({
    # req(input$posteriorpredictive_geographyfile)
    req(posteriorpredictive_states_dat())
    
    if (!input$posteriorpredictive_geographyfile_header) {
      showModal(modalDialog("Please edit the discrete-geography file so that the first row indicates column names.",
                            title = "Invalid input",
                            easyClose = F,
                            size = "m"
      ))
      posteriorpredictive_input_condition$trait_invalid <- T
    } else {
      posteriorpredictive_input_condition$trait_invalid <- F
    }
    
    updateSelectInput(session = session, inputId = "posteriorpredictive_geographyfile_taxoncolumn_name", 
                      choices = colnames(posteriorpredictive_states_dat()), selected = colnames(posteriorpredictive_states_dat())[1])
    updateSelectInput(session = session, inputId = "posteriorpredictive_geographyfile_traitcolumn_name", 
                      choices = colnames(posteriorpredictive_states_dat()), selected = colnames(posteriorpredictive_states_dat())[2])
  })
  
  observe({
    shinyjs::toggleClass(selector = "#posteriorpredictive_input_tabs li a[data-value=logtree]", class = "divdisabled", 
                         condition = is.null(posteriorpredictive_states_dat()) ||
                           (!input$posteriorpredictive_geographyfile_header) || is.null(input$posteriorpredictive_geographyfile_taxoncolumn_name) ||
                           is.null(input$posteriorpredictive_geographyfile_traitcolumn_name) || posteriorpredictive_input_condition$trait_invalid)
  })
  
  
  posteriorpredictive_logfile_defaultupload_paths <- list(list(log = c("./data/post_processing/ctmc/HIV_datasetA_posterior_run1.log",
                                                                       "./data/post_processing/ctmc/HIV_datasetA_posterior_run2.log"),
                                                               tree = c("./data/post_processing/ctmc/HIV_datasetA_posterior_run1.trees",
                                                                        "./data/post_processing/ctmc/HIV_datasetA_posterior_run2.trees")),
                                                          list(log = c("./data/post_processing/exphyper/HIV_datasetA_posterior_run1.log",
                                                                       "./data/post_processing/exphyper/HIV_datasetA_posterior_run2.log"),
                                                               tree = c("./data/post_processing/exphyper/HIV_datasetA_posterior_run1.trees",
                                                                        "./data/post_processing/exphyper/HIV_datasetA_posterior_run2.trees")))
  
  input_default_static_posteriorpredictive_logfile <- lapply(posteriorpredictive_logfile_defaultupload_paths, function(x) {
    data.frame(name = basename(x$log), datapath = x$log, check.names = F, stringsAsFactors = F)
  })
  input_default_static_posteriorpredictive_treefile <- lapply(posteriorpredictive_logfile_defaultupload_paths, function(x) {
    data.frame(name = basename(x$tree), datapath = x$tree, check.names = F, stringsAsFactors = F)
  })
  
  observeEvent(input$posteriorpredictive_logfile_defaultupload, {
    if (input$posteriorpredictive_logfile_defaultupload) {
      if (posteriorpredictive_loginputset$num == 1) {
        shinyjs::runjs("$('#posteriorpredictive_loginput_add').click();")
      } else if (posteriorpredictive_loginputset$num > 2) {
        posteriorpredictive_loginputset$num <- 2L
      }
    } else {
      if (posteriorpredictive_loginputset$num_inputui == 2 && (is.null(input[[paste0("posteriorpredictive_logfile", 2)]])) && (is.null(input[[paste0("posteriorpredictive_treefile", 2)]]))) {
        shinyjs::runjs("$('#posteriorpredictive_loginput_remove').click();")
      }
    }
  })

  
  posteriorpredictive_loginputset <- reactiveValues(num = 1L, num_inputui = 1L)
  observeEvent(input$posteriorpredictive_loginput_add, {
    posteriorpredictive_loginputset$num <- posteriorpredictive_loginputset$num + 1L
    posteriorpredictive_loginputset$num_inputui <- posteriorpredictive_loginputset$num_inputui + 1L
    
    insertUI(selector = "#posteriorpredictive_loginput_addremove_div", where = "beforeBegin", 
             ui = div(id = paste0("posteriorpredictive_logfile_div", posteriorpredictive_loginputset$num),
                      class = ifelse(input$posteriorpredictive_logfile_defaultupload, "divdisabled", ""),
                      tagList(h6(''),
                              p(paste0("Upload estimate files under prior No. ", posteriorpredictive_loginputset$num_inputui), align = "center", 
                                style = "font-size: 95%; font-weight: bold;"),
                              fileInput(inputId = paste0("posteriorpredictive_logfile", posteriorpredictive_loginputset$num_inputui), 
                                        label = "log file(s)", 
                                        multiple = T, accept = ".log"),
                              fileInput(inputId = paste0("posteriorpredictive_treefile", posteriorpredictive_loginputset$num_inputui), 
                                        label = "tree file(s)",
                                        multiple = T, accept = c(".trees", ".tree", ".tre")))))
    
  })
  observeEvent(input$posteriorpredictive_loginput_remove, {
    if (posteriorpredictive_loginputset$num > 1) {
      removeUI(selector = paste0("#posteriorpredictive_logfile_div", posteriorpredictive_loginputset$num))
      posteriorpredictive_loginputset$num <- posteriorpredictive_loginputset$num - 1L
      posteriorpredictive_loginputset$num_inputui <- posteriorpredictive_loginputset$num_inputui - 1L
    }
  })
  
  observe({
    for (i in 1:posteriorpredictive_loginputset$num_inputui) {
      shinyjs::toggleClass(id = paste0("posteriorpredictive_logfile_div", i), class = "divdisabled", 
                           condition = input$posteriorpredictive_logfile_defaultupload)
    }
  })
  observeEvent(input$posteriorpredictive_logfile_defaultupload, {
    shinyjs::toggleClass(id = "posteriorpredictive_loginput_addremove_div", class = "divdisabled", 
                         condition = input$posteriorpredictive_logfile_defaultupload)
  })
  
  posteriorpredictive_logfileset <- reactiveValues(file = NULL)
  posteriorpredictive_treefileset <- reactiveValues(file = NULL)
  observe({
    isolate(posteriorpredictive_logfileset$file <- vector("list", posteriorpredictive_loginputset$num))
    isolate(posteriorpredictive_treefileset$file <- vector("list", posteriorpredictive_loginputset$num))
    for (i in 1:posteriorpredictive_loginputset$num) {
      if (input$posteriorpredictive_logfile_defaultupload) {
        req(posteriorpredictive_loginputset$num == length(input_default_static_posteriorpredictive_logfile))
        isolate(posteriorpredictive_logfileset$file[[i]] <- input_default_static_posteriorpredictive_logfile[[i]])
        isolate(posteriorpredictive_treefileset$file[[i]] <- input_default_static_posteriorpredictive_treefile[[i]])
      } else {
        if (!is.null(input[[paste0("posteriorpredictive_logfile", i)]])) {
          isolate(posteriorpredictive_logfileset$file[[i]] <- input[[paste0("posteriorpredictive_logfile", i)]])
        }
        if (!is.null(input[[paste0("posteriorpredictive_treefile", i)]])) {
          isolate(posteriorpredictive_treefileset$file[[i]] <- input[[paste0("posteriorpredictive_treefile", i)]])
        }
      }
    }
  })
  
  
  observe({
    
    req(posteriorpredictive_states_dat())
    req(input$posteriorpredictive_geographyfile_traitcolumn_name)
    req(posteriorpredictive_input_condition$trait_invalid == F)
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_logfileset$file[[i]])
    }
    
    states_dat <- posteriorpredictive_states_dat()
    discrete_trait_name <- input$posteriorpredictive_geographyfile_traitcolumn_name
    states <- sort(unique(states_dat[, discrete_trait_name]))
    states <- states[states != "?"]
    states_num <- length(states)
    
    for (i in 1:posteriorpredictive_loginputset$num) {
      
      req(nrow(posteriorpredictive_logfileset$file[[i]]) >= 1)
      log_invalid <- F
      for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
        
        col_names <- colnames(read.table(posteriorpredictive_logfileset$file[[i]]$datapath[j], 
                                         header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
        if ((!any(grepl(".rates", col_names))) || (!any(grepl(".clock.rate$", col_names)))) {
          log_invalid <- T
          break
        } else {
          rates_num <- max(grep(".rates", col_names)) - min(grep(".rates", col_names)) + 1
          if ((rates_num != states_num * (states_num - 1)) && (rates_num != states_num * (states_num - 1)/2)) {
            log_invalid <- T
            break
          }
        }
      }
      
      if (log_invalid) break
    }
    
    posteriorpredictive_input_condition$log_invalid <- log_invalid
    if (log_invalid) {
      showModal(modalDialog("The uploaded log file(s) don't contain the required columns (.clock.rate and/or .rates) or the number of 'rates' elements is not consistent with the number of states in the discrete-geography file (whether the geographic model is symmetric or asymmetric).",
                            title = "Invalid input",
                            easyClose = F,
                            size = "m"
      ))
    }
  })
  
  
  observe({
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_treefileset$file[[i]])
    }
    
    tree1 <- NULL
    tip_lab1 <- NULL
    for (i in 1:posteriorpredictive_loginputset$num) {
      
      req(nrow(posteriorpredictive_treefileset$file[[i]]) >= 1)
      tree_invalid <- F
      for (j in 1:nrow(posteriorpredictive_treefileset$file[[i]])) {
        
        tree_text <- readtreefile_linebyline(filepath = posteriorpredictive_treefileset$file[[i]]$datapath[j])
        if (!any(grepl("&", tree_text))) {
          tree_invalid <- T
          break
        } else {
          tmptree_path <- tempfile(fileext = ".tree")
          cat(c(tree_text[1:min(grep("&", tree_text))], "End;"), file = tmptree_path, sep = "\n")
          tree <- ape::read.nexus(tmptree_path)
          file.remove(tmptree_path)
          
          if (is.null(tree1) && is.null(tip_lab1)) {
            tree1 <- tree
            tip_lab1 <- sort(tree$tip.label)
          } else {
            if (!identical(sort(tree$tip.label), tip_lab1)) {
              tree_invalid <- T
              break
            }
          }
        }
      }
      
      if (tree_invalid) break
    }
    
    posteriorpredictive_input_condition$tree_invalid <- tree_invalid
    if (tree_invalid) {
      showModal(modalDialog("No tree in the uploaded tree file or the trees have different tips.",
                            title = "Invalid input",
                            easyClose = F,
                            size = "m"
      ))
    } else {
      posteriorpredictive_tree_values$tree <- tree1
    }
  })
  
  observe({
    req(posteriorpredictive_input_condition$tree_invalid == F)
    req(posteriorpredictive_input_condition$log_invalid == F)
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_logfileset$file[[i]])
      req(posteriorpredictive_treefileset$file[[i]])
    }
    
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(nrow(posteriorpredictive_logfileset$file[[i]]) >= 1)
      req(nrow(posteriorpredictive_treefileset$file[[i]]) >= 1)
      logtree_invalid <- F
      
      if (nrow(posteriorpredictive_logfileset$file[[i]]) != nrow(posteriorpredictive_treefileset$file[[i]])) {
        logtree_invalid <- T
        break
      }
      
      logfile_name <- sort(sapply(1:nrow(posteriorpredictive_logfileset$file[[i]]), function(j)
        unlist(strsplit(posteriorpredictive_logfileset$file[[i]]$name[j], "\\."))[1]))
      treefile_name <- sort(sapply(1:nrow(posteriorpredictive_treefileset$file[[i]]), function(j)
        unlist(strsplit(posteriorpredictive_treefileset$file[[i]]$name[j], "\\."))[1]))
      
      if (!identical(logfile_name, treefile_name)) {
        logtree_invalid <- T
        break
      }
    }
    
    posteriorpredictive_input_condition$logtree_invalid <- logtree_invalid
    if (logtree_invalid) {
      showModal(modalDialog("The name of each log file (without the '.log' extension) has to be identical to the corresponding tree file's name (without the '.tree' or '.trees' extension).",
                            title = "Invalid input",
                            easyClose = F,
                            size = "m"
      ))
    }
      
  })
  
  # sanity check: tree and discrete data match in terms of taxon list
  observe({
    
    req(posteriorpredictive_states_dat())
    req(input$posteriorpredictive_geographyfile_taxoncolumn_name)
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_treefileset$file[[i]])
    }
    
    req(posteriorpredictive_tree_values$tree)
    req(posteriorpredictive_states_dat())
    req(posteriorpredictive_input_condition$tree_invalid == F)
    req(posteriorpredictive_input_condition$trait_invalid == F)
    
    tip_lab <- sort(posteriorpredictive_tree_values$tree$tip.label)
    if (input$posteriorpredictive_geographyfile_taxoncolumn_name != "") {
      taxa <- sort(as.vector(posteriorpredictive_states_dat()[, input$posteriorpredictive_geographyfile_taxoncolumn_name]))
      
      if (!identical(tip_lab, taxa)) {
        showModal(modalDialog("Taxa list in the tree file doesn't match the discrete-geography file.",
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
                              
        ))
        posteriorpredictive_input_condition$treetrait_invalid <- T
      } else {
        posteriorpredictive_input_condition$treetrait_invalid <- F
      }
    }
  })
  

  output$posteriorpredictive_logprocessing_ui <- renderUI({
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    posteriorpredictive_logprocessing_ui_list <- tagList()
    
    logprocessing_perpanel_height <- 540 / posteriorpredictive_loginputset$num
    if (logprocessing_perpanel_height < 200) logprocessing_perpanel_height <- 200
    
    for (i in 1:posteriorpredictive_loginputset$num) {
      
      if (!is.null(posteriorpredictive_logfileset$file[[i]])) {
        
        powers <- numeric(nrow(posteriorpredictive_logfileset$file[[i]]))
        for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
          if (grepl("_underprior", posteriorpredictive_logfileset$file[[i]]$name[j])) {
            powers[j] <- 0
          } else if (grepl("_posterior", posteriorpredictive_logfileset$file[[i]]$name[j])) {
            powers[j] <- 1
          } else if (grepl("_datacloning", posteriorpredictive_logfileset$file[[i]]$name[j])) {
            powers[j] <- as.numeric(gsub("datacloning", "", grep("datacloning", unlist(strsplit(posteriorpredictive_logfileset$file[[i]]$name[j], "_|\\.")), value = T)[1]))
          } else {
            col_names <- colnames(read.table(posteriorpredictive_logfileset$file[[i]]$datapath[j], header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
            if ("pathLikelihood.theta" %in% col_names) powers[j] <- 0.5
          }
        }
        
        posteriorpredictive_logprocessing_ui_list <- tagAppendChildren(posteriorpredictive_logprocessing_ui_list,
                                                                       HTML(paste0("<div id = \"posteriorpredictive_logprocessing_panel", i, 
                                                                                   "\" class = \"panel-heading\"><p class=\"panel-title\" style=\"font-size: 95%; font-weight: bold;\">", 
                                                                                   "<a data-toggle=\"collapse\" href=\"#posteriorpredictive_logprocessing", i,
                                                                                   "\"><span class=\"glyphicon glyphicon-chevron-up\" aria-hidden=\"true\"></span>", 
                                                                                   "Settings for estimate log file(s) under prior model No. ", i, 
                                                                                   "</a></p></div>")),
                                                                       div(id = paste0("posteriorpredictive_logprocessing", i), 
                                                                           class = "panel-collapse collapse in", 
                                                                           fluidRow(style = paste0("max-height: ", logprocessing_perpanel_height, "px; overflow: auto; background-color: #EBF5FB; padding: 2px 10px 5px 10px;"),
                                                                                    lapply(1:nrow(posteriorpredictive_logfileset$file[[i]]), function(j) {
                                                                                      tagList(
                                                                                        p(posteriorpredictive_logfileset$file[[i]]$name[j], align = "center", 
                                                                                          style = "font-size: 95%; font-weight: bold;"),
                                                                                        # numericInput(inputId = paste0("posteriorpredictive_logfile", i, "_log", j, "_likelihoodpower"), 
                                                                                        #              label = "Number of copies of the data",
                                                                                        #              value = powers[j], min = 0),
                                                                                        checkboxInput(inputId = paste0("posteriorpredictive_logfile", i, "_log", j, "_include"),
                                                                                                      label = "include this log file", value = T),
                                                                                        sliderInput(inputId = paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin"),
                                                                                                    label = "burn-in proportion",
                                                                                                    min = 0, max = 99, value = 10),
                                                                                        p("", align = "center", style = "font-size: 85%;"))
                                                                                    })[order(powers)])
                                                                           
                                                                       ))
        if (i != posteriorpredictive_loginputset$num) {
          posteriorpredictive_logprocessing_ui_list <- tagAppendChildren(posteriorpredictive_logprocessing_ui_list, h6(""))
        }
      }
    }
    
    posteriorpredictive_logprocessing_ui_list
  })
  
  # sanity check to make sure that at least one log file exist under each prior model
  observe({
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(!is.null(posteriorpredictive_logfileset$file[[i]]))
      sum <- 0
      
      for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
        req(!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]]))
        sum <- sum + input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]]
      }
      
      if (sum == 0) {
        showModal(modalDialog(paste0("No log file is included for prior model ", i,
                                     ". At least one log file needs to be included under each prior model."),
                              title = "Invalid input",
                              easyClose = F,
                              size = "m"
        ))
        break
      }
    }
  })
  
  observe({
    lapply(1:posteriorpredictive_loginputset$num, function(i) {
      shinyjs::runjs(HTML(paste0("$('#posteriorpredictive_logprocessing", i, "').on('shown.bs.collapse', function() {
                              $('#posteriorpredictive_logprocessing_panel", i, "').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
                            });
                            $('#posteriorpredictive_logprocessing", i, "').on('hidden.bs.collapse', function() {
                              $('#posteriorpredictive_logprocessing_panel", i, "').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
                            });")))
    })
  })
  
  
  output$posteriorpredictive_priormodelname_ui <- renderUI({
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    posteriorpredictive_priormodelname_ui_list <- tagList()
    
    for (i in 1:posteriorpredictive_loginputset$num) {
      if (!is.null(posteriorpredictive_logfileset$file[[i]])) {
        posteriorpredictive_priormodelname_ui_list <- tagAppendChildren(posteriorpredictive_priormodelname_ui_list,
                                                                        textInput(inputId = paste0("posteriorpredictive_priormodelname", i),
                                                                                  label = paste0("Name of prior No. ", i),
                                                                                  value = paste0("prior", i)))
      }
    }
    
    posteriorpredictive_priormodelname_ui_list
  })
  
  
  posteriorpredictive_startprocessing_status <- reactiveValues(enabled = T, clicked = F)
  observe({
    shinyjs::toggle(id = "posteriorpredictive_startprocessing", condition = posteriorpredictive_startprocessing_status$enabled)
  })
  
  # only enable the panel when all the input files are in place and valid
  observe({
    shinyjs::toggleClass(selector = "#posteriorpredictive_input_tabs li a[data-value=startsimulations]", class = "divdisabled", 
                         condition = !posteriorpredictive_startprocessing_status$enabled)
  })
  
  observe({
    shinyjs::toggleClass(id = "posteriorpredictive_simulatenumber_div", class = "divdisabled", 
                         condition = input$posteriorpredictive_simulateall)
  })
  
  observe({

    posteriorpredictive_startprocessing_status$enabled <- T

    if (is.null(input$posteriorpredictive_geographyfile_traitcolumn_name) ||
        is.null(input$posteriorpredictive_geographyfile_taxoncolumn_name) || is.null(posteriorpredictive_states_dat()) ||
        (posteriorpredictive_input_condition$trait_invalid == T)) {

      posteriorpredictive_startprocessing_status$enabled <- F

    } else if ((posteriorpredictive_input_condition$tree_invalid == T) || (posteriorpredictive_input_condition$log_invalid == T) ||
               (posteriorpredictive_input_condition$treetrait_invalid == T) || (posteriorpredictive_input_condition$logtree_invalid == T) ||
               is.null(posteriorpredictive_tree_values$tree)) {

      posteriorpredictive_startprocessing_status$enabled <- F
    } else {
      req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
      req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
      
      for (i in 1:posteriorpredictive_loginputset$num) {
        if (is.null(posteriorpredictive_logfileset$file[[i]]) || is.null(posteriorpredictive_treefileset$file[[i]])) {

          posteriorpredictive_startprocessing_status$enabled <- F
          break
        } else if ((nrow(posteriorpredictive_logfileset$file[[i]]) < 1) || (nrow(posteriorpredictive_treefileset$file[[i]]) < 1) ||
                   (nrow(posteriorpredictive_logfileset$file[[i]]) != nrow(posteriorpredictive_treefileset$file[[i]]))) {

          posteriorpredictive_startprocessing_status$enabled <- F
          break
        }
      }
    }

    posteriorpredictive_startprocessing_status$clicked <- F
  })
  
  observe({
    posteriorpredictive_tab2_disabled <- (input$posteriorpredictive_startprocessing == 0) || (!posteriorpredictive_startprocessing_status$clicked) ||
      is.null(posteriorpredictive_values$teststatistics_alldf_processed)
    
    shinyjs::toggleClass(selector = "#posteriorpredictive_tabs li a[data-value=processing_settings]", class = "divdisabled", 
                         condition = posteriorpredictive_tab2_disabled)
    if (!posteriorpredictive_tab2_disabled) {
      updateNavlistPanel(session, inputId = "posteriorpredictive_tabs", selected = "processing_settings")
    }
  })
  
  observe({
    posteriorpredictive_tab3_disabled <- (input$posteriorpredictive_startprocessing == 0) || (!posteriorpredictive_startprocessing_status$clicked) ||
      is.null(posteriorpredictive_values$teststatistics_alldf_processed)
    
    shinyjs::toggleClass(selector = "#posteriorpredictive_tabs li a[data-value=download_output]", class = "divdisabled", 
                         condition = posteriorpredictive_tab3_disabled)
  })
  
  observe({
    posteriorpredictive_tab2_disabled <- (input$posteriorpredictive_startprocessing == 0) || (!posteriorpredictive_startprocessing_status$clicked) || 
      is.null(posteriorpredictive_values$teststatistics_alldf_processed)
    shinyjs::toggle(id = "posteriorpredictive_result_div", condition = !posteriorpredictive_tab2_disabled)
  })

  
  posteriorpredictive_values <- reactiveValues(teststatistics_alldf_raw = NULL, teststatistics_alldf_processed = NULL, simulateddata_tmppath = NULL)
  observeEvent(input$posteriorpredictive_startprocessing, {
    
    # req(input$posteriorpredictive_geographyfile)
    req(posteriorpredictive_states_dat())
    req(input$posteriorpredictive_geographyfile_traitcolumn_name)
    req(input$posteriorpredictive_geographyfile_taxoncolumn_name)
    req(posteriorpredictive_input_condition$trait_invalid == F)
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_logfileset$file[[i]])
      req(posteriorpredictive_treefileset$file[[i]])
    }
    
    req(posteriorpredictive_input_condition$tree_invalid == F)
    req(posteriorpredictive_input_condition$log_invalid == F)
    req(posteriorpredictive_tree_values$tree)
    req(posteriorpredictive_input_condition$treetrait_invalid == F)
    req(posteriorpredictive_input_condition$logtree_invalid == F)
    
    if (!is.null(posteriorpredictive_values$simulateddata_tmppath)) {
      file.remove(posteriorpredictive_values$simulateddata_tmppath)
    }
    
    simsamples_num <- 0
    if ((!input$posteriorpredictive_simulateall) && (!is.null(input$posteriorpredictive_simulatenumber)) && (input$posteriorpredictive_simulatenumber >= 1)) {
      simsamples_num <- floor(input$posteriorpredictive_simulatenumber)
    }
    
    lognum_all <- sum(sapply(1:posteriorpredictive_loginputset$num, function(i) nrow(posteriorpredictive_logfileset$file[[i]])))
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Reading in log and tree files", value = 0)
    
    teststatistics_alldfall <- vector("list", length(posteriorpredictive_loginputset$num))
    simulated_tmppaths <- c()
    for (i in 1:posteriorpredictive_loginputset$num) {
      
      teststatistics_alldf <- vector("list", nrow(posteriorpredictive_logfileset$file[[i]]))
      powers_repnum <- c()
      
      for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
        
        progress$inc(1 / lognum_all, detail = paste0("Simulating using posterior log and tree file No.", j , " under prior model No.", i))
        
        log_path <- posteriorpredictive_logfileset$file[[i]]$datapath[j]
        log_name <- posteriorpredictive_logfileset$file[[i]]$name[j]
        simulated_tmppath <- paste0(tempdir(), "/prior", i, "_log", j, "_simulateddataset.tsv")
        simulated_tmppaths <- c(simulated_tmppaths, simulated_tmppath)
        
        teststatistics_df <- historySimulator_teststatisticsComputer(states_dat = posteriorpredictive_states_dat(), 
                                                                     taxon_name = input$posteriorpredictive_geographyfile_taxoncolumn_name, 
                                                                     discrete_trait_name = input$posteriorpredictive_geographyfile_traitcolumn_name,
                                                                     tree_path = posteriorpredictive_treefileset$file[[i]]$datapath[j], 
                                                                     log_path = log_path,
                                                                     simsamples_num = simsamples_num, simulated_tmppath = simulated_tmppath)
        
        col_names <- colnames(read.table(log_path, header = T, sep = "\t", nrows = 1, stringsAsFactors = F, check.names = F))
        if ("pathLikelihood.theta" %in% col_names) {
          col_classes <- rep("NULL", length(col_names))
          col_classes[col_names == "pathLikelihood.theta"] <- "numeric"
          
          focalparam_df <- read.table(log_path, header = T, sep = "\t", colClasses = col_classes, stringsAsFactors = F)
          
          powers_current <- sort(unique(focalparam_df$pathLikelihood.theta))
          powers_repnum_current <- rep(1, length(powers_current))
          names(powers_repnum_current) <- powers_current
          for (k in 1:length(powers_repnum_current)) {
            if (as.numeric(names(powers_repnum_current)[k]) %in% as.numeric(names(powers_repnum))) {
              powers_repnum_current[k] <- powers_repnum[names(powers_repnum_current)[k]] + 1
              powers_repnum[names(powers_repnum_current)[k]] <- powers_repnum[names(powers_repnum_current)[k]] + 1
            } else {
              powers_repnum <- c(powers_repnum, 1)
              names(powers_repnum)[length(powers_repnum)] <- names(powers_repnum_current)[k]
            }
          }
          
          teststatistics_df <- data.frame(cbind(focalparam_df$pathLikelihood.theta, as.integer(powers_repnum_current[as.character(focalparam_df$pathLikelihood.theta)]),
                                                teststatistics_df))
          
        } else {
          
          power <- 1
          if (grepl("_underprior", log_name)) {
            power <- 0
          } else if (grepl("_posterior", log_name)) {
            power <- 1
          } else if (grepl("_datacloning", log_name)) {
            power <- as.numeric(gsub("datacloning", "", grep("datacloning", unlist(strsplit(log_name, "_|\\.")), value = T)[1]))
          }
          
          repnum <- 1
          if (power %in% as.numeric(names(powers_repnum))) {
            repnum <- as.integer(powers_repnum[as.character(power)] + 1)
            powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
          } else {
            powers_repnum <- c(powers_repnum, 1)
            names(powers_repnum)[length(powers_repnum)] <- power
          }
          
          teststatistics_df <- data.frame(cbind(rep(power, nrow(teststatistics_df)), rep(repnum, nrow(teststatistics_df)), teststatistics_df), stringsAsFactors = F)
        }
        
        colnames(teststatistics_df) <- c("likelihoodpower", "repid", colnames(teststatistics_df)[-c(1, 2)])
        teststatistics_alldf[[j]] <- teststatistics_df
      }
      
      teststatistics_alldfall[[i]] <- teststatistics_alldf
    }
    
    posteriorpredictive_values$teststatistics_alldf_raw <- teststatistics_alldfall
    posteriorpredictive_values$simulateddata_tmppath <- simulated_tmppaths
    
    # posteriorpredictive_startprocessing_status$enabled <- F
    posteriorpredictive_startprocessing_status$clicked <- T
  })
  
  
  observe({
    
    # req(input$posteriorpredictive_geographyfile)
    req(posteriorpredictive_states_dat())
    req(input$posteriorpredictive_geographyfile_traitcolumn_name)
    req(input$posteriorpredictive_geographyfile_taxoncolumn_name)
    req(posteriorpredictive_input_condition$trait_invalid == F)
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_logfileset$file[[i]])
      req(posteriorpredictive_treefileset$file[[i]])
    }
    
    req(posteriorpredictive_input_condition$tree_invalid == F)
    req(posteriorpredictive_input_condition$log_invalid == F)
    req(posteriorpredictive_tree_values$tree)
    req(posteriorpredictive_input_condition$treetrait_invalid == F)
    req(posteriorpredictive_input_condition$logtree_invalid == F)
    
    req(posteriorpredictive_values$teststatistics_alldf_raw)
    req(length(posteriorpredictive_values$teststatistics_alldf_raw) == posteriorpredictive_loginputset$num)
    
    teststatistics_alldf_raw <- posteriorpredictive_values$teststatistics_alldf_raw
    teststatistics_alldf_processed <- vector("list", length(teststatistics_alldf_raw))
    
    for (i in 1:posteriorpredictive_loginputset$num) {
      
      req(length(teststatistics_alldf_raw[[i]]) == nrow(posteriorpredictive_logfileset$file[[i]]))
      teststatistics_alldf <- teststatistics_alldf_raw[[i]]
      
      posterior_notinpowerposterior_exist <- prior_notinpowerposterior_exist <- F
      
      # figuring out if posterior and prior is already there and if so we won't include them from the power posterior analyses
      for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
        if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]]) {
          
          powers <- unique(teststatistics_alldf[[j]]$likelihoodpower)
          if (length(powers) == 1) {
            if (powers == 1) {
              posterior_notinpowerposterior_exist <- T
            } else if (powers == 0) {
              prior_notinpowerposterior_exist <- T
            }
          }
          
        }
      }
      
      powers_repnum <- c()
      k <- 1
      for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
        if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]]) {
          
          teststatistics_df <- teststatistics_alldf[[j]]
          burnin_prop <- 0
          if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]] > 0) {
            burnin_prop <- input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]]/100
          }
          
          if (length(unique(teststatistics_df$likelihoodpower)) >= 2) {
            
            powers <- sort(unique(teststatistics_df$likelihoodpower))
            if (any(powers == 0) && prior_notinpowerposterior_exist) powers <- powers[powers != 0]
            if (any(powers == 1) && posterior_notinpowerposterior_exist) powers <- powers[powers != 1]
            
            if (length(powers) == 0) {
              teststatistics_df <- teststatistics_df[-(1:nrow(teststatistics_df)),]
            } else {
              teststatistics_powerdf <- vector("list", length(powers))
              
              for (l in 1:length(powers)) {
                teststatistics_powerdf[[l]] <- teststatistics_df[teststatistics_df$likelihoodpower == powers[l], ]
                
                power <- powers[l]
                repnum <- 1
                if (power %in% as.numeric(names(powers_repnum))) {
                  repnum <- as.integer(powers_repnum[as.character(power)] + 1)
                  powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
                } else {
                  powers_repnum <- c(powers_repnum, 1)
                  names(powers_repnum)[length(powers_repnum)] <- power
                }
                teststatistics_powerdf[[l]]$repid <- rep(repnum, nrow(teststatistics_powerdf[[l]]))
                
                burnin <- ceiling(nrow(teststatistics_powerdf[[l]]) * burnin_prop)
                
                if (burnin == nrow(teststatistics_powerdf[[l]])) {
                  teststatistics_powerdf[[l]] <- teststatistics_powerdf[[l]][nrow(teststatistics_powerdf[[l]]), ]
                } else if (burnin > 0) {
                  teststatistics_powerdf[[l]] <- teststatistics_powerdf[[l]][-(1:burnin), ]
                }
                
                teststatistics_powerdf[[l]] <- data.frame(teststatistics_powerdf[[l]])
              }
              
              teststatistics_df <- do.call(rbind, teststatistics_powerdf)
            }
          } else {
            
            power <- unique(teststatistics_df$likelihoodpower)[1]
            
            repnum <- 1
            if (power %in% as.numeric(names(powers_repnum))) {
              repnum <- as.integer(powers_repnum[as.character(power)] + 1)
              powers_repnum[as.character(power)] <- powers_repnum[as.character(power)] + 1
            } else {
              powers_repnum <- c(powers_repnum, 1)
              names(powers_repnum)[length(powers_repnum)] <- power
            }
            teststatistics_df$repid <- rep(repnum, nrow(teststatistics_df))
            
            burnin <- ceiling(nrow(teststatistics_df) * burnin_prop)
            if (burnin == nrow(teststatistics_df)) {
              teststatistics_df <- teststatistics_df[nrow(teststatistics_df), ]
            } else if (burnin > 0) {
              teststatistics_df <- teststatistics_df[-(1:burnin), ]
            }
            
            teststatistics_df <- data.frame(teststatistics_df)
          }
          
          teststatistics_alldf[[k]] <- teststatistics_df
          k <- k + 1
        }
      }
      
      teststatistics_alldf <- do.call(rbind, teststatistics_alldf[1:(k - 1)])
      teststatistics_alldf <- teststatistics_alldf[order(teststatistics_alldf$likelihoodpower, teststatistics_alldf$repid), ]
      teststatistics_alldf_processed[[i]] <- teststatistics_alldf
    }
    
    posteriorpredictive_values$teststatistics_alldf_processed <- teststatistics_alldf_processed
  })
  
  shinyjs::runjs("$('#posteriorpredictive_par_edits').on('shown.bs.collapse', function() {
    $('#posteriorpredictive_par_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#posteriorpredictive_par_edits').on('hidden.bs.collapse', function() {
    $('#posteriorpredictive_par_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#posteriorpredictive_xaxis_edits').on('shown.bs.collapse', function() {
    $('#posteriorpredictive_xaxis_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#posteriorpredictive_xaxis_edits').on('hidden.bs.collapse', function() {
    $('#posteriorpredictive_xaxis_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  shinyjs::runjs("$('#posteriorpredictive_yaxis_edits').on('shown.bs.collapse', function() {
    $('#posteriorpredictive_yaxis_edits_panel').find('.glyphicon-chevron-down').addClass('glyphicon-chevron-up').removeClass('glyphicon-chevron-down');
  });
  $('#posteriorpredictive_yaxis_edits').on('hidden.bs.collapse', function() {
    $('#posteriorpredictive_yaxis_edits_panel').find('.glyphicon-chevron-up').addClass('glyphicon-chevron-down').removeClass('glyphicon-chevron-up');
  });")
  
  observe({
    req(input$posteriorpredictive_teststatistic)
    updateTextInput(session = session, inputId = "posteriorpredictive_plot_y_lab", value = paste(input$posteriorpredictive_teststatistic, "statistic"))
  })
  
  
  observe({
    req(posteriorpredictive_logfileset$file[[1]])
    req(input$posteriorpredictive_teststatistic)
    
    teststatistic <- switch(input$posteriorpredictive_teststatistic, "Parsimony" = "parsimonyScore", 
                            "Tip-wise multinomial" = "tipwiseMultinomialLikelihood")
    file_name <- paste0(unlist(strsplit(posteriorpredictive_logfileset$file[[1]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1], 
                        "_posteriorPredictive_", teststatistic)
    updateTextInput(session = session, inputId = "posteriorpredictive_plot_downloadname", value = file_name)
  })
  
  observe({
    req(posteriorpredictive_values$teststatistics_alldf_processed)
    shinyjs::runjs('Shiny.onInputChange("posteriorpredictive_plot_width", document.getElementById("posteriorpredictive_plot").offsetWidth);')
  })
  
  
  observe({
    output$posteriorpredictive_plot <- renderPlot({
      
      # req(input$posteriorpredictive_geographyfile)
      req(posteriorpredictive_states_dat())
      req(input$posteriorpredictive_geographyfile_traitcolumn_name)
      req(input$posteriorpredictive_geographyfile_taxoncolumn_name)
      req(posteriorpredictive_input_condition$trait_invalid == F)
      
      req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
      req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
      for (i in 1:posteriorpredictive_loginputset$num) {
        req(posteriorpredictive_logfileset$file[[i]])
        req(posteriorpredictive_treefileset$file[[i]])
      }
      req(posteriorpredictive_input_condition$tree_invalid == F)
      req(posteriorpredictive_input_condition$log_invalid == F)
      req(posteriorpredictive_tree_values$tree)
      req(posteriorpredictive_input_condition$treetrait_invalid == F)
      req(posteriorpredictive_input_condition$logtree_invalid == F)
      
      req(posteriorpredictive_values$teststatistics_alldf_raw)
      req(length(posteriorpredictive_values$teststatistics_alldf_raw) == posteriorpredictive_loginputset$num)
      
      req(posteriorpredictive_values$teststatistics_alldf_processed)
      req(length(posteriorpredictive_values$teststatistics_alldf_processed) == posteriorpredictive_loginputset$num)
      req(input$posteriorpredictive_teststatistic)
      
      teststatistics_alldf_processed <- posteriorpredictive_values$teststatistics_alldf_processed
      
      value_list <- list()
      group_at<- c()
      rep_id <- c()
      boxplot_col <- c()
      priormodel_name <- character(length(teststatistics_alldf_processed))
      powers_all <- c()
      
      for (i in 1:length(teststatistics_alldf_processed)) {
        
        teststatistics_alldf <- teststatistics_alldf_processed[[i]]
        
        priormodel_name[i] <- paste0("prior", i)
        if (!is.null(input[[paste0("posteriorpredictive_priormodelname", i)]])) priormodel_name[i] <- input[[paste0("posteriorpredictive_priormodelname", i)]]
        
        powers <- sort(unique(teststatistics_alldf$likelihoodpower))
        numpowerbetween01 <- 0
        # numpowerbetween01 <- input$posteriorpredictive_numpowerbetween01
        
        if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
          
          if (numpowerbetween01 <= 0) {
            powers <- sort(powers[powers <= 0 | powers >= 1])
          } else {
            powers_between01 <- powers[powers > 0 & powers < 1]
            powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
            powers_rep <- powers_rep[-c(1, length(powers_rep))]
            powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
            powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
          }
        }
        
        powers_all <- c(powers_all, powers)
        
        for (j in 1:length(powers)) {
          
          if (powers[j] == 1) {
            col <- mycolorpalette[5]
          } else if (powers[j] == 0) {
            col <- mycolorpalette[4]
          } else {
            col <- "#A6ACAF"
          }
          
          repids <- sort(unique(teststatistics_alldf$repid[teststatistics_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            teststatistics_df <- teststatistics_alldf[teststatistics_alldf$likelihoodpower == powers[j] & teststatistics_alldf$repid == repids[k], ]
            
            if (input$posteriorpredictive_teststatistic == "Parsimony") {
              values_df <- teststatistics_df[, c("observed_parsimonyscore", "simulated_parsimonyscore")]
            } else if (input$posteriorpredictive_teststatistic == "Tip-wise multinomial") {
              values_df <- teststatistics_df[, c("observed_multinomiallikelihood", "simulated_multinomiallikelihood")]
            }
            
            values <- as.vector(apply(values_df, 1, function(x) x[2] - x[1]))
            
            if (k == 1 || (!input$posteriorpredictive_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              group_at <- c(group_at, i)
              rep_id <- c(rep_id, k)
              boxplot_col <- c(boxplot_col, col)
            } else if (input$posteriorpredictive_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
        
      }
      
      xaxis_lab_cex <- 1.35
      if (!is.null(input$posteriorpredictive_plot_xaxis_lab_cex)) xaxis_lab_cex <- input$posteriorpredictive_plot_xaxis_lab_cex
      
      xaxis_lab_line <- -1.5
      if (!is.null(input$posteriorpredictive_plot_xaxis_lab_line)) xaxis_lab_line <- input$posteriorpredictive_plot_xaxis_lab_line
      
      yaxis_lab_cex <- 0.75
      if (!is.null(input$posteriorpredictive_plot_yaxis_lab_cex)) yaxis_lab_cex <- input$posteriorpredictive_plot_yaxis_lab_cex
      
      yaxis_lab_line <- -1.5
      if (!is.null(input$posteriorpredictive_plot_yaxis_lab_line)) yaxis_lab_line <- input$posteriorpredictive_plot_yaxis_lab_line
      
      y_lab <- paste(input$posteriorpredictive_teststatistic, "statistic")
      if (!is.null(input$posteriorpredictive_plot_y_lab)) y_lab <- input$posteriorpredictive_plot_y_lab
      
      y_lab_cex <- 1.6
      if (!is.null(input$posteriorpredictive_plot_y_lab_cex)) y_lab_cex <- input$posteriorpredictive_plot_y_lab_cex
      
      y_lab_line <- 0.9
      if (!is.null(input$posteriorpredictive_plot_y_lab_line)) y_lab_line <- input$posteriorpredictive_plot_y_lab_line
      
      mai_bottom <- 0.05
      if (!is.null(input$posteriorpredictive_plot_margin_bottom)) mai_bottom <- input$posteriorpredictive_plot_margin_bottom
      
      mai_left <- 0.45
      if (!is.null(input$posteriorpredictive_plot_margin_left)) mai_left <- input$posteriorpredictive_plot_margin_left
      
      mai_top <- 0.15
      if (!is.null(input$posteriorpredictive_plot_margin_top)) mai_top <- input$posteriorpredictive_plot_margin_top
      
      mai_right <- 0
      if (!is.null(input$posteriorpredictive_plot_margin_right)) mai_right <- input$posteriorpredictive_plot_margin_right
      
      par(lend = 2, mai = c(mai_bottom, mai_left, mai_top, mai_right), xpd = F)
      multigroup_boxplot(value_list = value_list, value_anchor = 0, group_at = group_at, rep_id = rep_id,
                         box_col = boxplot_col, whiskcol = boxplot_col, staplecol = boxplot_col, 
                         plot_xaxis = T, xaxis_lab = priormodel_name, xaxis_lab_cex = xaxis_lab_cex, xaxis_lab_line = xaxis_lab_line, xaxis_side = 3, 
                         plot_xaxis_boxgroup = F,
                         plot_yaxis = F)
      
      axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
      axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      
      mtext(y_lab, side = 2, line = y_lab_line, cex = y_lab_cex)
      
    }, height = ifelse(!is.null(input$posteriorpredictive_plot_width) && as.numeric(input$posteriorpredictive_plot_width) > 0, 
                       as.numeric(input$posteriorpredictive_plot_width) * 0.55, 500))
  })
  
  
  output$posteriorpredictive_plot_download <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$posteriorpredictive_plot_downloadformat, "PDF" = ".pdf", "EPS" = ".eps", "PNG" = ".png", "JPEG" = ".jpeg", "TIFF" = ".tiff")
      paste0(input$posteriorpredictive_plot_downloadname, file_extension)
    },
    
    content = function(file) {
      
      teststatistics_alldf_processed <- posteriorpredictive_values$teststatistics_alldf_processed
      
      value_list <- list()
      group_at<- c()
      rep_id <- c()
      boxplot_col <- c()
      priormodel_name <- character(length(teststatistics_alldf_processed))
      powers_all <- c()
      
      for (i in 1:length(teststatistics_alldf_processed)) {
        
        teststatistics_alldf <- teststatistics_alldf_processed[[i]]
        
        priormodel_name[i] <- paste0("prior", i)
        if (!is.null(input[[paste0("posteriorpredictive_priormodelname", i)]])) priormodel_name[i] <- input[[paste0("posteriorpredictive_priormodelname", i)]]
        
        powers <- sort(unique(teststatistics_alldf$likelihoodpower))
        numpowerbetween01 <- 0
        # numpowerbetween01 <- input$posteriorpredictive_numpowerbetween01
        
        if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
          
          if (numpowerbetween01 <= 0) {
            powers <- sort(powers[powers <= 0 | powers >= 1])
          } else {
            powers_between01 <- powers[powers > 0 & powers < 1]
            powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
            powers_rep <- powers_rep[-c(1, length(powers_rep))]
            powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
            powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
          }
        }
        
        powers_all <- c(powers_all, powers)
        
        for (j in 1:length(powers)) {
          
          if (powers[j] == 1) {
            col <- mycolorpalette[5]
          } else if (powers[j] == 0) {
            col <- mycolorpalette[4]
          } else {
            col <- "#A6ACAF"
          }
          
          repids <- sort(unique(teststatistics_alldf$repid[teststatistics_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            teststatistics_df <- teststatistics_alldf[teststatistics_alldf$likelihoodpower == powers[j] & teststatistics_alldf$repid == repids[k], ]
            
            if (input$posteriorpredictive_teststatistic == "Parsimony") {
              values_df <- teststatistics_df[, c("observed_parsimonyscore", "simulated_parsimonyscore")]
            } else if (input$posteriorpredictive_teststatistic == "Tip-wise multinomial") {
              values_df <- teststatistics_df[, c("observed_multinomiallikelihood", "simulated_multinomiallikelihood")]
            }
            
            values <- as.vector(apply(values_df, 1, function(x) x[2] - x[1]))
            
            if (k == 1 || (!input$posteriorpredictive_logcombinereplicates)) {
              value_list <- c(value_list, list(values))
              group_at <- c(group_at, i)
              rep_id <- c(rep_id, k)
              boxplot_col <- c(boxplot_col, col)
            } else if (input$posteriorpredictive_logcombinereplicates && k > 1) {
              value_list[[length(value_list)]] <- c(value_list[[length(value_list)]], values)
            }
            
          }
        }
        
      }
      
      xaxis_lab_cex <- 1.35
      if (!is.null(input$posteriorpredictive_plot_xaxis_lab_cex)) xaxis_lab_cex <- input$posteriorpredictive_plot_xaxis_lab_cex
      
      xaxis_lab_line <- -1.5
      if (!is.null(input$posteriorpredictive_plot_xaxis_lab_line)) xaxis_lab_line <- input$posteriorpredictive_plot_xaxis_lab_line
      
      yaxis_lab_cex <- 0.75
      if (!is.null(input$posteriorpredictive_plot_yaxis_lab_cex)) yaxis_lab_cex <- input$posteriorpredictive_plot_yaxis_lab_cex
      
      yaxis_lab_line <- -1.5
      if (!is.null(input$posteriorpredictive_plot_yaxis_lab_line)) yaxis_lab_line <- input$posteriorpredictive_plot_yaxis_lab_line
      
      y_lab <- paste(input$posteriorpredictive_teststatistic, "statistic")
      if (!is.null(input$posteriorpredictive_plot_y_lab)) y_lab <- input$posteriorpredictive_plot_y_lab
      
      y_lab_cex <- 1.6
      if (!is.null(input$posteriorpredictive_plot_y_lab_cex)) y_lab_cex <- input$posteriorpredictive_plot_y_lab_cex
      
      y_lab_line <- 0.9
      if (!is.null(input$posteriorpredictive_plot_y_lab_line)) y_lab_line <- input$posteriorpredictive_plot_y_lab_line
      
      plot_width <- 500
      if (!is.null(input$posteriorpredictive_plot_width) && (as.numeric(input$posteriorpredictive_plot_width) > 0)) plot_width <- as.numeric(input$posteriorpredictive_plot_width)
      plot_height <- plot_width * 0.55
      res <- 300
      if (input$posteriorpredictive_plot_downloadformat == "PDF") {
        pdf(file = file, width = plot_width/72, height = plot_height/72)
      } else if (input$posteriorpredictive_plot_downloadformat == "EPS") {
        setEPS()
        postscript(file = file, width = plot_width/72, height = plot_height/72)
      } else if (input$posteriorpredictive_plot_downloadformat == "PNG") {
        png(file = file, width = plot_width/72*res, height = plot_height/72*res, res = res)
      } else if (input$posteriorpredictive_plot_downloadformat == "JPEG") {
        jpeg(file = file, width = plot_width/72*res, height = plot_height/72*res, res = res)
      } else if (input$posteriorpredictive_plot_downloadformat == "TIFF") {
        tiff(file = file, width =  plot_width/72*res, height = plot_height/72*res, res = res)
      }
      
      mai_bottom <- 0.05
      if (!is.null(input$posteriorpredictive_plot_margin_bottom)) mai_bottom <- input$posteriorpredictive_plot_margin_bottom
      
      mai_left <- 0.45
      if (!is.null(input$posteriorpredictive_plot_margin_left)) mai_left <- input$posteriorpredictive_plot_margin_left
      
      mai_top <- 0.15
      if (!is.null(input$posteriorpredictive_plot_margin_top)) mai_top <- input$posteriorpredictive_plot_margin_top
      
      mai_right <- 0
      if (!is.null(input$posteriorpredictive_plot_margin_right)) mai_right <- input$posteriorpredictive_plot_margin_right
      
      par(lend = 2, mai = c(mai_bottom, mai_left, mai_top, mai_right), xpd = F)
      multigroup_boxplot(value_list = value_list, value_anchor = 0, group_at = group_at, rep_id = rep_id,
                         box_col = boxplot_col, whiskcol = boxplot_col, staplecol = boxplot_col, 
                         plot_xaxis = T, xaxis_lab = priormodel_name, xaxis_lab_cex = xaxis_lab_cex, xaxis_lab_line = xaxis_lab_line, xaxis_side = 3, 
                         plot_xaxis_boxgroup = F,
                         plot_yaxis = F)
      
      axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1, tck = -0.015)
      axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = yaxis_lab_cex, line = yaxis_lab_line)
      
      mtext(y_lab, side = 2, line = y_lab_line, cex = y_lab_cex)
      
      dev.off()
    }
  )
  
  
  output$posteriorpredictive_table <- DT::renderDataTable({
    
    # req(input$posteriorpredictive_geographyfile)
    req(posteriorpredictive_states_dat())
    req(input$posteriorpredictive_geographyfile_traitcolumn_name)
    req(input$posteriorpredictive_geographyfile_taxoncolumn_name)
    req(posteriorpredictive_input_condition$trait_invalid == F)
    
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_logfileset$file))
    req(posteriorpredictive_loginputset$num == length(posteriorpredictive_treefileset$file))
    for (i in 1:posteriorpredictive_loginputset$num) {
      req(posteriorpredictive_logfileset$file[[i]])
      req(posteriorpredictive_treefileset$file[[i]])
    }
    req(posteriorpredictive_input_condition$tree_invalid == F)
    req(posteriorpredictive_input_condition$log_invalid == F)
    req(posteriorpredictive_tree_values$tree)
    req(posteriorpredictive_input_condition$treetrait_invalid == F)
    req(posteriorpredictive_input_condition$logtree_invalid == F)
    
    req(posteriorpredictive_values$teststatistics_alldf_raw)
    req(length(posteriorpredictive_values$teststatistics_alldf_raw) == posteriorpredictive_loginputset$num)
    
    req(posteriorpredictive_values$teststatistics_alldf_processed)
    req(length(posteriorpredictive_values$teststatistics_alldf_processed) == posteriorpredictive_loginputset$num)
    req(input$posteriorpredictive_teststatistic)
    
    teststatistics_alldf_processed <- posteriorpredictive_values$teststatistics_alldf_processed
    
    parsimonyscore_value_list <- list()
    multinomiallikelihood_value_list <- list()
    rep_id <- c()
    priormodelname_all <- c()
    powers_all <- c()
    
    for (i in 1:length(teststatistics_alldf_processed)) {
      
      teststatistics_alldf <- teststatistics_alldf_processed[[i]]
      
      priormodel_name <- paste0("prior", i)
      if (!is.null(input[[paste0("posteriorpredictive_priormodelname", i)]])) priormodel_name <- input[[paste0("posteriorpredictive_priormodelname", i)]]
      
      powers <- sort(unique(teststatistics_alldf$likelihoodpower))
      numpowerbetween01 <- 0
      # numpowerbetween01 <- input$posteriorpredictive_numpowerbetween01
      
      if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
        
        if (numpowerbetween01 <= 0) {
          powers <- sort(powers[powers <= 0 | powers >= 1])
        } else {
          powers_between01 <- powers[powers > 0 & powers < 1]
          powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
          powers_rep <- powers_rep[-c(1, length(powers_rep))]
          powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
          powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
        }
      }
      
      for (j in 1:length(powers)) {
        
        repids <- sort(unique(teststatistics_alldf$repid[teststatistics_alldf$likelihoodpower == powers[j]]))
        for (k in 1:length(repids)) {
          
          teststatistics_df <- teststatistics_alldf[teststatistics_alldf$likelihoodpower == powers[j] & teststatistics_alldf$repid == repids[k], ]
          
          parsimonyscore_values <- as.vector(apply(teststatistics_df, 1, function(x) x["simulated_parsimonyscore"] - x["observed_parsimonyscore"]))
          multinomiallikelihood_values <- as.vector(apply(teststatistics_df, 1, function(x) x["simulated_multinomiallikelihood"] - x["observed_multinomiallikelihood"]))
          
          if (k == 1 || (!input$posteriorpredictive_logcombinereplicates)) {
            
            parsimonyscore_value_list <- c(parsimonyscore_value_list, list(parsimonyscore_values))
            multinomiallikelihood_value_list <- c(multinomiallikelihood_value_list, list(multinomiallikelihood_values))
            rep_id <- c(rep_id, k)
            powers_all <- c(powers_all, powers[j])
            priormodelname_all <- c(priormodelname_all, priormodel_name)
            
          } else if (input$posteriorpredictive_logcombinereplicates && k > 1) {
            parsimonyscore_value_list[[length(parsimonyscore_value_list)]] <- c(parsimonyscore_value_list[[length(parsimonyscore_value_list)]], parsimonyscore_values)
            multinomiallikelihood_value_list[[length(multinomiallikelihood_value_list)]] <- c(multinomiallikelihood_value_list[[length(multinomiallikelihood_value_list)]], multinomiallikelihood_values)
          }
          
        }
      }
      
    }
    
    parsimonyscore_ppps <- sapply(parsimonyscore_value_list, function(x) sum(x >= 0)/length(x))
    multinomiallikelihood_ppps <- sapply(multinomiallikelihood_value_list, function(x) sum(x >= 0)/length(x))
    
    teststatistics_df <- data.frame(cbind(priormodelname_all, rep_id, parsimonyscore_ppps, multinomiallikelihood_ppps), stringsAsFactors = F)
    colnames(teststatistics_df) <- c("Prior model", "Replicate", "Posterior-predictive p-value for the parsimony statistic", 
                                     "Posterior-predictive p-value for the tip-wise multinomial statistic")
    
    teststatistics_df
  })
  
  observe({
    req(posteriorpredictive_logfileset$file[[1]])
    
    file_name <- paste0(unlist(strsplit(posteriorpredictive_logfileset$file[[1]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1], 
                        "_posteriorPredictive_pvalues")
    updateTextInput(session = session, inputId = "posteriorpredictive_table_downloadname", value = file_name)
  })
  
  output$posteriorpredictive_table_download <- downloadHandler(
    
    filename = function() {
      file_extension <- switch(input$posteriorpredictive_table_downloadformat, "TSV" = ".tsv", "CSV" = ".csv")
      paste0(input$posteriorpredictive_table_downloadname, file_extension)
    },
    
    content = function(file) {
      
      teststatistics_alldf_processed <- posteriorpredictive_values$teststatistics_alldf_processed
      
      parsimonyscore_value_list <- list()
      multinomiallikelihood_value_list <- list()
      rep_id <- c()
      priormodelname_all <- c()
      powers_all <- c()
      
      for (i in 1:length(teststatistics_alldf_processed)) {
        
        teststatistics_alldf <- teststatistics_alldf_processed[[i]]
        
        priormodel_name <- paste0("prior", i)
        if (!is.null(input[[paste0("posteriorpredictive_priormodelname", i)]])) priormodel_name <- input[[paste0("posteriorpredictive_priormodelname", i)]]
        
        powers <- sort(unique(teststatistics_alldf$likelihoodpower))
        numpowerbetween01 <- 0
        # numpowerbetween01 <- input$posteriorpredictive_numpowerbetween01
        
        if (sum(powers > 0 & powers < 1) > numpowerbetween01) {
          
          if (numpowerbetween01 <= 0) {
            powers <- sort(powers[powers <= 0 | powers >= 1])
          } else {
            powers_between01 <- powers[powers > 0 & powers < 1]
            powers_rep <- qbeta(seq(from = 0, to = 1, length.out = numpowerbetween01 + 2), shape1 = 0.3, shape2 = 1)
            powers_rep <- powers_rep[-c(1, length(powers_rep))]
            powers_rep <- sapply(powers_rep, function(x) powers_between01[which(abs(x - powers_between01) == min(abs(x - powers_between01)))])
            powers <- sort(c(powers[powers <= 0 | powers >= 1], powers_rep))
          }
        }
        
        for (j in 1:length(powers)) {
          
          repids <- sort(unique(teststatistics_alldf$repid[teststatistics_alldf$likelihoodpower == powers[j]]))
          for (k in 1:length(repids)) {
            
            teststatistics_df <- teststatistics_alldf[teststatistics_alldf$likelihoodpower == powers[j] & teststatistics_alldf$repid == repids[k], ]
            
            parsimonyscore_values <- as.vector(apply(teststatistics_df, 1, function(x) x["simulated_parsimonyscore"] - x["observed_parsimonyscore"]))
            multinomiallikelihood_values <- as.vector(apply(teststatistics_df, 1, function(x) x["simulated_multinomiallikelihood"] - x["observed_multinomiallikelihood"]))
            
            if (k == 1 || (!input$posteriorpredictive_logcombinereplicates)) {
              
              parsimonyscore_value_list <- c(parsimonyscore_value_list, list(parsimonyscore_values))
              multinomiallikelihood_value_list <- c(multinomiallikelihood_value_list, list(multinomiallikelihood_values))
              rep_id <- c(rep_id, k)
              powers_all <- c(powers_all, powers[j])
              priormodelname_all <- c(priormodelname_all, priormodel_name)
              
            } else if (input$posteriorpredictive_logcombinereplicates && k > 1) {
              parsimonyscore_value_list[[length(parsimonyscore_value_list)]] <- c(parsimonyscore_value_list[[length(parsimonyscore_value_list)]], parsimonyscore_values)
              multinomiallikelihood_value_list[[length(multinomiallikelihood_value_list)]] <- c(multinomiallikelihood_value_list[[length(multinomiallikelihood_value_list)]], multinomiallikelihood_values)
            }
            
          }
        }
        
      }
      
      parsimonyscore_ppps <- sapply(parsimonyscore_value_list, function(x) sum(x >= 0)/length(x))
      multinomiallikelihood_ppps <- sapply(multinomiallikelihood_value_list, function(x) sum(x >= 0)/length(x))
      
      teststatistics_df <- data.frame(cbind(priormodelname_all, rep_id, parsimonyscore_ppps, multinomiallikelihood_ppps), stringsAsFactors = F)
      colnames(teststatistics_df) <- c("Prior model", "Replicate", "Posterior-predictive p-value for the parsimony statistic", 
                                       "Posterior-predictive p-value for the tip-wise multinomial statistic")
      
      write.table(teststatistics_df, file = file, sep = switch(input$posteriorpredictive_table_downloadformat, "TSV" = "\t", "CSV" = ","),
                  row.names = F)
    }
  )
  
  
  observe({
    req(posteriorpredictive_logfileset$file[[1]])
    
    file_name <- paste0(unlist(strsplit(posteriorpredictive_logfileset$file[[1]]$name[1], "_underprior|_posterior|_datacloning|_MLE"))[1], 
                        "_simulatedDataset")
    updateTextInput(session = session, inputId = "posteriorpredictive_simdata_downloadname", value = file_name)
  })
  
  output$posteriorpredictive_simdata_download <- downloadHandler(
    
    filename = function() {
      if (length(posteriorpredictive_values$simulateddata_tmppath) == 1) {
        paste0(input$posteriorpredictive_simdata_downloadname, ".tsv")
      } else {
        paste0(input$posteriorpredictive_simdata_downloadname, ".zip")
      }
    },
    
    content = function(file) {
      
      simdata_names <- c()
      teststatistics_alldf_raw <- posteriorpredictive_values$teststatistics_alldf_raw
      
      for (i in 1:posteriorpredictive_loginputset$num) {
        
        priormodel_name <- paste0("prior", i)
        if (!is.null(input[[paste0("posteriorpredictive_priormodelname", i)]])) {
          priormodel_name <- input[[paste0("posteriorpredictive_priormodelname", i)]]
        }
        
        if (!input$posteriorpredictive_logcombinereplicates) {
          
          for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
            if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]]) {
              
              teststatistics_df <- teststatistics_alldf_raw[[i]][[j]]
              
              burnin_prop <- 0
              if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]] > 0) {
                burnin_prop <- input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]] / 100
              }
              
              simdata_tmppath <- grep(paste0("/prior", i, "_log", j, "_simulateddataset.tsv"), posteriorpredictive_values$simulateddata_tmppath, value = T)
              simulated_dataset <- read.table(simdata_tmppath, header = T, sep = "\t", stringsAsFactors = F, check.names = F)
              
              burnin <- ceiling(nrow(teststatistics_df) * burnin_prop)
              if (burnin >= nrow(simulated_dataset)) {
                simulated_dataset <- simulated_dataset[nrow(simulated_dataset), , drop = F]
              } else if (burnin > 0) {
                simulated_dataset <- simulated_dataset[-(1:burnin), , drop = F]
              }
              
              log_name <- unlist(strsplit(posteriorpredictive_logfileset$file[[i]]$name[j], "\\."))[1]
              simdata_name <- paste0(log_name, "_", priormodel_name, "_simulateddataset.tsv")
              simdata_names <- c(simdata_names, simdata_name)
              
              write.table(simulated_dataset, file = simdata_name, quote = F, sep = "\t", row.names = F)
            }
          }
          
        } else {
          
          power_all <- c()
          simdata_tmppath_all <- c()
          burnin_all <- c()
          
          for (j in 1:nrow(posteriorpredictive_logfileset$file[[i]])) {
            if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_include")]]) {
              
              teststatistics_df <- teststatistics_alldf_raw[[i]][[j]]
              
              power <- unique(teststatistics_df$likelihoodpower)[1]
              simdata_tmppath <- grep(paste0("/prior", i, "_log", j, "_simulateddataset.tsv"), posteriorpredictive_values$simulateddata_tmppath, value = T)
              
              burnin_prop <- 0
              if ((!is.null(input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]])) && input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]] > 0) {
                burnin_prop <- input[[paste0("posteriorpredictive_logfile", i, "_log", j, "_burnin")]] / 100
              }
              burnin <- ceiling(nrow(teststatistics_df) * burnin_prop)
              
              power_all <- c(power_all, power)
              simdata_tmppath_all <- c(simdata_tmppath_all, simdata_tmppath)
              burnin_all <- c(burnin_all, burnin)
            }
          }
          
          for (power in sort(unique(power_all))) {
            
            simdata_tmppaths <- simdata_tmppath_all[power_all == power]
            burnins <- burnin_all[power_all == power]
            simulated_datasets <- vector("list", length(simdata_tmppaths))
            
            for (k in 1:length(simdata_tmppaths)) {
              
              simulated_dataset <- read.table(simdata_tmppaths[k], header = T, sep = "\t", stringsAsFactors = F, check.names = F)
              
              if (burnins[k] >= nrow(simulated_dataset)) {
                simulated_dataset <- simulated_dataset[nrow(simulated_dataset), ]
              } else if (burnins[k] > 0) {
                simulated_dataset <- simulated_dataset[-(1:burnins[k]), ]
              }
              
              simulated_datasets[[k]] <- data.frame(simulated_dataset)
            }
            
            simulated_dataset <- do.call(rbind, simulated_datasets)
            
            if (length(simdata_tmppaths) == 1) {
              log_name <- unlist(strsplit(posteriorpredictive_logfileset$file[[i]]$name[j], "\\."))[1]
            } else if (length(simdata_tmppaths) > 1) {
              log_name <- unlist(strsplit(posteriorpredictive_logfileset$file[[i]]$name[j], "_underprior|_posterior|_datacloning|_MLE"))[1]
              if (power == 0) {
                log_name <- paste0(log_name, "_underprior")
              } else if (power == 1) {
                log_name <- paste0(log_name, "_posterior")
              } else if (power > 1) {
                log_name <- paste0(log_name, "_datacloning", power)
              }
              log_name <- paste0(log_name, "_combined")
            }
            
            simdata_name <- paste0(log_name, "_", priormodel_name, "_simulateddataset.tsv")
            simdata_names <- c(simdata_names, simdata_name)
            
            write.table(simulated_dataset, file = simdata_name, quote = F, sep = "\t", row.names = F)
          }
        }
      }

      if (length(simdata_names) == 1) {
        file.rename(simdata_names, file)
      } else if (length(simdata_names) > 1) {
        zip(zipfile = file, files = simdata_names, flags = "-j")
      }
      
      file.remove(simdata_names)
    }
  )
  
  
  outputOptions(output, "posteriorpredictive_logprocessing_ui", suspendWhenHidden = F)
  outputOptions(output, "posteriorpredictive_priormodelname_ui", suspendWhenHidden = F)
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
