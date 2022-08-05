
methods_template_height <- 200
xml_viewer_height <- 400

# Define UI for app
ui <- shiny::fluidPage(
  
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(".divdisabled {pointer-events: none; opacity: 0.4;}"),
  shinyjs::inlineCSS(".disappeared {visibility:hidden;}"),
  shiny::tags$head(
    shiny::tags$script(src = "prism.js"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/prism-okaidia.css")
  ),
  shiny::tags$style(type = 'text/css', ".nav-tabs {font-size: 90%;}"),
  shiny::tags$style(type = 'text/css', ".nav-pills {font-size: 85%;}"),
  
  shiny::tags$script(shiny::HTML("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});"), type = "text/x-mathjax-config"),
  
  shiny::navbarPage(title = "PrioriTree", id = "main_menu",
                    
                    shiny::tabPanel(title = "Analysis Setup", value = "analysis_setup",
                                    shiny::fluidRow(
                                      shiny::column(width = 4, style = "padding-left: 10px; padding-right: 10px;",
                                                    shiny::wellPanel(style = "background-color: #F2F3F4;",
                                                                     
                                                                     shiny::navlistPanel(id = "main_tabs", well = F, widths = c(4, 8), 
                                                                                         
                                                                                         # data input panel
                                                                                         shiny::tabPanel(title = "Step 1: Import Data", value = "data_input",
                                                                                                         
                                                                                                         shiny::tabsetPanel(type = "tabs", id = "datainput_tabs",
                                                                                                                            # upload the discrete-trait data
                                                                                                                            shiny::tabPanel(title = "Discrete-Geography File", value = "trait",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::checkboxInput(inputId = "geography_file_defaultupload", label = "Load example discrete-geography file", value = F),
                                                                                                                                            
                                                                                                                                            shiny::div(id = "geography_file_div",
                                                                                                                                                       shiny::fileInput(inputId = "geography_file", label = "Choose discrete-geography file", 
                                                                                                                                                                        multiple = F, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt"))
                                                                                                                                            ),
                                                                                                                                            
                                                                                                                                            shiny::div(id = "geographyfile_attributes",
                                                                                                                                                       shiny::checkboxInput(inputId = "geographyfile_header", label = "File header", value = T),
                                                                                                                                                       shiny::selectInput(inputId = "taxoncolumn_name", label = "Taxon column name", choices = "", selectize = F),
                                                                                                                                                       shiny::selectInput(inputId = "traitcolumn_name", label = "Discrete-geography column name", choices = "", selectize = F)
                                                                                                                                            )
                                                                                                                            ),
                                                                                                                            
                                                                                                                            # upload either a posterior distribution of trees or a single (MCC) tree
                                                                                                                            shiny::tabPanel(title = "Tree(s) File", value = "tree",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::checkboxInput(inputId = "tree_file_defaultupload", label = "Load example tree(s) file", value = F),
                                                                                                                                            
                                                                                                                                            shiny::div(id = "tree_num_defaultupload_div",
                                                                                                                                                       shiny::radioButtons(inputId = "tree_num_defaultupload", label = "Type of example tree to load", 
                                                                                                                                                                           choices = c("distribution (multiple)", "single"), inline = T),
                                                                                                                                            ),
                                                                                                                                            
                                                                                                                                            shiny::div(id = "tree_file_div",
                                                                                                                                                       shiny::fileInput("tree_file", "Choose tree(s) file", multiple = F, accept = c(".tree", ".trees", ".tre"))
                                                                                                                                            )
                                                                                                                            )
                                                                                                                            
                                                                                                         )
                                                                                         ),
                                                                                         
                                                                                         # model specification panel
                                                                                         shiny::tabPanel(title = "Step 2: Specify Model", value = "model_specification",
                                                                                                         
                                                                                                         shiny::tabsetPanel(type = "tabs", 
                                                                                                                            shiny::tabPanel(title = "Geographic Model",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::radioButtons(inputId = "model_symmetry", label = "Rate-Matrix Symmetry", choices = c("symmetric", "asymmetric"), 
                                                                                                                                                                selected = prioritree:::.pkg_env$inputdefault_init$model_symmetry, inline = T),
                                                                                                                                            
                                                                                                                                            shiny::h3(""),
                                                                                                                                            shiny::checkboxInput(inputId = "with_bssvs", label = "Infer dispersal routes using BSSVS", value = prioritree:::.pkg_env$inputdefault_init$with_bssvs)),
                                                                                                                            shiny::tabPanel(title = "Tree Model",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::uiOutput("treemodel_ui")
                                                                                                                            )
                                                                                                         ),
                                                                                                         
                                                                                                         shiny::h4(""),
                                                                                                         shiny::actionButton(inputId = "enablestep3_button", label = "Proceed to next step")
                                                                                         ),
                                                                                         
                                                                                         # analysis configuration panel
                                                                                         shiny::tabPanel(title = "Step 3: Configure Analysis (Basic)", value = "analysis_configBasic",
                                                                                                         
                                                                                                         shiny::tabsetPanel(type = "tabs", id = "analysisconfigBasic_tabs",
                                                                                                                            
                                                                                                                            shiny::tabPanel(title = "MCMC Sampling", value = "mcmcsampling",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::numericInput(inputId = "mcmc_chainlength", label = "MCMC chain length", 
                                                                                                                                                                value = prioritree:::.pkg_env$inputdefault_init$mcmc_chainlength, min = 1),
                                                                                                                                            
                                                                                                                                            shiny::h5(""),
                                                                                                                                            shiny::numericInput(inputId = "mcmc_samplingfreq", label = "Sampling frequency of the MCMC chain", 
                                                                                                                                                                value = prioritree:::.pkg_env$inputdefault_init$mcmc_samplingfreq, min = 1),
                                                                                                                                            
                                                                                                                                            shiny::h4(""),
                                                                                                                                            shiny::numericInput(inputId = "mcmc_numreplicates", label = "Number of analysis replicates", 
                                                                                                                                                                value = prioritree:::.pkg_env$inputdefault_init$mcmc_numreplicates, min = 1)
                                                                                                                            ),
                                                                                                                            
                                                                                                                            shiny::tabPanel(title = "Proposal Weights", value = "proposalweights",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::uiOutput("proposalweight_ui")
                                                                                                                            )
                                                                                                         ),
                                                                                                         
                                                                                                         shiny::h4(""),
                                                                                                         shiny::actionButton(inputId = "enablestep4_button", label = "Proceed to next step")
                                                                                         ),
                                                                                         
                                                                                         shiny::tabPanel(title = "Step 4: Configure Analysis (Advanced)", value = "analysis_configAdvanced",
                                                                                                         
                                                                                                         shiny::tabsetPanel(type = "tabs", id = "analysisconfigAdvanced_tabs",
                                                                                                                            
                                                                                                                            shiny::tabPanel(title = "Summary Statistics",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::selectInput(inputId = "do_stochasticmapping", label = "Estimate number of dispersal events using",
                                                                                                                                                               choices = c(Choose = '', "Fast stochastic mapping (incomplete history, simulation-free)", 
                                                                                                                                                                           "Stochastic mapping (complete history, simulation-based)"), 
                                                                                                                                                               selected = "", multiple = F, selectize = F),
                                                                                                                                            shiny::div(id = "markovjumps_totalpairwise", style = "margin-left:15px",
                                                                                                                                                       shiny::checkboxInput(inputId = "markovjumps_total", label = "overall number of dispersal events", value = T),
                                                                                                                                                       shiny::checkboxInput(inputId = "markovjumps_pairwise", label = "number of dispersal events between each pair of areas", value = T)
                                                                                                                                            )
                                                                                                                            ),
                                                                                                                            shiny::tabPanel(title = "Model Exploration",
                                                                                                                                            shiny::h6(""),
                                                                                                                                            shiny::selectInput(inputId = "further_analysis", label = "Setting up further analysis for model exploration", 
                                                                                                                                                               choices = c(Choose = "", "Marginal likelihood estimation", "Under prior", "Data cloning"), 
                                                                                                                                                               multiple = F, selectize = F),
                                                                                                                                            shiny::uiOutput("furtheranalysis_ui")
                                                                                                                            )
                                                                                                         ),
                                                                                                         
                                                                                                         shiny::h4(""),
                                                                                                         shiny::actionButton(inputId = "enablestep5_button", label = "Proceed to next step")
                                                                                         ),
                                                                                         
                                                                                         shiny::tabPanel(title = "Step 5: Download BEAST XML and Methods Template", value = "src_export",
                                                                                                         
                                                                                                         shiny::tabsetPanel(type = "tabs", id = "download_tabs",
                                                                                                                            
                                                                                                                            shiny::tabPanel(title = "BEAST XML", style = "font-size:90%;",
                                                                                                                                            
                                                                                                                                            shiny::h6(''),
                                                                                                                                            shiny::textInput(inputId = "xml_name", label = "Name of the XML file (without .xml as it will be appended automatically)", value = "geo"),
                                                                                                                                            shiny::actionButton(inputId = "downloadxml_fake", label = "Download", icon = shiny::icon("download")),
                                                                                                                                            shiny::downloadButton(outputId = "downloadxml_real", label = "Download")
                                                                                                                            ),
                                                                                                                            
                                                                                                                            shiny::tabPanel(title = "Methods Template", style = "font-size:90%;",
                                                                                                                                            
                                                                                                                                            shiny::h6(''),
                                                                                                                                            shiny::radioButtons("downloadmethods_format", label = "Format of the Methods template file", choices = c("Word", "LaTex", "PDF", "HTML", "Markdown"), selected = "Word", inline = T),
                                                                                                                                            shiny::textInput(inputId = "methods_name", label = "Name of the Methods template file (without filename extension, e.g., '.doc', as it will be appended automatically)", value = "methods"),
                                                                                                                                            shiny::actionButton(inputId = "downloadmethods_fake", label = "Download", icon = shiny::icon("download")),
                                                                                                                                            shiny::downloadButton(outputId = "downloadmethods_real", label = "Download"))
                                                                                                         )
                                                                                         )
                                                                                         
                                                                     )
                                                    )
                                                    
                                      ),
                                      
                                      shiny::column(width = 8, offset = 0, style = "padding: 0px 15px 0px 15px; margin: 0%;",
                                                    
                                                    shiny::fluidRow(id = "analysis_specification_prior", style = "padding: 2px; margin: 0%;",
                                                                    
                                                                    shiny::HTML('<div id = "prior_specification_panel" class="panel-heading" style = "background: none; border: 0;"><h4 class="panel-title"><a data-toggle="collapse" href="#prior_specification"><span class="glyphicon glyphicon-chevron-down" aria-hidden="true"></span>Prior Specification</a></h4></div>'),
                                                                    shiny::div(id = 'prior_specification', class = "panel-collapse collapse", 
                                                                               shiny::column(width = 3, offset = 0, style = "padding: 7px 10px 0px 10px; margin: 0px 5px 0px 0px; background-color: #D6EAF8;",
                                                                                             shiny::tabsetPanel(type = "tabs", id = "prior_tabs",
                                                                                                                
                                                                                                                shiny::tabPanel(title = shiny::HTML("Number of Dispersal Routes, &Delta;"), value = "delta",
                                                                                                                                shiny::h6(""),
                                                                                                                                
                                                                                                                                shiny::selectInput(inputId = "delta_prior", label = shiny::HTML("Prior on &Delta;"), 
                                                                                                                                                   choices = c("Poisson", "Beta-Binomial", "Uniform"),
                                                                                                                                                   multiple = F, selectize = T),
                                                                                                                                shiny::uiOutput("deltaprior_ui")),
                                                                                                                shiny::tabPanel(title = shiny::HTML("Average Dispersal Rate, &mu;"), value = "mu", 
                                                                                                                                shiny::h6(""),
                                                                                                                                
                                                                                                                                shiny::selectInput(inputId = "mu_prior", label = shiny::HTML("Prior on &mu;"), 
                                                                                                                                                   choices = c("Hierarchical Exponential", "CTMC rate-ref (BEAST default)", "Empirical-Informed Exponential"),
                                                                                                                                                   multiple = F, selectize = T),
                                                                                                                                shiny::uiOutput("muprior_ui"),
                                                                                                                                shiny::checkboxInput(inputId = "parsimonyscore_alltree", label = "Recompute parsimony score using all trees (to make the app more efficient, the default value is computed using the first two trees)", value = F)
                                                                                                                )
                                                                                                                
                                                                                             )
                                                                               ),
                                                                               
                                                                               shiny::column(width = 4, offset = 0, style = "padding: 7px 10px 0px 10px; margin: 0px 12px 0px 5px; background-color: #F4F6F6;",
                                                                                             shiny::tabsetPanel(type = "tabs", id = "priornote_tabs",
                                                                                                                
                                                                                                                shiny::tabPanel(title = "Brief Description", value = "text",
                                                                                                                                shiny::h6(''),
                                                                                                                                shiny::uiOutput(outputId = "priordescription_ui")
                                                                                                                ),
                                                                                                                
                                                                                                                shiny::tabPanel(title = "Math Notation", value = "math",
                                                                                                                                shiny::h6(''),
                                                                                                                                shiny::uiOutput(outputId = "priornotation_ui")
                                                                                                                )                                                               
                                                                                             )
                                                                               ),
                                                                               
                                                                               shiny::column(width = 4, offset = 0, style = "padding: 0.2px; margin: 0%; background-color: #ffffff;",
                                                                                             shiny::uiOutput(outputId = "priorplot_ui")
                                                                               )
                                                                    )
                                                                    
                                                    ),
                                                    
                                                    shiny::div(id = "methods_template_div",
                                                               shiny::h3(""),
                                                               
                                                               shiny::HTML('<div id = "methods_template_panel" class = "panel-heading"><h4 class="panel-title"><a data-toggle="collapse" href="#methods_template"><span class="glyphicon glyphicon-chevron-down" aria-hidden="true"></span>Methods Template Viewer</a></h4></div>'),
                                                               shiny::div(id = 'methods_template', class = "panel-collapse collapse",
                                                                          shiny::tabsetPanel(type = "tabs", id = "methods_tabs",
                                                                                             
                                                                                             shiny::tabPanel(title = "Data", value = "data",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height:", methods_template_height, "px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;"),
                                                                                                                             shiny::uiOutput("methods_data")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Model", value = "model",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height:", methods_template_height, "px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;"),
                                                                                                                             shiny::uiOutput("methods_model")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Bayesian Inference (Prior)", value = "prior",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height:", methods_template_height, "px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;"),
                                                                                                                             shiny::uiOutput("methods_prior")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Analysis", value = "analysis",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height:", methods_template_height, "px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;"),
                                                                                                                             shiny::uiOutput("methods_analysis")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Full", value = "full",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height:", methods_template_height, "px; overflow: auto; background-color: #F4F6F6; padding: 2px 10px 5px 10px;"),
                                                                                                                             shiny::uiOutput("methods_full")
                                                                                                             )
                                                                                             )
                                                                          )
                                                                          
                                                               )
                                                    ),
                                                    
                                                    shiny::div(id = "xml_viewer_div",
                                                               shiny::h3(""),
                                                               
                                                               shiny::HTML('<div id = "xml_viewer_panel" class = "panel-heading"><h4 class="panel-title"><a data-toggle="collapse" href="#xml_viewer"><span class="glyphicon glyphicon-chevron-down" aria-hidden="true"></span>BEAST XML Viewer</a></h4></div>'),
                                                               shiny::div(id = 'xml_viewer', class = "panel-collapse collapse",
                                                                          
                                                                          shiny::tabsetPanel(type = "tabs", id = "xmlviewer_tabs",
                                                                                             
                                                                                             shiny::tabPanel(title = "Data", value = "xmldata",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height:", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_data")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Model and Prior", value = "xmlmodelprior",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height: ", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_modelprior")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "MCMC Sampling", value = "xmlmcmc",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height: ", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_mcmc")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Proposal", value = "xmlproposal",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height: ", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_proposal")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Phylogenetic Likelihood and Stochastic Mapping", value = "xmlphyloctmc",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height: ", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_phyloctmc")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Marginal Likelihood Estimation", value = "xmlpowerposterior",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height: ", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_powerposterior")
                                                                                                             )
                                                                                             ),
                                                                                             
                                                                                             shiny::tabPanel(title = "Full", value = "xmlfull",
                                                                                                             shiny::h6(""),
                                                                                                             shiny::fluidRow(style = paste0("height: ", xml_viewer_height, "px; overflow: auto;"),
                                                                                                                             shiny::uiOutput("beastxml_full")
                                                                                                             )
                                                                                             )
                                                                          )
                                                                          
                                                               )
                                                    )
                                      )
                                    )
                    ),
                    
                    shiny::navbarMenu(title = "Model-Exploration Post-Processing", menuName = "post_processing",
                                      
                                      shiny::tabPanel(title = "Data Cloning", value = "focalparam_summary",
                                                      shiny::fluidRow(
                                                        shiny::column(width = 4, style = "padding-left: 10px; padding-right: 10px; overflow-y:auto; max-height: 800px;",
                                                                      
                                                                      shiny::wellPanel(style = "background-color: #F2F3F4;",
                                                                                       
                                                                                       shiny::navlistPanel(id = "focalparamsummary_tabs", well = F, widths = c(4, 8), 
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 1: Upload Log File(s)", value = "log_input",
                                                                                                                           shiny::h6(""),
                                                                                                                           
                                                                                                                           shiny::checkboxInput(inputId = "focalparamsummary_logfile_defaultupload", label = "Load example log files", value = F),
                                                                                                                           
                                                                                                                           shiny::div(id = paste0("focalparamsummary_logfile_div", 1),
                                                                                                                                      shiny::fileInput(inputId = paste0("focalparamsummary_logfile", 1), 
                                                                                                                                                       label = paste0("Estimate log file(s) under prior No. ", 1), 
                                                                                                                                                       multiple = T, accept = ".log")
                                                                                                                           ),
                                                                                                                           
                                                                                                                           shiny::div(id = "focalparamsummary_loginput_addremove_div",
                                                                                                                                      shiny::p("Add or remove prior model", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                      shiny::actionButton(inputId = "focalparamsummary_loginput_add", label = "", icon = shiny::icon("plus", lib = "glyphicon")),
                                                                                                                                      shiny::actionButton(inputId = "focalparamsummary_loginput_remove", label = "", icon = shiny::icon("minus", lib = "glyphicon"))
                                                                                                                           ),
                                                                                                                           
                                                                                                                           shiny::uiOutput(outputId = "focalparamsummary_paramname_ui"),
                                                                                                                           shiny::actionButton(inputId = "focalparamsummary_startprocessing", label = "Read in files and start initial processing")
                                                                                                           ),
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 2: Configure Post-Processing Settings", value = "processing_settings",
                                                                                                                           
                                                                                                                           shiny::tabsetPanel(type = "tabs", id = "focalparamsummary_processingsettings_tabs",
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Post-processing settings", value = "log_processing",
                                                                                                                                                              shiny::h6(""),
                                                                                                                                                              
                                                                                                                                                              shiny::checkboxInput(inputId = "focalparamsummary_logcombinereplicates", label = "Combine replicates for all log files", value = F),
                                                                                                                                                              
                                                                                                                                                              shiny::uiOutput(outputId = "focalparamsummary_logprocessing_ui")
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Edit figure or table", value = "figuresettings_specific",
                                                                                                                                                              
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              shiny::uiOutput(outputId = "focalparamsummary_priormodelname_ui"),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "focalparamsummary_par_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparamsummary_par_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Margin edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'focalparamsummary_par_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_margin_bottom", label = "Bottom margin (in inches) of the plot", value = 0.4, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_margin_left", label = "Left margin  (in inches) of the plot", value = 0.45, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_margin_top", label = "Top margin (in inches) of the plot", value = 0.15, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_margin_right", label = "Right margin (in inches) of the plot", value = 0, min = 0)
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "focalparamsummary_xaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparamsummary_xaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>X-axis edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'focalparamsummary_xaxis_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_xaxis_lab_cex", label = "Cex of prior model name label", value = 1.35, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_xaxis_lab_line", label = "Line of prior model name label", value = -2),
                                                                                                                                                                         
                                                                                                                                                                         shiny::textInput(inputId = "focalparamsummary_plot_x_lab", label = "X-axis name", value = "number of data clones"),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_x_lab_cex", label = "Cex of X-axis name", value = 1.6, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_x_lab_line", label = "Line of X-axis name", value = 0.75),
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_xaxis_boxgroup_labcex", label = "Cex of clone number label", value = 1.1, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_xaxis_boxgroup_labline", label = "Line of clone number label", value = -2)
                                                                                                                                                                         
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "focalparamsummary_yaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparamsummary_yaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Y-axis edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'focalparamsummary_yaxis_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::textInput(inputId = "focalparamsummary_plot_y_lab", label = "Y-axis name", value = ""),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_y_lab_cex", label = "Cex of Y-axis name", value = 1.6, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_y_lab_line", label = "Line of Y-axis name", value = 0.9),
                                                                                                                                                                         
                                                                                                                                                                         shiny::checkboxInput(inputId = "focalparamsummary_plot_yaxis_log", label = "Y-axis on the log scale", value = F),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_yaxis_lab_cex", label = "Cex of Y-axis label", value = 0.75, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparamsummary_plot_yaxis_lab_line", label = "Line of Y-axis label", value = -1.5)
                                                                                                                                                              )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                           )
                                                                                                                           
                                                                                                           ),
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 3: Download Output", value = "download_output",
                                                                                                                           shiny::h6(''),
                                                                                                                           shiny::p("Download figure", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                           shiny::radioButtons("focalparamsummary_plot_downloadformat", label = "Format of the figure", choices = c("PDF", "EPS", "PNG", "JPEG", "TIFF"), selected = "PDF", inline = T),
                                                                                                                           shiny::textInput(inputId = "focalparamsummary_plot_downloadname", label = "Name of the figure (without filename extension, e.g., '.pdf', as it will be appended automatically)", value = "figure"),
                                                                                                                           shiny::downloadButton(outputId = "focalparamsummary_plot_download", label = "Download"),
                                                                                                                           
                                                                                                                           shiny::h4(''),
                                                                                                                           shiny::p("Download table", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                           shiny::radioButtons("focalparamsummary_table_downloadformat", label = "Format of the table", choices = c("TSV", "CSV"), selected = "TSV", inline = T),
                                                                                                                           shiny::textInput(inputId = "focalparamsummary_table_downloadname", label = "Name of the table (without filename extension, e.g., '.tsv', as it will be appended automatically)", value = "table"),
                                                                                                                           shiny::downloadButton(outputId = "focalparamsummary_table_download", label = "Download")
                                                                                                           )
                                                                                                           
                                                                                       )
                                                                      )
                                                        ),
                                                        
                                                        shiny::column(id = "focalparamsummary_result_column", width = 8, offset = 0, 
                                                                      style = "padding: 0px 15px 0px 15px; margin: 0%;", 
                                                                      
                                                                      shiny::fluidRow(id = "focalparamsummary_result_div",
                                                                                      
                                                                                      shiny::tabsetPanel(type = "tabs", id = "focalparamsummary_result_tabs",
                                                                                                         
                                                                                                         shiny::tabPanel(title = "Figure", value = "figure",
                                                                                                                         shiny::plotOutput(outputId = "focalparamsummary_plot", height = "auto")
                                                                                                         ),
                                                                                                         
                                                                                                         shiny::tabPanel(title = "Table", value = "table",
                                                                                                                         shiny::h6(''),
                                                                                                                         DT::dataTableOutput(outputId = "focalparamsummary_table")
                                                                                                                         
                                                                                                         )
                                                                                                         
                                                                                      )
                                                                      )
                                                        )
                                                      )
                                      ),
                                      
                                      
                                      shiny::tabPanel(title = "Robust Bayesian", value = "focalparam2_summary",
                                                      shiny::fluidRow(
                                                        shiny::column(width = 4, style = "padding-left: 10px; padding-right: 10px; overflow-y:auto; max-height: 800px;",
                                                                      
                                                                      shiny::wellPanel(style = "background-color: #F2F3F4;",
                                                                                       
                                                                                       shiny::navlistPanel(id = "focalparam2summary_tabs", well = F, widths = c(4, 8), 
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 1: Upload Log File(s)", value = "log_input",
                                                                                                                           shiny::h6(""),
                                                                                                                           
                                                                                                                           shiny::checkboxInput(inputId = "focalparam2summary_logfile_defaultupload", label = "Load example log files", value = F),
                                                                                                                           
                                                                                                                           shiny::div(id = paste0("focalparam2summary_logfile_div", 1),
                                                                                                                                      shiny::fileInput(inputId = paste0("focalparam2summary_logfile", 1), 
                                                                                                                                                       label = paste0("Estimate log file(s) under prior No. ", 1), 
                                                                                                                                                       multiple = T, accept = ".log")
                                                                                                                           ),
                                                                                                                           
                                                                                                                           shiny::div(id = "focalparam2summary_loginput_addremove_div",
                                                                                                                                      shiny::p("Add or remove prior model", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                      shiny::actionButton(inputId = "focalparam2summary_loginput_add", label = "", icon = shiny::icon("plus", lib = "glyphicon")),
                                                                                                                                      shiny::actionButton(inputId = "focalparam2summary_loginput_remove", label = "", icon = shiny::icon("minus", lib = "glyphicon"))
                                                                                                                           ),
                                                                                                                           
                                                                                                                           shiny::uiOutput(outputId = "focalparam2summary_paramname_ui"),
                                                                                                                           shiny::actionButton(inputId = "focalparam2summary_startprocessing", label = "Read in files and start initial processing")
                                                                                                           ),
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 2: Configure Post-Processing Settings", value = "processing_settings",
                                                                                                                           
                                                                                                                           shiny::tabsetPanel(type = "tabs", id = "focalparam2summary_processingsettings_tabs",
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Post-processing settings", value = "log_processing",
                                                                                                                                                              shiny::h6(""),
                                                                                                                                                              
                                                                                                                                                              shiny::checkboxInput(inputId = "focalparam2summary_logcombinereplicates", label = "Combine replicates for all log files", value = F),
                                                                                                                                                              
                                                                                                                                                              shiny::uiOutput(outputId = "focalparam2summary_logprocessing_ui")
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Edit figure or table", value = "figuresettings_specific",
                                                                                                                                                              
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              shiny::uiOutput(outputId = "focalparam2summary_priormodelname_ui"),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "focalparam2summary_par_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparam2summary_par_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Margin edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'focalparam2summary_par_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_margin_bottom", label = "Bottom margin (in inches) of the plot", value = 0.25, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_margin_left", label = "Left margin  (in inches) of the plot", value = 0.45, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_margin_top", label = "Top margin (in inches) of the plot", value = 0.15, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_margin_right", label = "Right margin (in inches) of the plot", value = 0, min = 0)
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "focalparam2summary_xaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparam2summary_xaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>X-axis edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'focalparam2summary_xaxis_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_xaxis_lab_cex", label = "Cex of prior model name label", value = 1.35, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_xaxis_lab_line", label = "Line of prior model name label", value = -2),
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_xaxis_boxgroup_labcex", label = "Cex of prior or posterior label", value = 1.1, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_xaxis_boxgroup_labline", label = "Line of prior or posterior label", value = -2)
                                                                                                                                                                         
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "focalparam2summary_yaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#focalparam2summary_yaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Y-axis edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'focalparam2summary_yaxis_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::textInput(inputId = "focalparam2summary_plot_y_lab", label = "Y-axis name", value = ""),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_y_lab_cex", label = "Cex of Y-axis name", value = 1.6, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_y_lab_line", label = "Line of Y-axis name", value = 0.9),
                                                                                                                                                                         
                                                                                                                                                                         shiny::checkboxInput(inputId = "focalparam2summary_plot_yaxis_log", label = "Y-axis on the log scale", value = F),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_yaxis_lab_cex", label = "Cex of Y-axis label", value = 0.75, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "focalparam2summary_plot_yaxis_lab_line", label = "Line of Y-axis label", value = -1.5)
                                                                                                                                                              )
                                                                                                                                              )
                                                                                                                           )
                                                                                                                           
                                                                                                           ),
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 3: Download Output", value = "download_output",
                                                                                                                           shiny::h6(''),
                                                                                                                           shiny::p("Download figure", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                           shiny::radioButtons("focalparam2summary_plot_downloadformat", label = "Format of the figure", choices = c("PDF", "EPS", "PNG", "JPEG", "TIFF"), selected = "PDF", inline = T),
                                                                                                                           shiny::textInput(inputId = "focalparam2summary_plot_downloadname", label = "Name of the figure (without filename extension, e.g., '.pdf', as it will be appended automatically)", value = "figure"),
                                                                                                                           shiny::downloadButton(outputId = "focalparam2summary_plot_download", label = "Download"),
                                                                                                                           
                                                                                                                           shiny::h4(''),
                                                                                                                           shiny::p("Download table", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                           shiny::radioButtons("focalparam2summary_table_downloadformat", label = "Format of the table", choices = c("TSV", "CSV"), selected = "TSV", inline = T),
                                                                                                                           shiny::textInput(inputId = "focalparam2summary_table_downloadname", label = "Name of the table (without filename extension, e.g., '.tsv', as it will be appended automatically)", value = "table"),
                                                                                                                           shiny::downloadButton(outputId = "focalparam2summary_table_download", label = "Download")
                                                                                                           )
                                                                                                           
                                                                                       )
                                                                      )
                                                        ),
                                                        
                                                        shiny::column(id = "focalparam2summary_result_column", width = 8, offset = 0, 
                                                                      style = "padding: 0px 15px 0px 15px; margin: 0%;", 
                                                                      
                                                                      shiny::fluidRow(id = "focalparam2summary_result_div",
                                                                                      
                                                                                      shiny::tabsetPanel(type = "tabs", id = "focalparam2summary_result_tabs",
                                                                                                         
                                                                                                         shiny::tabPanel(title = "Figure", value = "figure",
                                                                                                                         shiny::plotOutput(outputId = "focalparam2summary_plot", height = "auto")
                                                                                                         ),
                                                                                                         
                                                                                                         shiny::tabPanel(title = "Table", value = "table",
                                                                                                                         shiny::h6(''),
                                                                                                                         DT::dataTableOutput(outputId = "focalparam2summary_table")
                                                                                                                         
                                                                                                         )
                                                                                                         
                                                                                      )
                                                                      )
                                                        )
                                                      )
                                      ),
                                      
                                      
                                      shiny::tabPanel(title = "Posterior-Predictive Checking", value = "posterior_predictive",
                                                      shiny::fluidRow(
                                                        shiny::column(width = 4, style = "padding-left: 10px; padding-right: 10px; overflow-y:auto; max-height: 800px;",
                                                                      
                                                                      shiny::wellPanel(style = "background-color: #F2F3F4;",
                                                                                       
                                                                                       shiny::navlistPanel(id = "posteriorpredictive_tabs", well = F, widths = c(4, 8), 
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 1: Upload Files and Setup Simulations", value = "log_input",
                                                                                                                           
                                                                                                                           shiny::tabsetPanel(type = "tabs", id = "posteriorpredictive_input_tabs",
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Discrete-Geography File", value = "trait",
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              
                                                                                                                                                              shiny::checkboxInput(inputId = "posteriorpredictive_geographyfile_defaultupload", label = "Load example discrete-geography file", value = F),
                                                                                                                                                              
                                                                                                                                                              shiny::div(id = "posteriorpredictive_geographyfile_div", 
                                                                                                                                                                         shiny::fileInput("posteriorpredictive_geographyfile", "Choose discrete-geography file", multiple = F, 
                                                                                                                                                                                          accept = c("text/csv","text/comma-separated-values,text/plain",".csv", ".txt"))
                                                                                                                                                              ),
                                                                                                                                                              shiny::div(id = "posteriorpredictive_geographyfile_attributes",
                                                                                                                                                                         shiny::checkboxInput(inputId = "posteriorpredictive_geographyfile_header", label = "header", value = T),
                                                                                                                                                                         shiny::selectInput(inputId = "posteriorpredictive_geographyfile_taxoncolumn_name", label = "Taxon column name", choices = "", selectize = F),
                                                                                                                                                                         shiny::selectInput(inputId = "posteriorpredictive_geographyfile_traitcolumn_name", label = "Discrete-geography column name", choices = "", selectize = F)
                                                                                                                                                              ),
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Log and Tree Files", value = "logtree",
                                                                                                                                                              
                                                                                                                                                              shiny::checkboxInput(inputId = "posteriorpredictive_logfile_defaultupload", label = "Load example log and tree files", value = F),
                                                                                                                                                              
                                                                                                                                                              shiny::div(id = paste0("posteriorpredictive_logfile_div", 1),
                                                                                                                                                                         shiny::h6(''),
                                                                                                                                                                         shiny::p(paste0("Upload estimate files under prior No. ", 1), align = "center", 
                                                                                                                                                                                  style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                                                         shiny::fileInput(inputId = paste0("posteriorpredictive_logfile", 1), 
                                                                                                                                                                                          label = "log file(s)", 
                                                                                                                                                                                          multiple = T, accept = ".log"),
                                                                                                                                                                         shiny::fileInput(inputId = paste0("posteriorpredictive_treefile", 1), 
                                                                                                                                                                                          label = "tree file(s)",
                                                                                                                                                                                          multiple = T, accept = c(".trees", ".tree", ".tre"))
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::div(id = "posteriorpredictive_loginput_addremove_div",
                                                                                                                                                                         shiny::p("Add or remove prior model", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                                                         shiny::actionButton(inputId = "posteriorpredictive_loginput_add", label = "", icon = shiny::icon("plus", lib = "glyphicon")),
                                                                                                                                                                         shiny::actionButton(inputId = "posteriorpredictive_loginput_remove", label = "", icon = shiny::icon("minus", lib = "glyphicon"))
                                                                                                                                                              )
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Perform Simulations", value = "startsimulations",
                                                                                                                                                              shiny::checkboxInput(inputId = "posteriorpredictive_simulateall",
                                                                                                                                                                                   label = "Perform one simulation for each sample", value = T),
                                                                                                                                                              shiny::div(id = "posteriorpredictive_simulatenumber_div",
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_simulatenumber",
                                                                                                                                                                                             label = "Number of simulations",
                                                                                                                                                                                             value = 100, min = 1, step = 1),
                                                                                                                                                                         shiny::p("*sampling with replacement so this number can be greater than the number of samples", style = "font-size:90%; margin-top:-1em;")
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::h5(''),
                                                                                                                                                              shiny::actionButton(inputId = "posteriorpredictive_startprocessing", label = "Start posterior-predictive simulation")
                                                                                                                                              )
                                                                                                                           )
                                                                                                           ),
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 2: Summarize Posterior-Predictive Statistics", value = "processing_settings",
                                                                                                                           
                                                                                                                           shiny::tabsetPanel(type = "tabs", id = "posteriorpredictive_processingsettings_tabs",
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Post-processing settings", value = "log_processing",
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              
                                                                                                                                                              shiny::radioButtons(inputId = "posteriorpredictive_teststatistic", label = "Choose the summary statistic to plot", 
                                                                                                                                                                                  choices = c("Parsimony", "Tip-wise multinomial"), selected = "Parsimony", inline = F),
                                                                                                                                                              
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              shiny::checkboxInput(inputId = "posteriorpredictive_logcombinereplicates", label = "Combine replicates for all log files", value = F),
                                                                                                                                                              
                                                                                                                                                              shiny::uiOutput(outputId = "posteriorpredictive_logprocessing_ui")
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Edit figure or table", value = "figuresettings_specific",
                                                                                                                                                              
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              shiny::uiOutput(outputId = "posteriorpredictive_priormodelname_ui"),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "posteriorpredictive_par_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#posteriorpredictive_par_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Margin edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'posteriorpredictive_par_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_margin_bottom", label = "Bottom margin (in inches) of the plot", value = 0.05, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_margin_left", label = "Left margin  (in inches) of the plot", value = 0.45, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_margin_top", label = "Top margin (in inches) of the plot", value = 0.15, min = 0),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_margin_right", label = "Right margin (in inches) of the plot", value = 0, min = 0)
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "posteriorpredictive_xaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#posteriorpredictive_xaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>X-axis edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'posteriorpredictive_xaxis_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_xaxis_lab_cex", label = "Cex of prior model name label", value = 1.35, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_xaxis_lab_line", label = "Line of prior model name label", value = -2)
                                                                                                                                                              ),
                                                                                                                                                              
                                                                                                                                                              shiny::HTML('<div id = "posteriorpredictive_yaxis_edits_panel" class = "panel-heading"><p style = "font-size: 95%; font-weight: bold;" align="center" class="panel-title"><a data-toggle="collapse" href="#posteriorpredictive_yaxis_edits"><span class="glyphicon glyphicon-chevron-up" aria-hidden="true"></span>Y-axis edits</a></p></div>'),
                                                                                                                                                              shiny::div(id = 'posteriorpredictive_yaxis_edits', class = "panel-collapse collapse in",
                                                                                                                                                                         
                                                                                                                                                                         shiny::textInput(inputId = "posteriorpredictive_plot_y_lab", label = "Y-axis name", value = ""),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_y_lab_cex", label = "Cex of Y-axis name", value = 1.6, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_y_lab_line", label = "Line of Y-axis name", value = 0.9),
                                                                                                                                                                         
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_yaxis_lab_cex", label = "Cex of Y-axis label", value = 0.75, min = 0.1),
                                                                                                                                                                         shiny::numericInput(inputId = "posteriorpredictive_plot_yaxis_lab_line", label = "Line of Y-axis label", value = -1.5)
                                                                                                                                                              )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                           )
                                                                                                                           
                                                                                                           ),
                                                                                                           
                                                                                                           shiny::tabPanel(title = "Step 3: Download Output", value = "download_output",
                                                                                                                           
                                                                                                                           shiny::tabsetPanel(type = "tabs", id = "posteriorpredictive_downloadoutput_tabs",
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Figure and table", value = "figuretable",
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              shiny::p("Download figure", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                                              shiny::radioButtons("posteriorpredictive_plot_downloadformat", label = "Format of the figure", choices = c("PDF", "EPS", "PNG", "JPEG", "TIFF"), selected = "PDF", inline = T),
                                                                                                                                                              shiny::textInput(inputId = "posteriorpredictive_plot_downloadname", label = "Name of the figure (without filename extension, e.g., '.pdf', as it will be appended automatically)", value = "figure"),
                                                                                                                                                              shiny::downloadButton(outputId = "posteriorpredictive_plot_download", label = "Download"),
                                                                                                                                                              
                                                                                                                                                              shiny::h4(''),
                                                                                                                                                              shiny::p("Download table", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                                              shiny::radioButtons("posteriorpredictive_table_downloadformat", label = "Format of the table", choices = c("TSV", "CSV"), selected = "TSV", inline = T),
                                                                                                                                                              shiny::textInput(inputId = "posteriorpredictive_table_downloadname", label = "Name of the table (without filename extension, e.g., '.tsv', as it will be appended automatically)", value = "table"),
                                                                                                                                                              shiny::downloadButton(outputId = "posteriorpredictive_table_download", label = "Download")
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              shiny::tabPanel(title = "Simulated dataset(s)", value = "simdata",
                                                                                                                                                              shiny::h6(''),
                                                                                                                                                              shiny::p("Download simulated dataset(s)", align = "center", style = "font-size: 95%; font-weight: bold;"),
                                                                                                                                                              shiny::textInput(inputId = "posteriorpredictive_simdata_downloadname", label = "Name of the simulated data file (without filename extension, e.g., '.tsv' or '.zip', as it will be appended automatically)", value = "simulated"),
                                                                                                                                                              shiny::downloadButton(outputId = "posteriorpredictive_simdata_download", label = "Download")
                                                                                                                                              )
                                                                                                                           )
                                                                                                           )
                                                                                       )
                                                                      )
                                                        ),
                                                        
                                                        shiny::column(id = "posteriorpredictive_result_column", width = 8, offset = 0, 
                                                                      style = "padding: 0px 15px 0px 15px; margin: 0%;", 
                                                                      
                                                                      shiny::fluidRow(id = "posteriorpredictive_result_div",
                                                                                      
                                                                                      shiny::tabsetPanel(type = "tabs", id = "posteriorpredictive_result_tabs",
                                                                                                         
                                                                                                         shiny::tabPanel(title = "Figure", value = "figure",
                                                                                                                         shiny::plotOutput(outputId = "posteriorpredictive_plot", height = "auto")
                                                                                                         ),
                                                                                                         
                                                                                                         shiny::tabPanel(title = "Table", value = "table",
                                                                                                                         shiny::h6(''),
                                                                                                                         DT::dataTableOutput(outputId = "posteriorpredictive_table")
                                                                                                                         
                                                                                                         )
                                                                                                         
                                                                                      )
                                                                      )
                                                        )
                                                      )
                                      )
                    ),
                    
                    shiny::tabPanel(title = "About", value = "about",
                                    shiny::fluidRow(
                                      shiny::includeMarkdown(paste0(system.file("extdata/", package = "prioritree"), "/tool_input/about.md"))
                                    ))
  )
)
