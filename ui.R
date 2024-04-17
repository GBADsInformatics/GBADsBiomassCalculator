# Set-up and Header -------------------------------------------------------
fluidPage(
  includeCSS("styles.css"),
  use_googlefont("Raleway"),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = "'Raleway'"
    )
  )
  ),
  
  setBackgroundColor(
    color = c("#ffffff")
  ),
  
  navbarPage("", id = "nav",
             tags$head(
               tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    "))
             ),
             # Country Select Page -----------------------------------------------------
             tabPanel("landing",
                      column(12, 
                             style = "padding-top:150px"
                      ),
                      
                      # Logo --------------------------------------------------------------------
                      column(6, offset = 3, 
                             HTML('<center><img src="logo.png" width="350"></center>'),
                             titleAndHelp("landing"),
                             align = "center"
                      ),
                      column(12,
                             div(br(),
                                 h5("Select you biomass estimation method."),
                                 actionBttn("pntMethod", "Point estimation using TLUs", style = "pill", color = "warning"),
                                 br(),br(),
                                 actionBttn("distMethod", "GBADs Stochastic estimation", style = "pill", color = "warning"),
                               align = "center"
                             )
                      )
             ),
             
             
             tabPanel("pointPage",
                      column(1, offset = 1, 
                             style = "padding-top:20px",
                             actionBttn("backPnt",
                                        icon("chevron-left"),
                                        style = "pill",
                                        color = "warning",
                                        size = "sm")
                      ),
                      column(6, offset = 1, 
                             HTML('<center><img src="logo.png" width="200"></center>'),
                             titleAndHelp("pnt"),
                             uiOutput("pntTluOutput"),
                             uiOutput("pntBiomassOutput"),
                             br(),hr(),
                             align = "center"
                      ),
                      column(6, offset = 3, 
                             uiOutput("dynamicInputs"),
                             align = "center"
                      ),
                      column(1, offset = 9, 
                             actionButton("pntSubmit", "Enter", style = "border-radius: 100px; background-color: #F59031; color: white; width: 75px;")
                      ),
                      column(12,
                             column(6,
                                    div(
                                      actionButton("subSpecies", "Remove Last Species", icon = icon("minus"), style = "font-size:10px; border-radius: 100px"),
                                      align = "right"
                                    )
                             ),
                             column(6,
                                    div(
                                      actionButton("addSpecies", "Add Another Species", icon = icon("plus"), style = "font-size:10px; border-radius: 100px"),
                                      align = "left"
                                    )
                             ),
                             div(style = "height:200px")
                      )
             ),
             
             
             
             tabPanel("distributionPage",
                      column(1, offset = 1, 
                             style = "padding-top:20px",
                             actionBttn("backDist",
                                        icon("chevron-left"),
                                        style = "pill",
                                        color = "warning",
                                        size = "sm")
                      ),
                      column(6, offset = 1, 
                             HTML('<center><img src="logo.png" width="200"></center>'),
                             titleAndHelp("dist"),
                             uiOutput("dstBiomassOutput"),
                             br(),hr(),
                             align = "center"
                      ),
                      column(1, offset = 1, 
                             br(),
                             dropdownButton(
                               tagList(
                                 tags$span("Distribution Type"),
                                 tags$span(
                                   popify(
                                     icon("info-circle", verify_fa = FALSE),
                                     "Distribution Type",
                                     paste(
                                       "Changing the distribution type allows you to more accurately represent the natural variation in the real-world. By default the Pert distribution is used."
                                     )
                                   )
                                 )
                               ),
                               selectInput("distType", label = NULL, choices = c("Pert"), selected = "Pert"),
                               tagList(
                                 tags$span("Sample Size"),
                                 tags$span(
                                   popify(
                                     icon("info-circle", verify_fa = FALSE),
                                     "Sample Size",
                                     paste(
                                       "Adjusting the sample size allows you to determine how many times the liveweight distribution is sampled. More samples will result in an estimate closer to the mode."
                                     )
                                   )
                                 )
                               ),
                               numericInput("distRuns", label = NULL, min = 1, max = 100000, step = 1000, value = 1000), 
                               tagList(
                                 tags$span("Set Seed"),
                                 tags$span(
                                   popify(
                                     icon("info-circle", verify_fa = FALSE),
                                     "Set Seed",
                                     paste(
                                       "Setting the seed makes sure that when you run simulations, you get the same result every time, even though the process uses random sampling. This helps in checking your work and sharing it with others."
                                     )
                                   )
                                 )
                               ),
                               numericInput("distSeed", label =  NULL, min = 1, max = 1000, step = 1, value = 123), 
                               hr(),
                               selectInput("popProfile", "Population Profile", choices = c("Ethiopia - Cattle")),
                               selectInput("lwProfile", "Liveweight Profile", choices = c("Ethiopia - Cattle")),
                               actionBttn("profileSubmit", "Use Profiles", style = "pill", size = "xs", color = "warning"),
                               br(),
                               status = "default",
                               icon = icon("gear"), size = "s",
                               tooltip = tooltipOptions(title = "Additional controls"),
                               right = T
                             )
                      ),
                      column(5, offset = 1, 
                             div(
                               column(8,h4("Female Intact", style = "margin-bottom: -20px")),
                               column(4,h4("Weight", style = "margin-bottom: -20px")),
                               style = "margin-bottom: 31px"
                             ),
                             hr(),
                             m3_input("Juvenile", "f", "juv", "int"),
                             m3_input("Sub-Adult", "f", "sa", "int"),
                             m3_input("Adult", "f", "ad", "int"),
                             br(),
                             div(
                               column(7,h4("Female Castrated", style = "margin-bottom: -20px")),
                               column(5,h4("Weight", style = "margin-bottom: -20px")),
                               style = "margin-bottom: 31px"
                             ),
                             hr(),
                             m3_input("Juvenile", "f", "juv", "fix"),
                             m3_input("Sub-Adult", "f", "sa", "fix"),
                             m3_input("Adult", "f", "ad", "fix")
                      ),
                      column(5,
                             div(
                               column(7,h4("Male Intact", style = "margin-bottom: -20px")),
                               column(5,h4("Weight", style = "margin-bottom: -20px")),
                               style = "margin-bottom: 31px"
                             ),
                             hr(),
                             m3_input("Juvenile", "m", "juv", "int"),
                             m3_input("Sub-Adult", "m", "sa", "int"), 
                             m3_input("Adult", "m", "ad", "int"),
                             br(),
                             div(
                               column(7,h4("Male Castrated", style = "margin-bottom: -20px")),
                               column(5,h4("Weight", style = "margin-bottom: -20px")),
                               style = "margin-bottom: 31px"
                             ),
                             hr(),
                             m3_input("Juvenile", "m", "juv", "fix"),
                             m3_input("Sub-Adult", "m", "sa", "fix"),
                             m3_input("Adult", "m", "ad", "fix")
                      ),
                      fluidRow(
                        column(10, offset = 1, hr(),
                               div(
                               div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                                   fileInput("uploadJson", NULL, multiple = FALSE, accept = ".json", placeholder = "Upload Profile")
                               ),
                               div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                                   downloadButton("downloadJson", "", style = "border-radius: 100px; width: 75px;")
                               ),
                               div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                                   actionButton("dstClear", "Clear", style = "border-radius: 100px; width: 75px;")
                               ),
                               div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                                   actionButton("distSubmit", "Enter", style = "border-radius: 100px; background-color: #F59031; color: white; width: 75px;")
                               ),
                               style = "float: right;"
                               ),
                               br(), br()
                        )
                      )
                      
  )
)
)

