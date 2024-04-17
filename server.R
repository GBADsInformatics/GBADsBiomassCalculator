function(input, output, session) {
  

# App Navigation ----------------------------------------------------------

  observeEvent(input$pntMethod, {
    updateTabsetPanel(session, "nav", selected = "pointPage")
  })
  observeEvent(input$distMethod, {
    updateTabsetPanel(session, "nav", selected = "distributionPage")
  })
  observeEvent(input$backPnt, {
    updateTabsetPanel(session, "nav", selected = "landing")
  })
  observeEvent(input$backDist, {
    updateTabsetPanel(session, "nav", selected = "landing")
  })
  

# Information Modal -------------------------------------------------------
  observeEvent(input$landingHelp, {
    showModal(modalDialog(
      title = "GBADs Biomass Calculator",
      includeMarkdown("helpText.Rmd"),
      align = "center",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$pntHelp, {
    showModal(modalDialog(
      title = "GBADs Biomass Calculator",
      includeMarkdown("helpText.Rmd"),
      align = "center",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$distHelp, {
    showModal(modalDialog(
      title = "GBADs Biomass Calculator",
      includeMarkdown("helpText.Rmd"),
      align = "center",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  

# Point Method ------------------------------------------------------------

## Dynamically Add/Remove Rows in UI ---------------------------------------

  inputGroups <- reactiveValues(count = 1)
  inputValues <- reactiveValues(data = list())
  
  observeEvent(input$addSpecies, {
    if(inputGroups$count < 12) {
    inputGroups$count <- inputGroups$count + 1
    }
  })
  
  observeEvent(input$subSpecies, {
    if(inputGroups$count > 1) {
      isolate({
        inputValues$data <- inputValues$data[1:(inputGroups$count - 1)]
      })
      inputGroups$count <- inputGroups$count - 1
    }
  })
  
  observe({
    for (i in 1:inputGroups$count) {
      isolate({
        inputValues$data[[paste0("pntPop", i)]] <- input[[paste0("pntPop", i)]]
        inputValues$data[[paste0("pntSpecies", i)]] <- input[[paste0("pntSpecies", i)]]
      })
    }
  })
  
  output$dynamicInputs <- renderUI({
    inputList <- lapply(1:inputGroups$count, function(i) {
      popValue <- if (!is.null(inputValues$data[[paste0("pntPop", i)]])) inputValues$data[[paste0("pntPop", i)]] else NULL
      speciesValue <- if (!is.null(inputValues$data[[paste0("pntSpecies", i)]])) inputValues$data[[paste0("pntSpecies", i)]] else NULL
      
      if (i == 1) {
        fluidRow(
          column(4, numericInput(paste0("pntPop", i), "Population:", min = 0, max = 10000000, step = 100000, value = popValue)),
          column(4, selectInput(paste0("pntSpecies", i), "Select Species:", choices = unique(ratios$item), selected = speciesValue)),
          column(4, uiOutput(paste0("pntSingle",i), style = "padding-top:24px"))
        )
      } else {
        fluidRow(
          column(4, numericInput(paste0("pntPop", i), label = NULL, min = 0, max = 10000000, step = 100000, value = popValue)),
          column(4, selectInput(paste0("pntSpecies", i), label = NULL, choices = unique(ratios$item), selected = speciesValue)),
          column(4, uiOutput(paste0("pntSingle",i)))
        )
      }
    })
    do.call(tagList, inputList)
  })
  

## Calculations ------------------------------------------------------------
  speciesTLUs <- reactiveValues()
  speciesBM <- reactiveValues()
  speciesContribution <- reactiveValues(tlu = list())
  
  results <- eventReactive(input$pntSubmit, {
    
    speciesContribution$tlu <- list()
    
    totalTLU <- 0
    totalBiomass <- 0
    for (i in 1:inputGroups$count) {
      species <- input[[paste0("pntSpecies", i)]]
      population <- input[[paste0("pntPop", i)]]
      
      tlu <- NULL
      biomass <- NULL
      
      if (!is.null(species) && !is.null(population) && species != "" && !is.na(population) && population != "") {
        population <- as.numeric(population)
        tlu_ratio <- ifelse(species %in% ratios$item, ratios$tlu_ratio[ratios$item == species], 0)
        tlu <- population * tlu_ratio
        biomass <- tlu * 250
        
        totalTLU <- totalTLU + tlu
        totalBiomass <- totalBiomass + biomass
      }
      
      speciesTLUs[[paste0("tlu", i)]] <- tlu
      speciesBM[[paste0("bm", i)]] <- biomass
    
    if (!is.null(tlu) && tlu > 0) {
      if (!is.null(speciesContribution$tlu[[species]])) {
        speciesContribution$tlu[[species]] <- speciesContribution$tlu[[species]] + tlu
      } else {
        speciesContribution$tlu[[species]] <- tlu
      }
    }
  }
    
    list(TLU = totalTLU, Biomass = totalBiomass)
  }, ignoreNULL = TRUE)
  
  observe({ 
    for (i in 1:inputGroups$count) {
      local({
        my_i <- i
        output[[paste0("pntSingle", my_i)]] <- renderUI({
          tlu <- speciesTLUs[[paste0("tlu", my_i)]]
          bm <- speciesBM[[paste0("bm", my_i)]]
          if (!is.null(tlu) && !is.na(tlu) && tlu > 0) {
            h5(paste(format(round(tlu, 1), big.mark = ","), " TLU (", format(round(bm, 1), big.mark = ","), "kg)"), style = "color: #F7931D; font-family: helvetica")
          } else {
            NULL
          }
        })
      })
    }
  })

## Outputs -----------------------------------------------------------------

  output$pntTluOutput <- renderUI({
    if (!is.null(results())) {
      h4("Estimated Total TLU: ", paste(format(round(results()$TLU,1), big.mark = ",")), " Units", style = "color: #F7931D; font-family: helvetica;")
    } 
  })
  
  output$pntBiomassOutput <- renderUI({
    if (!is.null(results())) {
      tagList(
      h4("Estimated Total Biomass: ", paste(format(round(results()$Biomass,1), big.mark = ",")), " kg", "(", paste(format(results()$Biomass*2.205, big.mark = ",")), " lbs )", style = "color: #F7931D; font-family: helvetica"),
      br(),
      h4("Breakdown by species", style = "margin-bottom: -10px;position: relative; z-index:1000"),
      plotOutput("pntBarPlot", height = "300px")
      )
    }
  })
  
  output$pntBarPlot <- renderPlot({
    speciesTotals <- unlist(speciesContribution$tlu)
    if (length(speciesTotals) == 0) return(NULL)
    speciesData <- data.frame(Species = names(speciesTotals), TLU = speciesTotals)
    speciesData <- speciesData[order(-speciesData$TLU),]

    ggplot(speciesData, aes(x = Species, y = TLU * 250, fill = Species)) +
      geom_bar(stat = "identity", width = 0.4) +
      scale_fill_manual(values = rep(c("#F59031", "grey", "skyblue"),4))+
      labs(title = "", x = "", y = "Estimated Biomass (kg)") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            text = element_text(size= 16),
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            axis.text.y = element_text(family = "sans")) 
  })

# Distribution Method -------------------------------------------------------

## Update by Selected Profile -----------------------------------
  observeEvent(input$profileSubmit, {
    selected_profile <- input$popProfile
    
    if (!is.null(profiles[[selected_profile]])) {
      profile_data <- profiles[[selected_profile]]
      # Populations
      updateNumericInput(session, "f_juv_int_pop", value = profile_data[["Female Intact"]][["Juvenile"]][["Population"]])
      updateNumericInput(session, "f_sa_int_pop", value = profile_data[["Female Intact"]][["Sub-Adult"]][["Population"]])
      updateNumericInput(session, "f_ad_int_pop", value = profile_data[["Female Intact"]][["Adult"]][["Population"]])
      updateNumericInput(session, "f_juv_fix_pop", value = profile_data[["Female Castrated"]][["Juvenile"]][["Population"]])
      updateNumericInput(session, "f_sa_fix_pop", value = profile_data[["Female Castrated"]][["Sub-Adult"]][["Population"]])
      updateNumericInput(session, "f_ad_fix_pop", value = profile_data[["Female Castrated"]][["Adult"]][["Population"]])
      updateNumericInput(session, "m_juv_int_pop", value = profile_data[["Male Intact"]][["Juvenile"]][["Population"]])
      updateNumericInput(session, "m_sa_int_pop", value = profile_data[["Male Intact"]][["Sub-Adult"]][["Population"]])
      updateNumericInput(session, "m_ad_int_pop", value = profile_data[["Male Intact"]][["Adult"]][["Population"]])
      updateNumericInput(session, "m_juv_fix_pop", value = profile_data[["Male Castrated"]][["Juvenile"]][["Population"]])
      updateNumericInput(session, "m_sa_fix_pop", value = profile_data[["Male Castrated"]][["Sub-Adult"]][["Population"]])
      updateNumericInput(session, "m_ad_fix_pop", value = profile_data[["Male Castrated"]][["Adult"]][["Population"]])
      
      # Live Weight Min
      updateNumericInput(session, "f_juv_int_lw_min", value = profile_data[["Female Intact"]][["Juvenile"]][["Weight"]][["Min"]])
      updateNumericInput(session, "f_sa_int_lw_min", value = profile_data[["Female Intact"]][["Sub-Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "f_ad_int_lw_min", value = profile_data[["Female Intact"]][["Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "f_juv_fix_lw_min", value = profile_data[["Female Castrated"]][["Juvenile"]][["Weight"]][["Min"]])
      updateNumericInput(session, "f_sa_fix_lw_min", value = profile_data[["Female Castrated"]][["Sub-Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "f_ad_fix_lw_min", value = profile_data[["Female Castrated"]][["Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "m_juv_int_lw_min", value = profile_data[["Male Intact"]][["Juvenile"]][["Weight"]][["Min"]])
      updateNumericInput(session, "m_sa_int_lw_min", value = profile_data[["Male Intact"]][["Sub-Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "m_ad_int_lw_min", value = profile_data[["Male Intact"]][["Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "m_juv_fix_lw_min", value = profile_data[["Male Castrated"]][["Juvenile"]][["Weight"]][["Min"]])
      updateNumericInput(session, "m_sa_fix_lw_min", value = profile_data[["Male Castrated"]][["Sub-Adult"]][["Weight"]][["Min"]])
      updateNumericInput(session, "m_ad_fix_lw_min", value = profile_data[["Male Castrated"]][["Adult"]][["Weight"]][["Min"]])
      
      # Live Weight Max
      updateNumericInput(session, "f_juv_int_lw_max", value = profile_data[["Female Intact"]][["Juvenile"]][["Weight"]][["Max"]])
      updateNumericInput(session, "f_sa_int_lw_max", value = profile_data[["Female Intact"]][["Sub-Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "f_ad_int_lw_max", value = profile_data[["Female Intact"]][["Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "f_juv_fix_lw_max", value = profile_data[["Female Castrated"]][["Juvenile"]][["Weight"]][["Max"]])
      updateNumericInput(session, "f_sa_fix_lw_max", value = profile_data[["Female Castrated"]][["Sub-Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "f_ad_fix_lw_max", value = profile_data[["Female Castrated"]][["Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "m_juv_int_lw_max", value = profile_data[["Male Intact"]][["Juvenile"]][["Weight"]][["Max"]])
      updateNumericInput(session, "m_sa_int_lw_max", value = profile_data[["Male Intact"]][["Sub-Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "m_ad_int_lw_max", value = profile_data[["Male Intact"]][["Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "m_juv_fix_lw_max", value = profile_data[["Male Castrated"]][["Juvenile"]][["Weight"]][["Max"]])
      updateNumericInput(session, "m_sa_fix_lw_max", value = profile_data[["Male Castrated"]][["Sub-Adult"]][["Weight"]][["Max"]])
      updateNumericInput(session, "m_ad_fix_lw_max", value = profile_data[["Male Castrated"]][["Adult"]][["Weight"]][["Max"]])
      
      # Live Weight Mode
      updateNumericInput(session, "f_juv_int_lw_mode", value = profile_data[["Female Intact"]][["Juvenile"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "f_sa_int_lw_mode", value = profile_data[["Female Intact"]][["Sub-Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "f_ad_int_lw_mode", value = profile_data[["Female Intact"]][["Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "f_juv_fix_lw_mode", value = profile_data[["Female Castrated"]][["Juvenile"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "f_sa_fix_lw_mode", value = profile_data[["Female Castrated"]][["Sub-Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "f_ad_fix_lw_mode", value = profile_data[["Female Castrated"]][["Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "m_juv_int_lw_mode", value = profile_data[["Male Intact"]][["Juvenile"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "m_sa_int_lw_mode", value = profile_data[["Male Intact"]][["Sub-Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "m_ad_int_lw_mode", value = profile_data[["Male Intact"]][["Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "m_juv_fix_lw_mode", value = profile_data[["Male Castrated"]][["Juvenile"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "m_sa_fix_lw_mode", value = profile_data[["Male Castrated"]][["Sub-Adult"]][["Weight"]][["Mode"]])
      updateNumericInput(session, "m_ad_fix_lw_mode", value = profile_data[["Male Castrated"]][["Adult"]][["Weight"]][["Mode"]])

    }
  })  
  

# Clear Inputs ------------------------------------------------------------

  observeEvent(input$dstClear, {

      updateNumericInput(session, "f_juv_int_pop", value = "")
      updateNumericInput(session, "f_sa_int_pop", value = "")
      updateNumericInput(session, "f_ad_int_pop", value = "")
      updateNumericInput(session, "f_juv_fix_pop", value = "")
      updateNumericInput(session, "f_sa_fix_pop", value = "")
      updateNumericInput(session, "f_ad_fix_pop", value = "")
      updateNumericInput(session, "m_juv_int_pop", value = "")
      updateNumericInput(session, "m_sa_int_pop", value = "")
      updateNumericInput(session, "m_ad_int_pop", value = "")
      updateNumericInput(session, "m_juv_fix_pop", value = "")
      updateNumericInput(session, "m_sa_fix_pop", value = "")
      updateNumericInput(session, "m_ad_fix_pop", value = "")
      
      # Live Weight Min
      updateNumericInput(session, "f_juv_int_lw_min", value = "")
      updateNumericInput(session, "f_sa_int_lw_min", value = "")
      updateNumericInput(session, "f_ad_int_lw_min", value = "")
      updateNumericInput(session, "f_juv_fix_lw_min", value = "")
      updateNumericInput(session, "f_sa_fix_lw_min", value = "")
      updateNumericInput(session, "f_ad_fix_lw_min", value = "")
      updateNumericInput(session, "m_juv_int_lw_min", value = "")
      updateNumericInput(session, "m_sa_int_lw_min", value = "")
      updateNumericInput(session, "m_ad_int_lw_min", value = "")
      updateNumericInput(session, "m_juv_fix_lw_min", value = "")
      updateNumericInput(session, "m_sa_fix_lw_min", value = "")
      updateNumericInput(session, "m_ad_fix_lw_min", value = "")
      
      # Live Weight Max
      updateNumericInput(session, "f_juv_int_lw_max", value = "")
      updateNumericInput(session, "f_sa_int_lw_max", value = "")
      updateNumericInput(session, "f_ad_int_lw_max", value = "")
      updateNumericInput(session, "f_juv_fix_lw_max", value = "")
      updateNumericInput(session, "f_sa_fix_lw_max", value = "")
      updateNumericInput(session, "f_ad_fix_lw_max", value = "")
      updateNumericInput(session, "m_juv_int_lw_max", value = "")
      updateNumericInput(session, "m_sa_int_lw_max", value = "")
      updateNumericInput(session, "m_ad_int_lw_max", value = "")
      updateNumericInput(session, "m_juv_fix_lw_max", value = "")
      updateNumericInput(session, "m_sa_fix_lw_max", value = "")
      updateNumericInput(session, "m_ad_fix_lw_max", value = "")
      
      # Live Weight Mode
      updateNumericInput(session, "f_juv_int_lw_mode", value = "")
      updateNumericInput(session, "f_sa_int_lw_mode", value = "")
      updateNumericInput(session, "f_ad_int_lw_mode", value = "")
      updateNumericInput(session, "f_juv_fix_lw_mode", value = "")
      updateNumericInput(session, "f_sa_fix_lw_mode", value = "")
      updateNumericInput(session, "f_ad_fix_lw_mode", value = "")
      updateNumericInput(session, "m_juv_int_lw_mode", value = "")
      updateNumericInput(session, "m_sa_int_lw_mode", value = "")
      updateNumericInput(session, "m_ad_int_lw_mode", value = "")
      updateNumericInput(session, "m_juv_fix_lw_mode", value = "")
      updateNumericInput(session, "m_sa_fix_lw_mode", value = "")
      updateNumericInput(session, "m_ad_fix_lw_mode", value = "")
  })    
  

## Calculations ----------------------------------------
  distMethodResults <- reactiveValues()
  distMethodIcons <- reactiveValues()
  
  observeEvent(input$distSubmit, {
    variable_names <- c("f_juv_int", "f_sa_int", "f_ad_int", "f_juv_fix", "f_sa_fix", "f_ad_fix", "m_juv_int", "m_sa_int", "m_ad_int", "m_juv_fix", "m_sa_fix", "m_ad_fix")
    results <- list()
    icons <- list()
    set.seed(input$distSeed)
    
    for (var_name in variable_names) {
      
      min_param <- paste(var_name, "lw_min", sep = "_")
      max_param <- paste(var_name, "lw_max", sep = "_")
      mode_param <- paste(var_name, "lw_mode", sep = "_")
      pop_param <- paste(var_name, "pop", sep = "_")
      output_name <- paste(var_name, "check", sep = "_")
      
      pop_value <- ifelse(is.null(input[[pop_param]]) || input[[pop_param]] == "" || is.na(input[[pop_param]]), 0, input[[pop_param]])
      mode_value <- ifelse(is.null(input[[mode_param]]) || input[[mode_param]] == "" || is.na(input[[mode_param]]), 0, input[[mode_param]])
      
      min_missing_or_invalid <- is.null(input[[min_param]]) || input[[min_param]] == "" || is.na(input[[min_param]])
      max_missing_or_invalid <- is.null(input[[max_param]]) || input[[max_param]] == "" || is.na(input[[max_param]])
      
      if (min_missing_or_invalid || max_missing_or_invalid) {
        min_value <- mode_value
        max_value <- mode_value
      } else {
        min_value <- input[[min_param]]
        max_value <- input[[max_param]]
      }
      
      if(min_value <= mode_value && mode_value <= max_value){
        result <- mean(rpert(input$distRuns, x_min = min_value, x_max = max_value, x_mode = mode_value)) * pop_value
      } else {
        result <- 0
      }
      
      if (result != 0 && result != "" && !is.na(result) && !is.null(result)) {
        if (min_missing_or_invalid || max_missing_or_invalid) {
          icons[[var_name]] <- "partial"
        } else {
          icons[[var_name]] <- "full"
        }
      } else {
        icons[[var_name]] <- "not"
      }
      
      results[[var_name]] <- result
    }
    
    distMethodResults$results <- results
    distMethodIcons$icons <- icons
    
  })
  
  
  

## Outputs ---------------------------------------------
 
  output$dstBiomassOutput <- renderUI({
    if (!is.null(distMethodResults$results)) {
      
      results <- distMethodResults$results
      dstBiomassEstimate <- sum(unlist(results))
      
      tagList(
      h4("Estimated Total Biomass: ", paste(format(round(dstBiomassEstimate, 1), big.mark = ",")), " kg", "(", paste(format(round(dstBiomassEstimate * 2.205, 1), big.mark = ",")), " lbs )", style = "color: #F7931D; font-family: helvetica"),
      br(),
      h4("Breakdown by population sub-group", style = "margin-bottom: -10px;position: relative; z-index:1000"),
      plotOutput("distBarChart", height = "350px"),
      )
    }
  })
  
  output$distBarChart <- renderPlot({
    if (!is.null(distMethodResults$results)) {
     
       df <- data.frame(variable = names(distMethodResults$results), 
                       mass = unlist(distMethodResults$results))
      
      df <- df %>%
        mutate(gender = ifelse(grepl("^f", variable), "Female", "Male"),
               age_group = case_when(
                 grepl("juv", variable) ~ "Juvenile",
                 grepl("sa", variable) ~ "Sub Adult",
                 grepl("ad", variable) ~ "Adult"
               ),
               status = ifelse(grepl("int", variable), "Intact", "Castrated")) %>% 
        mutate(age_group = factor(age_group, levels = c("Adult", "Sub Adult", "Juvenile")))
      
      pal <- rep(c("#F59031", "grey", "skyblue"),2)
      
      ggplot(df, aes(x = age_group, y = mass, fill = interaction(age_group, status))) +
        geom_bar(stat = "identity") +
        facet_nested(~gender + status, scales = "free_x") +
        scale_fill_manual(values = pal, guide = FALSE) + 
        labs(title = "", x = "", y = "Estimated Biomass (kg)") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              text = element_text(size= 16),
              strip.text = element_text(size = 15, color = "grey20"),
              strip.background = element_rect(fill = "grey95", color = "grey95"),
              plot.title = element_text(hjust = 0.5),
              panel.grid.major.x = element_blank(),
              axis.text.y = element_text(family = "sans")) 
      
    }
  })
  
  
  observeEvent(input$distSubmit, {
    variable_names <- c("f_juv_int", "f_sa_int", "f_ad_int", "f_juv_fix", "f_sa_fix", "f_ad_fix", "m_juv_int", "m_sa_int", "m_ad_int", "m_juv_fix", "m_sa_fix", "m_ad_fix")
    for(var_name in variable_names) {
      output_name <- paste(var_name, "check", sep = "_")
      ###Use an Immediately Invoked Function Expression (IIFE) instead to prevent the last icon determining all icons.
      local({
        local_var_name <- var_name
        output[[output_name]] <- renderUI({
          decision <- distMethodIcons$icons[[local_var_name]]
          
          if (decision == "full") {
            tags$span(title = "Row included in calculation.", style = "color: green;", icon("check"))
          } else if (decision == "partial"){
            tags$span(title = "Row included in calculation, but only population and mode were used.", style = "color: #F59031;", icon("exclamation"))
          } else{
            tags$span(title = "Row not included in calculation due to missing information.", style = "color: red;", icon("x"))
          }
        })
      })
    }
  })
  

# Download Profiles -------------------------------------------------------
  profileData <- reactive({
    list(
    `Female Intact` = list(
      Juvenile = list(
        Population = input$f_juv_int_pop,
        Weight = list(
          Min = input$f_juv_int_lw_min,
          Max = input$f_juv_int_lw_max,
          Mode = input$f_juv_int_lw_mode
        )
      ),
      `Sub-Adult` = list(
        Population = input$f_sa_int_pop,
        Weight = list(
          Min = input$f_sa_int_lw_min,
          Max = input$f_sa_int_lw_max,
          Mode = input$f_sa_int_lw_mode
        )
      ),
      `Adult` = list(
        Population = input$f_ad_int_pop,
        Weight = list(
          Min = input$f_ad_int_lw_min,
          Max = input$f_ad_int_lw_max,
          Mode = input$f_ad_int_lw_mode
        )
      )
    ),
    `Female Castrated` = list(
      Juvenile = list(
        Population = input$f_juv_fix_pop,
        Weight = list(
          Min = input$f_juv_fix_lw_min,
          Max = input$f_juv_fix_lw_max,
          Mode = input$f_juv_fix_lw_mode
        )
      ),
      `Sub-Adult` = list(
        Population = input$f_sa_fix_pop,
        Weight = list(
          Min = input$f_sa_fix_lw_min,
          Max = input$f_sa_fix_lw_max,
          Mode = input$f_sa_fix_lw_mode
        )
      ),
      `Adult` = list(
        Population = input$f_ad_fix_pop,
        Weight = list(
          Min = input$f_ad_fix_lw_min,
          Max = input$f_ad_fix_lw_max,
          Mode = input$f_ad_fix_lw_mode
        )
      )
    ),
    `Male Intact` = list(
      Juvenile = list(
        Population = input$m_juv_int_pop,
        Weight = list(
          Min = input$m_juv_int_lw_min,
          Max = input$m_juv_int_lw_max,
          Mode = input$m_juv_int_lw_mode
        )
      ),
      `Sub-Adult` = list(
        Population = input$m_sa_int_pop,
        Weight = list(
          Min = input$m_sa_int_lw_min,
          Max = input$m_sa_int_lw_max,
          Mode = input$m_sa_int_lw_mode
        )
      ),
      `Adult` = list(
        Population = input$m_ad_int_pop,
        Weight = list(
          Min = input$m_ad_int_lw_min,
          Max = input$m_ad_int_lw_max,
          Mode = input$m_ad_int_lw_mode
        )
      )
    ),
    `Male Castrated` = list(
      Juvenile = list(
        Population = input$m_juv_fix_pop,
        Weight = list(
          Min = input$m_juv_fix_lw_min,
          Max = input$m_juv_fix_lw_max,
          Mode = input$m_juv_fix_lw_mode
        )
      ),
      `Sub-Adult` = list(
        Population = input$m_sa_fix_pop,
        Weight = list(
          Min = input$m_sa_fix_lw_min,
          Max = input$m_sa_fix_lw_max,
          Mode = input$m_sa_fix_lw_mode
        )
      ),
      `Adult` = list(
        Population = input$m_ad_fix_pop,
        Weight = list(
          Min = input$m_ad_fix_lw_min,
          Max = input$m_ad_fix_lw_max,
          Mode = input$m_ad_fix_lw_mode
        )
      )
    )
  )  
  })
  
  
  output$downloadJson <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-GBADsBiomassCalculatorCustomProfile.json")
    },
    content = function(file) {
      json_data <- toJSON(profileData(), pretty = TRUE)
      writeLines(json_data, file)
    }
  )
  

# Upload Profiles ---------------------------------------------------------

  observeEvent(input$uploadJson, {
    req(input$uploadJson)

    uploaded_json <- fromJSON(readLines(input$uploadJson$datapath))
    
    updateNumericInput(session, "f_juv_int_pop", value = uploaded_json[["Female Intact"]][["Juvenile"]][["Population"]])
    updateNumericInput(session, "f_sa_int_pop", value = uploaded_json[["Female Intact"]][["Sub-Adult"]][["Population"]])
    updateNumericInput(session, "f_ad_int_pop", value = uploaded_json[["Female Intact"]][["Adult"]][["Population"]])
    updateNumericInput(session, "f_juv_fix_pop", value = uploaded_json[["Female Castrated"]][["Juvenile"]][["Population"]])
    updateNumericInput(session, "f_sa_fix_pop", value = uploaded_json[["Female Castrated"]][["Sub-Adult"]][["Population"]])
    updateNumericInput(session, "f_ad_fix_pop", value = uploaded_json[["Female Castrated"]][["Adult"]][["Population"]])
    updateNumericInput(session, "m_juv_int_pop", value = uploaded_json[["Male Intact"]][["Juvenile"]][["Population"]])
    updateNumericInput(session, "m_sa_int_pop", value = uploaded_json[["Male Intact"]][["Sub-Adult"]][["Population"]])
    updateNumericInput(session, "m_ad_int_pop", value = uploaded_json[["Male Intact"]][["Adult"]][["Population"]])
    updateNumericInput(session, "m_juv_fix_pop", value = uploaded_json[["Male Castrated"]][["Juvenile"]][["Population"]])
    updateNumericInput(session, "m_sa_fix_pop", value = uploaded_json[["Male Castrated"]][["Sub-Adult"]][["Population"]])
    updateNumericInput(session, "m_ad_fix_pop", value = uploaded_json[["Male Castrated"]][["Adult"]][["Population"]])
    
    # Live Weight Min
    updateNumericInput(session, "f_juv_int_lw_min", value = uploaded_json[["Female Intact"]][["Juvenile"]][["Weight"]][["Min"]])
    updateNumericInput(session, "f_sa_int_lw_min", value = uploaded_json[["Female Intact"]][["Sub-Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "f_ad_int_lw_min", value = uploaded_json[["Female Intact"]][["Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "f_juv_fix_lw_min", value = uploaded_json[["Female Castrated"]][["Juvenile"]][["Weight"]][["Min"]])
    updateNumericInput(session, "f_sa_fix_lw_min", value = uploaded_json[["Female Castrated"]][["Sub-Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "f_ad_fix_lw_min", value = uploaded_json[["Female Castrated"]][["Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "m_juv_int_lw_min", value = uploaded_json[["Male Intact"]][["Juvenile"]][["Weight"]][["Min"]])
    updateNumericInput(session, "m_sa_int_lw_min", value = uploaded_json[["Male Intact"]][["Sub-Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "m_ad_int_lw_min", value = uploaded_json[["Male Intact"]][["Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "m_juv_fix_lw_min", value = uploaded_json[["Male Castrated"]][["Juvenile"]][["Weight"]][["Min"]])
    updateNumericInput(session, "m_sa_fix_lw_min", value = uploaded_json[["Male Castrated"]][["Sub-Adult"]][["Weight"]][["Min"]])
    updateNumericInput(session, "m_ad_fix_lw_min", value = uploaded_json[["Male Castrated"]][["Adult"]][["Weight"]][["Min"]])
    
    # Live Weight Max
    updateNumericInput(session, "f_juv_int_lw_max", value = uploaded_json[["Female Intact"]][["Juvenile"]][["Weight"]][["Max"]])
    updateNumericInput(session, "f_sa_int_lw_max", value = uploaded_json[["Female Intact"]][["Sub-Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "f_ad_int_lw_max", value = uploaded_json[["Female Intact"]][["Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "f_juv_fix_lw_max", value = uploaded_json[["Female Castrated"]][["Juvenile"]][["Weight"]][["Max"]])
    updateNumericInput(session, "f_sa_fix_lw_max", value = uploaded_json[["Female Castrated"]][["Sub-Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "f_ad_fix_lw_max", value = uploaded_json[["Female Castrated"]][["Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "m_juv_int_lw_max", value = uploaded_json[["Male Intact"]][["Juvenile"]][["Weight"]][["Max"]])
    updateNumericInput(session, "m_sa_int_lw_max", value = uploaded_json[["Male Intact"]][["Sub-Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "m_ad_int_lw_max", value = uploaded_json[["Male Intact"]][["Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "m_juv_fix_lw_max", value = uploaded_json[["Male Castrated"]][["Juvenile"]][["Weight"]][["Max"]])
    updateNumericInput(session, "m_sa_fix_lw_max", value = uploaded_json[["Male Castrated"]][["Sub-Adult"]][["Weight"]][["Max"]])
    updateNumericInput(session, "m_ad_fix_lw_max", value = uploaded_json[["Male Castrated"]][["Adult"]][["Weight"]][["Max"]])
    
    # Live Weight Mode
    updateNumericInput(session, "f_juv_int_lw_mode", value = uploaded_json[["Female Intact"]][["Juvenile"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "f_sa_int_lw_mode", value = uploaded_json[["Female Intact"]][["Sub-Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "f_ad_int_lw_mode", value = uploaded_json[["Female Intact"]][["Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "f_juv_fix_lw_mode", value = uploaded_json[["Female Castrated"]][["Juvenile"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "f_sa_fix_lw_mode", value = uploaded_json[["Female Castrated"]][["Sub-Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "f_ad_fix_lw_mode", value = uploaded_json[["Female Castrated"]][["Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "m_juv_int_lw_mode", value = uploaded_json[["Male Intact"]][["Juvenile"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "m_sa_int_lw_mode", value = uploaded_json[["Male Intact"]][["Sub-Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "m_ad_int_lw_mode", value = uploaded_json[["Male Intact"]][["Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "m_juv_fix_lw_mode", value = uploaded_json[["Male Castrated"]][["Juvenile"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "m_sa_fix_lw_mode", value = uploaded_json[["Male Castrated"]][["Sub-Adult"]][["Weight"]][["Mode"]])
    updateNumericInput(session, "m_ad_fix_lw_mode", value = uploaded_json[["Male Castrated"]][["Adult"]][["Weight"]][["Mode"]])

  })
  
  
# Close Server ------------------------------------------------------------
}



