# # ==== SETUP ====
library(shiny)
library(bslib)
library(openxlsx)
library(lmtest)
library(AICcmodavg)
library(tidyverse)
library(patchwork)
library(ggrepel)
library()
# # ===============


# ==== APP CODE ====
# Define UI
ui <- page_navbar(
  
  bg = "cadetblue",
  inverse = TRUE,
  
  nav_spacer(),
  
  nav_panel(title = "FLUXER", 
            
            fluidRow(
              column(2),            
              column(8, withMathJax((includeMarkdown("Docs/FLUXER_IntroText.md")))),
              column(2))),
  
  
  nav_panel(title = "Flux evaluation", 

            layout_columns(
              card(# --- Input data ---
                   tags$b("Input:"),
                   
                   fileInput("File", label = NULL, 
                             placeholder = "Choose input file..."),
                   
                   tags$hr(),
                   
                   # --- Graph manipulation ----
                   tags$b("Data evaluation:"),
                   
                   selectInput("selectID", "Select data", choices = "", multiple = FALSE,
                               selected = NULL, selectize = FALSE),
                   
                   radioButtons("selectFit", "Select fit",
                                choices = c("Linear", "Quadratic"),
                                selected = "Linear"),
                   
                   actionButton("saveModel", "Save model"),
                   
                   tags$hr(),
                   
                   
                   # --- Results table ----
                   tags$b("Result:"),
                   
                   tableOutput("ResultTable"),
                   
                   tags$hr(),
                   
                   # ---  Output ----
                   tags$b("Output:"),
                   
                   downloadButton("saveFile", "Save output file")),
            
            card(layout_columns(plotOutput("ConcPlot",click = "selectedPoint"),
                 plotOutput("DiagPlot"))),
            
            col_widths = c(3, 9))

                    )
)
  
  

server <- function(input, output, session) {

# ---- Initialise ----   
  dataInput <- reactive({
    
    req(input$File)
    
    read.xlsx(input$File$datapath) %>%
      pivot_longer(6:ncol(.), names_to = "Parameter", values_to = "Conc") %>%
      mutate(ID = paste(Code, Unit, Parameter),
             Removed = FALSE) %>% 
      group_by(ID) %>% 
      filter(sum(!is.na(Conc)) > 1) %>% 
      ungroup()%>% 
      arrange(Code, Parameter, Unit, Sample)
  })
  
  
  observe({
    req(dataInput())
    updateSelectInput(session, "selectID", choices = unique(dataInput()$ID)) 
  })
    
    
  dataOutput <- reactiveValues(unitRes = NULL, sampleRes = NULL)
  removePoints <- reactiveValues(Model = NULL)

  
  observeEvent(dataInput(), {
    dataOutput$unitRes <- dataInput() %>%
                            group_by(ID) %>%
                            group_modify(~ model.fit(., "Linear")$unitRes) %>%
                            ungroup() %>%
                            mutate(ID = paste(Code, Unit, Parameter),
                                   across(C:StdErrC, ~ as.numeric(.x)))
  
    dataOutput$sampleRes <- dataInput() %>%
                               group_by(ID) %>%
                               group_modify(~ model.fit(., "Linear")$sampleRes)%>%
                               ungroup()
  })  
  
  
# ---- Processing ----    
  
  observe({
    req(dataOutput$unitRes)
    updateRadioButtons(session, "selectFit", selected = dataOutput$unitRes %>%
                         filter(ID == input$selectID) %>%
                         pull(Fit)) 
    })
  
  
   modelOutput <- reactive({
     req(dataOutput$sampleRes)

     dataOutput$sampleRes %>%
     filter(ID == input$selectID) %>%
     select(ID:Removed) %>%
      mutate(Removed = removePoints$Model) %>%   
     model.fit(., input$selectFit)
  }) 
  
  observeEvent({
    input$selectID
    dataOutput$sampleRes
    }, {
    removePoints$Model<- dataOutput$sampleRes %>%
                                   filter(ID == input$selectID) %>% 
                                   pull(Removed)
  })
  
  observeEvent({
      input$selectedPoint
      input$selectID
      dataOutput$sampleRes
    }, {
      
    res <- nearPoints(dataOutput$sampleRes %>%
                        filter(ID == input$selectID),
                      input$selectedPoint, allRows = TRUE)
    
    removePoints$Model <- xor(removePoints$Model, res$selected_)
    
  })
   
   
  observeEvent(input$saveModel, {
    
    df <- modelOutput()
    
    dataOutput$unitRes <- dataOutput$unitRes %>%
             rows_update(., df$unitRes %>%
                           mutate(ID = paste(Code, Unit, Parameter),
                                  across(C:StdErrC, ~ as.numeric(.x))) %>% 
                                  relocate(ID), by = "ID")
    
    dataOutput$sampleRes <- dataOutput$sampleRes %>%
      rows_update(., df$sampleRes, by = "SampleID")

  })   
  
  
# ---- Outputs ----  

  output$saveFile <- downloadHandler(
    filename = function() {
      paste('FluxEval_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      Output <- createWorkbook()

      addWorksheet(Output, "UnitResults")
      addWorksheet(Output, "SampleResults")

      writeData(Output, sheet = "UnitResults", x = dataOutput$unitRes %>% 
                                                   select(-ID))
      writeData(Output, sheet = "SampleResults", x = dataOutput$sampleRes %>% 
                                                        select(-ID, -SampleID))

      saveWorkbook(Output, file)

  })



  output$ResultTable <- renderTable({

    req(modelOutput()$unitRes)
    
    df <- modelOutput()$unitRes 
    
    df %>%
      select(Flux, AdjR2, FitAICc) %>%
      rename("Flux\n(mmol/m2/d)" = Flux,
             "Adj R2\n " = AdjR2,
             "AICc\n " = FitAICc)
      
  }, align = "l")



# - Plot concentration vs time (main plot) -


output$ConcPlot <- renderPlot({

  req(modelOutput()$sampleRes)

  modelResult <- modelOutput()$sampleRes %>%
    filter(Removed == FALSE, !is.na(Conc))

  outliers <- modelOutput()$sampleRes %>%
    filter(Removed == TRUE, !is.na(Conc))


  #suppressWarnings(print(
  ggplot(data = modelResult,
       aes(x = Time, y = Conc)) +
  geom_smooth(method = 'lm', colour = "cadetblue", linewidth = 1.2, se = FALSE,
              formula = ifelse(input$selectFit == "Linear",
                               'y ~ x', 'y ~ x + I(x^2)')) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = Sample), size = 4) +
  geom_point(data = outliers, colour = "coral3") +
  geom_text_repel(data = outliers, aes(label = Sample), 
                  colour = "coral3", size = 4) +
  scale_x_continuous(limits = c(0, max(modelOutput()$sampleRes$Time)*1.1),
                     expand = c(0, NA)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = unique(modelResult$ID),
       y = "Concentration (uM)",
       x = "Time (min)")#))

})

# --- Diagnostic plots ---
  output$DiagPlot <- renderPlot({

    req(modelOutput()$sampleRes)


    modelResult <- modelOutput()$sampleRes %>%
      filter(Removed == FALSE, !is.na(Conc))

# - Residuals vs fitted values -
    ResVsFit <- ggplot(data = modelResult,
           aes(x = Conc, y = Residual)) +
      geom_hline(yintercept = 0, linetype = 2, linewidth = 1.2,
                 colour = "cadetblue3") +
      geom_smooth(method = 'loess', formula = 'y ~ x', colour = "cadetblue",
                  linewidth = 1.2, se = FALSE) +
      geom_point(size = 2) +
      geom_text_repel(aes(label = Sample), size = 4) +
      labs(title = "Residuals vs fitted",
           y = "Residuals",
           x = "Fitted values")

# - Scale-location -
    ScaleLoc <- ggplot(data = modelResult,
                       aes(x = Conc, y = Root.r.stan)) +
      geom_smooth(method = 'loess', formula = 'y ~ x', colour = "cadetblue",
                  linewidth = 1.2, se = FALSE) +
      geom_point(size = 2) +
      geom_text_repel(aes(label = Sample), size = 4) +
      labs(title = "Scale-location",
           y = expression(bold(sqrt("Standardized residuals"))),
           x = "Fitted values")

# - Residuals vs time -
    ResTime <- ggplot(data = modelResult,
                       aes(x = Time, y = Residual)) +
      geom_smooth(method = 'loess', formula = 'y ~ x', colour = "cadetblue",
                  linewidth = 1.2, se = FALSE) +
      geom_point(size = 2) +
      geom_text_repel(aes(label = Sample), size = 4) +
      labs(title = "Residuals vs time",
           y = "Residuals",
           x = "Time")

# - Normal Q-Q -
    QQ <- ggplot(data = modelResult,
                      aes(x = Theor.quant, y = R.stan)) +
      geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.2,
                  colour = "cadetblue3") +
      geom_smooth(method = 'loess', formula = 'y ~ x', colour = "cadetblue",
                  linewidth = 1.2, se = FALSE) +
      geom_point(size = 2) +
      geom_text_repel(aes(label = Sample), size = 4) +
      labs(title = "Normal QQ",
           y = "Standardized residuals",
           x = "Theoretical quantiles")

# - Influential points -
    k <- case_when(input$selectFit == "Linear" ~ 1,
                   input$selectFit == "Quadratic" ~ 2)

    p <- case_when(input$selectFit == "Linear" ~ 2,
                   input$selectFit == "Quadratic" ~ 3)

    InfPoint <- ggplot(data = modelResult,
                 aes(x = Leverage, y = R.stud)) +
      geom_hline(yintercept = c(3*p/nrow(modelResult),-3*p/nrow(modelResult)),
                 linetype = 2, linewidth = 1.2,
                 colour = "cadetblue3") +
      geom_hline(yintercept = c(3, -3),
                 linetype = 3, linewidth = 1.2,
                 colour = "cadetblue3") +
      geom_point(size = 2) +
      geom_point(data = modelResult %>%
                   filter(Cooks > 4/(nrow(modelResult) - k - 1)),
                 aes(x = Leverage, y = R.stud), shape = 1, size = 6, stroke = 2,
                 colour = "cadetblue") +
      geom_text_repel(aes(label = Sample), size = 4) +
      labs(title = "Hat Values",
           y = "Studentized residuals",
           x = "Influence")


# - Plot assembly -
    suppressWarnings(print(ResVsFit + ScaleLoc + ResTime + QQ + InfPoint +
                            plot_layout(ncol = 2) &
                            theme_bw() +
                            theme(axis.text = element_text(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank())))

  })

}


# ==== REGRESSION MODEL ====
model.fit <- function(df,fit){
  
  dfModel <- df %>% 
    mutate(across(c(Time, Conc), 
                  ~ ifelse(!Removed, .x, NA))) %>%     
    filter(!is.na(Conc)) 
  
  
  x <- dfModel$Time
  y <- dfModel$Conc
  
  req(fit)
  req(x)
  req(y)
  if (fit == "Linear") {
    regModel <- lm(y ~ x, na.action = na.omit) %>% 
      suppressWarnings()
    slopeGrad <- coef(regModel)[[2]]
  }
  
  if (fit == "Quadratic") {
    regModel <- lm(y ~ x + I(x^2), na.action = na.omit) %>% 
      suppressWarnings()
    slopeGrad <- coef(regModel)[[2]] + 2*coef(regModel)[[3]]*min(x,na.rm = T)
  }
  
  
  unitRes <- data.frame(Code = unique(df$Code),
                        Unit = unique(df$Unit),
                        Parameter = unique(df$Parameter),
                        Flux = slopeGrad*60*24*unique(df$Height)/100,
                        Fvalue = summary(regModel)$fstatistic[[1]],
                        pF = pf(summary(regModel)$fstatistic[1],
                                summary(regModel)$fstatistic[2],
                                summary(regModel)$fstatistic[3],lower.tail = F),
                        FitAICc = AICc(regModel),
                        Fit = fit,
                        AdjR2 = summary(regModel)$adj.r.squared, 
                        A = coef(regModel)[[1]],
                        pA = summary(regModel)$coef[,4][[1]],                              
                        StdErrA = summary(regModel)$coef[,2][[1]],
                        CIb_2.5 = confint(regModel)[2,1],
                        CIb_97.5 = confint(regModel)[2,2],
                        B = coef(regModel)[[2]],
                        pB = summary(regModel)$coef[,4][[2]],
                        StdErrB = summary(regModel)$coef[,2][[2]],
                        C = if (fit == "Quadratic") {
                          coef(regModel)[[3]]} else {NA},
                        pC = if (fit == "Quadratic") {
                          summary(regModel)$coef[,4][[3]]} else {NA},
                        StdErrC = if (fit == "Quadratic") {
                          summary(regModel)$coef[,2][[3]]} else {NA})
  
  sampleRes <- df %>%
    mutate(SampleID = paste(Code, Unit, Parameter, Sample)) %>%
    left_join(., data.frame(Time = regModel$model$x,
                            FitValue = fitted(regModel), 
                            Residual = resid(regModel),
                            R.stan = rstandard(regModel),
                            Theor.quant = qqnorm(resid(regModel), plot.it = F)$x,
                            Root.r.stan = sqrt(abs(rstandard(regModel))),
                            R.stud = rstudent(regModel),
                            Leverage = hatvalues(regModel),
                            Cooks = cooks.distance(regModel)),
              by = "Time")  
  
  return(list(unitRes = unitRes, sampleRes = sampleRes))
}

shinyApp(ui = ui, server = server)








