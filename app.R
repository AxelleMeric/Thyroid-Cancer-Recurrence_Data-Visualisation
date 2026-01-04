library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"), #medical design
  titlePanel("Thyroid Cancer Reccurence detection with a Logistic Regression"),
  
  sidebarLayout( #Everything on the side = the parameters
    sidebarPanel(
      h4("Clinical informations of the patient"),
      helpText("Please, fill the following information."),
      
      #INPUT
      #AGE
      numericInput("Age", "Age of the patient:", value = 45, min = 0, max = 120),
      
      #RESSPONSE
      selectInput("Response", "Patient's repsonse to the first traitment (Response):", 
                  choices = c("Excellent" = "Excellent", 
                              "Indeterminate" = "Indeterminate",
                              "Biochemical Incomplete" = "Biochemical Incomplete",
                              "Structural Incomplete" = "Structural Incomplete")),
      
      #RISK
      selectInput("Risk", "Patient's risk (Risk):",
                  choices = c("Low" = "Low", 
                              "Intermediate" = "Intermediate", 
                              "High" = "High")),
      
      #STAGE
      selectInput("Stage", "Stage of the cancer (Stage):",
                  choices = c("I" = "I",
                              "II" = "II",
                              "III" = "III",
                              "IVA" = "IVA",
                              "IVB" = "IVB")),
      
      #N
      selectInput("N", "Stage N (lymph node):",
                  choices = c("N0" = "N0", "N1a" = "N1a", "N1b" = "N1b")),
      
      #PATHOLOGY
      selectInput("Pathology", "Type of pathology:",
                  choices = c("Papillary" = "Papillary",
                              "Micropapillary" = "Micropapillary",
                              "Follicular" = "Follicular",
                              "Hurthle cell" = "Hurthle cell")),
      
      #THYROID FUNCTION
      selectInput("Thyroid_Function", "Thyroid Function:",
                  choices = c("Euthyroid" = "Euthyroid",
                              "Clinical Hyperthyroidism" = "Clinical Hyperthyroidism",
                              "Clinical Hypothyroidism" = "Clinical Hypothyroidism",
                              "Subclinical Hypothyroidism"= "Subclinical Hypothyroidism")),
      
      br(), #br = break line = \\
      actionButton("computation_button", "Computation of the risk"  , 
                   class = "btn-primary",
                   style = "background-color: #E4CBF9; border-color: #E4CBF9; color: black;")
    ),
    
    mainPanel( #Everything on the middle/the right = the outcome
      div(style = "text-align: center; padding-top: 20px;",
          h3("Risk analysis"),
          hr(), #hr = horizontal rule = --------
          
          #Probability part
          h1(textOutput("probability_text"), style = "color: black; font-weight: bold;"),
          p("Estimated probability"),
          uiOutput("probability_scale"),
          
          br(), hr(), br(), 
          
          #Classification part
          h4("Classification by the model:"),
          uiOutput("decision_status"), 
          
          br(),
          p(style = "font-size: 0.8em; color: grey;", 
            "Note that this classification is based on a optimal statistical threshold computed by the model.")
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$computation_button, {
    
    #COEF AND MODEL DEFINITION
    
    threshold <- 0.3874921
    
    intercept <- -1.801642  
    
    Response_exc_coef <- -1.33244375
    Response_str_inccomp_coef <-  1.18263183
    Risk_low_coef <- -0.55136080
    Response_indet_coef <- -0.46324889
    Stage_II_coef <-  0.27560477
    N_NIa_coef <-  0.20491250
    Age_coef <-  0.10117990
    Pathology_Hurthel_coef <- -0.05975526
    Thyroid_Euthyroid_coef <-  0.04787256
    
    #SCORE COMPUTATION STEP BY STEP
    score <- intercept
    
    #AGE
    score <- score + (input$Age * Age_coef)
    
    # RESPONSE
    if (input$Response == "Excellent") {
      score <- score + Response_exc_coef
    } else if (input$Response == "Structural Incomplete") {
      score <- score + Response_str_inccomp_coef
    } else if (input$Response == "Indeterminate") {
      score <- score + Response_indet_coef
    }
    
    #RISK
    if (input$Risk == "Low") score <- score + Risk_low_coef
    
    #STAGE
    if (input$Stage == "StageII") score <- score + Stage_II_coef
    
    #N
    if (input$N == "N1b") score <- score + N_NIa_coef
    
    #PATHOLOGY
    if (input$Pathology == "Hurthel cell") score <- score + Pathology_Hurthel_coef
    
    #THYROID FUNCTION
    if (input$Thyroid_Function == "Euthyroid") score <- score + Thyroid_Euthyroid_coef
    

    
    
    #SIGMOID TRANSFO
    probability <- 1 / (1 + exp(-score))
    risk_percent <- round(probability * 100, 2)
    
    #PRINT PART
    output$probability_text <- renderText({
      paste0(risk_percent, " %")
    })
    
    #As here it is a visual for doctors and present a danger we put green/orange/red colour and not the usual lila one
    #Proba number
    output$probability_scale <- renderUI({
      color <- if(risk_percent < 20) "success" else if(risk_percent < 50) "warning" else "danger"
      tags$div(class = "progress", style = "height: 25px;",
               tags$div(class = paste0("progress-bar bg-", color), 
                        style = paste0("width: ", risk_percent, "%;"),
                        paste0("Risk: ", risk_percent, "%")))
    })
    
    #Proba bar
    output$probability_scale <- renderUI({
      couleur <- if(risk_percent < 15) "success"
      else if(risk_percent < 50) "warning"
      else "danger"
      
      tags$div(class = "progress", style = "height: 30px;",
               tags$div(class = paste0("progress-bar bg-", couleur), 
                        role = "progressbar",
                        style = paste0("width: ", risk_percent, "%;"),
                        aria_valuenow = risk_percent, 
                        aria_valuemin = "0", 
                        aria_valuemax = "100",
                        paste0(risk_percent, "%")
               )
      )
    })
    
    #Classification part
    output$decision_status <- renderUI({
      is_recurrent <- probability > threshold
      
      if(is_recurrent) {
        #Danger sign and style
        lab <- "RECURRENT"
        cl <- "btn btn-danger btn-lg"
        icon_name <- "exclamation-triangle"
      } else {
        # Safe sign and style
        lab <- "NON RECURRENT"
        cl <- "btn btn-success btn-lg"
        icon_name <- "check-circle"
      }
      
      #Big visual button
      tags$button(type = "button", class = cl, style = "pointer-events: none;",
                  icon(icon_name), " ", lab)
    })
    
  })
}
shinyApp(ui, server)