#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggpubr)
source("functions.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Analisis exploratorio", tabName = "visualization", icon = icon("search")),
    menuItem("Preprocesamiento", tabName = "preprocessing", icon = icon("table")),
    menuItem("Normalidad", tabName = "normal", icon = icon("poll")),
    menuItem("Homocedasticidad",tabName="homoced",icon=icon('bar-chart')),
    menuItem("Tests",tabName="tests",icon=icon('poll')),
    menuItem("Datos Categoricos I",tabName="categorical",icon=icon('table')),
    menuItem("Datos Categoricos II",tabName="categorical2",icon=icon('table')),
    menuItem("Confusores y MdE",tabName="effect_mod",icon=icon('table'))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "visualization",
              fluidRow(
                sidebarPanel(
                fileInput("file1", "Escoger Archivo CSV",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separador",
                             choices = c(Coma = ",",
                                         PuntoYcoma = ";",
                                         Tab = "\t"),
                             selected = ","),
                tags$hr(),
                
                # Input: Select number of rows to display ----
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         Todo = "all"),
                             selected = "head")),
                mainPanel(width = 12,
                  
                  # Output: Data file ----
                  tabsetPanel(id='tables',
                  tabPanel("Datos",DT::dataTableOutput("contents")),
                  tabPanel("Tipos de datos",DT::dataTableOutput("dataTypes")),
                  tabPanel("Datos Faltantes",DT::dataTableOutput("missing")))
                  # uiOutput("out")))
                  
                )
                #uiOutput("out"),
                
                
              )),
      tabItem(
        tabName = "tests",
        sidebarPanel(
          selectInput("select_col_test","Seleccionar la columna de variable continua",choices = ""),
          selectInput("group_col_test","Seleccionar columna de variable dicotomica",choices=""),
          
          ),
        mainPanel(verbatimTextOutput("test_out"))
        
      ),
      tabItem(
        tabName = "homoced",
        sidebarPanel(
          selectInput("select_col_homoced","Seleccionar la columna de variable continua",choices = ""),
          selectInput("group_col_homoced","Seleccionar columna de variable dicotomica",choices=""),
        ),
        mainPanel(verbatimTextOutput("homoced_out"))
        
      ),
      tabItem(
        tabName = "preprocessing",
        sidebarPanel(
          selectInput("select_col1","Seleccionar una columna",choices = ""),
          actionButton("do", "Convertir a Factor"))
        
      ),
      tabItem(
        tabName = "effect_mod",
        mainPanel(
          tableOutput("contingency_table_strat"),
          tags$head(tags$style("#contingency_table_strat table {background-color: white; }", media="screen", type="text/css")),
          verbatimTextOutput("odds_strat"),
          verbatimTextOutput("breslow_day"),
          verbatimTextOutput("odds_mh"))
        
      ),
      tabItem(
        tabName = "categorical2",
        mainPanel(
          verbatimTextOutput("resid"),
          tableOutput("residuals"),
          verbatimTextOutput("odds")
        )
        
      ),
      
      tabItem(
        tabName = "normal",
              fluidRow(
                
                  sidebarPanel(
                    selectInput("select_col2","Variable para analizar normalidad",choices = ""),
                    selectInput("group_col","Variable para agrupar",choices = ""),
                    sliderInput("bins",
                                "Cantindad de bines",
                                min = 1,
                                max = 50,
                                value = 30)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("histograma"),
                    plotOutput("boxplot"),
                    plotOutput("qq_plot"),
                    verbatimTextOutput(outputId="shapiro_wilk"),
                    verbatimTextOutput("kolgomorov")
                  )
              )
        ),
      tabItem(
        tabName = "categorical",
        sidebarPanel(
          selectInput("select_categorical1","Seleccionar una columna 1",choices = ""),
          selectInput("select_categorical2","Seleccionar una columna 2",choices = ""),
          selectInput("select_categorical_group","Seleccionar columna para ajustar",choices = "")
        
      ),
      mainPanel(
        verbatimTextOutput("text_cont_table"),
        tableOutput("contingency_table"),
        verbatimTextOutput("chisq"),
        verbatimTextOutput("expected_text"),
        tableOutput("expected_values"),
       
       
       
        
        
      )
  )
)
)
)

server <- function(input, output,session) {
  #x = reactiveVal()
  values <- reactiveValues(df_data = NULL)
  
  observeEvent(input$file1, {
    values$df = read_file(input$file1$datapath,input$header,input$sep)
    
    cols = names(values$df)#[sapply(values$df, is.numeric)]     #Si descomento, solo permite seleccionar variables numéricas 
    updateSelectInput(session,"select_col1",choices=cols)
    updateSelectInput(session,"select_col2",choices=cols)
    updateSelectInput(session,"select_col_test",choices=cols)
    updateSelectInput(session,"select_col_homoced",choices=cols)
    updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical1",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical2",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical_group",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    
    
    
    
    
  })
  observeEvent(input$sep, {
    req(input$file1)
    values$df = read_file(input$file1$datapath,input$header,input$sep)
    cols = names(values$df)[sapply(values$df, is.numeric)]
    updateSelectInput(session,"select_col1",choices=cols)
    updateSelectInput(session,"select_col2",choices=cols)
    updateSelectInput(session,"select_col_test",choices=cols)
    updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical1",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical2",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical_group",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    
    
  })
  observeEvent(input$header, {
    req(input$file1)
    values$df = read_file(input$file1$datapath,input$header,input$sep)
    cols = names(values$df)[sapply(values$df, is.numeric)]
    updateSelectInput(session,"select_col1",choices=cols)
    updateSelectInput(session,"select_col2",choices=cols)
    updateSelectInput(session,"select_col_test",choices=cols)
    updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical1",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical2",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    updateSelectInput(session,"select_categorical_group",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    
    
    
  })
  
  output$contents <- DT::renderDataTable({
    req(input$file1)
    
    df <- values$df
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }})
  
    
   
  
  output$missing <- DT::renderDataTable({
    req(input$file1)
    tryCatch(
          {
            na_count = na_counts(values$df)
            
            return(na_count)

          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
            }
          )
    
   
  })
  
output$dataTypes = DT::renderDataTable({
  req(input$file1)
  tryCatch(
    {
      data_types <- data_type(values$df)
      return(data_types)
      
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
})

observeEvent(input$do,
             {
               req(input$select_col1)
              values$df <-  convert_to_factor( values$df,input$select_col1)
              cols = names(values$df)[sapply(values$df, is.numeric)]
              updateSelectInput(session,"select_col1",choices=cols)
              updateSelectInput(session,"select_col2",choices=cols)
              updateSelectInput(session,"select_col_test",choices=cols)
              updateSelectInput(session,"select_col_homoced",choices=cols)
              updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
              updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
              updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
              updateSelectInput(session,"select_categorical1",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
              updateSelectInput(session,"select_categorical2",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
              updateSelectInput(session,"select_categorical_group",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
              
              
              
               
             })
output$histograma = renderPlot({
  req(input$select_col2)
  req(input$group_col)
  histogram(values$df,input$group_col,input$select_col2,input$bins)
})

output$boxplot = renderPlot({
   req(input$select_col2)
   req(input$group_col)
   my_boxplot(values$df, input$select_col2,input$group_col)
  
 })
output$qq_plot = renderPlot({
  req(input$select_col2)
  req(input$group_col)
  my_qqplot(values$df,input$select_col2,input$group_col)
})


values$col = reactive({
  req(input$select_col2)
  shapiro_w(values$df,input$select_col2)
})

output$shapiro_wilk = renderText({
  req(input$select_col2)
  req(input$group_col)
  paste0(capture.output(shapiro_w(values$df,input$select_col2,input$group_col)),collapse = '\n')
 })

output$kolgomorov <- renderText({ 
  req(input$select_col2)
  req(input$group_col)
  paste0(capture.output(lilliefors(values$df,input$select_col2,input$group_col)),collapse = '\n')  
})

output$test_out = renderText({
  req(input$select_col_test)
  req(input$group_col_test)
  paste0(capture.output(my_test(values$df,input$select_col_test,input$group_col_test)),collapse = '\n')
  
})
output$homoced_out = renderText({
  req(input$select_col_homoced)
  req(input$group_col_homoced)
  paste0(capture.output(levene(values$df,input$select_col_homoced,input$group_col_homoced)),collapse = '\n')
})

output$text_cont_table = renderText("Tabla de contingencia")
output$expected_text = renderText("Valores esperados")
output$resid = renderText("Residuos")

output$contingency_table = renderTable({
  req(input$select_categorical1)
  req(input$select_categorical2)
  #with(values$df,table(get(input$select_categorical1),get(input$select_categorical2)))
  t = as.data.frame.matrix(contingency(values$df, input$select_categorical1,input$select_categorical2))
  
},include.rownames=TRUE)

output$expected_values = renderTable({
  req(input$select_categorical1)
  req(input$select_categorical2)
  #with(values$df,table(get(input$select_categorical1),get(input$select_categorical2)))
  t = as.data.frame.matrix(chisq_expected(values$df, input$select_categorical1,input$select_categorical2))
  
},include.rownames=TRUE)

output$residuals = renderTable({
  req(input$select_categorical1)
  req(input$select_categorical2)
  #with(values$df,table(get(input$select_categorical1),get(input$select_categorical2)))
  t = as.data.frame.matrix(chisq_residuals(values$df, input$select_categorical1,input$select_categorical2))
  
},include.rownames=TRUE)

output$chisq = renderText({
  req(input$select_categorical1)
  req(input$select_categorical2)
  paste0(capture.output(chicuadrado(values$df,input$select_categorical1,input$select_categorical2)),collapse = '\n')
})

output$odds = renderText({
  req(input$select_categorical1)
  req(input$select_categorical2)
  paste0(capture.output(odds_r(values$df,input$select_categorical1,input$select_categorical2)),collapse = '\n')
})

output$odds_mh = renderText({
  req(input$select_categorical1)
  req(input$select_categorical2)
  req(input$select_categorical_group)
  paste0(capture.output(odds_r_mh(values$df,input$select_categorical1,input$select_categorical2,input$select_categorical_group)),collapse = '\n')
})
output$contingency_table_strat = renderTable({
  req(input$select_categorical1)
  req(input$select_categorical2)
  req(input$select_categorical_group)
  t = cont_t_s(values$df, input$select_categorical1,input$select_categorical2,input$select_categorical_group)
  
})

output$odds_strat = renderText({
  req(input$select_categorical1)
  req(input$select_categorical2)
  req(input$select_categorical_group)
  paste0(capture.output(odds_s(values$df,input$select_categorical1,input$select_categorical2,input$select_categorical_group)),collapse = '\n')
})

output$breslow_day = renderText({
  req(input$select_categorical1)
  req(input$select_categorical2)
  req(input$select_categorical_group)
  paste0(capture.output(bdt(values$df,input$select_categorical1,input$select_categorical2,input$select_categorical_group)),collapse = '\n')
})
}

# Run the application 
shinyApp(ui = ui, server = server)
