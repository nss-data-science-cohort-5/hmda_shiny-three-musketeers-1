# HMDA shiny app

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel(
    h1("Fair lending analysis", align = "center")
  ),
  h4("Created by Chris Harrelson, Eli Lavender, and Alex Zhang", align = "center"),
  h4("Nashville Software School Data Science Cohort 5", align = "center"),
  
  br(),
  h4("Type in or select one or multiple choices in dropdown menus", align = "center"),
  
  hr(),
  
  # Filters of LEI and counties on the top 
  fluidRow(
    column(width = 3,
           selectInput(inputId = "lei1",
                       label = "Choose the 1st LEI(s):",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(`Entity Name`) %>% 
                                     unique() %>% 
                                     sort()))
    ),
    
    column(width = 3,
           selectInput(inputId = "county1",
                       label = "County of the 1st LEI(s):",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(Name) %>% 
                                     unique() %>% 
                                     sort()))
    ),
    
    column(width = 3,
           selectInput(inputId = "lei2",
                       label = "Choose the 2nd LEI(s):",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(`Entity Name`) %>% 
                                     unique() %>% 
                                     sort()))
    ),
    
    column(3,
           selectInput(inputId = "county2",
                       label = "County of the 2nd LEI(s):",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(Name) %>% 
                                     unique() %>% 
                                     sort()))),
  ),
  
  hr(),
  
  # Filters of loan type, type of purchaser, dwelling category/type
  fluidRow(
    column(3,
           selectInput(inputId = "loan_type",
                       label = "Loan Type:",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(loan_type) %>% 
                                     unique() %>% 
                                     sort()))
    ),
    
    column(3,
           selectInput(inputId = "purchaser_type",
                       label = "Purchaser Type:",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(purchaser_type) %>% 
                                     unique() %>% 
                                     sort()))
    ),
    
    column(3,
           selectInput(inputId = "derived_dwelling_category",
                       label = "Dwelling Category:",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(derived_dwelling_category) %>% 
                                     unique() %>% 
                                     sort()))
    ),
    
    column(3,
           selectInput(inputId = "activity_year",
                       label = "Year:",
                       multiple = TRUE,
                       selected = "All",
                       choices = c("All", hmda_lei_census %>%
                                     pull(activity_year) %>% 
                                     unique() %>% 
                                     sort()))
    )
  ),
  
  hr(),
  
  #1 Race and race census plots/tables
  h2("Loan Applications by Applicants' Race and Race Data in Census", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("racePlot1")),
             tabPanel("Table", dataTableOutput("raceTable1")),
             tabPanel("Census Plot", plotOutput("raceCensusPlot1")),
             tabPanel("Census Table", dataTableOutput("raceCensusTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("racePlot2")),
             tabPanel("Table", dataTableOutput("raceTable2")),
             tabPanel("Census Plot", plotOutput("raceCensusPlot2")),
             tabPanel("Census Table", dataTableOutput("raceCensusTable2"))
           )
    )
  ),
  
  br(),
  # hr(),
  
  #2 Sex and sex census plots/tables
  h2("Loan Applications by Applicants' Sex and Sex Data in Census", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("sexPlot1")),
             tabPanel("Table", dataTableOutput("sexTable1")),
             tabPanel("Census Plot", plotOutput("sexCensusPlot1")),
             tabPanel("Census Table", dataTableOutput("sexCensusTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("sexPlot2")),
             tabPanel("Table", dataTableOutput("sexTable2")),
             tabPanel("Census Plot", plotOutput("sexCensusPlot2")),
             tabPanel("Census Table", dataTableOutput("sexCensusTable2"))
           )
    )
  ),
  
  #3 Age and age census plots/tables
  h2("Loan Applications by Applicants' Age and Age Data in Census", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("agePlot1")),
             tabPanel("Table", dataTableOutput("ageTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("agePlot2")),
             tabPanel("Table", dataTableOutput("ageTable2"))
           )
    )
  ),
  
  #4 Distribution of Loan Amounts plots/tables
  h2("Distribution of Loan Amounts", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("distPlot1")),
             tabPanel("Table", dataTableOutput("distTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("distPlot2")),
             tabPanel("Table", dataTableOutput("distTable2"))
           )
    )
  ),
  
  #5 Applicants' Credit Scores plots/tables
  h2("Applicants' Credit Scores", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("creditPlot1")),
             tabPanel("Table", dataTableOutput("creditTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("creditPlot2")),
             tabPanel("Table", dataTableOutput("creditTable2"))
           )
    )
  ),
  
  #6 Denial Reasons of Loan Applications plots/tables
  h2("Denial Reasons of Loan Applications", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("denialPlot1")),
             tabPanel("Table", dataTableOutput("denialTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("denialPlot2")),
             tabPanel("Table", dataTableOutput("denialTable2"))
           )
    )
  ),
  
  #7 Loan Applications by Action Taken plots/tables
  h2("Loan Applications by Action Taken", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("actionPlot1")),
             tabPanel("Table", dataTableOutput("actionTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("actionPlot2")),
             tabPanel("Table", dataTableOutput("actionTable2"))
           )
    )
  ),
  
  #8 Loan Applications by County map/tables
  h2("Loan Applications by County", align = "center"),
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("mapPlot1")),
             tabPanel("Table", dataTableOutput("mapTable1"))
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("mapPlot2")),
             tabPanel("Table", dataTableOutput("mapTable2"))
           )
    )
  )

)
)