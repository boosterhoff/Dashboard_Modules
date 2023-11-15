library(shiny)
library(shinydashboard)
library(DT)

####pull sample data from git hub#### 
APP_DATA <- readRDS(gzcon(url("https://github.com/boosterhoff/Dashboard_Modules/raw/main/FAKE_DATA.rds")))

####UI with filters####

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "assessment_feed", icon = icon("chart-bar")),
      menuItem("Data Filters", icon = icon("chart-bar"),
               dateRangeInput("date_range",
                              "Screener Date:",
                              min = min(APP_DATA$data$date6),
                              start = min(APP_DATA$data$date6),
                              format = "mm-dd-yyyy"
               ),
               selectInput("gender_checkbox",
                           "Gender:",
                           choices = sort(unique(APP_DATA$data$gender_comb)),
                           selected = sort(unique(APP_DATA$data$gender_comb)),
                           multiple = T,
                           selectize = TRUE
               ),
               sliderInput("age_slider",
                           "Age:",
                           min = min(APP_DATA$data$age, na.rm = T),
                           max = max(APP_DATA$data$age, na.rm = T),
                           value = c(min(APP_DATA$data$age, na.rm = T), max(APP_DATA$data$age, na.rm = T)),
                           step = 1
               ),
               selectInput("raceselect",
                           "Race:",
                           choices = sort(unique(APP_DATA$data$race2)),
                           selected = sort(unique(APP_DATA$data$race2)),
                           multiple = T,
                           selectize = TRUE
               ),
               selectInput("lang_select",
                           "Language:",
                           choices = sort(unique(APP_DATA$data$lang)),
                           selected = sort(unique(APP_DATA$data$lang)),
                           multiple = T,
                           selectize = TRUE
               ),
               selectInput("sexid_select",
                           "Sexual Identificaiton:",
                           choices = sort(unique(APP_DATA$data$sexid)),
                           selected = sort(unique(APP_DATA$data$sexid)),
                           multiple = T,
                           selectize = TRUE
               ),
               selectInput("fin_select",
                           "Financial Strain:",
                           choices = sort(unique(APP_DATA$data$fin)),
                           selected = sort(unique(APP_DATA$data$fin)),
                           multiple = T,
                           selectize = TRUE
               )
      )
    )
  ),


####module UI####
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "assessment_feed",

#UI to be functionized                
        
        box(
          title = "Assessment Feed",
          status = "primary",
          collapsible = FALSE,
          solidHeader = FALSE,
          width = 12,
          DTOutput("assessment_feed"))))))

####Server UI####

server <- function(input, output, session) {

####server file code for filters####
  ns <- session$ns
  
  date_range <- reactive(input$date_range)
  gender_checkbox <- reactive(input$gender_checkbox)
  age_slider <- reactive(input$age_slider)
  raceselect <- reactive(input$raceselect)
  lang_select <- reactive(input$lang_select)
  sexid_select <- reactive(input$sexid_select)
  fin_select <- reactive(input$fin_select)

    data_reactive2 <- reactive({
    
    df <- APP_DATA$data %>%
      filter(gender_comb %in% gender_checkbox()) %>% 
      filter(date6 >= date_range()[1] & date6 <= date_range()[2])  %>%
      filter(age >= age_slider()[1] & age <= age_slider()[2]) %>% 
      filter(race2 %in% raceselect()) %>% 
      filter(date6 >= date_range()[1] & date6 <= date_range()[2]) %>%
      filter(lang %in% lang_select()) %>% 
      filter(sexid %in% sexid_select()) %>% 
      filter(fin %in% fin_select()) 
    
    return(df)
  })
  
####Server Code to Edit####
    
# DataTable object
  output$assessment_feed <- DT::renderDT({
    
    #interesting that I didn't need to put this in a reactive environment
    #this code just sorts the dataset by date, more recent ontop
    ordered <- data_reactive2()[order(data_reactive2()$date6, decreasing = TRUE), ]
    
    DT::datatable(
      ordered[, c("idlucine", "date6", "age", "gender_comb")], #select columsn to display
      rownames = FALSE,
      colnames = c("ID", "Date", "Age", "Gender"), #change col names
      style = "bootstrap",
      selection = 'single',
      #  caption = "",
      options = list(
        dom = 'ftp',
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 8, #max displayed cases
        scrollY = TRUE, #need this for pageLength to work
        stateSave = TRUE)
    )
  })
  DTproxy <- DT::dataTableProxy("assessment_feed")
}

####Shiny App Call####
shinyApp(ui, server)
