#
# December 2018 version, data is dummied for public github repo
#
#
#




# Load libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyr)
library(dplyr)
library(DT)


# Load data
grantees1 <- read_excel("Data_max_useful.xlsx")
grantees2 <- read_excel("Data_max_useful2.xlsx")
grantees <- bind_rows(grantees1, grantees2)

# Add subject area to grantees #####
grantees <- mutate(grantees,
                   subject_broad = case_when(
                      startsWith(grantees$recip_subject_code, "SM") ~ "Agriculture, fishing, and forestry",
                      startsWith(grantees$recip_subject_code, "SA") ~ "Arts and culture",
                      startsWith(grantees$recip_subject_code, "SN") ~ "Community and economic development",
                      startsWith(grantees$recip_subject_code, "SB") ~ "Education",
                      startsWith(grantees$recip_subject_code, "SC") ~ "Environment",
                      startsWith(grantees$recip_subject_code, "SE") ~ "Health",
                      startsWith(grantees$recip_subject_code, "SR") ~ "Human rights",
                      startsWith(grantees$recip_subject_code, "SS") ~ "Human services",
                      startsWith(grantees$recip_subject_code, "SH") ~ "Information and communications",
                      startsWith(grantees$recip_subject_code, "ST") ~ "International relations",
                      startsWith(grantees$recip_subject_code, "SD") ~ "Philanthropy",
                      startsWith(grantees$recip_subject_code, "SK") ~ "Public affairs",
                      startsWith(grantees$recip_subject_code, "SJ") ~ "Public safety",
                      startsWith(grantees$recip_subject_code, "SP") ~ "Religion",
                      startsWith(grantees$recip_subject_code, "SF") ~ "Science",
                      startsWith(grantees$recip_subject_code, "SG") ~ "Social sciences",
                      startsWith(grantees$recip_subject_code, "SQ") ~ "Sports and recreation",
                      TRUE ~ "Unknown or not classified"),
                    subject_mid = case_when(
                      startsWith(grantees$recip_subject_code, "SM01") ~ "Agriculture",
                      startsWith(grantees$recip_subject_code, "SM04") ~ "Fishing and aquaculture",
                      startsWith(grantees$recip_subject_code, "SM02") ~ "Food security",
                      startsWith(grantees$recip_subject_code, "SM03") ~ "Forestry",
                      startsWith(grantees$recip_subject_code, "SA01") ~ "Arts services",
                      startsWith(grantees$recip_subject_code, "SA04") ~ "Cultural awareness",
                      startsWith(grantees$recip_subject_code, "SA02") ~ "Folk arts",
                      startsWith(grantees$recip_subject_code, "SA09") ~ "Historical activities",
                      startsWith(grantees$recip_subject_code, "SA08") ~ "Humanities",
                      startsWith(grantees$recip_subject_code, "SA07") ~ "Museums",
                      startsWith(grantees$recip_subject_code, "SA06") ~ "Performing Arts",
                      startsWith(grantees$recip_subject_code, "SA03") ~ "Public arts",
                      startsWith(grantees$recip_subject_code, "SA05") ~ "Visual arts",
                      startsWith(grantees$recip_subject_code, "SN06") ~ "Business and industry",
                      startsWith(grantees$recip_subject_code, "SN03") ~ "Community improvement",
                      startsWith(grantees$recip_subject_code, "SN02") ~ "Economic development",
                      startsWith(grantees$recip_subject_code, "SN05") ~ "Financial services",
                      startsWith(grantees$recip_subject_code, "SN04") ~ "Housing development",
                      startsWith(grantees$recip_subject_code, "SN01") ~ "Sustainable development",
                      startsWith(grantees$recip_subject_code, "SB07") ~ "Adult education",
                      startsWith(grantees$recip_subject_code, "SB09") ~ "Education services",
                      startsWith(grantees$recip_subject_code, "SB02") ~ "Educational management",
                      startsWith(grantees$recip_subject_code, "SB03") ~ "Elementary and secondary education",
                      startsWith(grantees$recip_subject_code, "SB01") ~ "Equal opportunity in education",
                      startsWith(grantees$recip_subject_code, "SB06") ~ "Graduate and professional education",
                      startsWith(grantees$recip_subject_code, "SB05") ~ "Higher education",
                      startsWith(grantees$recip_subject_code, "SB08") ~ "Student services",
                      startsWith(grantees$recip_subject_code, "SB04") ~ "Vocational education",
                      startsWith(grantees$recip_subject_code, "SC04") ~ "Biodiversity",
                      startsWith(grantees$recip_subject_code, "SC02") ~ "Climate change",
                      startsWith(grantees$recip_subject_code, "SC05") ~ "Domesticated animals",
                      startsWith(grantees$recip_subject_code, "SC06") ~ "Environmental education",
                      startsWith(grantees$recip_subject_code, "SC01") ~ "Environmental justice",
                      startsWith(grantees$recip_subject_code, "SC03") ~ "Natural resources",
                      startsWith(grantees$recip_subject_code, "SE15") ~ "Diseases and conditions",
                      startsWith(grantees$recip_subject_code, "SE02") ~ "Healthcare access",
                      startsWith(grantees$recip_subject_code, "SE03") ~ "Healthcare administration and financing",
                      startsWith(grantees$recip_subject_code, "SE01") ~ "Healthcare quality",
                      startsWith(grantees$recip_subject_code, "SE10") ~ "Holistic medicine",
                      startsWith(grantees$recip_subject_code, "SE04") ~ "In-patient medical care",
                      startsWith(grantees$recip_subject_code, "SE14") ~ "Medical specialties",
                      startsWith(grantees$recip_subject_code, "SE11") ~ "Medical support services",
                      startsWith(grantees$recip_subject_code, "SE12") ~ "Mental healthcare",
                      startsWith(grantees$recip_subject_code, "SE06") ~ "Nursing care",
                      startsWith(grantees$recip_subject_code, "SE05") ~ "Out-patient medical care",
                      startsWith(grantees$recip_subject_code, "SE13") ~ "Public health",
                      startsWith(grantees$recip_subject_code, "SE08") ~ "Rehabilitation",
                      startsWith(grantees$recip_subject_code, "SE07") ~ "Reproductive healthcare",
                      startsWith(grantees$recip_subject_code, "SE09") ~ "Traditional medicine and healing",
                      startsWith(grantees$recip_subject_code, "SR04") ~ "Antidiscrimination",
                      startsWith(grantees$recip_subject_code, "SR05") ~ "Diversity and intergroup relations",
                      startsWith(grantees$recip_subject_code, "SR01") ~ "Individual liberties",
                      startsWith(grantees$recip_subject_code, "SR03") ~ "Justice rights",
                      startsWith(grantees$recip_subject_code, "SR02") ~ "Social rights",
                      startsWith(grantees$recip_subject_code, "SS03") ~ "Basic and emergency aid",
                      startsWith(grantees$recip_subject_code, "SS04") ~ "Family services",
                      startsWith(grantees$recip_subject_code, "SS02") ~ "Human services information",
                      startsWith(grantees$recip_subject_code, "SS01") ~ "Human services management",
                      startsWith(grantees$recip_subject_code, "SS08") ~ "Job services",
                      startsWith(grantees$recip_subject_code, "SS06") ~ "Personal services",
                      startsWith(grantees$recip_subject_code, "SS07") ~ "Shelter and residential care",
                      startsWith(grantees$recip_subject_code, "SS09") ~ "Special population support",
                      startsWith(grantees$recip_subject_code, "SS05") ~ "Youth development",
                      startsWith(grantees$recip_subject_code, "SH04") ~ "Communication media",
                      startsWith(grantees$recip_subject_code, "SH05") ~ "Information and communications technology",
                      startsWith(grantees$recip_subject_code, "SH02") ~ "Libraries",
                      startsWith(grantees$recip_subject_code, "SH03") ~ "Media access and policy",
                      startsWith(grantees$recip_subject_code, "SH01") ~ "News and public information",
                      startsWith(grantees$recip_subject_code, "ST01") ~ "Democracy and civil society development",
                      startsWith(grantees$recip_subject_code, "ST02") ~ "Foreign policy",
                      startsWith(grantees$recip_subject_code, "ST03") ~ "Goodwill promotion",
                      startsWith(grantees$recip_subject_code, "ST04") ~ "International development",
                      startsWith(grantees$recip_subject_code, "ST05") ~ "International economics and trade",
                      startsWith(grantees$recip_subject_code, "ST06") ~ "International exchange",
                      startsWith(grantees$recip_subject_code, "ST08") ~ "International peace and security",
                      startsWith(grantees$recip_subject_code, "ST07") ~ "Multilateral cooperation",
                      startsWith(grantees$recip_subject_code, "SD02") ~ "Foundations",
                      startsWith(grantees$recip_subject_code, "SD04") ~ "Nonprofits",
                      startsWith(grantees$recip_subject_code, "SD01") ~ "Philanthropy and public policy",
                      startsWith(grantees$recip_subject_code, "SD05") ~ "Venture philanthropy",
                      startsWith(grantees$recip_subject_code, "SD03") ~ "Voluntarism",
                      startsWith(grantees$recip_subject_code, "SK04") ~ "Democracy",
                      startsWith(grantees$recip_subject_code, "SK02") ~ "Leadership development",
                      startsWith(grantees$recip_subject_code, "SK06") ~ "National security",
                      startsWith(grantees$recip_subject_code, "SK05") ~ "Public administration",
                      startsWith(grantees$recip_subject_code, "SK01") ~ "Public policy",
                      startsWith(grantees$recip_subject_code, "SK07") ~ "Public utilities",
                      startsWith(grantees$recip_subject_code, "SK03") ~ "Public/private ventures",
                      startsWith(grantees$recip_subject_code, "SJ02") ~ "Abuse prevention",
                      startsWith(grantees$recip_subject_code, "SJ09") ~ "Consumer protection",
                      startsWith(grantees$recip_subject_code, "SJ05") ~ "Corrections and penology",
                      startsWith(grantees$recip_subject_code, "SJ03") ~ "Courts",
                      startsWith(grantees$recip_subject_code, "SJ01") ~ "Crime prevention",
                      startsWith(grantees$recip_subject_code, "SJ06") ~ "Disasters and emergency management",
                      startsWith(grantees$recip_subject_code, "SJ07") ~ "Fire prevention and control",
                      startsWith(grantees$recip_subject_code, "SJ04") ~ "Legal services",
                      startsWith(grantees$recip_subject_code, "SJ08") ~ "Safety education",
                      startsWith(grantees$recip_subject_code, "SP01") ~ "Baha'i",
                      startsWith(grantees$recip_subject_code, "SP02") ~ "Buddhism",
                      startsWith(grantees$recip_subject_code, "SP03") ~ "Christianity",
                      startsWith(grantees$recip_subject_code, "SP04") ~ "Confucianism",
                      startsWith(grantees$recip_subject_code, "SP05") ~ "Hinduism",
                      startsWith(grantees$recip_subject_code, "SP11") ~ "Interfaith",
                      startsWith(grantees$recip_subject_code, "SP06") ~ "Islam",
                      startsWith(grantees$recip_subject_code, "SP07") ~ "Judaism",
                      startsWith(grantees$recip_subject_code, "SP09") ~ "Shintoism",
                      startsWith(grantees$recip_subject_code, "SP08") ~ "Sikhism",
                      startsWith(grantees$recip_subject_code, "SP12") ~ "Spirituality",
                      startsWith(grantees$recip_subject_code, "SP13") ~ "Theology",
                      startsWith(grantees$recip_subject_code, "SP10") ~ "Tribal and indigenous religions",
                      startsWith(grantees$recip_subject_code, "SF04") ~ "Biology",
                      startsWith(grantees$recip_subject_code, "SF03") ~ "Engineering",
                      startsWith(grantees$recip_subject_code, "SF06") ~ "Forensic science",
                      startsWith(grantees$recip_subject_code, "SF05") ~ "Mathematics",
                      startsWith(grantees$recip_subject_code, "SF01") ~ "Physical and earth sciences",
                      startsWith(grantees$recip_subject_code, "SF02") ~ "Technology",
                      startsWith(grantees$recip_subject_code, "SG01") ~ "Anthropology",
                      startsWith(grantees$recip_subject_code, "SG03") ~ "Economics",
                      startsWith(grantees$recip_subject_code, "SG07") ~ "Geography",
                      startsWith(grantees$recip_subject_code, "SG09") ~ "Interdisciplinary studies",
                      startsWith(grantees$recip_subject_code, "SG08") ~ "Law",
                      startsWith(grantees$recip_subject_code, "SG10") ~ "Paranormal and mystic studies",
                      startsWith(grantees$recip_subject_code, "SG05") ~ "Political science",
                      startsWith(grantees$recip_subject_code, "SG06") ~ "Population studies",
                      startsWith(grantees$recip_subject_code, "SG04") ~ "Psychology and behavioral science",
                      startsWith(grantees$recip_subject_code, "SG02") ~ "Sociology",
                      startsWith(grantees$recip_subject_code, "SQ01") ~ "Community recreation",
                      startsWith(grantees$recip_subject_code, "SQ02") ~ "Sports",
                      TRUE ~ "Unknown or not classified")
                    )

#####

# Define UI for application that draws a histogram


ui <- dashboardPage(
  dashboardHeader(title = "Follow The Leader"),
  dashboardSidebar( #####
                    
    # Subject area
    h5("Subject Area"),
    selectInput("cause", "Cause",
                choices = unique(grantees$subject_broad),
                multiple = TRUE),
    conditionalPanel(
      condition = "input.cause",
      
      uiOutput("subcause")
    ),
                    
    # Geography
    h5("Geography"),
    selectInput("state", "State",
                choices = unique(grantees$recip_state),
                multiple = TRUE),
    uiOutput("county"),
    
    sidebarMenu(
    menuItem("Nonprofit list", tabName = "nonprofit", icon = icon("database")),
    menuItem("Grants list", tabName = "grants", icon = icon("database"))
    )
  ), #####
  dashboardBody( #####
    tabItems(
      tabItem(tabName = "nonprofit",
              textOutput("numberGrantees"),
              dataTableOutput("results")
      ),
      tabItem(tabName = "grants",
              textOutput("numberGrants"),
              dataTableOutput("grants")
      )
    )
  ) #####
)

# Server
server <- function(input, output) {
  
  # Prepare data ####
  filteredGrantees <- reactive({
    copy <- grantees
    if(!is.null(input$cause)) {
      copy <- copy %>%
        filter(subject_broad %in% input$cause)
      if(!is.null(input$subcause)) {
        copy <- copy %>%
          filter(subject_mid %in% input$subcause)
      }
    }
    
    if(!is.null(input$state)) {
      copy <- copy %>%
        filter(recip_state %in% input$state)
    }
    
    if(!is.null(input$county)) {
      copy <- copy %>%
        filter(recip_county %in% input$county)
    }
    return(copy)
  })
  summarizedGrantees <- reactive({
    filteredGrantees() %>%
      group_by(recip_name) %>%
      summarize(averageDuration = mean(duration),
                averageAmount = mean(amount),
                numFunders = length(unique(gm_name)),
                numGrants = length(recip_name)) %>%
      mutate(averageDuration = format(round(averageDuration, 1))#,
             #averageAmount = format(round(averageAmount), big.mark = ",")
             ) 
  })
  # /Prepare data ####
  
  
  # Sidebar UI inputs
  output$county <- renderUI({
     if(is.null(input$state)) {
       selectInput("county", "County",
                   unique(grantees$recip_county),
                   multiple = TRUE)
     } else {
       userStates <- filter(grantees, recip_state %in% input$state)
       selectInput("county", "County", unique(userStates$recip_county),
                   multiple = TRUE)
     }
   })
   
  output$subcause <- renderUI({
     subcauses <- grantees %>%
       filter(subject_broad %in% input$cause) %>%
       select(subject_mid)
     
     selectInput("subcause", "Subcause",
                 choices = unique(subcauses$subject_mid),
                 multiple = TRUE)
   })
  
  # Nonprofit list tab ####
  output$numberGrantees <- renderText({
    paste0("Your search returned ", nrow(summarizedGrantees()), " nonprofits") # Style this better?
  })
  
  output$results <- DT::renderDataTable({
    datatable(summarizedGrantees(),
              options = list(dom = "ftpi"),
              rownames = F,
              colnames = c("Nonprofit" = "recip_name",
                           "Average Grant Duration" = "averageDuration",
                           "Average Grant Amount" = "averageAmount",
                           "Number of foundations supporting" = "numFunders",
                           "Number of foundation grants" = "numGrants")) %>%
      formatCurrency(columns = "Average Grant Amount")
  })
  
  # /Nonprofit list tab ####
  
  # Grants list tab ####
  output$numberGrants <- renderText({
    paste0(nrow(filteredGrantees()), " grant records")
  })
  
  output$grants <- DT::renderDataTable({
    datatable(select(filteredGrantees(), gm_name, recip_name, recip_state, recip_county,
           amount, duration, yr_issued, subject_broad, subject_mid),
           options = list(dom = "ftpi"),
           rownames = F,
           colnames = c("Foundation" = "gm_name",
                        "Nonprofit" = "recip_name",
                        "NP state" = "recip_state",
                        "NP county" = "recip_county",
                        "Amount" = "amount",
                        "Duration" = "duration",
                        "Year Issued" = "yr_issued",
                        "Cause" = "subject_broad",
                        "Subcause" = "subject_mid")) %>%
      formatCurrency(columns = "Amount")
  })
  
  # /Grants list tab ####
   
}

# Run the application 
shinyApp(ui = ui, server = server)

