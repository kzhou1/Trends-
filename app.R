#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install
install.packages("gtrendsR")
install.packages("shiny")
install.packages("shinymaterial")
install.packages("plotly")
install.packages("rtweet")
install.packages("dplyr")
install.packages("glue")
install.packages("reactable")
install.packages("purrr")

#library
library(gtrendsR)
library(shiny)
library(shinymaterial)
library(plotly)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)

make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}

# Define UI for application that draws a histogram
ui <- material_page(title = "Programming Language Trend Analyzer",
                    nav_bar_color = "purple",
                    nav_bar_fixed = TRUE,
                    include_fonts = TRUE,
                    
                    #side_nave
                    
                    material_side_nav(
                      fixed = TRUE,
                      image_source = "NYU-Emblem.png",
                      material_side_nav_tabs(
                        side_nav_tabs = c(
                          
                          "Google Trends" = "google_trends",
                          "Twitter Trends" = "twitter_trends",
                          "About Project" = "project_id"
                        ),
                        icons = c("insert_chart", "explore", "cast")
                      )
                    ),
                    
                    #First Trends
                    material_side_nav_tab_content(
                      side_nav_tab_id = "google_trends",
                      tags$br(),
                      material_row(
                        material_column(
                          width = 5,
                          material_card(
                            title = "Please enter the keywords you want to search on Google Trends and use comma to separate keywords",
                            depth = 5,
                            
                            material_text_box(
                              input_id = "input_result",
                              label = "Enter Trends"
                            ),
                            material_dropdown(
                              input_id = "geography",
                              label = "Country",
                              choices = list(
                                "Worldwide"
                              )),
                            material_dropdown(
                              input_id = "period",
                              label = "Time Period",
                              choices = c(
                                "Last day",
                                "Last seven days",
                                "Past 30 days",
                                "Past 90 days",
                                "Past 12 months",
                                "Last five years"
                              ),
                              selected = "Last five years"
                            ),
                            submitButton("Submit"),
                          )
                        ),
                        material_column(
                          width = 9,
                          material_card(
                            title = "Google Trends",
                            depth = 4,
                            #trend chat
                            plotlyOutput("gtrends_plot"),
                          ),
                          tags$a(href="https://trends.google.com/trends/?geo=US", "Source: Google Trends", target = "_blank")
                        )
                      )
                    ),
                    
                    #Second
                    material_side_nav_tab_content(
                      side_nav_tab_id = "twitter_trends",
                      tags$br(),
                      material_column(
                        width = 5,
                        material_card(
                          title = "Please enter the keywords tag",
                          tags$a("Warning: Before start the project, please get the Twitter token first"),
                          depth = 5,
                        numericInput("num_tweet",
                          "Number of tweets to request", 
                                     min = 10,
                                     max = 20000,
                                     value = 10),
                        textInput("tweet_search",
                                  "Please enter the tags to search: ",
                                  value = "#"),
                        actionButton("get_twitter", "Submit", class="btn-primary")
                      ),
                      reactableOutput("tweet_table")
                      )
                      ),
                    
                    #Third
                    material_side_nav_tab_content(
                      side_nav_tab_id = "project_id",
                      tags$br(),
                      material_column(
                        width = 9,
                        material_card(
                          title = "About Project",
                          tags$br(),
                          p("Project Manger: Kunqiao Zhou"),
                          p("Project Sponsor: Eric Lui"),
                        )
                      )
                    )
                    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  ### Get Country Code
  geo <- reactive({
    if (input$geography == "Worldwide") {
      ""
    }
    
    else{
      countrycode(input$geography, 'country.name', 'iso2c')
    }
    
  })
  
  ### Time
  start_date <- reactive({
    if (input$period == "Last five years") {
      "today+5-y"
    }
    else if (input$period == "Past 12 months") {
      "today 12-m"
    }
    else if (input$period == "Past 90 days") {
      "today 3-m"
    }
    else if (input$period == "Past 30 days") {
      "today 1-m"
    }
    else if (input$period == "Last seven days") {
      "now 7-d"
    }
    else if (input$period == "Last  day") {
      "now 1-d"
    }
  })
  
  
  out <- reactive({
    if (length(input$input_result) > 0) {
      unlist(strsplit(input$input_result, ","))
    }
  })
  
  trend_plot <- reactive({
    if (length(input$input_result != 0))
      req(input$input_result)
    {
      gtrends(keyword = out(),
              time = start_date(),
              geo = geo())
      }
  })
  
  #Plot the Trend
  output$gtrends_plot <- renderPlotly({
    plot(trend_plot())
  })


  tweet_df <- eventReactive(input$get_twitter, {
    search_tweets(input$tweet_search, n = input$num_tweet, include_rts = FALSE)
  })
  
  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  output$tweet_table <- renderReactable({
    reactable::reactable(tweet_table_data(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                           URLs = colDef(html = TRUE)
                         )
    )
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
