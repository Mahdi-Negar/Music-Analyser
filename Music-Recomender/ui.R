#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Music Recommender"),
  
  # Sidebar with a slider input for number of bins
  tabsetPanel(
    tabPanel("Recommend by Tag",
  sidebarLayout(
    sidebarPanel(
       textInput("songName", label = h3("Song Name"), 
                 value = "Hey you"),
       textInput("artistName", label = h3("Artist Name"), 
                 value = "Pink Floyd"),
       actionButton("searchSong", "Search Song"),
       
       textInput("songId", label = h3("Song ID"), 
                 value = "TRNIKJY128F9308A0C"),
       sliderInput("relativeSize", label = h3("Number of Output"),
                   min = 1, max = 20, value = 5),
       checkboxInput("includeThis", label = "Include This Artist", value = TRUE),
       actionButton("searchId", "Recommend me")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Search", tableOutput("songNames")),
        tabPanel("Song Recomender", fluidRow(
          column(6, tableOutput("recommendedSongs")),
          column(3, tableOutput("yourTags")),
          column(3, tableOutput("recommendedTags"))
        ))
      )
      # ,tags$head(tags$style("#recommendedSongs table {background-color: red; }", media="screen", type="text/css"))
    )
  )),
  tabPanel("Recommend By Users",
           sidebarLayout(
             sidebarPanel(
               textInput("artistNameR", label = h3("Song Name"), 
                         value = "Nightwish"),
               actionButton('addArtist','Add'),
               actionButton('resetArtist','Reset'),
               sliderInput("artistSize", label = h3("Number of Output"),
                           min = 1, max = 50, value = 20)
             ),
             mainPanel(
               h3("Selected Artist"),
               verbatimTextOutput('selectedArtist'),
               h3("Recommended Artist"),
               verbatimTextOutput('recommendedArtist')
             )
           )
  ))
))
