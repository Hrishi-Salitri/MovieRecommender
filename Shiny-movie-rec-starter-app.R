library(shiny)
library(arules)
library(dplyr)
library(bslib)


# Make sure your app.R file is saved in same folder as this .RData
load("MOVIERECFall2023.RData")

all_movies <- sort(POPULARITY$title)
default_movies = sample(all_movies, 5)



ui <- fluidPage(
  # Use built-in Shiny themes
  theme = bs_theme(
    version = 5, # Version 5 of Bootstrap
    bg = "#e9f0ed", # Set the background color to light
    fg = "#043d28" # Set the primary color to a lighter shade of green
  ),
#-----------Styling the title row----------------
  tags$head(
    tags$style(HTML("
      .title-panel {
        background-color: #c7e3d0; /* Light green background color */
        padding: 10px; /* Add padding for space */
        border-radius: 5px; /* Rounded corners */
        text-align: center; /* Center text */
        display: flex; /* Use flexbox for layout */
        justify-content: space-between; /* Align items with space between */
        align-items: center; /* Center items vertically */
      }
      .title-panel img {
        width: 4.0em; /* Set the width of the image to match the icon size */
        height: 4.0em; /* Set the height of the image to match the icon size */
      }
    "))
  ),
  
  div(class = "title-panel container",
      img(src = "https://seeklogo.com/images/P/platform-9-3-4-logo-4183841033-seeklogo.com.png"
      ), # Harry potter
      img(src = "https://i.pinimg.com/originals/ec/eb/2d/eceb2d767da08508a41389d97eb15165.png"
      ), # Barbie
      img(src = "https://www.iconarchive.com/download/i61331/majdi-khawaja/shrek/Shrek-2.ico"
      ), # Shrek
      img(src = "https://cdn4.iconfinder.com/data/icons/avatars-xmas-giveaway/128/anime_spirited_away_no_face_nobody-512.png"
          ), # Spirited away
      img(src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/56c2776e-9a6b-42bc-b233-8e0e4b7e64d6/d23gdb6-81d1916c-4efb-45b5-914d-99c2df9fb60b.png?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7InBhdGgiOiJcL2ZcLzU2YzI3NzZlLTlhNmItNDJiYy1iMjMzLThlMGU0YjdlNjRkNlwvZDIzZ2RiNi04MWQxOTE2Yy00ZWZiLTQ1YjUtOTE0ZC05OWMyZGY5ZmI2MGIucG5nIn1dXSwiYXVkIjpbInVybjpzZXJ2aWNlOmZpbGUuZG93bmxvYWQiXX0.a_NHyv210tt4N8rSJQRgxSvSNLUN9belkxGDtkUj4l8"
          ), # Ghost busters
      
      
      h1("Movie Recommendation", style = "font-weight: bold;"),
      
      
      img(src = "https://www.transparentpng.com/download/the-hunger-games/jCHOtE-circle-gold-the-hunger-games-icon.png"
      ), # Hunger games
      img(src = "https://i.pinimg.com/originals/da/3c/14/da3c14ace9add095f28b5d5ed200aa61.png"
      ), # Minion
      img(src = "https://www.pngmart.com/files/12/Darth-Vader-Helmet-Transparent-PNG.png"
      ), # Darth vader
      img(src = "https://pngimg.com/d/captain_america_PNG85.png"
      ), # Captain America
      img(src = "https://upload.wikimedia.org/wikipedia/commons/f/f9/Jurassic_Park_icon.svg"
      ) # Jurrasic park
  ),

#--------------Styling the results data frame----------
tags$head(
  tags$style(HTML("
    .result-table {
      background-color: #d9e6ed; /* Lighter shade of background color */ #538fb0
      border-radius: 10px; /* Rounded corners */
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); /* Box shadow for a bit of depth */
    }
    .result-table th, .result-table td {
      padding: 8px; /* Add padding for space */
    }
    .result-table tbody tr:nth-child(odd) {
      background-color: #f2f2f2; /* Alternate row color */
    }
    .result-table tbody tr:hover {
      background-color: #cce5ff; /* Background color on hover */
    }
  "))
),
  
  hr(),
 
  #br(),
#-----------User inputs-----------------
  sidebarLayout(
    sidebarPanel(
      selectInput("movies", "Select the movies you want to base you recommendations on:",
                  choices = all_movies, 
                  selected = default_movies,
                  multiple = TRUE),
      sliderInput("confidence", "Minimum level of confidence in rec (%):", # We are ____% confident in the rec
                  min = 1, max = 99, value = 20),
      numericInput("popularity_cap", "Popularity/Obscurity 0-40 (percent of pople that have rated the movie)", #lower number means more obscure
                  min = 0, max = 40, step = 0.01, value = 1),
      numericInput("num_recs", "How many recommendations would you like?",
                   min = 1, max = 100, value = 5),
      submitButton(text = "Give me movies!", icon = icon("fa-solid fa-film"))
    ),
    mainPanel(
      div(class = "result-table", 
        DT::dataTableOutput("output")
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  output$output <- DT::renderDataTable({
    # TODO: Make a datafram of recs
    movies.too.popular <- POPULARITY$title[which(POPULARITY$PercentSeen > input$popularity_cap)] # Hunt for super obscure games played by ____(ex: <= 0.75%) of players
    
    # Filter out movies that are "too popular" (but keep all movies the user input of course since you need those to get recommendations!)   
    # setdiff(x,y) gives the elements of x that don't also appear in y
    
    dont.recommend <- setdiff(movies.too.popular, input$movies)
    
    #Let's find rules that apply to at least 4 people, with the input level of confidence
    #minlen=2 ensure rules like if A then Z, if A&B then Z, etc.
    #maxtime=0 ensures it finds all rules
    #none=dont.consider means it will exclude from consideration all moves that are too popular
    #lhs=input$movies means it will only base the "if" statements of the movies based on the
    #movies that the user input
    #default="rhs" is there to preserve the default appearance of the "then" part of the rules
    #control=list(verbose=FALSE) prevents irrelevant, extraneous output to be printed out

    RULES <- apriori(TRANS,
                     parameter = list(supp = 4 / length(TRANS), conf = input$confidence/100, minlen = 2, maxtime = 0),
                     appearance = list(none = dont.recommend, lhs = input$movies, default = "rhs"),
                     control = list(verbose = FALSE)
    )
    
    if (length(RULES) == 0) {
      error_msg <- data.frame(
        message = "No recs :( try lowering confidence or increasing popularity"
      )
      return(error_msg)
    }
    
    
    # Remove non-statistically significant rules
    RULES <- RULES[is.significant(RULES, TRANS)]
    
    if (length(RULES) == 0) {
      error_msg <- data.frame(
        message = "No recs :( try lowering confidence or increasing popularity"
      )
      return(error_msg)
    }    
    
    
    # Convert into a dataframe the discovered rules
    RULESDF <- DATAFRAME(RULES, separate = TRUE, setStart = "", itemSep = " + ", setEnd = "")
    
    legit.recommendations <- setdiff(RULESDF$RHS, input$movies) # In RHS but not in input list of games
    RULESDF <- RULESDF %>% filter(RHS %in% legit.recommendations)
    
    RECS <- RULESDF %>%
      group_by(RHS) %>%
      summarize(Confidence = max(confidence))
    
  
    # Remember POPULARITY that we made before?  Now we can use it!
    RESULTS <- RECS %>%
      left_join(POPULARITY, by = c("RHS" = "title")) %>%
      arrange(desc(Confidence))
    names(RESULTS) <- c("Movie", "Confidence level", "Seen by...", "IMDB Rating", "Year")
    
    row.names(RESULTS) <- NULL
    
    # Making dataframe look nicer
    convert_decimal_to_percent <- function(x) {
      paste0(formatC(x * 100, format = "f", digits = 2), "%")
    }
    columns_to_convert <- c("Confidence level")
    RESULTS[columns_to_convert] <- lapply(RESULTS[columns_to_convert], convert_decimal_to_percent)
    
    RESULTS["Seen by..."] = lapply(RESULTS["Seen by..."], paste0, "%")
    
    # Print
    head(RESULTS, input$num_recs)
  
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
