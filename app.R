# Load required libraries
library(odbc)
library(DBI)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(plotly)
library(readr)
library(shiny)
library(DT)
library(shinycssloaders)
library(bslib)
library(shinyWidgets)
library(sysfonts)
library(showtext)
library(grid)
library(shinyscreenshot)
library(ggrepel)
library(parallel)




# Make the tracking data reactive to the player not the selected game
# Every time a player is selected, pull all of their games and store that in a dataframe 
# Then use the game selector as just a filter on this data to make sure new game data isn't 
# being pulled every single time a game is changed, only pull it all one time for each player

# 1. Add Google Font (e.g., Roboto)
font_add_google(name = "Roboto", family = "roboto")

# 2. Enable showtext for custom fonts in plots
showtext_auto()

pal <- colorRampPalette(c("red", "blue"))


api_url <- "https://api.profootballfocus.com"


# replace this with your PFF API key
api_key <- Sys.getenv("pff_api_key")


# Build the temporary access token URL
temp_access_token_url <- paste0(api_url, "/auth/login")

# next request a token
authorization_response_object <- POST(
  url = temp_access_token_url,
  add_headers("x-api-key" = api_key)
)
# extract the jwt from the response object
jwt_object <- fromJSON(rawToChar(authorization_response_object$content))


# Establish connection to Azure SQL Database to Workbench
conn <- dbConnect(
  odbc::odbc(),
  Driver = "SQLServer",
  Server = Sys.getenv("Server_Name"),
  Database = Sys.getenv("database_pff"),
  UID = Sys.getenv("sql_uid"),
  PWD = Sys.getenv("sql_pwd"),
  Port = "1433",
  TrustServerCertificate="yes"
)

today = today(tzone="EDT")



# Must have api_url & jwt_object loaded to run this function, 
# jwt_object needs to be refreshed every 30 minutes or so but don't want to hit limit
# So the refresh is done outside of this function rather than everytime it is called

get_data <- function(endpoint){
  
  # once we have a jwt we can make a request from the API, (make a gzip request to minimize traffic)
  object <- GET(
    url=api_url,
    path=endpoint,
    add_headers(.headers = c(
      "Authorization"=paste0("Bearer ", jwt_object$jwt)),
      "Accept-Encoding"="gzip"
    )
  )
  
  # finally we convert the data response to a data frame with readr
  data <- read_csv(object$content) 
  
  return(data)
  
}

# Need to pull down all games going back to 2018, teams can pull just once but need this loop to pull all games
seasons <- c("2018","2019", "2020", "2021", "2022", "2023", "2024", "2025")




fetch_season <- function(season) {
  games_endpoint <- paste0("/v1/ncaa/", season, "/games")
  games_object <- GET(
    url = api_url,
    path = games_endpoint,
    add_headers(.headers = c(
      "Authorization" = paste0("Bearer ", jwt_object$jwt),
      "Accept-Encoding" = "gzip"
    ))
  )
  games_content <- content(games_object, "text", encoding = "UTF-8")
  fromJSON(games_content)
}

# Run all 8 seasons in parallel
results <- mclapply(seasons, fetch_season, mc.cores = length(seasons))

# Extract games from each result and combine
all_games <- bind_rows(lapply(results, function(r) as.data.frame(r$games)))

# Teams only need to come from one result (they don't change season-to-season)
content_list <- results[[length(results)]]
teams <- as.data.frame(content_list$teams) %>%
  mutate(team = paste(city, nickname)) %>%
  select(id, gsis_abbreviation, team)


full_game <- all_games %>% 
  left_join(teams, by=join_by(away_franchise_id == id)) %>% 
  left_join(teams, by=join_by(home_franchise_id == id), 
            suffix = c("_away", "_home")) %>% 
  mutate(matchup = paste(team_away, "@", team_home, "-", as.Date(start))) %>% 
  select(id, matchup, start, week, season, home_franchise_id, 
         gsis_abbreviation_home, team_home, away_franchise_id, 
         gsis_abbreviation_away, team_away)



qb_info <- dbGetQuery(conn, "SELECT game_id, team_id, player_id, player_name, position, jersey_number FROM roster_feed WHERE position = 'QB'") 



# Pull in NCAA teams to get rid of senior games and special rosters
ncaa_teams <- dbReadTable(conn, "ncaa_team_codes") %>% pull(pff_franchise_id)


game_roster <- full_game %>% 
  left_join(qb_info, by=join_by(id == game_id), relationship = "one-to-many") %>% 
  mutate(player_team = ifelse(team_id == away_franchise_id, team_away, team_home)) %>% 
  filter(!is.na(player_team), team_id %in% ncaa_teams)


# will need to draw the field multiple times - so make a function to reduce lines of code
draw_field <- function(data) {

    ggplot(data) + 
      
      geom_rect(aes(xmin = -28, xmax=28, ymin=-50, ymax=50), fill = "lightgreen") +
      annotate("segment", x = -26.666, y = 5, xend = 26.666, yend = 5, color = "white") +
      annotate("segment", x = -26.666, y = 15, xend = 26.666, yend = 15, color = "white") +
      annotate("segment", x = -26.666, y = 25, xend = 26.666, yend = 25, color = "white") +
      annotate("segment", x = -26.666, y = -5, xend = 26.666, yend = -5, color = "white") +
      annotate("segment", x = -26.666, y = -15, xend = 26.666, yend = -15, color = "white") +
      annotate("segment", x = -26.666, y = -25, xend = 26.666, yend = -25, color = "white") +
      annotate("segment", x = -26.666, y = 10, xend = 26.666, yend = 10, color = "white") +
      annotate("segment", x = -26.666, y = 20, xend = 26.666, yend = 20, color = "white") +
      annotate("segment", x = -26.666, y = 30, xend = 26.666, yend = 30, color = "white") +
      annotate("segment", x = -26.666, y = 40, xend = 26.666, yend = 40, color = "white") +
      annotate("segment", x = -26.666, y = 0, xend = 26.666, yend = 0, color = "black") +
      annotate("segment", x = -26.666, y = -10, xend = 26.666, yend = -10, color = "white") +
      annotate("segment", x = -26.666, y = -20, xend = 26.666, yend = -20, color = "white") +
      annotate("segment", x = -26.666, y = -30, xend = 26.666, yend = -30, color = "white") +
      annotate("segment", x = -26.666, y = -40, xend = 26.666, yend = -40, color = "white")+
      scale_colour_manual(
        name = "pressure",
        values = c("Yes" = "red", "No" = "blue")
      ) + 
      theme_classic()


}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("QB OPP Scout"),
  
  theme = bs_theme(
    version = 5,
    base_font    = font_google("Roboto"),
    heading_font = font_google("Roboto"),
    code_font    = font_google("Roboto"),
    
    "body-font-family"    = "Roboto, sans-serif",
    "body-font-weight"    = 400,
    "body-font-size"      = "1em",
    "body-line-height"    = 1.5,
    "navbar-height"        = "48px",
    "body-letter-spacing" = "0.00938em",
    "border-radius"        = "4px",
    "btn-border-width"     = "1px"
  ),
  
  
  card(
    
    class = "mb-3",
    style = "
      background-color: #CF102D; 
      color: white;
      padding: 12px 18px;
    ",
    
    layout_columns(
      
      col_widths = c(6,6),
      gap = "20px",
      pickerInput("qb_select",
                  label = "Select QB(s)",
                  choices = unique(game_roster$player_name),
                  options = pickerOptions(container = "body", 
                                          liveSearch = TRUE),
                  selected = "Fernando Mendoza",
                  multiple = FALSE,
                  width = "75%"
      ),
      
      pickerInput("game_select",
                  label = "Select Game(s)",
                  choices = unique(game_roster$matchup),
                  options = pickerOptions(container = "body",
                                          liveSearch = TRUE),
                  multiple = TRUE,
                  width = "75%"
      )
    ),
    
    
    div(style = "display: flex; justify-content: center;",
    layout_columns(
      col_widths = c(6),
      # Create a button to control visual updating, otherwise will render each time the game selection is changed
      actionButton(
        inputId = "visual_update",
        label = markdown("**Load Report**"),
        style =
          "border: 1px solid #212427;
             border-radius: 2px;
             color: #212427;
             background-color: white;"
      )
    )
    )
  ),
  
  
  div(id = "takescreenshot", 
      
  fluidRow(
    h1(textOutput("games"))
  ),
  
  
  
  # Show a plot of the generated distribution
  fluidRow(
    column(
      4,
      align = "center",
      withSpinner(
        plotOutput("llama_graph"),
        type = 6,
        color = "#CF102D",
        caption = "Loading Visual..."
      )
    ),
    column(
      4,
      align = "center",
      withSpinner(
        plotOutput("camel_graph"),
        type = 6,
        color = "#CF102D",
        caption = "Loading Visual..."
      )
    ),
    column(
      4,
      align = "center",
      withSpinner(
        plotOutput("rhino_graph"),
        type = 6,
        color = "#CF102D",
        caption = "Loading Visual..."
      )
    )
  ),
  
  br(),
  
  fluidRow(
    withSpinner(
      DTOutput("stats_table"),
      type = 6,
      color = "#CF102D",
      caption = "Loading Datatable..."
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      6,
    withSpinner(
      plotOutput("spider"),
      type = 6,
      color = "#CF102D",
      caption = "Loading Visual"
    )
    ),
    
    column(
      6,
    withSpinner(
      plotOutput("feel_good"),
      type = 6,
      color = "#CF102D",
      caption = "Loading Visual"
    )
    )
  )
  
  ),
  
  br(),
  
  fluidRow(
    column(
      12,
      align = "center",
  actionButton("go", "Take screenshot", )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  selected_player_id <- reactive({
    
    # Ensure input is available
    req(input$qb_select)
    
    
    game_roster %>%
      dplyr::filter(player_name == input$qb_select) %>%
      dplyr::pull(player_id) %>%
      unique() %>%
      first() %>%
      as.character()
    
  })
  
  # filter games_roster based on selected player
  player_data <-reactive({
    
    game_roster %>% filter(player_id == selected_player_id())
    
  })
  
  
  # update Game selection choices dynamically
  observeEvent(input$qb_select, {
    
    available_games <- unique(player_data()$matchup)
    
    
    updatePickerInput(
      session,
      "game_select",
      choices = available_games,
      selected = available_games
    )
  })
  
  selected_games <- reactive({
    
    unique(player_data()$id)
    
    
  })
  
  game_id <- reactive({
    
    player_data()$id[player_data()$matchup %in% input$game_select]
    
  })
  
  
  # When QB is selected, find all game id's they are in and pull this data from tracking all at once
  # use a button for reloading games so it only triggers when pushed, not every time someone selects games
  
  game_tracking <- eventReactive(input$visual_update, {
    
    req(input$qb_select)
    
    game_ids <- paste0("(",paste(game_id(), collapse = ", "), ")")

    
    dbGetQuery(conn, paste0("SELECT * FROM player_tracks WHERE player_id = ", selected_player_id(), "AND game_id IN ", game_ids)) %>% 
      mutate(play_id = as.character(play_id))
    
  })
  
  
  
  # Have reactive function that looks at selected player and queries SQL with new query
  play_data <- eventReactive(input$visual_update, {
    
    # Ensure input is available
    req(input$qb_select)
    
    
    game_ids <- paste0("(",paste(selected_games(), collapse = ", "), ")")
    
    play_query <- paste("SELECT * FROM play_feed WHERE game_id IN", game_ids)
    
    dbGetQuery(conn, play_query) %>% filter(no_play == 0)
    
  })
  
  
  passing_data <- eventReactive(input$visual_update, {
    
    # Ensure input is available
    req(input$game_select)
    
    game_ids <- paste0("(",paste(game_id(), collapse = ", "), ")")

    
    passing_query <- paste("SELECT * FROM passing_feed WHERE passer_player_id =", selected_player_id(), "AND game_id IN", game_ids)
    
    dbGetQuery(conn,passing_query) %>% 
      filter(no_play == 0) %>% 
      mutate(play_id = as.character(play_id))
    
  })
  
  
  tracking_data <- eventReactive(input$visual_update, {
    
    left_join(passing_data(), game_tracking(), by=c("play_id", "game_id"), keep=FALSE, relationship = "one-to-many") %>%
      mutate(play_details = paste0(play_id, " - ", paste0("Q",quarter),", Down: ", as.character(down), ", Distance: ", as.character(distance)),
             pressure = ifelse(is.na(pressure), "No", "Yes")) %>%
      filter(attempt == 1, game_id %in% game_id())
  })
  
  
  tracking_llama <- reactive({
    
    #All data from left hash
    tracking_data() %>%  filter(hash == "L")
  
    
  })
  
  tracking_rhino <- reactive({
    
    # All data from right hash
    tracking_data() %>%  filter(hash == "R")
    
  })
  
  tracking_camel <- reactive({
    
    # All data from center hash
    tracking_data() %>%  filter(hash == "C")
  })
  

  
  # have 3 different graphs side by side each with a selector above it
  # selector will display the plays from that hash so user can choose to show all plays or just an individual play
  output$camel_graph <- renderPlot(
    

    draw_field(tracking_camel()) +
               # Vertical Lines
               annotate("segment", x = -6.666, y = 19.5, xend = -6.666, yend = 20.5, color = "white") + 
               annotate("segment", x = 6.666, y = 19.5, xend = 6.666, yend = 20.5, color = "white") +
               annotate("segment", x = -6.666, y = 9.5, xend = -6.666, yend = 10.5, color = "white") + 
               annotate("segment", x = 6.666, y = 9.5, xend = 6.666, yend = 10.5, color = "white") +
               annotate("segment", x = -6.666, y = -.5, xend = -6.666, yend = .5, color = "white") + 
               annotate("segment", x = 6.666, y = -.5, xend = 6.666, yend = .5, color = "white") +
               annotate("segment", x = -6.666, y = -9.5, xend = -6.666, yend = -10.5, color = "white") + 
               annotate("segment", x = 6.666, y = -9.5, xend = 6.666, yend = -10.5, color = "white") +
               annotate("segment", x = -6.666, y = -19.5, xend = -6.666, yend = -20.5, color = "white") + 
               annotate("segment", x = 6.666, y = -19.5, xend = 6.666, yend = -20.5, color = "white") +
               annotate("segment", x = -6.666, y = -24.5, xend = -6.666, yend = -25.5, color = "white") + 
               annotate("segment", x = 6.666, y = -24.5, xend = 6.666, yend = -25.5, color = "white") +
               annotate("segment", x = -6.666, y = 24.5, xend = -6.666, yend = 25.5, color = "white") + 
               annotate("segment", x = 6.666, y = 24.5, xend = 6.666, yend = 25.5, color = "white") +
      
      # Sidelines
              annotate("segment", x = -26.666, y = -40, xend = -26.666, yend = 40, color = "grey", size = 2) +
              annotate("segment", x = 26.666, y = -40, xend = 26.666, yend = 40, color = "grey", size = 2) +
              geom_rect(aes(xmin = -26.666, xmax=-30, ymin=-50, ymax=50), fill = "white") +
              geom_rect(aes(xmin = 26.666, xmax=30, ymin=-50, ymax=50), fill = "white") +
      
      
      
              # Yard lines - Left
              annotate("segment", x = -8.666, y = 1, xend = -6.666, yend = 1, color = "white") +
              annotate("segment", x = -8.666, y = 2, xend = -6.666, yend = 2, color = "white") + 
              annotate("segment", x = -8.666, y = 3, xend = -6.666, yend = 3, color = "white") + 
              annotate("segment", x = -8.666, y = 4, xend = -6.666, yend = 4, color = "white") + 
              annotate("segment", x = -8.666, y = 6, xend = -6.666, yend = 6, color = "white") + 
              annotate("segment", x = -8.666, y = 7, xend = -6.666, yend = 7, color = "white") + 
              annotate("segment", x = -8.666, y = 8, xend = -6.666, yend = 8, color = "white") + 
              annotate("segment", x = -8.666, y = 9, xend = -6.666, yend = 9, color = "white") + 
              annotate("segment", x = -8.666, y = 11, xend = -6.666, yend = 11, color = "white") + 
              annotate("segment", x = -8.666, y = 12, xend = -6.666, yend = 12, color = "white") + 
              annotate("segment", x = -8.666, y = 13, xend = -6.666, yend = 13, color = "white") + 
              annotate("segment", x = -8.666, y = 14, xend = -6.666, yend = 14, color = "white") + 
              annotate("segment", x = -8.666, y = 16, xend = -6.666, yend = 16, color = "white") + 
              annotate("segment", x = -8.666, y = 17, xend = -6.666, yend = 17, color = "white") + 
              annotate("segment", x = -8.666, y = 18, xend = -6.666, yend = 18, color = "white") + 
              annotate("segment", x = -8.666, y = 19, xend = -6.666, yend = 19, color = "white") + 
              annotate("segment", x = -8.666, y = 21, xend = -6.666, yend = 21, color = "white") + 
              annotate("segment", x = -8.666, y = 22, xend = -6.666, yend = 22, color = "white") + 
              annotate("segment", x = -8.666, y = 23, xend = -6.666, yend = 23, color = "white") + 
              annotate("segment", x = -8.666, y = 24, xend = -6.666, yend = 24, color = "white") + 
      
              annotate("segment", x = -8.666, y = -1, xend = -6.666, yend = -1, color = "white") +
              annotate("segment", x = -8.666, y = -2, xend = -6.666, yend = -2, color = "white") + 
              annotate("segment", x = -8.666, y = -3, xend = -6.666, yend = -3, color = "white") + 
              annotate("segment", x = -8.666, y = -4, xend = -6.666, yend = -4, color = "white") + 
              annotate("segment", x = -8.666, y = -6, xend = -6.666, yend = -6, color = "white") + 
              annotate("segment", x = -8.666, y = -7, xend = -6.666, yend = -7, color = "white") + 
              annotate("segment", x = -8.666, y = -8, xend = -6.666, yend = -8, color = "white") + 
              annotate("segment", x = -8.666, y = -9, xend = -6.666, yend = -9, color = "white") + 
              annotate("segment", x = -8.666, y = -11, xend = -6.666, yend = -11, color = "white") + 
              annotate("segment", x = -8.666, y = -12, xend = -6.666, yend = -12, color = "white") + 
              annotate("segment", x = -8.666, y = -13, xend = -6.666, yend = -13, color = "white") + 
              annotate("segment", x = -8.666, y = -14, xend = -6.666, yend = -14, color = "white") + 
              annotate("segment", x = -8.666, y = -16, xend = -6.666, yend = -16, color = "white") + 
              annotate("segment", x = -8.666, y = -17, xend = -6.666, yend = -17, color = "white") + 
              annotate("segment", x = -8.666, y = -18, xend = -6.666, yend = -18, color = "white") + 
              annotate("segment", x = -8.666, y = -19, xend = -6.666, yend = -19, color = "white") + 
              annotate("segment", x = -8.666, y = -21, xend = -6.666, yend = -21, color = "white") + 
              annotate("segment", x = -8.666, y = -22, xend = -6.666, yend = -22, color = "white") + 
              annotate("segment", x = -8.666, y = -23, xend = -6.666, yend = -23, color = "white") + 
              annotate("segment", x = -8.666, y = -24, xend = -6.666, yend = -24, color = "white") + 
      
              # Yard lines - Right
              annotate("segment", x = 8.666, y = 1, xend = 6.666, yend = 1, color = "white") +
              annotate("segment", x = 8.666, y = 2, xend = 6.666, yend = 2, color = "white") + 
              annotate("segment", x = 8.666, y = 3, xend = 6.666, yend = 3, color = "white") + 
              annotate("segment", x = 8.666, y = 4, xend = 6.666, yend = 4, color = "white") + 
              annotate("segment", x = 8.666, y = 6, xend = 6.666, yend = 6, color = "white") + 
              annotate("segment", x = 8.666, y = 7, xend = 6.666, yend = 7, color = "white") + 
              annotate("segment", x = 8.666, y = 8, xend = 6.666, yend = 8, color = "white") + 
              annotate("segment", x = 8.666, y = 9, xend = 6.666, yend = 9, color = "white") + 
              annotate("segment", x = 8.666, y = 11, xend = 6.666, yend = 11, color = "white") + 
              annotate("segment", x = 8.666, y = 12, xend = 6.666, yend = 12, color = "white") + 
              annotate("segment", x = 8.666, y = 13, xend = 6.666, yend = 13, color = "white") + 
              annotate("segment", x = 8.666, y = 14, xend = 6.666, yend = 14, color = "white") + 
              annotate("segment", x = 8.666, y = 16, xend = 6.666, yend = 16, color = "white") + 
              annotate("segment", x = 8.666, y = 17, xend = 6.666, yend = 17, color = "white") + 
              annotate("segment", x = 8.666, y = 18, xend = 6.666, yend = 18, color = "white") + 
              annotate("segment", x = 8.666, y = 19, xend = 6.666, yend = 19, color = "white") + 
              annotate("segment", x = 8.666, y = 21, xend = 6.666, yend = 21, color = "white") + 
              annotate("segment", x = 8.666, y = 22, xend = 6.666, yend = 22, color = "white") + 
              annotate("segment", x = 8.666, y = 23, xend = 6.666, yend = 23, color = "white") + 
              annotate("segment", x = 8.666, y = 24, xend = 6.666, yend = 24, color = "white") + 
              
              annotate("segment", x = 8.666, y = -1, xend = 6.666, yend = -1, color = "white") +
              annotate("segment", x = 8.666, y = -2, xend = 6.666, yend = -2, color = "white") + 
              annotate("segment", x = 8.666, y = -3, xend = 6.666, yend = -3, color = "white") + 
              annotate("segment", x = 8.666, y = -4, xend = 6.666, yend = -4, color = "white") + 
              annotate("segment", x = 8.666, y = -6, xend = 6.666, yend = -6, color = "white") + 
              annotate("segment", x = 8.666, y = -7, xend = 6.666, yend = -7, color = "white") + 
              annotate("segment", x = 8.666, y = -8, xend = 6.666, yend = -8, color = "white") + 
              annotate("segment", x = 8.666, y = -9, xend = 6.666, yend = -9, color = "white") + 
              annotate("segment", x = 8.666, y = -11, xend = 6.666, yend = -11, color = "white") + 
              annotate("segment", x = 8.666, y = -12, xend = 6.666, yend = -12, color = "white") + 
              annotate("segment", x = 8.666, y = -13, xend = 6.666, yend = -13, color = "white") + 
              annotate("segment", x = 8.666, y = -14, xend = 6.666, yend = -14, color = "white") + 
              annotate("segment", x = 8.666, y = -16, xend = 6.666, yend = -16, color = "white") + 
              annotate("segment", x = 8.666, y = -17, xend = 6.666, yend = -17, color = "white") + 
              annotate("segment", x = 8.666, y = -18, xend = 6.666, yend = -18, color = "white") + 
              annotate("segment", x = 8.666, y = -19, xend = 6.666, yend = -19, color = "white") + 
              annotate("segment", x = 8.666, y = -21, xend = 6.666, yend = -21, color = "white") + 
              annotate("segment", x = 8.666, y = -22, xend = 6.666, yend = -22, color = "white") + 
              annotate("segment", x = 8.666, y = -23, xend = 6.666, yend = -23, color = "white") + 
              annotate("segment", x = 8.666, y = -24, xend = 6.666, yend = -24, color = "white") + 
      

               geom_path(
                 aes(x = rel_path_x, y = rel_path_y, label = passer_name, colour = pressure, group = play_id),
                 linewidth = 0.5,
                 # alpha = 0.2,
                 arrow = arrow(length = unit(0.2, "cm"),type = "closed", angle = 315)
               ) +
               annotate("text", x = -22.5, y = 1, label = "LOS", fontface = "bold") +
      
               coord_cartesian(xlim = c(-27, 27), ylim = c(-25, 25)) 
               

    
              )
  
  output$llama_graph <- renderPlot(
    

    draw_field(tracking_llama()) +
        
        # Sideline
        annotate("segment", x = -20, y = -40, xend = -20, yend = 40, color = "grey", size = 2) +
        geom_rect(aes(xmin = -20, xmax=-25, ymin=-50, ymax=50), fill = "white") +

        
        # Hash lines - Left
        annotate("segment", x = -2, y = 1, xend = 0, yend = 1, color = "white") +
        annotate("segment", x = -2, y = 2, xend = 0, yend = 2, color = "white") + 
        annotate("segment", x = -2, y = 3, xend = 0, yend = 3, color = "white") + 
        annotate("segment", x = -2, y = 4, xend = 0, yend = 4, color = "white") + 
        annotate("segment", x = -2, y = 6, xend = 0, yend = 6, color = "white") + 
        annotate("segment", x = -2, y = 7, xend = 0, yend = 7, color = "white") + 
        annotate("segment", x = -2, y = 8, xend = 0, yend = 8, color = "white") + 
        annotate("segment", x = -2, y = 9, xend = 0, yend = 9, color = "white") + 
        annotate("segment", x = -2, y = 11, xend = 0, yend = 11, color = "white") + 
        annotate("segment", x = -2, y = 12, xend = 0, yend = 12, color = "white") + 
        annotate("segment", x = -2, y = 13, xend = 0, yend = 13, color = "white") + 
        annotate("segment", x = -2, y = 14, xend = 0, yend = 14, color = "white") + 
        annotate("segment", x = -2, y = 16, xend = 0, yend = 16, color = "white") + 
        annotate("segment", x = -2, y = 17, xend = 0, yend = 17, color = "white") + 
        annotate("segment", x = -2, y = 18, xend = 0, yend = 18, color = "white") + 
        annotate("segment", x = -2, y = 19, xend = 0, yend = 19, color = "white") + 
        annotate("segment", x = -2, y = 21, xend = 0, yend = 21, color = "white") + 
        annotate("segment", x = -2, y = 22, xend = 0, yend = 22, color = "white") + 
        annotate("segment", x = -2, y = 23, xend = 0, yend = 23, color = "white") + 
        annotate("segment", x = -2, y = 24, xend = 0, yend = 24, color = "white") + 
        
        annotate("segment", x = -2, y = -1, xend = 0, yend = -1), color = "white") +
        annotate("segment", x = -2, y = -2, xend = 0, yend = -2, color = "white") + 
        annotate("segment", x = -2, y = -3, xend = 0, yend = -3, color = "white") + 
        annotate("segment", x = -2, y = -4, xend = 0, yend = -4, color = "white") + 
        annotate("segment", x = -2, y = -6, xend = 0, yend = -6, color = "white") + 
        annotate("segment", x = -2, y = -7, xend = 0, yend = -7, color = "white") + 
        annotate("segment", x = -2, y = -8, xend = 0, yend = -8, color = "white") + 
        annotate("segment", x = -2, y = -9, xend = 0, yend = -9, color = "white") + 
        annotate("segment", x = -2, y = -11, xend = 0, yend = -11, color = "white") + 
        annotate("segment", x = -2, y = -12, xend = 0, yend = -12, color = "white") + 
        annotate("segment", x = -2, y = -13, xend = 0, yend = -13, color = "white") + 
        annotate("segment", x = -2, y = -14, xend = 0, yend = -14, color = "white") + 
        annotate("segment", x = -2, y = -16, xend = 0, yend = -16, color = "white") + 
        annotate("segment", x = -2, y = -17, xend = 0, yend = -17, color = "white") + 
        annotate("segment", x = -2, y = -18, xend = 0, yend = -18, color = "white") + 
        annotate("segment", x = -2, y = -19, xend = 0, yend = -19, color = "white") + 
        annotate("segment", x = -2, y = -21, xend = 0, yend = -21, color = "white") + 
        annotate("segment", x = -2, y = -22, xend = 0, yend = -22, color = "white") + 
        annotate("segment", x = -2, y = -23, xend = 0, yend = -23, color = "white") + 
        annotate("segment", x = -2, y = -24, xend = 0, yend = -24, color = "white") + 
        
        
        # Hash lines - Right
        annotate("segment", x = 13.333, y = 1, xend = 15.333, yend = 1, color = "white") +
        annotate("segment", x = 13.333, y = 2, xend = 15.333, yend = 2, color = "white") + 
        annotate("segment", x = 13.333, y = 3, xend = 15.333, yend = 3, color = "white") + 
        annotate("segment", x = 13.333, y = 4, xend = 15.333, yend = 4, color = "white") + 
        annotate("segment", x = 13.333, y = 6, xend = 15.333, yend = 6, color = "white") + 
        annotate("segment", x = 13.333, y = 7, xend = 15.333, yend = 7, color = "white") + 
        annotate("segment", x = 13.333, y = 8, xend = 15.333, yend = 8, color = "white") + 
        annotate("segment", x = 13.333, y = 9, xend = 15.333, yend = 9, color = "white") + 
        annotate("segment", x = 13.333, y = 11, xend = 15.333, yend = 11, color = "white") + 
        annotate("segment", x = 13.333, y = 12, xend = 15.333, yend = 12, color = "white") + 
        annotate("segment", x = 13.333, y = 13, xend = 15.333, yend = 13, color = "white") + 
        annotate("segment", x = 13.333, y = 14, xend = 15.333, yend = 14, color = "white") + 
        annotate("segment", x = 13.333, y = 16, xend = 15.333, yend = 16, color = "white") + 
        annotate("segment", x = 13.333, y = 17, xend = 15.333, yend = 17, color = "white") + 
        annotate("segment", x = 13.333, y = 18, xend = 15.333, yend = 18, color = "white") + 
        annotate("segment", x = 13.333, y = 19, xend = 15.333, yend = 19, color = "white") + 
        annotate("segment", x = 13.333, y = 21, xend = 15.333, yend = 21, color = "white") + 
        annotate("segment", x = 13.333, y = 22, xend = 15.333, yend = 22, color = "white") + 
        annotate("segment", x = 13.333, y = 23, xend = 15.333, yend = 23, color = "white") + 
        annotate("segment", x = 13.333, y = 24, xend = 15.333, yend = 24, color = "white") + 
        
        annotate("segment", x = 13.333, y = -1, xend = 15.333, yend = -1, color = "white") +
        annotate("segment", x = 13.333, y = -2, xend = 15.333, yend = -2, color = "white") + 
        annotate("segment", x = 13.333, y = -3, xend = 15.333, yend = -3, color = "white") + 
        annotate("segment", x = 13.333, y = -4, xend = 15.333, yend = -4, color = "white") + 
        annotate("segment", x = 13.333, y = -6, xend = 15.333, yend = -6, color = "white") + 
        annotate("segment", x = 13.333, y = -7, xend = 15.333, yend = -7, color = "white") + 
        annotate("segment", x = 13.333, y = -8, xend = 15.333, yend = -8, color = "white") + 
        annotate("segment", x = 13.333, y = -9, xend = 15.333, yend = -9, color = "white") + 
        annotate("segment", x = 13.333, y = -11, xend = 15.333, yend = -11, color = "white") + 
        annotate("segment", x = 13.333, y = -12, xend = 15.333, yend = -12, color = "white") + 
        annotate("segment", x = 13.333, y = -13, xend = 15.333, yend = -13, color = "white") + 
        annotate("segment", x = 13.333, y = -14, xend = 15.333, yend = -14, color = "white") + 
        annotate("segment", x = 13.333, y = -16, xend = 15.333, yend = -16, color = "white") + 
        annotate("segment", x = 13.333, y = -17, xend = 15.333, yend = -17, color = "white") + 
        annotate("segment", x = 13.333, y = -18, xend = 15.333, yend = -18, color = "white") + 
        annotate("segment", x = 13.333, y = -19, xend = 15.333, yend = -19, color = "white") + 
        annotate("segment", x = 13.333, y = -21, xend = 15.333, yend = -21, color = "white") + 
        annotate("segment", x = 13.333, y = -22, xend = 15.333, yend = -22, color = "white") + 
        annotate("segment", x = 13.333, y = -23, xend = 15.333, yend = -23, color = "white") + 
        annotate("segment", x = 13.333, y = -24, xend = 15.333, yend = -24, color = "white") + 
      
        # Vertical Lines
        annotate("segment", x = 0, y = -24.5, xend = 0, yend = -25.5, color = "white") + 
        annotate("segment", x = 0, y = -19.5, xend = 0, yend = -20.5, color = "white") + 
        annotate("segment", x = 0, y = -14.5, xend = 0, yend = -15.5, color = "white") + 
        annotate("segment", x = 0, y = -9.5, xend = 0, yend = -10.5, color = "white") + 
        annotate("segment", x = 0, y = -4.5, xend = 0, yend = -5.5, color = "white") + 
        annotate("segment", x = 0, y = -0.5, xend = 0, yend = 0.5, color = "white") + 
        annotate("segment", x = 0, y = 4.5, xend = 0, yend = 5.5, color = "white") + 
        annotate("segment", x = 0, y = 9.5, xend = 0, yend = 10.5, color = "white") + 
        annotate("segment", x = 0, y = 14.5, xend = 0, yend = 15.5, color = "white") + 
        annotate("segment", x = 0, y = 19.5, xend = 0, yend = 20.5, color = "white") + 
        annotate("segment", x = 0, y = 24.5, xend = 0, yend = 25.5, color = "white") + 
        
        annotate("segment", x = 13.333, y = -24.5, xend = 13.333, yend = -25.5, color = "white") + 
        annotate("segment", x = 13.333, y = -19.5, xend = 13.333, yend = -20.5, color = "white") + 
        annotate("segment", x = 13.333, y = -14.5, xend = 13.333, yend = -15.5, color = "white") + 
        annotate("segment", x = 13.333, y = -9.5, xend = 13.333, yend = -10.5, color = "white") + 
        annotate("segment", x = 13.333, y = -4.5, xend = 13.333, yend = -5.5, color = "white") + 
        annotate("segment", x = 13.333, y = -0.5, xend = 13.333, yend = 0.5, color = "white") + 
        annotate("segment", x = 13.333, y = 4.5, xend = 13.333, yend = 5.5, color = "white") + 
        annotate("segment", x = 13.333, y = 9.5, xend = 13.333, yend = 10.5, color = "white") + 
        annotate("segment", x = 13.333, y = 14.5, xend = 13.333, yend = 15.5, color = "white") + 
        annotate("segment", x = 13.333, y = 19.5, xend = 13.333, yend = 20.5, color = "white") + 
        annotate("segment", x = 13.333, y = 24.5, xend = 13.333, yend = 25.5, color = "white") + 
        
        
        
        
        
    
        
        
        geom_path(
          aes(x = rel_path_x, y = rel_path_y, label = passer_name, colour = pressure, group = play_id),
          linewidth = 0.5,
          # alpha = 0.2,
          arrow = arrow(length = unit(0.2, "cm"),type = "closed", angle = 315)
        ) +
        # scale_colour_manual(
        #   name = "Pressured",
        #   values = c("Yes" = "red", "No" = "blue")
        # ) +
        annotate("text", x = -21.5, y = 1, label = "LOS", fontface = "bold") +
        coord_cartesian(xlim = c(-21, 25), ylim = c(-25, 25))
        # theme_classic() 
        # labs(title = "Left Plays", x = "X", y = "Y"))
  
  
  output$rhino_graph <- renderPlot(
    
    draw_field(tracking_rhino()) +

    # ggplot(tracking_rhino()) +
    #            geom_rect(aes(xmin = -30, xmax=26.666, ymin=-50, ymax=50), fill = "lightgreen") +
    #            # geom_rect(aes(xmin = -26.666, xmax=26.666, ymin=-60, ymax=-50), fill = "red") +
    #            # geom_rect(aes(xmin = -26.666, xmax=26.666, ymin=50, ymax=60), fill = "red") +
    #            geom_segment(aes(x = -30, y = 5, xend = 30, yend = 5), color = "white") +
    #            geom_segment(aes(x = -30, y = 15, xend = 30, yend = 15), color = "white") +
    #            geom_segment(aes(x = -30, y = 25, xend = 30, yend = 25), color = "white") +
    #            geom_segment(aes(x = -30, y = -5, xend = 30, yend = -5), color = "white") +
    #            geom_segment(aes(x = -30, y = -15, xend = 30, yend = -15), color = "white") +
    #            geom_segment(aes(x = -30, y = -25, xend = 30, yend = -25), color = "white") +
    #            geom_segment(aes(x = -30, y = 10, xend = 26.666, yend = 10), color = "white") +
    #            geom_segment(aes(x = -30, y = 20, xend = 26.666, yend = 20), color = "white") +
    #            geom_segment(aes(x = -30, y = 30, xend = 26.666, yend = 30), color = "white") +
    #            geom_segment(aes(x = -30, y = 40, xend = 26.666, yend = 40), color = "white") +
    #            geom_segment(aes(x = -30, y = 0, xend = 26.666, yend = 0), color = "black") +
    #            geom_segment(aes(x = -30, y = -10, xend = 26.666, yend = -10), color = "white") +
    #            geom_segment(aes(x = -30, y = -20, xend = 26.666, yend = -20), color = "white") +
    #            geom_segment(aes(x = -30, y = -30, xend = 26.666, yend = -30), color = "white") +
    #            geom_segment(aes(x = -30, y = -40, xend = 26.666, yend = -40), color = "white")+
      
      # Sideline
               annotate("segment", x = 20, y = -40, xend = 20, yend = 40, color = "grey", size = 2) +
               geom_rect(aes(xmin = 20, xmax=25, ymin=-50, ymax=50), fill = "white") +
      
      
      # Hash lines - Left
      annotate("segment", x = 2, y = 1, xend = 0, yend = 1, color = "white") +
      annotate("segment", x = 2, y = 2, xend = 0, yend = 2, color = "white") + 
      annotate("segment", x = 2, y = 3, xend = 0, yend = 3, color = "white") + 
      annotate("segment", x = 2, y = 4, xend = 0, yend = 4, color = "white") + 
      annotate("segment", x = 2, y = 6, xend = 0, yend = 6, color = "white") + 
      annotate("segment", x = 2, y = 7, xend = 0, yend = 7, color = "white") + 
      annotate("segment", x = 2, y = 8, xend = 0, yend = 8, color = "white") + 
      annotate("segment", x = 2, y = 9, xend = 0, yend = 9, color = "white") + 
      annotate("segment", x = 2, y = 11, xend = 0, yend = 11, color = "white") + 
      annotate("segment", x = 2, y = 12, xend = 0, yend = 12, color = "white") + 
      annotate("segment", x = 2, y = 13, xend = 0, yend = 13, color = "white") + 
      annotate("segment", x = 2, y = 14, xend = 0, yend = 14, color = "white") + 
      annotate("segment", x = 2, y = 16, xend = 0, yend = 16, color = "white") + 
      annotate("segment", x = 2, y = 17, xend = 0, yend = 17, color = "white") + 
      annotate("segment", x = 2, y = 18, xend = 0, yend = 18, color = "white") + 
      annotate("segment", x = 2, y = 19, xend = 0, yend = 19, color = "white") + 
      annotate("segment", x = 2, y = 21, xend = 0, yend = 21, color = "white") + 
      annotate("segment", x = 2, y = 22, xend = 0, yend = 22, color = "white") + 
      annotate("segment", x = 2, y = 23, xend = 0, yend = 23, color = "white") + 
      annotate("segment", x = 2, y = 24, xend = 0, yend = 24, color = "white") + 
      
      annotate("segment", x = 2, y = -1, xend = 0, yend = -1, color = "white") +
      annotate("segment", x = 2, y = -2, xend = 0, yend = -2, color = "white") + 
      annotate("segment", x = 2, y = -3, xend = 0, yend = -3, color = "white") + 
      annotate("segment", x = 2, y = -4, xend = 0, yend = -4, color = "white") + 
      annotate("segment", x = 2, y = -6, xend = 0, yend = -6, color = "white") + 
      annotate("segment", x = 2, y = -7, xend = 0, yend = -7, color = "white") + 
      annotate("segment", x = 2, y = -8, xend = 0, yend = -8, color = "white") + 
      annotate("segment", x = 2, y = -9, xend = 0, yend = -9, color = "white") + 
      annotate("segment", x = 2, y = -11, xend = 0, yend = -11, color = "white") + 
      annotate("segment", x = 2, y = -12, xend = 0, yend = -12, color = "white") + 
      annotate("segment", x = 2, y = -13, xend = 0, yend = -13, color = "white") + 
      annotate("segment", x = 2, y = -14, xend = 0, yend = -14, color = "white") + 
      annotate("segment", x = 2, y = -16, xend = 0, yend = -16, color = "white") + 
      annotate("segment", x = 2, y = -17, xend = 0, yend = -17, color = "white") + 
      annotate("segment", x = 2, y = -18, xend = 0, yend = -18, color = "white") + 
      annotate("segment", x = 2, y = -19, xend = 0, yend = -19, color = "white") + 
      annotate("segment", x = 2, y = -21, xend = 0, yend = -21, color = "white") + 
      annotate("segment", x = 2, y = -22, xend = 0, yend = -22, color = "white") + 
      annotate("segment", x = 2, y = -23, xend = 0, yend = -23, color = "white") + 
      annotate("segment", x = 2, y = -24, xend = 0, yend = -24, color = "white") + 
      
      
      # Hash lines - Right
      annotate("segment", x = -13.333, y = 1, xend = -15.333, yend = 1, color = "white") +
      annotate("segment", x = -13.333, y = 2, xend = -15.333, yend = 2, color = "white") + 
      annotate("segment", x = -13.333, y = 3, xend = -15.333, yend = 3, color = "white") + 
      annotate("segment", x = -13.333, y = 4, xend = -15.333, yend = 4, color = "white") + 
      annotate("segment", x = -13.333, y = 6, xend = -15.333, yend = 6, color = "white") + 
      annotate("segment", x = -13.333, y = 7, xend = -15.333, yend = 7, color = "white") + 
      annotate("segment", x = -13.333, y = 8, xend = -15.333, yend = 8, color = "white") + 
      annotate("segment", x = -13.333, y = 9, xend = -15.333, yend = 9, color = "white") + 
      annotate("segment", x = -13.333, y = 11, xend = -15.333, yend = 11, color = "white") + 
      annotate("segment", x = -13.333, y = 12, xend = -15.333, yend = 12, color = "white") + 
      annotate("segment", x = -13.333, y = 13, xend = -15.333, yend = 13, color = "white") + 
      annotate("segment", x = -13.333, y = 14, xend = -15.333, yend = 14, color = "white") + 
      annotate("segment", x = -13.333, y = 16, xend = -15.333, yend = 16, color = "white") + 
      annotate("segment", x = -13.333, y = 17, xend = -15.333, yend = 17, color = "white") + 
      annotate("segment", x = -13.333, y = 18, xend = -15.333, yend = 18, color = "white") + 
      annotate("segment", x = -13.333, y = 19, xend = -15.333, yend = 19, color = "white") + 
      annotate("segment", x = -13.333, y = 21, xend = -15.333, yend = 21, color = "white") + 
      annotate("segment", x = -13.333, y = 22, xend = -15.333, yend = 22, color = "white") + 
      annotate("segment", x = -13.333, y = 23, xend = -15.333, yend = 23, color = "white") + 
      annotate("segment", x = -13.333, y = 24, xend = -15.333, yend = 24, color = "white") + 
      
      annotate("segment", x = -13.333, y = -1, xend = -15.333, yend = -1, color = "white") +
      annotate("segment", x = -13.333, y = -2, xend = -15.333, yend = -2, color = "white") + 
      annotate("segment", x = -13.333, y = -3, xend = -15.333, yend = -3, color = "white") + 
      annotate("segment", x = -13.333, y = -4, xend = -15.333, yend = -4, color = "white") + 
      annotate("segment", x = -13.333, y = -6, xend = -15.333, yend = -6, color = "white") + 
      annotate("segment", x = -13.333, y = -7, xend = -15.333, yend = -7, color = "white") + 
      annotate("segment", x = -13.333, y = -8, xend = -15.333, yend = -8, color = "white") + 
      annotate("segment", x = -13.333, y = -9, xend = -15.333, yend = -9, color = "white") + 
      annotate("segment", x = -13.333, y = -11, xend = -15.333, yend = -11, color = "white") + 
      annotate("segment", x = -13.333, y = -12, xend = -15.333, yend = -12, color = "white") + 
      annotate("segment", x = -13.333, y = -13, xend = -15.333, yend = -13, color = "white") + 
      annotate("segment", x = -13.333, y = -14, xend = -15.333, yend = -14, color = "white") + 
      annotate("segment", x = -13.333, y = -16, xend = -15.333, yend = -16, color = "white") + 
      annotate("segment", x = -13.333, y = -17, xend = -15.333, yend = -17, color = "white") + 
      annotate("segment", x = -13.333, y = -18, xend = -15.333, yend = -18, color = "white") + 
      annotate("segment", x = -13.333, y = -19, xend = -15.333, yend = -19, color = "white") + 
      annotate("segment", x = -13.333, y = -21, xend = -15.333, yend = -21, color = "white") + 
      annotate("segment", x = -13.333, y = -22, xend = -15.333, yend = -22, color = "white") + 
      annotate("segment", x = -13.333, y = -23, xend = -15.333, yend = -23, color = "white") + 
      annotate("segment", x = -13.333, y = -24, xend = -15.333, yend = -24, color = "white") + 
      
      
      
      ###
      annotate("segment", x = 0, y = -24.5, xend = 0, yend = -25.5, color = "white") + 
      annotate("segment", x = 0, y = -19.5, xend = 0, yend = -20.5, color = "white") + 
      annotate("segment", x = 0, y = -14.5, xend = 0, yend = -15.5, color = "white") + 
      annotate("segment", x = 0, y = -9.5, xend = 0, yend = -10.5, color = "white") + 
      annotate("segment", x = 0, y = -4.5, xend = 0, yend = -5.5, color = "white") + 
      annotate("segment", x = 0, y = -0.5, xend = 0, yend = 0.5, color = "white") + 
      annotate("segment", x = 0, y = 4.5, xend = 0, yend = 5.5, color = "white") + 
      annotate("segment", x = 0, y = 9.5, xend = 0, yend = 10.5, color = "white") + 
      annotate("segment", x = 0, y = 14.5, xend = 0, yend = 15.5, color = "white") + 
      annotate("segment", x = 0, y = 19.5, xend = 0, yend = 20.5, color = "white") + 
      annotate("segment", x = 0, y = 24.5, xend = 0, yend = 25.5, color = "white") + 
      
      annotate("segment", x = -13.333, y = -24.5, xend = -13.333, yend = -25.5, color = "white") + 
      annotate("segment", x = -13.333, y = -19.5, xend = -13.333, yend = -20.5, color = "white") + 
      annotate("segment", x = -13.333, y = -14.5, xend = -13.333, yend = -15.5, color = "white") + 
      annotate("segment", x = -13.333, y = -9.5, xend = -13.333, yend = -10.5, color = "white") + 
      annotate("segment", x = -13.333, y = -4.5, xend = -13.333, yend = -5.5, color = "white") + 
      annotate("segment", x = -13.333, y = -0.5, xend = -13.333, yend = 0.5, color = "white") + 
      annotate("segment", x = -13.333, y = 4.5, xend = -13.333, yend = 5.5, color = "white") + 
      annotate("segment", x = -13.333, y = 9.5, xend = -13.333, yend = 10.5, color = "white") + 
      annotate("segment", x = -13.333, y = 14.5, xend = -13.333, yend = 15.5, color = "white") + 
      annotate("segment", x = -13.333, y = 19.5, xend = -13.333, yend = 20.5, color = "white") + 
      annotate("segment", x = -13.333, y = 24.5, xend = -13.333, yend = 25.5, color = "white") + 
      
               geom_path(
                 aes(x = rel_path_x, y = rel_path_y, label = passer_name, colour = pressure, group = play_id),
                 linewidth = 0.5,
                 # alpha = 0.2,
                 arrow = arrow(length = unit(0.2, "cm"),type = "closed", angle = 315)
               ) +    
               annotate("text", x = 21.5, y = 1, label = "LOS", fontface = "bold") +
      
               coord_cartesian(xlim = c(-25, 21), ylim = c(-25, 25)) 
              
    )
  
  
  press <- reactive({
    
    press <- tracking_data() %>% 
      group_by(play_id, passer_name) %>% 
      filter(pressure == "Yes") %>% 
      mutate(see_pressure = as.numeric(time_to_pressure) - 1.0,
             see_pressure_frame = see_pressure * 10,
             max_frame = max(frame_number),
             escape_frame = see_pressure_frame + 30) %>% 
      ungroup() %>% 
      filter(frame_number >= see_pressure_frame, frame_number <= escape_frame) %>% 
      group_by(passer_name, play_id, pressure) %>% 
      reframe(mean_x = mean(rel_path_x),
              mean_y = mean(rel_path_y)) %>% 
      mutate(escape_quad = case_when(
        mean_x <= 0 & mean_y <= -5 ~ "Back Left",
        mean_x >= 0 & mean_y <= -5 ~ "Back Right",
        mean_x <= 0 & mean_y >= -5 ~ "Forward Left",
        mean_x >= 0 & mean_y >= -5 ~ "Forward Right"
      )) %>% 
      group_by(escape_quad, passer_name, pressure) %>% 
      reframe(count = n(),
              avg_x = mean(mean_x),
              avg_y = mean(mean_y)) %>% 
      mutate(total=sum(count), 
             percentage = round(count / total, 2),
             perc_text = paste0(as.character(percentage * 100), "%"))
    
    appended_data <- data.frame(escape_quad = c("Back Left", "Back Right",
                                                "Forward Left", "Forward Right"),
                                passer_name = rep(unique(press$passer_name), times = 4),
                                pressure = rep("Yes", times = 4), 
                                count = rep(0, times = 4),
                                avg_x = rep(0, times = 4),
                                avg_y = rep(0, times = 4),
                                total = rep(0, times = 4),
                                percentage = rep(0, times = 4),
                                perc_text = rep("0%", times = 4))
    
    
    bind_rows(press, appended_data) %>% 
      group_by(escape_quad) %>%
      slice_max(count, n = 1) %>%
      ungroup() %>% 
      mutate(escape_quad = factor(escape_quad, levels = c("Forward Right", "Back Right", "Back Left", "Forward Left")))
  })
  
  qb_info <- reactive({
    teams <- unique(game_roster$player_team[game_roster$player_id == selected_player_id()])
    
    paste(input$qb_select,  "-" , paste(teams, collapse = ", "))    
    
  })
  
  output$games <- renderText(
    paste("QB OPP Scout for:", qb_info())
  )
  
  
  output$spider <- renderPlot(
  
  ggplot(press(), aes(x = escape_quad, y = percentage, fill = pressure)) + 
    geom_col( position = "dodge2", show.legend = TRUE, alpha = .9) +
    geom_text(aes(label = perc_text), position = position_stack(vjust = 0.5)) + 
    coord_polar() + 
    labs(title = "Pressure Escape Path Tendencies", 
         x = "Escape Path Direction", y 
         = "Percentage Used") 
  )
 
  
  qb_data_track <- reactive({
    
    tracking_data() %>% filter(down == 3, 
                                            pressure == "No",
                                            dropback == 1,
                                            scramble_drill == 0,
                                            play_action == 0, 
                                            screen == 0,
                                            is.na(run)) %>% 
    mutate(throw_frame = (as.numeric(time_to_throw) * 10),
           depth_at_throw = rel_path_y,
           width_at_throw = rel_path_x) %>% 
    group_by(play_id, passer_player_id, passer_name) %>% 
    filter(frame_number == throw_frame) %>% 
    ungroup()  
    
  })
  
  ciWidth <- reactive({ 
    
    t.test(qb_data_track()$width_at_throw, conf.level = 0.80)$conf.int
  
  })
  
  ciDepth <- reactive({ 
    
    t.test(qb_data_track()$depth_at_throw, conf.level = 0.80)$conf.int
    
  })
  
  
  avg_throw_location <- reactive({ 
    
    qb_data_track() %>% 
      summarise(average_depth = mean(depth_at_throw),
                average_width = mean(width_at_throw))    
  })
  
  output$feel_good <- renderPlot(
    
    
    ggplot(qb_data_track(), aes(x=width_at_throw, y=depth_at_throw)) +
               geom_rect(aes(xmin = -26.666, xmax=26.666, ymin=-50, ymax=50), fill = "lightgreen") +
               geom_rect(aes(xmin = -26.666, xmax=26.666, ymin=-60, ymax=-50), fill = "red") +
               geom_rect(aes(xmin = -26.666, xmax=26.666, ymin=50, ymax=60), fill = "red") +
               annotate("segment", x = -26.666, y = 10, xend = 26.666, yend = 10, color = "white") +
               annotate("segment", x = -26.666, y = 20, xend = 26.666, yend = 20, color = "white") +
               annotate("segment", x = -26.666, y = 30, xend = 26.666, yend = 30, color = "white") +
               annotate("segment", x = -26.666, y = 40, xend = 26.666, yend = 40, color = "white") +
               annotate("segment", x = -26.666, y = 0, xend = 26.666, yend = 0, color = "black") +
               annotate("segment", x = -26.666, y = -10, xend = 26.666, yend = -10, color = "white") +
               annotate("segment", x = -26.666, y = -20, xend = 26.666, yend = -20, color = "white") +
               annotate("segment", x = -26.666, y = -30, xend = 26.666, yend = -30, color = "white") +
               annotate("segment", x = -26.666, y = -40, xend = 26.666, yend = -40, color = "white")+
               geom_point() +
               geom_point(data = avg_throw_location(),
                          aes(x= average_width, y = average_depth), color= "red", size = 5, shape = 4, stroke = 2) +
               geom_label_repel(data = avg_throw_location(),
                               aes(x= average_width, y = average_depth),
                               # using nudge_x & nudge_y to manual adjust the location to avoid
                               # label overlay on a data point around 3.5 & 25    
                               color = "red", fontface = "bold",
                               label = paste("Width Range:", paste0(round(ciWidth(),2), collapse = " to "),
                                             paste("\nDepth Range:", paste0(round(ciDepth(),2), collapse = " to "))), 
                               nudge_x = 2, nudge_y = 4) +
               stat_ellipse(level = 0.80, color = "blue") +
               annotate("text", x = 0, y = 1, label = "LOS", fontface = "bold") +
               coord_cartesian(xlim = c(-12.5, 12.5), ylim = c(-15, 1)) +
               theme_classic() +
               labs(title = "3rd Down Pass Attempts",x = "Width", y = "Depth")
  )
  
  
  player_stats <- eventReactive(input$visual_update, {
    
    game_metrics <- play_data() %>% 
      filter(no_play == 0) %>% 
      select(play_id, expected_points_added, fumble) %>%
      mutate(play_id = as.character(play_id),
             neg_epa = ifelse(expected_points_added < 0, 1, 0))
    
    game_data <- left_join(passing_data(),
                           game_metrics, 
                           by="play_id", 
                           keep = FALSE) %>% 
      filter(game_id %in% game_id())
    
    
    game_stats <-  game_data %>%
      mutate(hash = ifelse(is.na(hash), 0, hash),
             completion = ifelse(is.na(completion), 0, completion),
             pressure = ifelse(is.na(pressure), "No", "Yes"),
             sack = ifelse(is.na(sack), 0, sack),
             run = ifelse(is.na(run), 0, run),
             fumble = ifelse(is.na(fumble), 0, fumble),
             interception = ifelse(is.na(interception), 0, interception),
             duress = ifelse(turnover_worthy_play == 1, 1,
                             ifelse(interception == 1, 1,
                                    ifelse(ing_pen == 1 , 1,
                                           ifelse(sack == 1, 1,
                                                  ifelse(fumble == 1, 1, 0)))))) %>%
      filter(run == 0) %>%
      reframe(plays = n(),
              total_attempts = sum(attempt,na.rm=TRUE),
              completions = sum(completion),
              completion_att = paste0(as.character(sum(completion)), "/", as.character(total_attempts)),
              sacks = sum(sack),
              cmp_pct = paste(round((completions/total_attempts) * 100, 2), "%"),
              sack_pct = paste(round((sacks/plays) * 100,2), "%"),
              neg_epa_plays = sum(neg_epa),
              neg_epa_pct = paste(round(mean(neg_epa)* 100, 2),"%"),
              duress_count = sum(duress),
              duress_pct = paste(round((duress_count/plays) * 100, 2),"%"),
              hash = "ALL") %>%
      select(hash, plays, completion_att,
             sacks, cmp_pct,
             sack_pct,
             neg_epa_pct, duress_pct)
    
    
    # Sum attempts and remove NAs
    passing_table <- game_data %>% 
      mutate(hash = ifelse(is.na(hash), 0, hash),
             completion = ifelse(is.na(completion), 0, completion),
             pressure = ifelse(is.na(pressure), "No", "Yes"),
             # Had to add this because PFF had errors, Malik Washington had a screen pass that wasn't counted as an attempt
             # filter screens out and check step_drop to see if 
             real_attempt = ifelse(attempt == 1 | screen == 1, 1, attempt),
             sack = ifelse(is.na(sack), 0, sack),
             run = ifelse(is.na(run), 0, run),
             fumble = ifelse(is.na(fumble), 0, fumble),
             interception = ifelse(is.na(interception), 0, interception),
             duress = ifelse(turnover_worthy_play == 1, 1,
                             ifelse(interception == 1, 1,
                                    ifelse(ing_pen == 1 , 1,
                                           ifelse(sack == 1, 1,
                                                  ifelse(fumble == 1, 1, 0)))))) %>% 
      filter(attempt == 1 | screen == 1 | sack == 1) %>% 
      group_by(hash) %>% 
      reframe(plays = n(),
              total_attempts = sum(real_attempt,na.rm=TRUE),
              completions = sum(completion),
              completion_att = paste0(as.character(sum(completion)), "/", as.character(total_attempts)),
              sacks = sum(sack),
              cmp_pct = paste(round((completions/total_attempts) * 100, 2), "%"),
              sack_pct = paste(round((sacks/plays) * 100,2), "%"),
              neg_epa_plays = sum(neg_epa),
              neg_epa_pct = paste(round(mean(neg_epa)* 100, 2),"%"),
              duress_count = sum(duress),
              duress_pct = paste(round((duress_count/plays) * 100, 2),"%")) %>%
      mutate(hash = factor(hash, levels = c("L", "C", "R"))) %>%
      arrange(hash) %>% 
      select(-c(total_attempts,completions, duress_count, neg_epa_plays))
    
    bind_rows(passing_table, game_stats) 
  })
  
  
  # Need to add a totals row at the bottom of this
  output$stats_table <- renderDT(
    
    
    datatable(player_stats(),
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'excel', 'print', 'colvis'),
                             rowGroup = list(dataSrc = 0),
                             searching = FALSE,
                             paging = FALSE,
                             width = "90%",
                             scrollX = TRUE), 
              rownames = FALSE
    ) %>% 
      formatStyle(
        columns = c(1:ncol(player_stats())), # Apply to all columns
        border = '1px solid #ddd'     # Define the border style
      )
  )
  
  
  
  observeEvent(input$go, {
    screenshot(id = "takescreenshot")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
