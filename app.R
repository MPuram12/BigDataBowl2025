library(shiny)
library(tidyverse)
library(plotly)
library(h2o)

# ==============================================================================
# 1. GLOBAL SETUP & DATA LOADING
# ==============================================================================

# Initialize H2O
h2o.init()
h2o.no_progress() 

# Load Models
if(dir.exists("h2o_models")) {
  tryCatch({
    model_no_id <- h2o.loadModel(file.path("h2o_models", "model_no_id"))
    model_with_id <- h2o.loadModel(file.path("h2o_models", "model_with_id"))
  }, error = function(e) {
    warning("Could not load H2O models. Check your 'h2o_models' path.")
  })
} else {
  warning("The 'h2o_models' directory was not found.")
}

# Load CSV Data
if(file.exists("supplementary_data.csv") & file.exists("tracking_df.csv")) {
  plays <- read_csv("supplementary_data.csv", show_col_types = FALSE)
  tracking_df <- read_csv("tracking_df.csv", show_col_types = FALSE)
} else {
  stop("Data files not found!")
}

# Define Colors
team_colors_mapping <- c(
  "ARI" = "#97233F", "ATL" = "#A71930", "BAL" = "#241773",
  "BUF" = "#00338D", "CAR" = "#0085CA", "CHI" = "#0B162A",
  "CIN" = "#FB4F14", "CLE" = "#311D00", "DAL" = "#003594",
  "DEN" = "#FB4F14", "DET" = "#0076B6", "GB" = "#203731",
  "HOU" = "#03202F", "IND" = "#002C5F", "JAX" = "#006778",
  "KC" = "#E31837", "LV" = "#000000", "LAC" = "#2072BA",
  "LA" = "#003594", "MIA" = "#008E97", "MIN" = "#4F2683",
  "NE" = "#0C2340", "NO" = "#D3BC8D", "NYG" = "#0B2265",
  "NYJ" = "#125740", "PHI" = "#004C54", "PIT" = "#FFB612",
  "SEA" = "#002244", "SF" = "#AA0000", "TB" = "#D50A0A",
  "TEN" = "#4B92DB", "WAS" = "#773141"
)

team_secondary_colors <- c(
  "ARI" = "#000000", "ATL" = "#000000", "BAL" = "#9E7C0C",
  "BUF" = "#C60C30", "CAR" = "#101820", "CHI" = "#C83803",
  "CIN" = "#000000", "CLE" = "#FF3C00", "DAL" = "#869397",
  "DEN" = "#002244", "DET" = "#B0B7BC", "GB" = "#FFB612",
  "HOU" = "#A5ACAF", "IND" = "#A2AAAD", "JAX" = "#D7A22A",
  "KC" = "#FFB81C", "LV" = "#A5ACAF", "LAC" = "#FFC20E",
  "LA" = "#FFD100", "MIA" = "#F58220", "MIN" = "#FFC62F",
  "NE" = "#C60C30", "NO" = "#101820", "NYG" = "#A2AAAD",
  "NYJ" = "#FFFFFF", "PHI" = "#A5ACAF", "PIT" = "#101820",
  "SEA" = "#69BE28", "SF" = "#101820", "TB" = "#FF7900",
  "TEN" = "#C8032B", "WAS" = "#FFB612"
)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================

# Function to generate a probability grid around a specific player
generate_catch_grid <- function(player_x, player_y, player_dir, time_left, nfl_id, model) {
  
  # Define grid (Polar) - Increased resolution (by=10) for smoother look
  grid <- expand.grid(
    angle_relative = seq(-90, 90, by = 10), 
    dist = seq(2, 30, by = 2)
  )
  
  grid <- grid %>%
    mutate(
      # ROTATION FIX: 270 offset correctly rotates the cone 180 degrees relative to player
      angle_rad = (270 - (player_dir + angle_relative)) * pi / 180,
      field_x = player_x + dist * cos(angle_rad),
      field_y = player_y + dist * sin(angle_rad),
      
      # Features for H2O
      dist_to_ball_land = dist, 
      angle_diff = abs(angle_relative),
      speed = 0, 
      accel = 0,
      time_left_s = time_left,
      nfl_id = nfl_id
    )
  
  # Predict
  h2o_grid <- as.h2o(grid)
  preds <- h2o.predict(model, h2o_grid)
  grid$prob <- as.vector(preds$p1)
  
  return(grid)
}

# Function to clean and prepare the play dataframe
get_play_df <- function(tracking_df, plays_df, game_id, play_id, only_predict = FALSE) {
  
  play_df <- tracking_df %>%
    filter(game_id == !!game_id, play_id == !!play_id)
  
  if (only_predict) {
    play_df <- play_df %>%
      mutate(player_to_predict = ifelse(player_role == "Passer", TRUE, player_to_predict),
             player_to_predict = ifelse(is.na(player_to_predict), TRUE, player_to_predict)) %>%
      filter(player_to_predict != FALSE)
  }
  
  # Join play info
  play_df <- play_df %>%
    left_join(
      plays_df %>%
        select(game_id, play_id, yards_to_go, yardline_number, play_description, pass_result, possession_team, defensive_team) %>%
        rename(line_of_scrimmage = yardline_number),
      by = c("game_id", "play_id")
    )
  
  # Fill static info
  play_df <- play_df %>%
    group_by(nfl_id) %>%
    fill(
      player_name, player_position, player_role,
      player_height, player_weight,
      possession_team, defensive_team, player_side,
      .direction = "downup"
    ) %>%
    ungroup() %>%
    mutate(
      hover_text = paste0(
        "Name: ", player_name, "<br>",
        "Role: ", player_role, "<br>",
        "Side: ", player_side
      )
    )
  
  # Colors
  play_df <- play_df %>%
    mutate(
      player_team = ifelse(player_side == "Offense", possession_team, defensive_team),
      player_color = team_colors_mapping[player_team],
      player_outline = team_secondary_colors[player_team]
    )
  
  # Global Frame ID
  play_df <- play_df %>%
    group_by(nfl_id) %>%
    arrange(file_type, frame_id) %>%
    mutate(global_frame_id = row_number()) %>%
    ungroup()
  
  # Replicate missing output frames
  output_frames <- play_df %>% filter(file_type == "output") %>% pull(global_frame_id) %>% unique()
  for(nfl in unique(play_df$nfl_id)) {
    missing_frames <- setdiff(output_frames, play_df$global_frame_id[play_df$nfl_id == nfl])
    if(length(missing_frames) > 0) {
      last_input <- play_df %>% filter(nfl_id == nfl) %>% arrange(global_frame_id) %>% slice_tail(n=1)
      replicated <- last_input[rep(1, length(missing_frames)), ]
      replicated$global_frame_id <- missing_frames
      replicated$file_type <- "output"
      play_df <- bind_rows(play_df, replicated)
    }
  }
  
  play_df <- play_df %>% arrange(global_frame_id, nfl_id)
  
  # Physics Calculations
  play_df <- play_df %>%
    group_by(nfl_id) %>%
    arrange(global_frame_id, .by_group = TRUE) %>%
    mutate(
      dx = x - lag(x),
      dy = y - lag(y),
      dt = 0.1,
      speed = sqrt(dx^2 + dy^2) / dt,
      accel = (speed - lag(speed)) / dt,
      # Raw direction calculation
      raw_direction = atan2(dy, dx) * 180 / pi
    ) %>%
    # FIX FOR STUCK VECTORS/DIRECTION:
    # If speed is near zero, raw_direction becomes 0 (East). 
    # We replace 0 with NA and fill forward to keep the player looking in their last valid direction.
    mutate(
      raw_direction = ifelse(speed < 0.5, NA, raw_direction)
    ) %>%
    fill(raw_direction, .direction = "down") %>%
    mutate(
      direction = replace_na(raw_direction, 0), # Default to 0 if start is static
      dx = replace_na(dx, 0), dy = replace_na(dy, 0), dt = replace_na(dt, 0),
      speed = replace_na(speed, 0), accel = replace_na(accel, 0)
    ) %>%
    ungroup()
  
  # --- HEATMAP GENERATION ---
  # Only for Output frames (Ball in air)
  receiver_info <- play_df %>% 
    filter(
      player_role == "Targeted Receiver", 
      file_type == "output"
    ) %>%
    select(global_frame_id, x, y, direction, nfl_id, frame_id) 
  
  if(nrow(receiver_info) > 0 && exists("model_with_id")) {
    grid_frames <- list()
    max_frame <- max(receiver_info$frame_id, na.rm = TRUE)
    
    for(i in 1:nrow(receiver_info)) {
      row <- receiver_info[i,]
      t_left <- if("time_left_s" %in% names(row)) row$time_left_s else (max_frame - row$frame_id) / 10
      
      frame_grid <- generate_catch_grid(
        player_x = row$x, 
        player_y = row$y, 
        player_dir = row$direction, 
        time_left = max(t_left, 0),
        nfl_id = row$nfl_id,
        model = model_with_id
      )
      frame_grid$global_frame_id <- row$global_frame_id
      grid_frames[[i]] <- frame_grid
    }
    
    heatmap_df <- bind_rows(grid_frames)
    attr(play_df, "heatmap_data") <- heatmap_df
  }
  
  return(play_df)
}

prepare_ball <- function(play_df) {
  qb_id <- play_df %>% filter(player_role == "Passer") %>% pull(nfl_id) %>% unique()
  qb_input <- play_df %>% filter(nfl_id == qb_id, file_type == "input") %>% arrange(global_frame_id) %>% select(global_frame_id, x, y)
  ball_end <- play_df %>% filter(player_role == "Passer") %>% select(ball_land_x, ball_land_y) %>% slice(1)
  output_frames <- play_df %>% filter(file_type == "output") %>% pull(global_frame_id) %>% unique()
  
  if(length(output_frames) > 0){
    last_input_frame <- max(qb_input$global_frame_id)
    last_input_pos <- qb_input %>% filter(global_frame_id == last_input_frame)
    n_output <- length(output_frames)
    arrive_frames <- max(n_output - 2, 1)
    
    ball_output <- data.frame(
      global_frame_id = output_frames,
      x = c(seq(last_input_pos$x, ball_end$ball_land_x, length.out = arrive_frames), rep(ball_end$ball_land_x, n_output - arrive_frames)),
      y = c(seq(last_input_pos$y, ball_end$ball_land_y, length.out = arrive_frames), rep(ball_end$ball_land_y, n_output - arrive_frames))
    )
    
    receiver_id <- play_df %>% filter(player_role == "Targeted Receiver") %>% pull(nfl_id) %>% unique()
    if(play_df$pass_result[1] == "C"){
      receiver_path <- play_df %>% filter(nfl_id == receiver_id, file_type == "output") %>% select(global_frame_id, x, y)
      full_frames <- data.frame(global_frame_id = output_frames)
      receiver_path <- full_join(full_frames, receiver_path, by="global_frame_id") %>%
        arrange(global_frame_id) %>%
        mutate(x = zoo::na.approx(x, na.rm = FALSE, rule = 2), y = zoo::na.approx(y, na.rm = FALSE, rule = 2))
      
      land_frame <- min(ball_output$global_frame_id[ball_output$x == ball_end$ball_land_x & ball_output$y == ball_end$ball_land_y])
      ball_output <- ball_output %>% left_join(receiver_path, by="global_frame_id", suffix=c("", "_recv")) %>%
        mutate(x = ifelse(global_frame_id >= land_frame, x_recv, x), y = ifelse(global_frame_id >= land_frame, y_recv, y)) %>%
        select(global_frame_id, x, y)
    }
  } else {
    ball_output <- data.frame(global_frame_id=integer(0), x=numeric(0), y=numeric(0))
  }
  ball <- bind_rows(qb_input, ball_output) %>%
  arrange(global_frame_id) %>%
  tidyr::fill(x, y, .direction = "down")

  return(ball)

}

animate_play_field_with_ball <- function(play_df) {
  
  ball_df <- prepare_ball(play_df)
  heatmap_df <- attr(play_df, "heatmap_data")
  
  field_length <- 120
  field_width <- 53.3
  los <- play_df$absolute_yardline_number[1]
  yards_to_go <- play_df$yards_to_go[1]
  play_direction <- play_df$play_direction[1]
  first_down_marker <- if(play_direction == "right") los + yards_to_go else los - yards_to_go
  
  offense_team <- unique(play_df$possession_team)
  defense_team <- unique(play_df$defensive_team)
  
  if(play_direction == "right") {
    left_endzone_color <- team_colors_mapping[offense_team]
    right_endzone_color <- team_colors_mapping[defense_team]
  } else {
    left_endzone_color <- team_colors_mapping[defense_team]
    right_endzone_color <- team_colors_mapping[offense_team]
  }
  
  ten_yard_lines <- seq(20, 100, 10)
  five_yard_lines <- seq(15, 105, 5)
  
  shapes_list <- c(
    lapply(ten_yard_lines, function(x) list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = field_width, line = list(color = "white", width = 2), layer = "below")),
    lapply(setdiff(five_yard_lines, ten_yard_lines), function(x) list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = field_width, line = list(color = "white", width = 1, dash = "dot"), layer = "below")),
    list(
      list(type="line", x0=los, x1=los, y0=0, y1=field_width, line=list(color="blue", dash = "dash", width=2), layer = "below"),
      list(type="line", x0=first_down_marker, x1=first_down_marker, y0=0, y1=field_width, line=list(color="yellow", dash = "dash", width=2), layer = "below")
    ),
    list(
      list(type="rect", x0=0, x1=10, y0=0, y1=field_width, fillcolor=left_endzone_color, line=list(width=0), layer = "below"),
      list(type="rect", x0=110, x1=120, y0=0, y1=field_width, fillcolor=right_endzone_color, line=list(width=0), layer = "below")
    )
  )
  
  fig <- plot_ly() %>%
    layout(
      title = list(text = paste0("Game ", play_df$game_id[1], " | Play ", play_df$play_id[1]), y = .965, x = 0.5, xanchor = "center", yanchor = "bottom"),
      xaxis=list(range=c(0, field_length), showticklabels=FALSE, zeroline=FALSE),
      yaxis=list(range=c(0, field_width), showticklabels=FALSE, zeroline=FALSE),
      plot_bgcolor="#00B140",
      shapes=shapes_list
    )
  
  # Add Heatmap
  if (!is.null(heatmap_df)) {
    fig <- fig %>% add_trace(
      data = heatmap_df, x = ~field_x, y = ~field_y, frame = ~global_frame_id,
      type = 'scatter', mode = 'markers',
      marker = list(size = 8, symbol = "square", opacity = 0.4, color = ~prob, colorscale = "Viridis", showscale = FALSE),
      hoverinfo = "text", text = ~paste0("Prob: ", round(prob, 2)), showlegend = FALSE
    )
  }
  
  # Vectors - FILTERED AND MASKED TO PREVENT GHOSTING
  arrow_scale <- 10
  vectors_df <- play_df %>%
  mutate(
    x_start = x,
    y_start = y,
    x_end = ifelse(speed > 0.1, x + dx * arrow_scale, x),
    y_end = ifelse(speed > 0.1, y + dy * arrow_scale, y)
    ) %>%
    rowwise() %>% 
    mutate(
      xs = list(c(x_start, x_end, NA)),
      ys = list(c(y_start, y_end, NA))
    ) %>%
    ungroup() %>%
    select(global_frame_id, nfl_id, xs, ys) %>%
    tidyr::unnest(cols = c(xs, ys)) %>%
    rename(x = xs, y = ys)

  
  fig <- fig %>% add_trace(
    data = vectors_df, x = ~x, y = ~y, frame = ~global_frame_id,
    type = 'scatter', mode = 'lines', line = list(color = 'black', width = 2),
    showlegend = FALSE, hoverinfo = 'none', split = ~nfl_id
  )
  
  # Players
  for(player in unique(play_df$nfl_id)) {
    player_df <- play_df %>% filter(nfl_id == player)
    hover_text <- if("reached_ball" %in% names(player_df)) { ~paste0("Pred: ", pred1, "\nReached: ", reached_ball) } else { ~paste0(player_name) }
    
    fig <- fig %>% add_trace(
      data = player_df, x = ~x, y = ~y, frame = ~global_frame_id,
      type = 'scatter', mode = 'markers+text',
      marker = list(size = 15, color = unique(player_df$player_color), line = list(color = unique(player_df$player_outline), width = 2)),
      text = hover_text, textposition = 'bottom center', textfont = list(color = 'black', size = 7),
      showlegend=FALSE, hoverinfo='none'
    )
  }
  
  # Ball (Added to end to ensure it renders on top)
  fig <- fig %>% add_trace(
    data = ball_df, x = ~x, y = ~y, frame = ~global_frame_id,
    type = 'scatter', mode = 'markers', marker = list(size=9, color="#815337", symbol="circle"),
    hoverinfo='none', showlegend=FALSE
  ) %>% animation_opts(frame=100, redraw=FALSE) %>% animation_slider(currentvalue=list(prefix="Frame: ")) %>% animation_button(label = "Play")
  
  return(fig)
}

# ==============================================================================
# 3. UI & SERVER
# ==============================================================================

ui <- fluidPage(
  titlePanel("NFL Play Animation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("week", "Select Week", choices = if(exists("plays")) sort(unique(plays$week)) else NULL),
      selectInput("game", "Select Game", choices = NULL),
      selectInput("play", "Select Play", choices = NULL),
      checkboxInput("only_predict", "Show Only Key Players", value = TRUE),
      actionButton("generate_btn", "Generate Play Animation")
    ),
    mainPanel(
      plotlyOutput("play_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    req(input$week)
    games_for_week <- plays %>% filter(week == input$week) %>%
      select(game_id, home_team_abbr, visitor_team_abbr) %>% distinct() %>%
      mutate(label = paste0(visitor_team_abbr, " @ ", home_team_abbr))
    updateSelectInput(session, "game", choices = setNames(games_for_week$game_id, games_for_week$label))
  })
  
  observe({
    req(input$game)
    plays_for_game <- plays %>% arrange(play_id) %>% filter(game_id == input$game) %>% pull(play_description)
    updateSelectInput(session, "play", choices = plays_for_game)
  })
  
  play_data <- eventReactive(input$generate_btn, {
    req(input$game, input$play)
    withProgress(message = 'Calculating Catch Probabilities...', value = 0, {
      play_id <- plays %>% filter(game_id == input$game, play_description == input$play) %>% pull(play_id) %>% .[1]
      incProgress(0.2, detail = "Loading Tracking Data")
      df <- get_play_df(tracking_df, plays, game_id = input$game, play_id = play_id, only_predict = input$only_predict)
      incProgress(0.8, detail = "Rendering Plot")
      return(df)
    })
  })
  
  output$play_plot <- renderPlotly({
    req(play_data())
    animate_play_field_with_ball(play_data())
  })
}

shinyApp(ui, server)
