library(shiny)
library(tidyverse)
library(plotly)


# -----------------------------
# Load and prepare data
# -----------------------------
plays <- read_csv("data/supplementary_data.csv") %>% 
  filter(season == 2023)
tracking_df <- read_csv("data/tracking_df.csv")

#plays <- read_csv("play_animation/data/supplementary_data.csv")
#tracking_df <- read_csv("play_animation/data/tracking_df.csv")

team_colors_mapping <- c(
  "ARI" = "#97233F", "ATL" = "#A71930", "BAL" = "#241773",
  "BUF" = "#00338D", "CAR" = "#0085CA", "CHI" = "#0B162A",
  "CIN" = "#FB4F14", "CLE" = "#311D00", "DAL" = "#003594",
  "DEN" = "#FB4F14", "DET" = "#0076B6", "GB" = "#203731",
  "HOU" = "#03202F", "IND" = "#002C5F", "JAX" = "#006778",
  "KC" = "#E31837", "LV" = "#000000", "LAC" = "#2072BA",
  "LA" = "#866D4B", "MIA" = "#008E97", "MIN" = "#4F2683",
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


get_play_df <- function(tracking_df, plays_df, game_id, play_id, only_predict = FALSE) {
  
  play_df <- tracking_df %>%
    filter(game_id == !!game_id, play_id == !!play_id)
  
  # Keep passer always; optionally filter other players
  if (only_predict) {
    play_df <- play_df %>%
      mutate(player_to_predict = ifelse(player_role == "Passer", TRUE, player_to_predict),
             player_to_predict = ifelse(is.na(player_to_predict), TRUE, player_to_predict)) %>%
      filter(player_to_predict != FALSE)
  }
  
  # Join essential play info
  play_df <- play_df %>%
    left_join(
      plays_df %>%
        select(game_id, play_id, yards_to_go, yardline_number, play_description, pass_result, possession_team, defensive_team) %>%
        rename(line_of_scrimmage = yardline_number),
      by = c("game_id", "play_id")
    )
  
  play_df <- play_df %>%
    group_by(nfl_id) %>%
    fill(
      player_name, player_position, player_role,
      player_height, player_weight,
      possession_team, defensive_team, player_side,
      .direction = "downup"   # fills both ways, so no NAs
    ) %>%
    ungroup() %>%
    mutate(
      hover_text = paste0(
        "Name: ", player_name, "<br>",
        "Position: ", player_position, "<br>",
        "Role: ", player_role, "<br>",
        "Height: ", player_height, "<br>",
        "Weight: ", player_weight, "<br>",
        "Side: ", player_side
      )
    )
  
  
  # Assign team colors
  play_df <- play_df %>%
    mutate(
      player_team = ifelse(player_side == "Offense", possession_team, defensive_team),
      player_color = team_colors_mapping[player_team],
      player_outline = team_secondary_colors[player_team]   # <-- secondary color
    )
  
  # Ensure global frame ID is sequential
  play_df <- play_df %>%
    group_by(nfl_id) %>%
    arrange(file_type, frame_id) %>%
    mutate(global_frame_id = row_number()) %>%
    ungroup()
  
  # Replicate missing output frames for any player absent in output
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
  
  
  # Sort final play_df
  play_df <- play_df %>% arrange(global_frame_id, nfl_id)
  
  # Compute speed, acceleration, and direction for each player
  play_df <- play_df %>%
    group_by(nfl_id) %>%
    arrange(global_frame_id, .by_group = TRUE) %>%
    mutate(
      dx = x - lag(x),
      dy = y - lag(y),
      dt = 0.1,
      speed = sqrt(dx^2 + dy^2) / dt,
      accel = (speed - lag(speed)) / dt,
      direction = atan2(dy, dx) * 180 / pi
    ) %>%
    ungroup() %>%
    mutate(
      dx = replace_na(dx, 0),
      dy = replace_na(dy, 0),
      dt = replace_na(dt, 0),
      speed = replace_na(speed, 0),
      accel = replace_na(accel, 0),
      direction = replace_na(direction, 0)
    )
  
  
  
  
  
  
  return(play_df)
}

# -----------------------------
# 4. Prepare ball trajectory
# -----------------------------
prepare_ball <- function(play_df) {
  
  qb_id <- play_df %>% filter(player_role == "Passer") %>% pull(nfl_id) %>% unique()

  # QB positions in input frames
  qb_input <- play_df %>%
    filter(nfl_id == qb_id, file_type == "input") %>%
    arrange(global_frame_id) %>%
    select(global_frame_id, x, y)
  
  # Ball landing position
  ball_end <- play_df %>%
    filter(player_role == "Passer") %>%
    select(ball_land_x, ball_land_y) %>%
    slice(1)
  
  # Output frames
  output_frames <- play_df %>% filter(file_type == "output") %>% pull(global_frame_id) %>% unique()
  
  if(length(output_frames) > 0){
    last_input_frame <- max(qb_input$global_frame_id)
    last_input_pos <- qb_input %>% filter(global_frame_id == last_input_frame)
    
    n_output <- length(output_frames)
    
    # Ball arrives 2 frames early
    arrive_frames <- max(n_output - 2, 1)
    
    # Ball path until landing
    ball_output <- data.frame(
      global_frame_id = output_frames,
      x = c(seq(last_input_pos$x, ball_end$ball_land_x, length.out = arrive_frames),
            rep(ball_end$ball_land_x, n_output - arrive_frames)),
      y = c(seq(last_input_pos$y, ball_end$ball_land_y, length.out = arrive_frames),
            rep(ball_end$ball_land_y, n_output - arrive_frames))
    )
    
    # Attach ball to receiver after landing if completed pass
    receiver_id <- play_df %>%
      filter(player_role == "Targeted Receiver") %>%
      pull(nfl_id) %>% unique()
    
    if(play_df$pass_result[1] == "C"){
      receiver_path <- play_df %>%
        filter(nfl_id == receiver_id, file_type == "output") %>%
        select(global_frame_id, x, y)
      
      # Ensure continuous receiver path
      full_frames <- data.frame(global_frame_id = output_frames)
      receiver_path <- full_join(full_frames, receiver_path, by="global_frame_id") %>%
        arrange(global_frame_id) %>%
        mutate(
          x = zoo::na.approx(x, na.rm = FALSE, rule = 2),
          y = zoo::na.approx(y, na.rm = FALSE, rule = 2)
        )
      
      # Replace ball positions after landing with receiver positions
      land_frame <- min(ball_output$global_frame_id[ball_output$x == ball_end$ball_land_x &
                                                      ball_output$y == ball_end$ball_land_y])
      
      ball_output <- ball_output %>%
        left_join(receiver_path, by="global_frame_id", suffix=c("", "_recv")) %>%
        mutate(
          x = ifelse(global_frame_id >= land_frame, x_recv, x),
          y = ifelse(global_frame_id >= land_frame, y_recv, y)
        ) %>%
        select(global_frame_id, x, y)
    }
    
  } else {
    ball_output <- data.frame(global_frame_id=integer(0), x=numeric(0), y=numeric(0))
  }
  
  # Combine QB movement + ball flight/following receiver
  ball_df <- bind_rows(qb_input, ball_output)
  
  return(ball_df)
}





# -----------------------------
# 5. Animate play
# -----------------------------
animate_play_field_with_ball <- function(play_df) {

  ball_df <- prepare_ball(play_df)

  # -----------------------------
  # Field setup
  # -----------------------------
  field_length <- 120
  field_width <- 53.3
  los <- play_df$absolute_yardline_number[1]
  yards_to_go <- play_df$yards_to_go[1]
  play_direction <- play_df$play_direction[1]
  first_down_marker <- if(play_direction == "right") los + yards_to_go else los - yards_to_go

  # Endzone colors
  offense_team <- unique(play_df$possession_team)
  defense_team <- unique(play_df$defensive_team)
  offense_color <- team_colors_mapping[offense_team]
  defense_color <- team_colors_mapping[defense_team]

  if(play_direction == "right") {
    left_endzone_color <- offense_color
    right_endzone_color <- defense_color
    left_endzone_text <- offense_team
    right_endzone_text <- defense_team
  } else {
    left_endzone_color <- defense_color
    right_endzone_color <- offense_color
    left_endzone_text <- defense_team
    right_endzone_text <- offense_team
  }

  # Yard lines
  ten_yard_lines <- seq(20, 100, 10)
  five_yard_lines <- seq(15, 105, 5)

  shapes_list <- c(
    # Ten-yard lines
    lapply(ten_yard_lines, function(x) list(
      type = "line", x0 = x, x1 = x, y0 = 0, y1 = field_width,
      line = list(color = "white", width = 2), layer = "below"
    )),
    # Five-yard lines (lighter)
    lapply(setdiff(five_yard_lines, ten_yard_lines), function(x) list(
      type = "line", x0 = x, x1 = x, y0 = 0, y1 = field_width,
      line = list(color = "white", width = 1, dash = "dot"), layer = "below"
    )),
    # Line of scrimmage & first down
    list(
      list(type="line", x0=los, x1=los, y0=0, y1=field_width, line=list(color="blue", dash = "dash", width=2), layer = "below"),
      list(type="line", x0=first_down_marker, x1=first_down_marker, y0=0, y1=field_width, line=list(color="yellow", dash = "dash", width=2), layer = "below")
    ),
    # Endzones
    list(
      list(type="rect", x0=0, x1=10, y0=0, y1=field_width, fillcolor=left_endzone_color, line=list(width=0), layer = "below"),
      list(type="rect", x0=110, x1=120, y0=0, y1=field_width, fillcolor=right_endzone_color, line=list(width=0), layer = "below")
    )
  )

  # Field numbers
  annotations_list <- lapply(ten_yard_lines, function(x) {
    yard_num <- ifelse(x <= 50, x - 10, 110 - x)
    list(
      x = x, y = field_width*0.05,
      text = as.character(yard_num),
      showarrow = FALSE,
      font = list(color="white", size=20),
      xref="x", yref="y",
      xanchor="center", yanchor="middle"
    )
  })
  

  # Hash marks
  left_hash <- 70.75 / 3
  right_hash <- field_width - 70.75 / 3
  hash_shapes <- list(
    list(type="line", x0=10, x1=110, y0=left_hash, y1=left_hash, line=list(color="white", width=1, dash="dot"), layer = "below"),
    list(type="line", x0=10, x1=110, y0=right_hash, y1=right_hash, line=list(color="white", width=1, dash="dot"), layer = "below")
  )
  shapes_list <- c(shapes_list, hash_shapes)

  # -----------------------------
  # Build velocity vectors
  # -----------------------------
  arrow_scale <- 10
  vectors_df <- play_df %>%
    mutate(
      dx = ifelse(is.na(dx), 0, dx),
      dy = ifelse(is.na(dy), 0, dy),
      xend = x + dx * arrow_scale,
      yend = y + dy * arrow_scale
    ) %>%
    rowwise() %>%
    mutate(
      xs = list(c(x, xend, NA)),
      ys = list(c(y, yend, NA))
    ) %>%
    ungroup() %>%
    select(global_frame_id, nfl_id, xs, ys) %>%
    tidyr::unnest(cols = c(xs, ys)) %>%
    rename(x = xs, y = ys)
  
  play_df <- play_df %>%
    mutate(speed_text = round(speed * 3600 / 1760, 1))  # convert yards/sec to mph
  
  
  

  # -----------------------------
  # Build base figure
  # -----------------------------
  fig <- plot_ly() %>%
    layout(
      title = list(text = paste0("Game ", play_df$game_id[1], " | Play ", play_df$play_id[1]),
                   y = .965, x = 0.5, xanchor = "center", yanchor = "bottom"),
      xaxis=list(range=c(0, field_length), showticklabels=FALSE, zeroline=FALSE),
      yaxis=list(range=c(0, field_width), showticklabels=FALSE, zeroline=FALSE),
      plot_bgcolor="#00B140",
      shapes=shapes_list,
      annotations=c(list(list(x=5, y=field_width/2, text=left_endzone_text, showarrow=FALSE, font=list(color="white", size=14)),
                         list(x=115, y=field_width/2, text=right_endzone_text, showarrow=FALSE, font=list(color="white", size=14))),
                    annotations_list)
    )

  # -----------------------------
  # Add velocity vectors first (behind markers)
  # -----------------------------
  fig <- fig %>%
    add_trace(
      data = vectors_df,
      x = ~x, y = ~y,
      frame = ~global_frame_id,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'black', width = 2),
      showlegend = FALSE,
      hoverinfo = 'none',
      split = ~nfl_id
    )
  
 
  # -----------------------------
  # Add player markers
  # -----------------------------
  for(player in unique(play_df$nfl_id)) {
    player_df <- play_df %>% filter(nfl_id == player)
    fig <- fig %>%
      add_trace(
        data = player_df,
        x = ~x, y = ~y,
        frame = ~global_frame_id,
        type = 'scatter',
        mode = 'markers+text',
        marker = list(
          size = 15,
          color = unique(player_df$player_color),
          line = list(color = unique(player_df$player_outline), width = 2)  # secondary color as outline
        ),
        text = ~speed_text,
        textposition = 'bottom center',  # puts number on top of the marker
        textfont = list(color = 'black', size = 7),  # <-- set color and size here
        showlegend=FALSE,
        hoverinfo='none'
      )
  }
  



  # -----------------------------
  # Add ball on top
  # -----------------------------
  fig <- fig %>%
    add_trace(
      data = ball_df,
      x = ~x, y = ~y,
      frame = ~global_frame_id,
      type = 'scatter',
      mode = 'markers',
      marker = list(size=9, color="#815337", symbol="circle"),
      hoverinfo='none',
      showlegend=FALSE
    ) %>%
    animation_opts(frame=100, redraw=FALSE) %>%
    animation_slider(currentvalue=list(prefix="Frame: ")) %>%
    animation_button(label = "Play")
  

  return(fig)
}

example_play <- get_play_df(tracking_df, plays,
                            game_id, play_id, only_predict = T)

#print(colnames(example_play))

animate_play_field_with_ball(example_play)








ui <- fluidPage(
  titlePanel("NFL Play Animation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("week", "Select Week", choices = sort(unique(plays$week))),
      selectInput("game", "Select Game", choices = NULL),
      selectInput("play", "Select Play", choices = NULL),
      checkboxInput("only_predict", "Show Only Key Players", value = TRUE),  # <-- new checkbox
      actionButton("generate_btn", "Generate Play Animation")
    ),
    
    mainPanel(
      plotlyOutput("play_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Update games based on week
  observe({
    req(input$week)
    games_for_week <- plays %>%
      filter(week == input$week) %>%
      select(game_id, home_team_abbr, visitor_team_abbr) %>%
      distinct() %>%
      mutate(label = paste0(visitor_team_abbr, " @ ", home_team_abbr))  # format: AWAY @ HOME
    updateSelectInput(session, "game", choices = setNames(games_for_week$game_id, games_for_week$label))
  })
  
  # Update plays based on game
  observe({
    req(input$game)
    plays_for_game <- plays %>%
      arrange(play_id) %>% 
      filter(game_id == input$game) %>%
      pull(play_description)
    updateSelectInput(session, "play", choices = plays_for_game)
  })
  
  # Only generate play when button is pressed
  play_data <- eventReactive(input$generate_btn, {
    req(input$game, input$play)
    
    # Get play_id from description
    play_id <- plays %>%
      filter(game_id == input$game, play_description == input$play) %>%
      pull(play_id) %>% .[1]
    
    # Get the tracking data for the selected play
    get_play_df(
      tracking_df, 
      plays, 
      game_id = input$game, 
      play_id = play_id,
      only_predict = input$only_predict  # <-- use checkbox value
    )
  })
  
  
  # Render animation
  output$play_plot <- renderPlotly({
    req(play_data())
    animate_play_field_with_ball(play_data())
  })
}



shinyApp(ui, server)

