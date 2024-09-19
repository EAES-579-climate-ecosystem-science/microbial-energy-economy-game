# app.R

# Load necessary libraries
library(shiny)
library(tidyverse)

# Define initial game setup based on scenario
setup_game <- function(scenario) {
  
  # Common starting structure for all scenarios
  players <- list(
    "oxygen" = list(type = "acceptor", ATP_per_electron = 3, available = ifelse(scenario == 1, Inf, 3)),
    "nitrate" = list(type = "acceptor", ATP_per_electron = 2, available = ifelse(scenario == 1, 0, 5)),
    "iron" = list(type = "acceptor", ATP_per_electron = 1, available = ifelse(scenario == 1, 0, 5)),
    "sulfur" = list(type = "acceptor", ATP_per_electron = 1, available = ifelse(scenario == 1, 0, 5)),
    "CO2" = list(type = "acceptor", ATP_per_electron = 1, available = 0, every_other_turn = TRUE),
    "cellulose" = list(type = "donor", available = ifelse(scenario == 1, 3, 0), can_use = FALSE),
    "glucose" = list(type = "donor", available = ifelse(scenario == 1, 3, 3), can_use = TRUE),
    "acetate" = list(type = "donor", available = 0, can_use = TRUE),
    "lactic_acid" = list(type = "donor", available = 0, can_use = TRUE),
    "hydrogen" = list(type = "donor", available = 0, can_use = TRUE)
  )
  
  # Heterotroph starting state with 6 ATP
  heterotroph <- list(ATP = 6, evolution_stage = 0, count = 1, alive = TRUE, evolved_paths = list(growth = FALSE, exo_enzymes = FALSE, anaerobiosis = FALSE, fermentation = FALSE))
  
  return(list(players = players, heterotroph = heterotroph, turn_counter = 1, scenario = scenario))
}

# Perform electron transfer and ATP generation
perform_electron_transfer <- function(game_state, donor, acceptor) {
  
  # Check if the donor can be used
  if (!game_state$players[[donor]]$can_use || game_state$players[[donor]]$available <= 0) {
    return(game_state)  # Donor cannot be used or is exhausted
  }
  
  if (game_state$players[[acceptor]]$available <= 0) {
    return(game_state)  # Acceptor exhausted
  }
  
  # Handle CO2 special case
  if (acceptor == "CO2" && game_state$turn_counter %% 2 == 0) {
    return(game_state)  # CO2 can only be used every other turn
  }
  
  # Transfer the electron and generate ATP
  game_state$players[[donor]]$available <- game_state$players[[donor]]$available - 1
  game_state$players[[acceptor]]$available <- game_state$players[[acceptor]]$available - 1
  ATP_generated <- game_state$players[[acceptor]]$ATP_per_electron
  
  # Divide ATP among heterotrophs and check for deaths
  total_heterotrophs <- game_state$heterotroph$count
  ATP_per_heterotroph <- floor(ATP_generated / total_heterotrophs)
  remaining_ATP <- ATP_generated %% total_heterotrophs
  
  # Only allocate newly generated ATP
  if (ATP_per_heterotroph == 0) {
    # Some heterotrophs will die if there isn't enough ATP
    heterotrophs_to_die <- total_heterotrophs - remaining_ATP
    game_state$heterotroph$count <- max(0, game_state$heterotroph$count - heterotrophs_to_die)
  }
  
  if (game_state$heterotroph$count == 0) {
    game_state$heterotroph$alive <- FALSE  # All heterotrophs died
  } else {
    game_state$heterotroph$ATP <- game_state$heterotroph$ATP + ATP_generated  # Add ATP to the community
  }
  
  return(game_state)
}

# Evolve heterotroph based on path chosen
evolve_heterotroph <- function(game_state, evolution_path) {
  
  if (game_state$heterotroph$ATP < 6) {
    return(game_state)  # Not enough ATP
  }
  
  if (evolution_path == "growth") {
    # Growth ability can be used multiple times to double the number of heterotrophs
    game_state$heterotroph$ATP <- game_state$heterotroph$ATP - 6
    game_state$heterotroph$count <- game_state$heterotroph$count * 2  # Double heterotroph count each time
    return(game_state)
  }
  
  if (game_state$heterotroph$evolved_paths[[evolution_path]]) {
    return(game_state)  # Already evolved in this path
  }
  
  # Spend ATP and evolve in chosen path
  game_state$heterotroph$ATP <- game_state$heterotroph$ATP - 6
  game_state$heterotroph$evolved_paths[[evolution_path]] <- TRUE
  
  # Evolution effects
  if (evolution_path == "exo_enzymes") {
    # Allow cellulose to be broken down into glucose
    game_state$players$cellulose$can_use <- TRUE
    game_state$players$glucose$available <- game_state$players$glucose$available + (3 * game_state$players$cellulose$available)
    game_state$players$cellulose$available <- 0  # All cellulose converted to glucose
    
  } else if (evolution_path == "anaerobiosis") {
    # Allows anaerobic respiration: unlocks nitrate, iron, and sulfur usage
    game_state$players$nitrate$available <- 5
    game_state$players$iron$available <- 5
    game_state$players$sulfur$available <- 5
    
  } else if (evolution_path == "fermentation") {
    # Allows fermentation: glucose -> lactic acid -> acetate -> hydrogen
    game_state$players$lactic_acid$available <- game_state$players$glucose$available
    game_state$players$acetate$available <- game_state$players$lactic_acid$available
    game_state$players$hydrogen$available <- game_state$players$acetate$available
  }
  
  return(game_state)
}

# Check if the game is over (win or lose conditions)
check_game_over <- function(game_state) {
  total_donors <- sum(sapply(game_state$players, function(p) if (p$type == "donor") p$available else 0))
  
  if (total_donors == 0) {
    # All donors consumed, win condition
    return(list(game_over = TRUE, win = TRUE))
  }
  
  if (!game_state$heterotroph$alive) {
    # All heterotrophs died, lose condition
    return(list(game_over = TRUE, win = FALSE))
  }
  
  return(list(game_over = FALSE))
}

# Define UI for the app
ui <- fluidPage(
  titlePanel("Microbial Energy Economy Game"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario", "Select Scenario", choices = c("1: Unlimited Oxygen", "2: Oxygen Limiting", "3: Oxygen Pulse", "4: Photoautotroph")),
      actionButton("start_btn", "Start Game"),
      br(),
      selectInput("donor", "Choose an Electron Donor", choices = c("cellulose", "glucose", "acetate", "lactic_acid", "hydrogen")),
      selectInput("acceptor", "Choose an Electron Acceptor", choices = c("oxygen", "nitrate", "iron", "sulfur", "CO2")),
      actionButton("transfer_btn", "Transfer Electron"),
      br(),
      selectInput("evolution_path", "Choose Evolution Path", choices = c("growth", "exo_enzymes", "anaerobiosis", "fermentation")),
      actionButton("evolve_btn", "Evolve Heterotroph"),
      h3("Heterotroph Status"),
      textOutput("heterotroph_status"),
      h3("Game Status"),
      textOutput("game_status")
    ),
    
    mainPanel(
      h3("Game State"),
      textOutput("turn_counter"),
      h4("Electron Donors"),
      tableOutput("donors_table"),
      h4("Electron Acceptors"),
      tableOutput("acceptors_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  game_state <- reactiveVal(NULL)
  
  observeEvent(input$start_btn, {
    scenario <- as.integer(sub(":.*", "", input$scenario))  # Extract scenario number
    game_state(setup_game(scenario))
  })
  
  observeEvent(input$transfer_btn, {
    new_state <- perform_electron_transfer(game_state(), input$donor, input$acceptor)
    new_state$turn_counter <- new_state$turn_counter + 1
    game_state(new_state)
    
    # Check if the game is over
    result <- check_game_over(new_state)
    if (result$game_over) {
      if (result$win) {
        showModal(modalDialog(title = "Game Over", "Congratulations! You have won the game.", easyClose = TRUE))
      } else {
        showModal(modalDialog(title = "Game Over", "Game Over! All heterotrophs have died.", easyClose = TRUE))
      }
    }
  })
  
  observeEvent(input$evolve_btn, {
    game_state(evolve_heterotroph(game_state(), input$evolution_path))
  })
  
  # Output the game state
  output$turn_counter <- renderText({
    if (is.null(game_state())) return("Turn: 0")
    paste("Turn:", game_state()$turn_counter)
  })
  
  output$donors_table <- renderTable({
    if (is.null(game_state())) return(NULL)
    donors <- game_state()$players
    data.frame(
      Donor = c("Cellulose", "Glucose", "Acetate", "Lactic Acid", "Hydrogen"),
      Available = c(donors$cellulose$available, donors$glucose$available, donors$acetate$available, donors$lactic_acid$available, donors$hydrogen$available)
    )
  })
  
  output$acceptors_table <- renderTable({
    if (is.null(game_state())) return(NULL)
    acceptors <- game_state()$players
    data.frame(
      Acceptor = c("Oxygen", "Nitrate", "Iron", "Sulfur", "CO2"),
      Available = c(acceptors$oxygen$available, acceptors$nitrate$available, acceptors$iron$available, acceptors$sulfur$available, acceptors$CO2$available)
    )
  })
  
  output$heterotroph_status <- renderText({
    if (is.null(game_state())) return("ATP: 0 | Evolution Stage: 0 | Paths: None | Heterotrophs: 1 (Alive)")
    ht <- game_state()$heterotroph
    status <- ifelse(ht$alive, "Alive", "Dead")
    paths <- names(which(ht$evolved_paths == TRUE))
    paste("ATP:", ht$ATP, "| Evolution Stage:", length(paths), "| Paths Evolved:", ifelse(length(paths) == 0, "None", paste(paths, collapse = ", ")), "| Heterotrophs:", ht$count, "(", status, ")")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)