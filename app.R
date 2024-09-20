# app.R

# Load necessary libraries
library(shiny)
library(tidyverse)

# Define initial game setup based on scenario
setup_game <- function(scenario) {
  
  # Common starting structure for all scenarios
  players <- list(
    "oxygen" = list(type = "acceptor", ATP_per_electron = 3, available = ifelse(scenario == 1, 999, 3)),
    "nitrate" = list(type = "acceptor", ATP_per_electron = 3, available = ifelse(scenario == 1, 0, 5)),
    "iron" = list(type = "acceptor", ATP_per_electron = 2, available = ifelse(scenario == 1, 0, 5)),
    "sulfur" = list(type = "acceptor", ATP_per_electron = 1, available = ifelse(scenario == 1, 0, 5)),
    "CO2" = list(type = "acceptor", ATP_per_electron = 1, available = 0, every_other_turn = TRUE),
    "cellulose" = list(type = "donor", available = ifelse(scenario == 1, 3, 0), can_use = FALSE),
    "glucose" = list(type = "donor", available = ifelse(scenario == 1, 3, 3), can_use = TRUE),
    "acetate" = list(type = "donor", available = 0, can_use = TRUE),
    "org_acid" = list(type = "donor", available = 0, can_use = TRUE),
    "hydrogen" = list(type = "donor", available = 0, can_use = TRUE)
  )
  
  # Heterotroph starting state with 0 ATP
  heterotroph <- list(ATP = 0, evolution_stage = 0, count = 1, alive = TRUE, evolved_paths = list(growth = FALSE, exo_enzymes = FALSE, anaerobiosis = FALSE, fermentation = FALSE))
  
  return(list(players = players, heterotroph = heterotroph, turn_counter = 1, scenario = scenario))
}

# Perform electron transfer and ATP generation
perform_electron_transfer <- function(game_state, donor, acceptor) {
  
  # Check if cellulose can be used (only after exo-enzymes evolve)
  if (donor == "cellulose" && !game_state$heterotroph$evolved_paths$exo_enzymes) {
    return(game_state)  # Exo-enzymes not evolved yet, cellulose cannot be used
  }
  
  # Determine the number of electron transfers that can occur
  total_heterotrophs <- game_state$heterotroph$count
  available_donors <- game_state$players[[donor]]$available
  available_acceptors <- game_state$players[[acceptor]]$available
  
  transfers <- min(total_heterotrophs, available_donors, available_acceptors)
  
  if (transfers == 0) {
    return(game_state)  # No transfers possible
  }
  
  # Transfer the electrons and generate ATP
  game_state$players[[donor]]$available <- game_state$players[[donor]]$available - transfers
  game_state$players[[acceptor]]$available <- game_state$players[[acceptor]]$available - transfers
  ATP_generated <- game_state$players[[acceptor]]$ATP_per_electron * transfers
  
  # Add ATP to the heterotroph community
  game_state$heterotroph$ATP <- game_state$heterotroph$ATP + ATP_generated
  
  return(game_state)
}

# Consume ATP for maintenance respiration
consume_maintenance_ATP <- function(game_state) {
  total_heterotrophs <- game_state$heterotroph$count
  required_ATP <- total_heterotrophs  # One ATP per microbe
  
  # If not enough ATP is available, heterotrophs die
  if (game_state$heterotroph$ATP < required_ATP) {
    heterotrophs_to_die <- total_heterotrophs - game_state$heterotroph$ATP
    game_state$heterotroph$count <- max(0, game_state$heterotroph$count - heterotrophs_to_die)
    game_state$heterotroph$ATP <- 0  # Use up all remaining ATP
  } else {
    game_state$heterotroph$ATP <- game_state$heterotroph$ATP - required_ATP  # Deduct ATP for maintenance
  }
  
  # If no heterotrophs remain alive
  if (game_state$heterotroph$count == 0) {
    game_state$heterotroph$alive <- FALSE
  }
  
  return(game_state)
}

# Evolve heterotroph based on path chosen
evolve_heterotroph <- function(game_state, evolution_path) {
  
  if (game_state$heterotroph$ATP < 3) {  # Minimum threshold is 3 ATP
    return(game_state)  # Not enough ATP
  }
  
  if (evolution_path == "growth") {
    # Growth ability can be used multiple times to double the number of heterotrophs
    game_state$heterotroph$ATP <- game_state$heterotroph$ATP - 3
    game_state$heterotroph$count <- game_state$heterotroph$count * 2  # Double heterotroph count each time
    return(game_state)
  }
  
  if (game_state$heterotroph$evolved_paths[[evolution_path]]) {
    return(game_state)  # Already evolved in this path
  }
  
  # Spend ATP and evolve in chosen path
  game_state$heterotroph$ATP <- game_state$heterotroph$ATP - 3
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
    # Allows fermentation: glucose -> org_acid -> acetate -> hydrogen
    game_state$players$org_acid$available <- game_state$players$glucose$available
    game_state$players$acetate$available <- game_state$players$org_acid$available
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
      uiOutput("donor_ui"), # Dynamically update the electron donor UI
      selectInput("acceptor", "Choose an Electron Acceptor", choices = c("oxygen", "nitrate", "iron", "sulfur", "CO2")),
      actionButton("transfer_btn", "Transfer Electron"),
      br(),
      selectInput("evolution_path", "Choose Evolution Path", choices = c("growth", "exo_enzymes", "anaerobiosis", "fermentation")),
      actionButton("evolve_btn", "Evolve Heterotroph"),
      h3("Heterotroph Status"),
      textOutput("heterotroph_status"),
      h3("Game Status"),
      textOutput("game_status"),
      imageOutput("game_image")  # Add this to display the image
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
    
    # Update electron donor choices dynamically
    donors <- names(game_state()$players)[sapply(game_state()$players, function(p) p$type == "donor")]
    updateSelectInput(session, "donor", choices = donors)
  })
  
  output$game_image <- renderImage({
    # Provide the path to the image you want to display
    list(src = "https://github.com/EAES-579-climate-ecosystem-science/microbial-energy-economy-game/blob/b4e2e48f57c0da3fbd08bf4a258b879c1a5ead46/Microbial%20Energy%20Economy%20PDF.png", 
         contentType = 'image/png', 
         alt = "Game State")
  }, deleteFile = FALSE)
  
  output$turn_counter <- renderText({
    if (is.null(game_state())) return("Turn: 0")
    paste("Turn:", game_state()$turn_counter)
  })
  
  output$donors_table <- renderTable({
    if (is.null(game_state())) return(NULL)
    donors <- game_state()$players
    data.frame(
      Donor = c("Cellulose", "Glucose", "Acetate", "Org Acid", "Hydrogen"),
      Available = c(as.integer(donors$cellulose$available),
                               as.integer(donors$glucose$available),
                                                     as.integer(donors$acetate$available),
                                                                as.integer(donors$org_acid$available),
                                                                           as.integer(donors$hydrogen$available))
    )
  })
  
  output$acceptors_table <- renderTable({
    if (is.null(game_state())) return(NULL)
    acceptors <- game_state()$players
    data.frame(
      Acceptor = c("Oxygen", "Nitrate", "Iron", "Sulfur", "CO2"),
      Available = c(as.integer(acceptors$oxygen$available),
                               as.integer(acceptors$nitrate$available),
                                          as.integer(acceptors$iron$available),
                                                     as.integer(acceptors$sulfur$available),
                                                                as.integer(acceptors$CO2$available))
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