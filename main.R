# Define the Player class
Player <- function(name) {
  list(
    name = name,
    last_choice = NULL,
    last_opponent_choice = NULL,
    class = "Player"
  )
}

# Method to update the player's memory
update_memory <- function(player, choice, opponent_choice) {
  player$last_choice <- choice
  player$last_opponent_choice <- opponent_choice
  return(player)
}

# Method to make a decision for the next game (random choice in this example)
make_decision <- function(player) {
  print(paste('Player ', player$name))
  choices <- c("Cooperate", "Defect")
  decision <- sample(choices, 1)
  print(paste('decision', decision))
  return(decision)
}

# Simulate a game between two players
simulate_game <- function(player1, player2) {
  player1_choice <- make_decision(player1)

  player2_choice <- make_decision(player2)

  # Update memories after the game
  player1 <- update_memory(player1, player1_choice, player2_choice)
  player2 <- update_memory(player2, player2_choice, player1_choice)

  # Calculate scores
  scores <- calculate_scores(player1_choice, player2_choice)

  list(
    player1 = player1,
    player2 = player2,
    player1_choice = player1_choice,
    player2_choice = player2_choice,
    scores = scores
  )
}


# Create two players
player1 <- Player("Alice")
player2 <- Player("Bob")

# Initialize variables to keep track of the tournament
tournament_results <- data.frame(
  Game = integer(),
  Player1 = character(),
  Player2 = character(),
  Player1_Choice = character(),
  Player2_Choice = character(),
  Player1_Score = integer(),
  Player2_Score = integer(),
  Cumulative_Player1_Score = integer(),
  Cumulative_Player2_Score = integer(),
  stringsAsFactors = FALSE
)

# Play a tournament of 10 games
cumulative_player1_score <- 0
cumulative_player2_score <- 0
for (game in 1:10) {
  game_result <- simulate_game(player1, player2)

  # Store the results of the current game
  tournament_results <- rbind(
    tournament_results,
    data.frame(
      Game = game,
      Player1 = player1$name,
      Player2 = player2$name,
      Player1_Choice = game_result$player1_choice,
      Player2_Choice = game_result$player2_choice,
      Player1_Score = game_result$scores$player1_score,
      Player2_Score = game_result$scores$player2_score,
      Cumulative_Player1_Score = cumulative_player1_score + game_result$scores$player1_score,
      Cumulative_Player2_Score = cumulative_player2_score + game_result$scores$player2_score
    )
  )

  # Update cumulative scores
  cumulative_player1_score <- cumulative_player1_score + game_result$scores$player1_score
  cumulative_player2_score <- cumulative_player2_score + game_result$scores$player2_score

  # Update players with new states
  player1 <- game_result$player1
  player2 <- game_result$player2
}

# Print the tournament results
print(tournament_results)



# Simulate a game
game_result <- simulate_game(player1, player2)
game_result <- simulate_game(player1, player2)

# Print the results
print(paste("Player 1 chose:", game_result$player1_choice))
print(paste("Player 2 chose:", game_result$player2_choice))
print(paste("Player 1 score:", game_result$scores$player1_score))
print(paste("Player 2 score:", game_result$scores$player2_score))

