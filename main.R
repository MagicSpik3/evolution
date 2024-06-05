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
  choices <- c("Cooperate", "Defect")
  decision <- sample(choices, 1)
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
player1 <- Player("Player 1")
player2 <- Player("Player 2")

# Simulate a game
game_result <- simulate_game(player1, player2)

# Print the results
print(paste("Player 1 chose:", game_result$player1_choice))
print(paste("Player 2 chose:", game_result$player2_choice))
print(paste("Player 1 score:", game_result$scores$player1_score))
print(paste("Player 2 score:", game_result$scores$player2_score))

