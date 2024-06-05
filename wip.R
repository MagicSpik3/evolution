# wip:
#install.packages("testthat")
library(testthat)
usethis::use_test("test_calculate_scores.R")
# Function to calculate scores in a Prisoner's Dilemma game
calculate_scores <- function(player1_choice, player2_choice) {
  # Initialize scores
  player1_score <- 0
  player2_score <- 0

  # Define the choices and corresponding scores
  if (player1_choice == "Cooperate" && player2_choice == "Cooperate") {
    player1_score <- 3
    player2_score <- 3
  } else if (player1_choice == "Defect" && player2_choice == "Defect") {
    player1_score <- 1
    player2_score <- 1
  } else if (player1_choice == "Cooperate" && player2_choice == "Defect") {
    player1_score <- 0
    player2_score <- 5
  } else if (player1_choice == "Defect" && player2_choice == "Cooperate") {
    player1_score <- 5
    player2_score <- 0
  } else {
    stop("Invalid choices. Players must choose 'Cooperate' or 'Defect'.")
  }

  return(list(player1_score = player1_score, player2_score = player2_score))
}

# Example usage:
# Player 1 chooses "Cooperate" and Player 2 chooses "Defect"
result <- calculate_scores("Cooperate", "Defect")
print(result)
# Output should be: $player1_score = 0, $player2_score = 5
@aliases
@concepts
@describeIn
@examples
@export
@family
@inheritParams
@keywords
@param
@rdname
@return
@section
@seealso
@format
@source
@include
@slot
@field
