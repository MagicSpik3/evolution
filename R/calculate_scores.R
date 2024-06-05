#' @title calculate_scores
#' @author Mark London
#' @description
#' Calculate Scores for a Prisoner's Dilemma Game
#'
#' This function calculates the scores for two players in a Prisoner's Dilemma game
#' based on their choices to either 'Cooperate' or 'Defect'. The classic payoff values are used:
#' 3 points for mutual cooperation, 1 point for mutual defection, 5 points for a defector when the other player cooperates, and 0 points for a cooperator when the other player defects.
#'
#' @param player1_choice A string indicating Player 1's choice: either "Cooperate" or "Defect".
#' @param player2_choice A string indicating Player 2's choice: either "Cooperate" or "Defect".
#' @return A list containing the scores for both players.
#' \item{player1_score}{The score for Player 1.}
#' \item{player2_score}{The score for Player 2.}
#' @examples
#' # Player 1 cooperates, Player 2 defects
#' calculate_scores("Cooperate", "Defect")
#' # Player 1 defects, Player 2 cooperates
#' calculate_scores("Defect", "Cooperate")
#' # Both players cooperate
#' calculate_scores("Cooperate", "Cooperate")
#' # Both players defect
#' calculate_scores("Defect", "Defect")
#' @export
#' @details This function implements the classic payoff matrix for the Prisoner's Dilemma:
#' \tabular{cc}{
#'   \strong{Player 2} \tab \strong{Cooperate} \tab \strong{Defect} \cr
#'   \strong{Player 1} \tab \tab \cr
#'   \strong{Cooperate} \tab (3, 3) \tab (0, 5) \cr
#'   \strong{Defect} \tab (5, 0) \tab (1, 1) \cr
#' }

#'
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
