

new_player <- function(name, age = 0, choosing_method = runif) {
  structure(
    list(
      name = name,
      last_choice = NULL,
      last_opponent_choice = NULL,
      my_next_choice = NULL,
      my_choosing_method = choosing_method,
      age = age
    ),
    class = "Player"
  )
}

print.Player <- function(x) {
  cat("Player: \n")
  cat("\tName: ", x$name, "\n", sep = "")
  cat("\tAge: ", x$age, "\n", sep = "")
  cat("\tMy Next Choice: ", x$my_next_choice, "\n", sep = "")
}

decidePlayer <- function(x) UseMethod("decide")

decide.Player <- function(x) {
  print(x$my_choosing_method)
  x$my_next_choice <- x$my_choosing_method(1) * x$age
  x
}

# Example usage
player1 <- new_player("Alice", age = 25, choosing_method = runif)
player1 <- decidePlayer(player1)
print(player1)

player2 <- new_player("Bob", age = 30, choosing_method = function(n) rnorm(n, mean = 5, sd = 2))
player2 <- decidePlayer(player2)
print(player2)
