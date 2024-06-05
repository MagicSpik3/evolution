# Write tests
test_that("Both players cooperate", {
  result <- calculate_scores("Cooperate", "Cooperate")
  expect_equal(result$player1_score, 3)
  expect_equal(result$player2_score, 3)
})

test_that("Both players defect", {
  result <- calculate_scores("Defect", "Defect")
  expect_equal(result$player1_score, 1)
  expect_equal(result$player2_score, 1)
})

test_that("Player 1 cooperates, Player 2 defects", {
  result <- calculate_scores("Cooperate", "Defect")
  expect_equal(result$player1_score, 0)
  expect_equal(result$player2_score, 5)
})

test_that("Player 1 defects, Player 2 cooperates", {
  result <- calculate_scores("Defect", "Cooperate")
  expect_equal(result$player1_score, 5)
  expect_equal(result$player2_score, 0)
})

test_that("Invalid choices", {
  expect_error(calculate_scores("Invalid", "Defect"))
  expect_error(calculate_scores("Cooperate", "Invalid"))
})

#test_that("This test fails to test the testing: Player 1 defects, Player 2 cooperates", {
#  result <- calculate_scores("Defect", "Cooperate")
#  expect_equal(result$player1_score, 4) # should be 5
#  expect_equal(result$player2_score, 0)
#})
