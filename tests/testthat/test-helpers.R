
# Replicate Dot args ------------------------------------------------------

test_that("test rep_args_len()",{

  # No args -> NULL
  expect_null(rep_args_len())
  # Length out = 0; return zero length vector
  expect_identical(rep_args_len(1, length.out = 0), list(numeric(0)))
  expect_identical(rep_args_len("a", length.out = 0), list(character(0)))

  # Replicate is OK
  expect_identical(rep_args_len(a = 1:2, length.out = 4), list(a = c(1:2, 1:2)))

})
