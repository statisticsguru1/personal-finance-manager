# =========================================================
# Test account id validation helper function
# =========================================================

test_that("is_valid_user_id correctly validates user IDs", {
  expect_true(is_valid_user_id("user123"))
  expect_true(is_valid_user_id("USER_abc456"))
  expect_false(is_valid_user_id("user-abc"))
  expect_false(is_valid_user_id("user abc"))     # contains space
  expect_false(is_valid_user_id("../../etc"))    # path traversal attempt
  expect_false(is_valid_user_id(""))             # empty string
})

# =========================================================
# Test create_user_account_bases helper function
# =========================================================

test_that("create_user_account_base creates account tree correctly", {
  tmp_dir <- tempfile("acct_test_")
  user_id <- "testuser001"

  # Should not error when user does not exist
  expect_invisible(create_user_account_base(user_id, base_dir = tmp_dir))
  user_path <- file.path(normalizePath(tmp_dir), user_id)
  expect_true(dir.exists(user_path))
  expect_true(file.exists(file.path(user_path, "account_tree.Rds")))

  # Read back and verify it's a MainAccount
  obj <- readRDS(file.path(user_path, "account_tree.Rds"))
  expect_s3_class(obj, "MainAccount")
  expect_equal(obj$name, "Main")
  expect_equal(obj$balance, 0)

  # Should throw error if user already exists
  expect_error(
    create_user_account_base(user_id, base_dir = tmp_dir),
    "User already exists"
  )
})

test_that("create_user_account_base accepts initial_balance", {
  tmp_dir <- tempfile("acct_test_balance_")
  user_id <- "initialBalUser"
  create_user_account_base(user_id, base_dir = tmp_dir, initial_balance = 1500)

  obj <- readRDS(file.path(normalizePath(tmp_dir), user_id, "account_tree.Rds"))
  expect_equal(obj$balance, 1500)
})

test_that("create_user_account_base rejects invalid user IDs", {
  tmp_dir <- tempfile("acct_test_invalid_")

  expect_error(
    create_user_account_base("bad/id!", base_dir = tmp_dir),
    "Invalid user ID format"
  )

  expect_error(
    create_user_account_base("../../../etc", base_dir = tmp_dir),
    "Invalid user ID format"
  )

  expect_error(
    create_user_account_base("user with space", base_dir = tmp_dir),
    "Invalid user ID format"
  )

  expect_error(
    create_user_account_base("", base_dir = tmp_dir),
    "Invalid user ID format"
  )
})

# =========================================================
# Test verify_token helper function
# =========================================================

test_that("verify_token returns decoded payload for valid token", {
  test_secret <- charToRaw("unit-test-secret-do-not-use-in-prod")
  
  payload <- jose::jwt_claim(user_id = "user123", role = "user")
  token <- jose::jwt_encode_hmac(payload, secret = test_secret)
  
  result <- verify_token(token, secret = test_secret)
  
  expect_type(result, "list")
  expect_equal(result$user_id, "user123")
  expect_equal(result$role, "user")
})

test_that("verify_token returns NULL for invalid token", {
  result <- verify_token(
    "this.is.not.valid",
    secret = charToRaw("unit-test-secret-do-not-use-in-prod")
  )
  expect_null(result)
})


test_that("verify_token returns NULL for NULL input", {
  expect_null(verify_token(NULL, secret = charToRaw("test")))
})

test_that("verify_token fails with wrong secret", {
  token <- jose::jwt_encode_hmac(
    jose::jwt_claim(user_id = "userX"),
    secret = charToRaw("correct-secret")
  )
  result <- verify_token(token, secret = charToRaw("wrong-secret"))
  expect_null(result)
})


# =========================================================
# Test load account helper function
# =========================================================

test_that("load_user_account() errors on invalid user ID", {
  expect_error(
    load_user_account("invalid id!"),
    "Invalid user ID format"
  )
})

test_that("load_user_account() errors if account file does not exist", {
  temp_dir <- tempfile("test_accounts_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  user_id <- "user_missing"
  expect_error(
    load_user_account(user_id, base_dir = temp_dir),
    "Account data not found"
  )
})

test_that("load_user_account() errors if account file is missing", {
  temp_dir <- tempfile("test_accounts_")
  user_id <- "user123"
  user_path <- file.path(temp_dir, user_id)
  
  dir.create(user_path, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # No RDS file created
  expect_error(
    load_user_account(user_id, base_dir = temp_dir),
    "Account data not found"
  )
})

test_that("load_user_account() successfully loads a valid account", {
  temp_dir <- tempfile("test_accounts_")
  user_id <- "user123"
  user_path <- file.path(temp_dir, user_id)
  
  dir.create(user_path, recursive = TRUE)
  mock_account <- structure(list(balance = 500), class = "MainAccount")
  saveRDS(mock_account, file.path(user_path, "account_tree.Rds"))
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  result <- load_user_account(user_id, base_dir = temp_dir)
  expect_s3_class(result, "MainAccount")
  expect_equal(result$balance, 500)
})



# =========================================================
# Test save account helper function
# =========================================================

test_that("save_user_account saves file correctly", {
  tmp_dir <- tempfile("test-account-base-")
  dir.create(tmp_dir, recursive = TRUE)
  user_id <- "user001"
  
  # Create a sample tree (mock structure)
  tree <- list(
    uuid = "mock-uuid",
    balance = 1000,
    transactions = list()
  )
  
  expect_false(file.exists(file.path(tmp_dir, user_id, "account_tree.Rds")))
  
  save_user_account(user_id, tree, base_dir = tmp_dir)
  
  expect_true(file.exists(file.path(tmp_dir, user_id, "account_tree.Rds")))
  
  # Check that the saved object is correct
  reloaded <- readRDS(file.path(tmp_dir, user_id, "account_tree.Rds"))
  expect_equal(reloaded$uuid, "mock-uuid")
  expect_equal(reloaded$balance, 1000)
})

test_that("save_user_account creates directory if missing", {
  tmp_dir <- tempfile("test-missing-dir-")
  user_id <- "userABC"
  tree <- list(uuid = "abc", balance = 500)
  
  # Directory should not exist initially
  user_dir <- file.path(tmp_dir, user_id)
  expect_false(dir.exists(user_dir))
  
  save_user_account(user_id, tree, base_dir = tmp_dir)
  
  expect_true(file.exists(file.path(user_dir, "account_tree.Rds")))
})

test_that("save_user_account throws error for invalid user_id", {
  tmp_dir <- tempfile("test-invalid-id-")
  tree <- list(dummy = TRUE)
  
  expect_error(
    save_user_account("invalid id!", tree, base_dir = tmp_dir),
    "Invalid user ID format"
  )
})


test_that("save_user_account errors if tree is NULL", {
  tmp_dir <- tempfile("test-null-tree-")
  dir.create(tmp_dir)
  expect_error(save_user_account("userX", NULL, base_dir = tmp_dir))
})


test_that("save_user_account overwrites existing file", {
  tmp_dir <- tempfile("test-overwrite-")
  dir.create(tmp_dir)
  user_id <- "user42"
  
  tree1 <- list(uuid = "v1", balance = 100)
  save_user_account(user_id, tree1, base_dir = tmp_dir)
  
  tree2 <- list(uuid = "v2", balance = 999)
  save_user_account(user_id, tree2, base_dir = tmp_dir)
  
  reloaded <- readRDS(file.path(tmp_dir, user_id, "account_tree.Rds"))
  expect_equal(reloaded$uuid, "v2")
  expect_equal(reloaded$balance, 999)
})


test_that("save_user_account errors if base_dir is invalid", {
  # An obviously invalid path
  # (use a locked or system-reserved dir in real cases)
  expect_error(
    save_user_account("user99", list(test = TRUE), base_dir = ""),
    "Invalid base_dir"
  )
  
})


