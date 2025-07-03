library(filelock)
library(withr)
library(jsonlite)
library(tidyverse)
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

# =================================================================
# Test create_user_account_base helper function (plugin-compatible)
# =================================================================

test_that("create_user_account_base creates account tree correctly", {
  tmp_dir <- tempfile("acct_test_")
  user_id <- "testuser001"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  # Should not error when user does not exist
  expect_invisible(create_user_account_base(user_id))
  expect_true(user_file_exists(user_id, file_name = "account_tree.Rds"))
  
  # Read back and verify it's a MainAccount
  obj <- load_user_file(user_id, file_name = "account_tree.Rds")
  expect_s3_class(obj, "MainAccount")
  expect_equal(obj$name, "Main")
  expect_equal(obj$balance, 0)
  
  # Should throw error if user already exists
  expect_error(
    create_user_account_base(user_id),
    "User already exists"
  )
})

test_that("create_user_account_base accepts initial_balance", {
  tmp_dir <- tempfile("acct_test_balance_")
  user_id <- "initialBalUser"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  create_user_account_base(user_id, initial_balance = 1500)
  
  obj <- load_user_file(user_id, file_name = "account_tree.Rds")
  expect_equal(obj$balance, 1500)
})

test_that("create_user_account_base rejects invalid user IDs", {
  tmp_dir <- tempfile("acct_test_invalid_")
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  expect_error(
    create_user_account_base("bad/id!"),
    "Invalid user ID format"
  )
  
  expect_error(
    create_user_account_base("../../../etc"),
    "Invalid user ID format"
  )
  
  expect_error(
    create_user_account_base("user with space"),
    "Invalid user ID format"
  )
  
  expect_error(
    create_user_account_base(""),
    "Invalid user ID format"
  )
})

# =================================================================
# Test load_user_file plugin launcher.
# =================================================================

test_that("load_user_file correctly loads from .Rds file", {
  tmp_dir <- tempfile("test_backend_rds_")
  user_id <- "testuser_rds"
  file_name <- "account_tree.Rds"
  file_path <- file.path(tmp_dir, user_id)
  dir.create(file_path, recursive = TRUE)
  
  test_data <- list(balance = 1000)
  saveRDS(test_data, file.path(file_path, file_name))
  
  # Temporarily override environment for test
  withr::local_envvar(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  result <- load_user_file(user_id, file_name)
  expect_equal(result$balance, 1000)
})

test_that("load_user_file correctly loads from .json file", {
  tmp_dir <- tempfile("test_backend_json_")
  user_id <- "testuser_json"
  file_name <- "data.json"
  file_path <- file.path(tmp_dir, user_id)
  dir.create(file_path, recursive = TRUE)
  
  jsonlite::write_json(list(x = 42), file.path(file_path, file_name))
  
  withr::local_envvar(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  result <- load_user_file(user_id, file_name)
  expect_equal(result$x, 42)
})

test_that("load_user_file correctly loads from .csv file", {
  tmp_dir <- tempfile("test_backend_csv_")
  user_id <- "testuser_csv"
  file_name <- "data.csv"
  file_path <- file.path(tmp_dir, user_id)
  dir.create(file_path, recursive = TRUE)
  
  df <- data.frame(id = 1:2, value = c(10, 20))
  write.csv(df, file.path(file_path, file_name), row.names = FALSE)
  
  withr::local_envvar(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  result <- load_user_file(user_id, file_name)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("load_user_file errors for unsupported file extension", {
  tmp_dir <- tempfile("test_backend_badext_")
  user_id <- "testuser_bad"
  file_name <- "notes.txt"
  file_path <- file.path(tmp_dir, user_id)
  dir.create(file_path, recursive = TRUE)
  
  writeLines("some text", file.path(file_path, file_name))
  
  withr::local_envvar(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  expect_error(
    load_user_file(user_id, file_name),
    "Unsupported file type"
  )
})

test_that("load_user_file errors if plugin does not exist", {
  withr::local_envvar(ACCOUNT_BACKEND = "nonexistent")
  
  expect_error(
    load_user_file("anyuser", "anyfile"),
    "No loader plugin found for backend"
  )
})



# =================================================================
# Test load_from_file helper function
# =================================================================

test_that("load_from_file reads Rds files correctly", {
  tmp_dir <- tempfile("test_userdir_rds_")
  dir.create(file.path(tmp_dir, "user001"), recursive = TRUE)
  obj <- list(name = "Main", balance = 100)
  saveRDS(obj, file.path(tmp_dir, "user001", "account_tree.Rds"))
  
  result <- load_from_file("user001", "account_tree.Rds", base_dir = tmp_dir)
  expect_equal(result$name, "Main")
  expect_equal(result$balance, 100)
})

test_that("load_from_file reads JSON files correctly", {
  tmp_dir <- tempfile("test_userdir_json_")
  dir.create(file.path(tmp_dir, "user002"), recursive = TRUE)
  json_data <- '{"name": "Main", "balance": 500}'
  writeLines(json_data, file.path(tmp_dir, "user002", "data.json"))
  
  result <- load_from_file("user002", "data.json", base_dir = tmp_dir)
  expect_equal(result$name, "Main")
  expect_equal(result$balance, 500)
})

test_that("load_from_file reads CSV files correctly", {
  tmp_dir <- tempfile("test_userdir_csv_")
  dir.create(file.path(tmp_dir, "user003"), recursive = TRUE)
  csv_data <- "name,balance\nMain,200"
  writeLines(csv_data, file.path(tmp_dir, "user003", "data.csv"))
  
  result <- load_from_file("user003", "data.csv", base_dir = tmp_dir)
  expect_true(is.data.frame(result))
  expect_equal(result$name[1], "Main")
  expect_equal(result$balance[1], 200)
})

test_that("load_from_file errors on unsupported file types", {
  tmp_dir <- tempfile("test_userdir_txt_")
  dir.create(file.path(tmp_dir, "user004"), recursive = TRUE)
  writeLines("unsupported content", file.path(tmp_dir, "user004", "data.txt"))
  
  expect_error(
    load_from_file("user004", "data.txt", base_dir = tmp_dir),
    "Unsupported file type"
  )
})

test_that("load_from_file errors when file does not exist", {
  tmp_dir <- tempfile("test_userdir_missing_")
  dir.create(file.path(tmp_dir, "user005"), recursive = TRUE)
  
  expect_error(
    load_from_file("user005", "missing_file.Rds", base_dir = tmp_dir),
    "cannot open the connection"
  )
})


# =================================================================
# Test save_user_file
# =================================================================
test_that("save_user_file saves RDS object correctly", {
  tmp_dir <- tempfile("save_test_rds_")
  user_id <- "userRds"
  file_name <- "my_data.Rds"
  object <- list(a = 1, b = 2)
  
  # Temporarily override backend env and base dir
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  save_user_file(user_id, object, file_name)
  
  saved_path <- file.path(tmp_dir, user_id, file_name)
  expect_true(file.exists(saved_path))
  
  loaded <- readRDS(saved_path)
  expect_equal(loaded, object)
})

test_that("save_user_file saves JSON correctly", {
  tmp_dir <- tempfile("save_test_json_")
  user_id <- "userJson"
  file_name <- "data.json"
  object <- list(name = "test", active = TRUE)
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  save_user_file(user_id, object, file_name)
  
  saved_path <- file.path(tmp_dir, user_id, file_name)
  expect_true(file.exists(saved_path))
  
  loaded <- jsonlite::fromJSON(saved_path)
  expect_equal(loaded$name, "test")
  expect_true(loaded$active)
})

test_that("save_user_file saves CSV correctly", {
  tmp_dir <- tempfile("save_test_csv_")
  user_id <- "userCsv"
  file_name <- "info.csv"
  object <- data.frame(a = 1:3, b = letters[1:3])
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  save_user_file(user_id, object, file_name)
  
  saved_path <- file.path(tmp_dir, user_id, file_name)
  expect_true(file.exists(saved_path))
  
  loaded <- read.csv(saved_path)
  expect_equal(loaded, object)
})

test_that("save_user_file fails if plugin is missing", {
  tmp_dir <- tempfile("save_test_missing_")
  user_id <- "missingPlugin"
  file_name <- "fail.Rds"
  object <- 1
  
  withr::local_envvar(c(ACCOUNT_BACKEND = "nonexistent"))
  
  expect_error(
    save_user_file(user_id, object, file_name),
    "No save plugin found"
  )
})

test_that("save_user_file creates user directory if missing", {
  tmp_dir <- tempfile("save_test_dir_create_")
  user_id <- "createUser"
  file_name <- "newfile.Rds"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  path <- file.path(tmp_dir, user_id)
  expect_false(dir.exists(path))
  
  save_user_file(user_id, list(x = 1), file_name)
  
  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, file_name)))
})

# =================================================================
# Test remove_user_file
# =================================================================

test_that("remove_user_file removes existing file", {
  tmp_dir <- tempfile("test_remove_user_")
  user_id <- "removal_user"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  obj <- list(x = 1, y = 2)
  save_user_file(user_id, obj, "test_file.Rds")
  file_path <- file.path(tmp_dir, user_id, "test_file.Rds")
  
  expect_true(file.exists(file_path))
  
  expect_silent(remove_user_file(user_id, "test_file.Rds"))
  expect_false(file.exists(file_path))
})

test_that("remove_user_file warns on missing file", {
  tmp_dir <- tempfile("test_remove_missing_")
  user_id <- "missing_user"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  expect_warning(
    remove_user_file(user_id, "nonexistent.Rds"),
    "File does not exist"
  )
})

test_that("remove_user_file handles other file types", {
  tmp_dir <- tempfile("test_remove_types_")
  user_id <- "csv_json_user"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  json_obj <- list(name = "Test")
  csv_obj <- data.frame(a = 1:3, b = letters[1:3])
  
  save_user_file(user_id, json_obj, "data.json")
  save_user_file(user_id, csv_obj, "data.csv")
  
  expect_true(file.exists(file.path(tmp_dir, user_id, "data.json")))
  expect_true(file.exists(file.path(tmp_dir, user_id, "data.csv")))
  
  expect_silent(remove_user_file(user_id, "data.json"))
  expect_silent(remove_user_file(user_id, "data.csv"))
  
  expect_false(file.exists(file.path(tmp_dir, user_id, "data.json")))
  expect_false(file.exists(file.path(tmp_dir, user_id, "data.csv")))
})

test_that("remove_user_file fails if plugin is missing", {
  user_id <- "bad_plugin_user"
  Sys.setenv(ACCOUNT_BACKEND = "not_a_plugin")
  
  expect_error(
    remove_user_file(user_id),
    "No remove plugin found for backend"
  )
})


test_that("remove_user_file removes .lock files correctly", {
  tmp_dir <- tempfile("test_lockfile_removal_")
  user_id <- "lock_user"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  # Manually create a .lock file
  lock_path <- file.path(tmp_dir, user_id)
  dir.create(lock_path, recursive = TRUE)
  lock_file <- file.path(lock_path, "account_tree.lock")
  writeLines("12345", lock_file)
  
  expect_true(file.exists(lock_file))
  
  expect_silent(remove_user_file(user_id, "account_tree.lock"))
  expect_false(file.exists(lock_file))
})
# =========================================================
# Tests for user_file_exists()
# =========================================================

test_that("user_file_exists() returns TRUE for existing file", {
  tmp_dir <- tempfile("user_exists_")
  user_id <- "existsUser"
  file_name <- "account_tree.Rds"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  # Create file
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  test_object <- list(a = 1)
  saveRDS(test_object, file.path(tmp_dir, user_id, file_name))
  
  expect_true(user_file_exists(user_id, file_name))
})

test_that("user_file_exists() returns FALSE for missing file", {
  tmp_dir <- tempfile("user_missing_")
  user_id <- "missingUser"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  expect_false(user_file_exists(user_id, file_name = "account_tree.Rds"))
})

test_that("user_file_exists() supports .lock files", {
  tmp_dir <- tempfile("user_lock_")
  user_id <- "lockUser"
  lock_name <- "account_tree.lock"
  Sys.setenv(ACCOUNT_BACKEND = "file", ACCOUNT_BASE_DIR = tmp_dir)
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  lock_path <- file.path(tmp_dir, user_id, lock_name)
  writeLines("lock", lock_path)
  
  expect_true(user_file_exists(user_id, lock_name))
})

test_that("user_file_exists() throws error for unknown backend", {
  Sys.setenv(ACCOUNT_BACKEND = "nonexistent")
  expect_error(
    user_file_exists("someUser"),
    "No file-exists plugin found for backend"
  )
})

# =================================================================
# Test save_to_file plugin
# =================================================================
test_that("save_to_file saves .Rds files correctly", {
  tmp_dir <- tempfile("save_test_rds_")
  dir.create(tmp_dir)
  
  user_id <- "testuser"
  obj <- list(a = 1, b = 2)
  file_name <- "test_data.Rds"
  
  save_to_file(user_id, obj, file_name, base_dir = tmp_dir)
  
  path <- file.path(tmp_dir, user_id, file_name)
  expect_true(file.exists(path))
  expect_equal(readRDS(path), obj)
})

test_that("save_to_file saves .json files correctly", {
  tmp_dir <- tempfile("save_test_json_")
  dir.create(tmp_dir)
  
  user_id <- "jsonuser"
  obj <- list(a = 1, b = list(x = 10))
  file_name <- "data.json"
  
  save_to_file(user_id, obj, file_name, base_dir = tmp_dir)
  
  path <- file.path(tmp_dir, user_id, file_name)
  expect_true(file.exists(path))
  loaded <- jsonlite::fromJSON(path)
  expect_equal(loaded$a, 1)
  expect_equal(loaded$b$x, 10)
})

test_that("save_to_file saves .csv files correctly", {
  tmp_dir <- tempfile("save_test_csv_")
  dir.create(tmp_dir)
  
  user_id <- "csvuser"
  df <- data.frame(name = c("A", "B"), score = c(10, 20))
  file_name <- "results.csv"
  
  save_to_file(user_id, df, file_name, base_dir = tmp_dir)
  
  path <- file.path(tmp_dir, user_id, file_name)
  expect_true(file.exists(path))
  
  loaded <- read.csv(path)
  expect_equal(loaded$name, df$name)
  expect_equal(loaded$score, df$score)
})

test_that("save_to_file creates directory if it doesn't exist", {
  tmp_dir <- tempfile("save_dir_creation_")
  user_id <- "newuser"
  file_name <- "newfile.Rds"
  obj <- "data"
  
  save_to_file(user_id, obj, file_name, base_dir = tmp_dir)
  expect_true(dir.exists(file.path(tmp_dir, user_id)))
})

test_that("save_to_file overwrites existing files", {
  tmp_dir <- tempfile("save_overwrite_")
  dir.create(tmp_dir, recursive = TRUE)
  
  user_id <- "overwrite_user"
  file_name <- "data.Rds"
  
  first_obj <- "first"
  second_obj <- "second"
  
  save_to_file(user_id, first_obj, file_name, base_dir = tmp_dir)
  save_to_file(user_id, second_obj, file_name, base_dir = tmp_dir)
  
  path <- file.path(tmp_dir, user_id, file_name)
  expect_equal(readRDS(path), "second")
})

test_that("save_to_file errors on unsupported file extension", {
  tmp_dir <- tempfile("save_bad_ext_")
  dir.create(tmp_dir)
  
  user_id <- "badextuser"
  file_name <- "invalid.xyz"
  obj <- list(x = 1)
  
  expect_warning(
    save_to_file(user_id, obj, "file.xyz", base_dir = tmp_dir),
    "Unrecognized file extension"
  )
  expect_true(file.exists(file.path(tmp_dir, user_id, "file.xyz")))
})


# =================================================================
# Test remove_from_file
# =================================================================
test_that("remove_from_file removes existing file", {
  tmp_dir <- tempfile("remove_test_")
  user_id <- "testuser"
  file_name <- "test_data.Rds"
  
  user_path <- file.path(tmp_dir, user_id)
  file_path <- file.path(user_path, file_name)
  
  dir.create(user_path, recursive = TRUE)
  saveRDS(42, file_path)
  
  expect_true(file.exists(file_path))
  
  remove_from_file(user_id, file_name, base_dir = tmp_dir)
  
  expect_false(file.exists(file_path))
})

test_that("remove_from_file issues warning for non-existent file", {
  tmp_dir <- tempfile("remove_test_warn_")
  user_id <- "nouser"
  file_name <- "nonexistent.Rds"
  
  expect_warning(
    remove_from_file(user_id, file_name, base_dir = tmp_dir),
    "File does not exist"
  )
})

test_that("remove_from_file handles subdirectories correctly", {
  tmp_dir <- tempfile("remove_test_nested_")
  user_id <- "nested_user"
  file_name <- "nested/testfile.csv"
  
  file_dir <- file.path(tmp_dir, user_id, dirname(file_name))
  file_path <- file.path(tmp_dir, user_id, file_name)
  
  dir.create(file_dir, recursive = TRUE)
  write.csv(data.frame(a = 1), file_path, row.names = FALSE)
  
  expect_true(file.exists(file_path))
  
  remove_from_file(user_id, file_name, base_dir = tmp_dir)
  
  expect_false(file.exists(file_path))
})

test_that("remove_from_file is silent on successful removal", {
  tmp_dir <- tempfile("remove_test_silent_")
  user_id <- "quietuser"
  file_name <- "quiet.Rds"
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  saveRDS("hello", file.path(tmp_dir, user_id, file_name))
  
  expect_silent(remove_from_file(user_id, file_name, base_dir = tmp_dir))
})

# =================================================================
# Test build_plugin_args
# =================================================================

test_that("remove_from_file removes existing file", {
  tmp_dir <- tempfile("remove_test_")
  user_id <- "testuser"
  file_name <- "test_data.Rds"
  
  user_path <- file.path(tmp_dir, user_id)
  file_path <- file.path(user_path, file_name)
  
  dir.create(user_path, recursive = TRUE)
  saveRDS(42, file_path)
  
  expect_true(file.exists(file_path))
  
  remove_from_file(user_id, file_name, base_dir = tmp_dir)
  
  expect_false(file.exists(file_path))
})

test_that("remove_from_file issues warning for non-existent file", {
  tmp_dir <- tempfile("remove_test_warn_")
  user_id <- "nouser"
  file_name <- "nonexistent.Rds"
  
  expect_warning(
    remove_from_file(user_id, file_name, base_dir = tmp_dir),
    "File does not exist"
  )
})

test_that("remove_from_file handles subdirectories correctly", {
  tmp_dir <- tempfile("remove_test_nested_")
  user_id <- "nested_user"
  file_name <- "nested/testfile.csv"
  
  file_dir <- file.path(tmp_dir, user_id, dirname(file_name))
  file_path <- file.path(tmp_dir, user_id, file_name)
  
  dir.create(file_dir, recursive = TRUE)
  write.csv(data.frame(a = 1), file_path, row.names = FALSE)
  
  expect_true(file.exists(file_path))
  
  remove_from_file(user_id, file_name, base_dir = tmp_dir)
  
  expect_false(file.exists(file_path))
})

test_that("remove_from_file is silent on successful removal", {
  tmp_dir <- tempfile("remove_test_silent_")
  user_id <- "quietuser"
  file_name <- "quiet.Rds"
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  saveRDS("hello", file.path(tmp_dir, user_id, file_name))
  
  expect_silent(remove_from_file(user_id, file_name, base_dir = tmp_dir))
})


# =================================================================
# Test build_plugin_args
# =================================================================
test_that("build_plugin_args returns correct args for file backend", {
  withr::local_envvar(c(ACCOUNT_BASE_DIR = "my_dir"))
  
  args <- build_plugin_args(
    "file",
    "load",
    user_id = "user1",
    file_name = "file.Rds"
  )
  
  expect_equal(args$base_dir, "my_dir")
  expect_equal(args$user_id, "user1")
  expect_equal(args$file_name, "file.Rds")
  
  args2 <- build_plugin_args(
    "file",
    "save",
    user_id = "user1",
    object = "data",
    file_name = "file.json"
  )
  expect_equal(args2$base_dir, "my_dir")
  expect_equal(args2$file_name, "file.json")
  expect_equal(args2$object, "data")
  
  args3 <- build_plugin_args("file", "file_exists", user_id = "user1",
                             file_name = "file.csv")
  expect_equal(args3$base_dir, "my_dir")
  expect_equal(args3$file_name, "file.csv")
  
  args4 <- build_plugin_args("file", "remove", user_id = "user1",
                             file_name = "file.csv")
  expect_equal(args4$base_dir, "my_dir")
})

test_that("build_plugin_args returns correct args for mongo backend", {
  withr::local_envvar(c(MONGO_URI = "mongodb://localhost:27017",
                        MONGO_DB = "testdb"))
  
  args <- build_plugin_args(
    "mongo",
    "load",
    user_id = "user2",
    file_name = "tree.Rds"
  )
  expect_equal(args$uri, "mongodb://localhost:27017")
  expect_equal(args$db, "testdb")
  expect_equal(args$user_id, "user2")
  
  args2 <- build_plugin_args(
    "mongo",
    "save",
    user_id = "user2",
    object = list(a = 1),
    file_name = "tree.Rds"
  )
  expect_equal(args2$uri, "mongodb://localhost:27017")
  expect_equal(args2$db, "testdb")
  expect_equal(args2$object, list(a = 1))
  
  args3 <- build_plugin_args("mongo", "file_exists", user_id = "user2",
                             file_name = "tree.Rds")
  expect_equal(args3$uri, "mongodb://localhost:27017")
  expect_equal(args3$db, "testdb")
  
  args4 <- build_plugin_args("mongo", "remove", user_id = "user2",
                             file_name = "tree.Rds")
  expect_equal(args4$uri, "mongodb://localhost:27017")
  expect_equal(args4$db, "testdb")
})

test_that("build_plugin_args returns correct args for gdrive backend", {
  withr::local_envvar(c(GDRIVE_FOLDER_ID = "abc123"))
  
  args <- build_plugin_args(
    "gdrive",
    "load",
    user_id = "user3",
    file_name = "tree.Rds"
  )
  expect_equal(args$folder_id, "abc123")
  expect_equal(args$user_id, "user3")
  
  args2 <- build_plugin_args(
    "gdrive",
    "save",
    user_id = "user3",
    object = "data",
    file_name = "tree.Rds"
  )
  expect_equal(args2$folder_id, "abc123")
  expect_equal(args2$object, "data")
  
  args3 <- build_plugin_args(
    "gdrive",
    "file_exists",
    user_id = "user3",
    file_name = "tree.Rds"
  )
  expect_equal(args3$folder_id, "abc123")
  
  args4 <- build_plugin_args("gdrive", "remove", user_id = "user3",
                             file_name = "tree.Rds")
  expect_equal(args4$folder_id, "abc123")
})

test_that("build_plugin_args fails with unknown backend or mode", {
  expect_error(
    build_plugin_args("unknown_backend", "load", user_id = "u"),
    "Unknown plugin configuration for backend"
  )
  expect_error(
    build_plugin_args("file", "unknown_mode", user_id = "u"),
    "Unknown plugin configuration for backend"
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
# Test file lock helper function
# =========================================================

test_that("with_account_lock() creates and removes lockfile (plugin-aware)", {
  tmp_dir <- tempfile("acct_lock_test_")
  user_id <- "testuser_lock1"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  defer(unlink(tmp_dir, recursive = TRUE), teardown_env())
  
  expect_false(user_file_exists(user_id, "account_tree.lock"))
  
  with_account_lock(user_id, {
    expect_true(user_file_exists(user_id, "account_tree.lock"))
  })
  
  expect_false(user_file_exists(user_id, "account_tree.lock"))
})

test_that("with_account_lock() times out if lockfile already exists", {
  tmp_dir <- tempfile("acct_lock_timeout_")
  user_id <- "testuser_lock2"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  save_user_file(user_id, Sys.getpid(), file_name = "account_tree.lock")
  defer(unlink(tmp_dir, recursive = TRUE), teardown_env())
  
  start <- Sys.time()
  expect_error(
    with_account_lock(user_id, {
      cat("Should not reach here")
    }, timeout = 1),
    "Could not acquire lock"
  )
  
  duration <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_gte(duration, 1)
})

test_that("with_account_lock() supports return values (plugin-compatible)", {
  tmp_dir <- tempfile("acct_lock_return_")
  user_id <- "testuser_lock3"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  defer(unlink(tmp_dir, recursive = TRUE), teardown_env())
  
  result <- with_account_lock(user_id, {
    999
  })
  
  expect_equal(result, 999)
  expect_false(user_file_exists(user_id, "account_tree.lock"))
})

test_that("with_account_lock() cleans up lockfile after error", {
  tmp_dir <- tempfile("acct_lock_error_")
  user_id <- "testuser_lock4"
  
  withr::local_envvar(c(
    ACCOUNT_BACKEND = "file",
    ACCOUNT_BASE_DIR = tmp_dir
  ))
  
  dir.create(file.path(tmp_dir, user_id), recursive = TRUE)
  defer(unlink(tmp_dir, recursive = TRUE), teardown_env())
  
  expect_error(
    with_account_lock(user_id, {
      stop("Simulated failure inside lock")
    }),
    "Simulated failure"
  )
  
  expect_false(user_file_exists(user_id, "account_tree.lock"))
})
test_that("with_account_lock errors on invalid user ID", {
  expect_error(
    with_account_lock("invalid id!", { message("This won't run") }),
    "Invalid user ID format"
  )
})

