% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{save_user_file}
\alias{save_user_file}
\title{Save a User-Specific File via Plugin Backend}
\usage{
save_user_file(user_id, object, file_name = "account_tree.Rds")
}
\arguments{
\item{user_id}{A character string representing the unique user ID.}

\item{object}{The R object to save. This can be any R object appropriate
for the file extension used (e.g., list, data.frame, custom class).}

\item{file_name}{The file name, including the extension (e.g.,
`"account_tree.Rds"`, `"meta.json"`, `"transactions.csv"`,
`"account_tree.lock"`).}
}
\value{
No return value. This function is invoked for its side effect of
persisting a file via the selected backend plugin.
}
\description{
Saves an R object to a user-specific location using the appropriate plugin
backend (e.g., `"file"`, `"mongo"`, `"gdrive"`). The format and destination
are determined by the backend and file extension.
}
\details{
This function acts as a plugin launcher for saving user-related data,
including account trees (`.Rds`), transaction records (`.csv`),
metadata (`.json`), or lockfiles (`.lock`). It delegates the actual
save operation to the corresponding backend plugin function (e.g.,
`save_to_file()`).


The appropriate plugin is selected based on the `ACCOUNT_BACKEND`
environment
variable (default: `"file"`). The function builds the arguments required by
the plugin using [build_plugin_args()], then delegates the save operation.

An error is raised if no suitable save plugin is found.
}
\examples{
\dontrun{
  # Save an account tree to .Rds
  save_user_file("user123", MainAccount$new(name = "Main"))

  # Save a data frame to CSV
  save_user_file("user123", data.frame(a = 1:3), "transactions.csv")

  # Save a lock file (PID as a number)
  save_user_file("user123", Sys.getpid(), "account_tree.lock")
}

}
\seealso{
[build_plugin_args()], [load_user_file()], [remove_user_file()],
  [user_file_exists()]
}
