% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{user_file_exists}
\alias{user_file_exists}
\title{Check If a User File Exists via Configured Storage Backend}
\usage{
user_file_exists(user_id, file_name = "account_tree.Rds")
}
\arguments{
\item{user_id}{A string representing the unique user ID.}

\item{file_name}{Name of the file to check, relative to the user's folder.
Defaults to `"account_tree.Rds"`.}
}
\value{
A logical value: `TRUE` if the file exists, `FALSE` otherwise.
}
\description{
Checks whether a specific file (e.g., `account_tree.Rds`, lock file, etc.)
exists for a given user by delegating to a backend-specific plugin.
}
\details{
This function supports checking files stored in different storage backends
such as local disk, MongoDB, Google Drive, etc., as configured via the
`ACCOUNT_BACKEND` environment variable.
}
\examples{
\dontrun{
  user_file_exists("user123") # Checks account_tree.Rds by default
  user_file_exists("user123", file_name = "account_tree.lock")
}

}
