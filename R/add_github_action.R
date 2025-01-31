#' @title GitHub Action workflows creation
#' @description
#' This function generates GitHub Actions workflows for specific tasks.
#' The yaml file association will be created at the root of the R working directory under folders ".github/workflows". Theses folders will be created automatically if necessary (you need to have the correct computer's file system access).
#' @param github_action_name Mandatory. Class "character" is expected. Identification of the GitHub Action that you want to generate.
#' @param arguments Optional. Class "NULL" of "character" is expected. By default NULL. Argument(s) for a dynamic substitution in the GitHub Action template.
#' @param test_mode Optional. Class "logical" is expected. By default FALSE. Optional argument used for the unit tests. Don't use it in a regular way.
#' @details
#' GitHub Actions workflows developed so far are:
#' - "mirror_github_git", create a workflow to establishing a mirror between a GitHub repository and a repository from another Git. For this process, you need to fill three variables in the "arguments" parameter: "github_repository_source_url", "secret_token_name" and "git_repository_target_url". A full documentation is available through this [link](https://umr-marbec.github.io/den_pages/en/pages/git/miroir_github_gitlab.html).
#' @returns No return in the R environment. If the function run successfully, the yaml file associated with the GitHub Action workflow is created in the folders ".github/workflows" at the root of your working directory.
#' @export
#' @examples
#' \dontrun{
#' # example for "mirror_github_git" GitHub Action workflow creation
#' add_github_action(github_action_name = "mirror_github_git",
#'                   arguments = c("github_repository_source_url" = "url_source",
#'                                 "secret_token_name" = "my-awesome-token-name",
#'                                 "git_repository_target_url" = "url_target"))
#' }
add_github_action <- function(github_action_name,
                              arguments = NULL,
                              test_mode = FALSE) {
  # Global arguments verifications ----
  if (missing(x = github_action_name)
      || !inherits(x = github_action_name, what = "character")
      || length(x = github_action_name) != 1) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"github_action_name\" argument.")
  }
  # Global process ----
  if (test_mode == FALSE
      && dir.exists(paths = file.path(".github",
                                      "workflows")) != TRUE) {
    dir.create(path = file.path(".github",
                                "workflows"),
               recursive = TRUE)
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Folders \".github/workflows\" created at the root of your working directory.")
  }
  # Process for miroir_github_git ----
  if (github_action_name == "mirror_github_git") {
    mirror_github_git_raw <- readLines(con = system.file("github_actions_templates",
                                                         "mirror_github_git.yml",
                                                         package = "sparck"))
    # Specific arguments verifications
    if (missing(x = arguments)
        || is.null(x = arguments)
        || !inherits(x = arguments, what = "character")
        || !all(c("github_repository_source_url",
                  "secret_token_name",
                  "git_repository_target_url") %in% names(x = arguments))) {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Error, invalid \"arguments\" argument.")
    } else {
      arguments <- as.list(x = arguments)
    }
    # Substitutions
    mirror_github_git_final <- mirror_github_git_raw
    for (argument in c("github_repository_source_url",
                       "secret_token_name",
                       "git_repository_target_url",
                       "github_repository_source_url_api")) {
      if (argument == "github_repository_source_url_api") {
        mirror_github_git_final <- stringr::str_replace_all(string = mirror_github_git_final,
                                                            pattern = paste0("<",
                                                                             argument,
                                                                             ">"),
                                                            replacement = stringr::str_extract(string = arguments[["github_repository_source_url"]],
                                                                                               pattern = stringr::str_extract(string = arguments[["github_repository_source_url"]],
                                                                                                                              pattern = "(?<=https://github\\.com/).+")))
      } else if (argument == "git_repository_target_url") {
        mirror_github_git_final <- stringr::str_replace_all(string = mirror_github_git_final,
                                                            pattern = paste0("<",
                                                                             argument,
                                                                             ">"),
                                                            replacement = stringr::str_extract(string = arguments[["git_repository_target_url"]],
                                                                                               pattern = stringr::str_extract(string = arguments[["git_repository_target_url"]],
                                                                                                                              pattern = "(?<=https://).+")))
      } else {
        mirror_github_git_final <- stringr::str_replace_all(string = mirror_github_git_final,
                                                            pattern = paste0("<",
                                                                             argument,
                                                                             ">"),
                                                            replacement = arguments[[argument]])
      }
    }
    if (test_mode == TRUE) {
      return(mirror_github_git_final)
    }
    # Save
    writeLines(text = mirror_github_git_final,
               con = file.path(".github",
                               "workflows",
                               paste0(github_action_name,
                                      ".yml")))
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - GitHub actions \"",
            paste0(github_action_name,
                   ".yml"),
            "\" workflow created successfully.")
  } else {
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Process for \"",
            github_action_name,
            "\" not developed yet.\n",
            "Use the sparck issues section (https://github.com/umr-marbec/den_pages/issues) if you want to propose a new feature or contact directly the package maintainer: ",
            utils::maintainer("sparck"))
  }
  invisible(x = NULL)
}
