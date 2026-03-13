# GitHub Action workflows creation

This function generates GitHub Actions workflows for specific tasks. The
yaml file association will be created at the root of the R working
directory under folders ".github/workflows". Theses folders will be
created automatically if necessary (you need to have the correct
computer's file system access).

## Usage

``` r
add_github_action(github_action_name, arguments = NULL, test_mode = FALSE)
```

## Arguments

- github_action_name:

  Mandatory. Class "character" is expected. Identification of the GitHub
  Action that you want to generate.

- arguments:

  Optional. Class "NULL" of "character" is expected. By default NULL.
  Argument(s) for a dynamic substitution in the GitHub Action template.

- test_mode:

  Optional. Class "logical" is expected. By default FALSE. Optional
  argument used for the unit tests. Don't use it in a regular way.

## Value

No return in the R environment. If the function run successfully, the
yaml file associated with the GitHub Action workflow is created in the
folders ".github/workflows" at the root of your working directory.

## Details

GitHub Actions workflows developed so far are:

- "mirror_github_git", create a workflow to establishing a mirror
  between a GitHub repository and a repository from another Git. For
  this process, you need to fill three variables in the "arguments"
  parameter: "github_repository_source_url", "secret_token_name" and
  "git_repository_target_url". A full documentation is available through
  this
  [link](https://umr-marbec.github.io/den_pages/en/pages/git/miroir_github_git.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# example for "mirror_github_git" GitHub Action workflow creation
add_github_action(github_action_name = "mirror_github_git",
                  arguments = c("github_repository_source_url" = "url_source",
                                "secret_token_name" = "my-awesome-token-name",
                                "git_repository_target_url" = "url_target"))
} # }
```
