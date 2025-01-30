test_that("messages expected", {
  expect_message(object = add_github_action(github_action_name = "not_developed_yet",
                                            test_mode = TRUE),
                 regexp = "^.+ - Process for \\\"not_developed_yet\\\" not developed yet\\.")
})

test_that("outputs expected", {
  expect_true(object = !all(stringr::str_detect(string = add_github_action(github_action_name = "mirror_github_git",
                                                                           arguments = c("github_repository_source_url" = "https://github.com/my-source-repository",
                                                                                         "secret_token_name" = "my-awesome-token-name",
                                                                                         "git_repository_target_url" = "https://gitlab.com/my-target-repository"),
                                                                           test_mode = TRUE),
                                                pattern = "<[^>]+>")))
})
