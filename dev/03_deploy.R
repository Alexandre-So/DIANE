# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile()
golem::add_dockerfile_with_renv()

## If you want to deploy to ShinyProxy
golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
golem::add_dockerfile_heroku()


golem::add_dockerfile_with_renv_shinyproxy(from = "rocker/shiny:4.4.1",
  source_folder = ".",
  output_dir = paste(
    "Docker_deployment_shinyproxy",
    paste0(golem::pkg_name(), "_",
           stringr::str_replace_all(
             format(Sys.time(), "%Y.%d.%m_%X"),
             pattern = ":",
             replacement = "."
           )), sep = "/"
  )
)

golem::add_dockerfile_with_renv(
  from = "rocker/shiny",
  source_folder = ".",
  output_dir = paste(
    "Docker_deployment_renv",
    paste0(
      golem::pkg_name(),
      "_",
      stringr::str_replace_all(
        format(Sys.time(), "%Y.%d.%m_%X"),
        pattern = ":",
        replacement = "."
      )
    ),
    sep = "/"
  ), lockfile = "./renv.lock", dockerfile_cmd = "/usr/bin/shiny-server", sysreqs = TRUE, port = 8086, expand = TRUE
)
