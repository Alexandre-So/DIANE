# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "DIANE", # The Name of the package containing the App 
  pkg_title = "DIANE", # The Title of the package containing the App 
  pkg_description = "Dashboard for the Inference and Analysis of Network from Expression data", # The Description of the package containing the App 
  author_first_name = "Oceane", # Your First Name
  author_last_name = "Cassan", # Your Last Name
  author_email = "oceane.cassan@supagro.fr", # Your Email
  repo_url = "https://github.com/OceaneCsn/DIANE" # The URL of the GitHub Repo (optional) 
)    

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
# usethis::use_mit_license( name = "Golem User" )  # You can set another license here
# usethis::use_readme_rmd( open = FALSE )
# usethis::use_code_of_conduct()
# usethis::use_lifecycle_badge( "Experimental" )
# usethis::use_news_md( open = FALSE )
usethis::use_gpl3_license(name = "Oceane Cassan")


## Use git ----
#usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
#golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon("D:/These/Thesis/hex-DIANE.png") # path = "path/to/ico". Can be an online file. 

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# Change Author
p <- c(person(given = "Océane",
              family = "Cassan",
              role = c("cre", "aut"),
              email = "oceane.cassan@lirm.fr",
              comment = c(ORCID = "0000-0002-4595-2457")),
       person(given = "Alexandre",
              family = "Soriano",
              role = c("aut"),
              email = "alexandre.soriano@cirad.fr",
              comment = c(ORCID = "0000-0003-1406-46067")))

desc::desc_set_authors(p)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

