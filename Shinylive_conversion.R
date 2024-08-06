
#Use pak to install the shinylive package
install.packages("pak")
install.packages("shinylive")

library("shinylive")

# Vector directory of the shiny app
shiny_dir <- "C:/Users/bblack/switchdrive/Private/git_laptop/NFF-coding"

# Vector directory of website to host the shiny app
website_dir <- "C:/Users/bblack/switchdrive/Private/git_laptop/NASCENT-Peru.github.io"

# Convert the shiny app to a shiny live app
shinylive::export(shiny_dir, "docs")

