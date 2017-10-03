# devtools::install_github("r-hub/sysreqs")
# devtools::install_github("o2r-project/containerit")

setwd("~/Documents/Github/hardwax_bot")

library(containerit)

script <- system.file("schedulescripts", "hardwax_bot.R", package = "googleComputeEngineR")

file.copy(script, getwd())

container <- dockerfile("hardwax_bot.R",
                        copy = "script_dir",
                        cmd = CMD_Rscript("hardwax_bot.R"),
                        soft = TRUE)
write(container, file = "Dockerfile")