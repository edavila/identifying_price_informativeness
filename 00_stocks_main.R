# Identifying Price Informativeness -- Davila/Parlatore, 2020

library(here)
path <- here::here(); print(path); setwd(path); rm(path)

# Removes intermediate and output folders ----

# file.remove(list.files("./intermediate", include.dirs = F, full.names = T, recursive = T))
# file.remove(list.files("./output",       include.dirs = F, full.names = T, recursive = T))

# Runs the main files ----
# 
# rmarkdown::render("01a_stocks_clean_crsp.Rmd");               rm(list = ls());
# rmarkdown::render("01b_stocks_clean_compustat.Rmd");          rm(list = ls());
# rmarkdown::render("01c_inst_ownership.Rmd");                  rm(list = ls());
# rmarkdown::render("02_stocks_merge.Rmd");                     rm(list = ls());
# rmarkdown::render("03a_stocks_select.Rmd");                   rm(list = ls());
# rmarkdown::render("03b_stocks_select_sumstats.Rmd");          rm(list = ls());
rmarkdown::render("04a_stocks_recover.Rmd");                  rm(list = ls());
rmarkdown::render("04b_stocks_recover_clean.Rmd");            rm(list = ls());
rmarkdown::render("05a_stocks_paper_quarterly.Rmd");          rm(list = ls());
rmarkdown::render("05b_stocks_paper_annual.Rmd");             rm(list = ls());
rmarkdown::render("05c_stocks_paper_full.Rmd");               rm(list = ls());
rmarkdown::render("05d_stocks_sumstats_informativeness.Rmd"); rm(list = ls());
rmarkdown::render("06_generate_csv.Rmd");                     rm(list = ls());

# Moves the html files to the output folder ----

files_to_move <- list.files(path = ".", pattern = "*.html", full.names = FALSE, recursive = FALSE)
file.copy(from = files_to_move, to = "output/html_code")
file.remove(files_to_move)

sessionInfo()
