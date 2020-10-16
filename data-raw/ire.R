## code to prepare `ire` dataset goes here
load("inst/app/www/ire.Rdata")
usethis::use_data(ire, overwrite = TRUE)

load("inst/app/www/RIP_rk_aggregated_data_merged_28Aug.RData")
usethis::use_data(merged_rk_data, overwrite = TRUE)


load("inst/app/www/rk_groupings.Rdata")
usethis::use_data(rk_grouped)
