get_regions <- function(){
  hamiltonExcessdeaths::merged_rk_data %>% 
    dplyr::distinct(Group) %>% 
    dplyr::filter(!is.na(Group))
}