
library(tidyverse)
library(magrittr)
library(snakecase)
library(lubridate)
library(rvest)
library(httr)

transfm_player <- function(n) {
  # set user string
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"
  
  #get HTML
  h <- GET(URLencode(glue(URL)),user_agent(ua)) %>% read_html
  if (h %>% html_element(".info-content") %>% length() >1) {
    return(NA)
  }
  
  shirt_number <- h %>% html_element(".data-header__shirt-number") %>% html_text(trim = T)
  
  last_name <- h %>% html_node("strong") %>% html_text(trim = T)
  
  # first names have to be processed a little differently, as they aren't given
  # their own element on the website, instead, lumped in with the last name and 
  # shirt number. Once those have been acquired (see above),
  first_name <- h %>% 
    html_node("h1")  %>% 
    html_text(trim = T) %>% 
    str_replace(pattern = str_c(" ",last_name), replacement = "")
  # we remove the last name from the string, and then (after checking that the 
  # shirt number is given for a particular player), remove that too, if it exists
  first_name <- ifelse(is.na(shirt_number), first_name, str_replace(pattern = shirt_number, replacement = "")) %>% str_trim()
  
  shirt_number <- substr(shirt_number, 2, 999) # taking off the "#" from the start of, say, "#10"
  
  club <- h %>% html_element(".data-header__club") %>% html_text(trim = T)
  
  dob <- h %>% html_nodes("[itemprop=birthDate]") %>% 
    html_text(trim = T) %>% 
    gsub(pattern = '\\"', replacement = "") %>% 
    substr(1, 12) %>% str_trim %>% mdy()
  
  citizenship <- h %>% html_nodes("[itemprop=nationality]") %>% html_text(trim = T)
  
  birth_city <- h %>% html_nodes("[itemprop=birthPlace]") %>% html_text(trim = T)
  
  birth_country <- h %>%  html_nodes(".data-header__label:contains('Place of birth:')") %>% html_node("img") %>% html_attr("title")
  
  position <- h %>% html_nodes(".weitere-daten-spielerprofil") %>% html_nodes("dd") %>% html_text(trim = T)
  
  height <-  h %>% html_nodes("[itemprop=height]") %>% html_text(trim = T) %>% substr(1, 4) %>% gsub(pattern = ",", replacement = "\\.") %>% as.numeric() 
  
  current_value <- ifelse(club == "Retired", NA, h %>% html_element(".data-header__market-value-wrapper") %>% html_text(trim = T) %>% str_extract(pattern = "[0-9]*[km]") %>% gsub(pattern = "\\k", replacement = "000") %>% gsub(pattern = "\\m", replacement = "000000") %>% as.numeric())
  
  name_in_country <- h %>% html_element(".info-table--right-space:contains('Name in home country:')") # check if person has a field for a name in their birth country. If yes, fill it in
  name_in_country <- ifelse(is.na(name_in_country), NA, name_in_country %>% html_element(".info-table__content--bold") %>% html_text())
  
  # as with name_in_country, so it is with foot 
  foot_i <- h %>% html_elements(".info-table__content") %>% html_text(trim = T)
  foot <- foot_i[("Foot:" == foot_i) %>% match(x = TRUE) +1]
  
  Sys.sleep(time = 8) #sleep for some time, to (hopefully!) avoid 403 errors
  
  return(as_tibble(list("id" = n, 
                        "last_name" = last_name, 
                        "first_name" = first_name, 
                        "club" = club, 
                        "dob" = as.character(dob), 
                        "citizenship" = citizenship, 
                        "birth_city" = birth_city, 
                        "birth_country" = birth_country, 
                        "shirt_number" = shirt_number, 
                        "main_position" = position[1], 
                        "other_position_1" = position[2], 
                        "other_position_2" = position[3], 
                        "height" = height, 
                        "current_value" = current_value, 
                        "name_in_country"= name_in_country, 
                        "foot" = foot)))
  
}