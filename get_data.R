# Creating the DOGS dataframe 
library(dplyr)

# load in datasets
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits <- breed_traits %>%
  arrange(Breed)

breed_rank_all <- breed_rank_all %>%
  arrange(Breed) %>%
  rename(Breed_R = Breed) 

breed_traits <- cbind(breed_traits,breed_rank_all )
breed_traits <- breed_traits %>%
  select(Breed, Breed_R, everything()) %>%
  select(-Breed_R) 

regex_pattern <- "(.*)\\((.*)\\)"
breed_traits$Breed <- gsub(regex_pattern, "\\2 \\1", breed_traits$Breed)

#Downloaded XLSX file from 'https://github.com/MeganSorenson/American-Kennel-Club-Breeds-by-Size-Dataset/blob/main/AmericanKennelClubBreedsBySize.xlsx', httr::write_disk("test.xlsx"))
breed_size <- readxl::read_excel("AmericanKennelClubBreedsBySize.xlsx")

combined_dog_data1 <- fuzzyjoin:: stringdist_left_join(breed_traits, breed_size, max_dist = 1,  ignore_case = TRUE, by = c(Breed = "Breed"))
combined_dog_data1 <- combined_dog_data1 %>%
  select(Breed.x, Breed.y, everything()) %>%
  filter(!is.na(Breed.y))

combined_dog_data2 <- fuzzyjoin:: stringdist_left_join(breed_traits, breed_size, max_dist = 2,  ignore_case = TRUE, by = c(Breed = "Breed"))
combined_dog_data2 <- combined_dog_data2 %>%
  filter(!Breed.x %in% combined_dog_data1$Breed.x) %>%
  select(Breed.x, Breed.y, everything()) %>%
  filter(!is.na(Breed.y))

combined_dog_data3 <- fuzzyjoin:: stringdist_left_join(breed_traits, breed_size, max_dist = 3,  ignore_case = TRUE, by = c(Breed = "Breed"))
combined_dog_data3 <- combined_dog_data3 %>%
  filter(!Breed.x %in% c(combined_dog_data1$Breed.x, combined_dog_data2$Breed.x)) %>%
  select(Breed.x, Breed.y, everything()) %>%
  filter(!is.na(Breed.y))

combined_dog_data4 <- fuzzyjoin:: stringdist_left_join(breed_traits, breed_size, max_dist = 4,  ignore_case = TRUE, by = c(Breed = "Breed"))
combined_dog_data4 <- combined_dog_data4 %>%
  filter(!Breed.x %in% c(combined_dog_data1$Breed.x, combined_dog_data2$Breed.x, combined_dog_data3$Breed.x)) %>%
  select(Breed.x, Breed.y, everything()) %>%
  filter(!is.na(Breed.y)) %>%
  filter(!Breed.x == "Poodles")

breed_size$Breed <- gsub("Poodle", "Poodles", breed_size$Breed)
ci_str_detect <- function(x, y){stringr::str_detect(x, stringr::regex(y, ignore_case = TRUE))}
combined_dog_data_poodles <-  fuzzyjoin::fuzzy_left_join(breed_size, breed_traits, match_fun = ci_str_detect, by = c(Breed="Breed"))
combined_dog_data_poodles <- combined_dog_data_poodles %>%
  filter(Breed.y == "Poodles") %>%
  filter(Breed.x == "Standard Poodles")

all_dogs <- rbind(combined_dog_data_poodles, combined_dog_data1, combined_dog_data2, combined_dog_data3, combined_dog_data4)
all_dogs <- all_dogs %>%
  rename(Breed= Breed.x) %>%
  select(-Breed.y)

names(all_dogs) <- janitor::make_clean_names(names(all_dogs))
readr::write_csv(all_dogs, "dogs.csv")
