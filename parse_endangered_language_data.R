


## parse the endangered language data 
library(tidyverse)

infile <- '~/endangered_language_sounds/endangered_language_data_from_ELP.csv'
dat <- read_csv(infile, col_names = FALSE)


new_names <- c('id', 'code', 'language_name', 'other_names', 'status', 'num_speakers', 'language_family', 'dialects',
               'language_notes', 'language_notes2', 'countries', 'region', 'coordinates')
names(dat) <- new_names


dat <- dat %>% 
  mutate(endangered_status = case_when(str_detect(status, 'At risk') ~ 'At Risk',
                                       str_detect(status, 'Awakening') ~ 'Awakening',
                                       str_detect(status, 'Critically Endangered') ~ 'Critically Endangered',
                                       str_detect(status, 'Dormant') ~ 'Dormant',
                                       str_detect(status, 'Severely Endangered') ~ 'Severely Endangered',
                                       str_detect(status, 'Safe') ~ 'Safe',
                                       str_detect(status, 'Vulnerable') ~ 'Vulnerable',
                                       str_detect(status, 'Threatened') ~ 'Threatened',
                                       str_detect(status, 'Endangered') ~ 'Endangered',
                                       TRUE ~ 'unknown'),
         id = as.character(id))

#dat %>% count(endangered_status)

write_csv(dat, '~/endangered_language_sounds/endangered_languages.csv')
