
library(tidyverse)
library(purrr)
library(glue)
library(lubridate)

# con <- connect_mitto()
# opp <- get_mitto_data(con, 'opportunity')
# account <- get_mitto_data(con, 'account')

## this file loads the phoneme description file (CHARCODE.STR) and parses in to create a data.frame() with one phoneme per row
## it makes the changes below as described at - http://www.phonetik.uni-frankfurt.de/upsid_changes.html
# Changes in CHARCOD.STR:
#   
# removed G< and h2 since they seem not to be used anywhere
# 
# nasally released -> nasally-released
# labio-dental -> labiodental
# labio-velar -> labial-velar
# palatal-alveolar -> palato-alveolar
# glottal plosive -> voiceless glottal plosive (?W, ?9, ?)
# "h" -> glottal fricative
# glottalized voiceless affricated dental click -> glottalized voiceless dental affricated click
# voiceless palatalized dental sibilant affricate -> palatalized voiceless dental sibilant affricate
# voiced sibilant palatal affricate -> voiced palatal sibilant affricate
# palatalized voiced sibilant dental fricative -> palatalized voiced dental sibilant fricative
# 
# Eh, Oh: lower-mid -> lower mid (27-01-2006)
# o+, o(+~: fronted mid back -> mid fronted back (27-01-2006)
#       o(+, o+: fronted higher mid back -> higher mid fronted back (27-01-2006)
#         "e_: retracted mid front -> mid retracted front (27-01-2006)
# oS: added 'overshort' (14-mar-2010)
# 6_: changed 'voiceless' to 'voiced' (24-aug-2012)
# RFW: added 'labialized' (08-aug-2016)

charcode_file <- '~/endangered_language_sounds/upsid_data/CHARCOD.STR'
charcode_raw <- read_lines(charcode_file)
chars = charcode_raw[seq(1, length(charcode_raw), by = 2)]
descriptions = charcode_raw[seq(2, length(charcode_raw), by = 2)]

phoneme_descriptions <- data.frame(phoneme = chars, description = descriptions)


clean_description <- function(description) {
  d <- str_replace(description, 'nasally released', 'nasally-released')
  d <- str_replace(d, 'labio-dental', 'labiodental')
  d <- str_replace(d, 'labio-velar', 'labial-velar')
  d <- str_replace(d, 'palatal-alveolar', 'palato-alveolar')
  d <- str_replace(d, 'glottal plosive', 'voiceless glottal plosive')
  d <- str_replace(d, '"h"', 'glottal fricative')
  
  # glottal plosive -> voiceless glottal plosive (?W, ?9, ?)
  # "h" -> glottal fricative
  d <- str_replace(d, 'glottalized voiceless affricated dental click', 'glottalized voiceless dental affricated click')
  d <- str_replace(d, 'voiceless palatalized dental sibilant affricate', 'palatalized voiceless dental sibilant affricate')
  d <- str_replace(d, 'voiced sibilant palatal affricate', 'voiced palatal sibilant affricate')
  d <- str_replace(d, 'palatalized voiced sibilant dental fricative', 'palatalized voiced dental sibilant fricative')
  d <- str_replace(d, 'lower-mid', 'lower mid')
  d <- str_replace(d, 'fronted mid back', 'mid fronted back')
  d <- str_replace(d, 'fronted higher mid back', 'higher mid fronted back')
  d <- str_replace(d, 'retracted mid front', 'mid retracted front')
  d <- str_replace(d, 'higher mid back rounded vowel', 'overshort higher mid back rounded vowel')
  
  return(d)
  
}


phoneme_descriptions <- phoneme_descriptions %>% 
  mutate(description_fixed = map_chr(description, clean_description),
         # not all fixes can be based on description alone
         description_fixed = case_when(phoneme == '6_' ~ 'voiced palato-alveolar fricative',
                                       phoneme == 'RFW' ~ 'labialized voiceed uvular fricative',
                                       TRUE ~ description_fixed))


write_csv(phoneme_descriptions, '~/endangered_language_sounds/phoneme_descriptions.csv')


## now parse the UPSID_MATRIX.txt file to create a data.frame with two columns (language, phoneme) and one row per language phoneme
# pair 

upsid_matrix_file <- '~/endangered_language_sounds/upsid_data/UPSID_MATRIX.txt'

upsid_raw = read_file(upsid_matrix_file)

langs <- str_split(upsid_raw, pattern = '\n') %>% flatten()
langs_split <- map(langs, ~str_split(., pattern = '\t'))


langs_split <- map(langs_split, ~flatten_chr(.))
lang_list <- langs_split %>% map(function(x) {
  language_name = x[1]
  phonemes = x[2:length(x)]
  phonemes = phonemes[phonemes != ""]
  phonemes = list(phonemes)
  names(phonemes) = language_name
  return(phonemes)
})


ll <- flatten(lang_list)
inventory <- map2_df(ll, names(ll), function(x,y) {
  data.frame(language = rep(y, length(x)),
             phoneme = x)}
)


write_csv(inventory, '~/endangered_language_sounds/phoneme_inventories.csv')
