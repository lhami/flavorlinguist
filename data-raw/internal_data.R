## Mostly regular expressions that are long and messy, so I don't want to define them
#at runtime every time I run the functions in R/quantities.R

number_dict <-
  c("a" = 1,
    "an" = 1,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "teen" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fifteen" = 15,
    "twenty" = 20,
    "thirty" = 30,
    "fourty" = 40,
    "forty" = 40,
    "fifty" = 50,
    "sixty" = 60,
    "seventy" = 70,
    "eighty" = 80,
    "ninety" = 90,
    "hundred" = 100,
    "thousand" = 1000,
    "million" = 1000000,
    "billion" = 1000000000,
    "half" = 1/2,
    "halve" = 1/2,
    "third" = 1/3,
    "quarter" = 1/4,
    "fourth" = 1/4,
    "fifth" = 1/5,
    "sixth" = 1/6,
    "seventh" = 1/7,
    "eighth" = 1/8,
    "ninth" = 1/9,
    "tenth" = 1/10,
    "eleventh" = 1/11,
    "twelfth" = 1/12,
    "thirteenth" = 1/13,
    "fourteenth" = 1/14,
    "fifteenth" = 1/15,
    "sixteenth" = 1/16,
    "percent" = 1/100)

number_words <- "((\\ban?\\b|one|two|three|four|five|six|seven|eighth?|nine|ten|eleven|twelve|teen|thirteen|fifteen|twenty|thirty|forty|fifty|ty\\b|ths?\\b|hundred|thousand|million|billion|half|quarter|third|percent|halve)s?[ \\-,]*(and *)?)+"
numbers_regex <- "((?:(?:\\ban?\\b|one|two|three|four|five|six|seven|eighth?|nine|ten|eleven|twelve|teen|thirteen|fifteen|twenty|thirty|forty|fifty|ty\\b|ths?\\b|hundred|thousand|million|billion|half|quarter|third|percent|halve|[\u00BC-\u00BE\u2150-\u215E0-9])s?[ \\-,]*(?:and *)?)+)"
ones_place <- "(?:(\\ban?\\b|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fifteen)(teen)?)"
tens_place <- "(twenty|thirty|fou?rty|fifty|sixty|seventy|eighty|ninety)"
fractions_place <- "(half|halve|third|quarter|fifth|sixth|eighth|ninth|tenth|sixteenth|percent)s?"
hundreds_place <- paste0("(?:", ones_place, "? *(hundred)s?)")
and_regex <- "[ ,]*(?:\\-|and)? *"
one_to_999 <- paste0("(", hundreds_place, "?", and_regex, tens_place, "?", and_regex, ones_place, "?)")
#If you run str_extract() on this, it returns:
#1- whole match,
#2- outer capture group,
#3- pseudo-ones place number of hundreds (1-13 or 15)
#4- teen for numbers 14, 16-19 (if anything is here, add 10)
#5- the word hundred, if this part matched at all (this could maybe be a non-capturing group idk)
#6- the tens place
#7- pseudo-ones place (1-13 or 15)
#8- teen for numbers 14, 16-19 (if anything is here, add 10)
#So we're doing (3 + 4) * 5 + 6 + 7 + 8

number_parts <- paste0("(?:", one_to_999, " *(billion)s?)?", and_regex,
                       "(?:", one_to_999, " *(million)s?)?", and_regex,
                       "(?:", one_to_999, " *(thousand)s?)?", and_regex,
                       one_to_999, "?", and_regex)
fraction_parts <- paste0(ones_place, "[\\s\\-]*", fractions_place)

#would be great if this and several other regexes here were compressed :/
#look at https://github.com/gleenn/regex_compressor algorithm if this ever
#becomes a performance bottleneck--I'd like to keep the definitions here relatively
#human-readable but since this code gets run on my computer to make the distributed
#source, I'm okay with doing some weird magic here before I save things.
units_regex <- paste0("(", paste0(stringr::str_sort(names(units_volume), decreasing = TRUE), collapse = "|"), ")")

usethis::use_data(number_dict,
                  number_words,
                  numbers_regex,
                  ones_place,
                  tens_place,
                  fractions_place,
                  hundreds_place,
                  and_regex,
                  one_to_999,
                  number_parts,
                  fraction_parts,
                  units_regex,
                  internal = TRUE, overwrite = TRUE)
