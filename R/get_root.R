#Custom function that attempts to retrieve the "root" of adjectival words by removing
#common adjective endings from the word
#This is best used to find words that exist in both noun and adjective forms in
#the data set, to collapse the counts of those rows. Sort of like using an internal dictionary
#It does also use an actual dictionary (hunspell) to try and navigate the fact that some
#roots ending in "e" lose their e when a suffix is added.
#Note that this is not perfect. E.g. it will return "hon" for "honey". It is intended to be used
#to collapse words in the dataset, so that one of the actual words present in the dataset can be
#used as the canonical form of the word.
#slightly changed from previously published code to stop it from putting e or y onto words
#it didn't make any changes to
get.root <- function(x) {
  poss.roots <- str_remove(x, "-?(full?|i?ness|al|ish|like|i?ed|ive|al|e?y|en|er)$")
  poss.roots <- ifelse(poss.roots != x & !hunspell_check(poss.roots) &
                         hunspell_check(str_c(poss.roots, "e")),
                       str_c(poss.roots, "e"),
                       poss.roots
  )
  poss.roots <- ifelse(str_length(poss.roots) > 0 & hunspell_check(poss.roots),
                       poss.roots,
                       x)
  ifelse(hunspell_check(str_c(poss.roots, "y")) & !(poss.roots %in% c("crisp", "tart", "soft", "full")),
         str_c(poss.roots, "y"),
         poss.roots
  )
}
#Yeah, this needs to be plugged into some more complicated rules or some database
#of known flavor words if I want to stop it from turning whiskey into whisk or tart
#into tarty
