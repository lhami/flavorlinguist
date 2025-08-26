#' Collapse parts-of-speech differences
#'
#' `get_root()` attempts to retrieve the "root" of adjectives, descriptive
#' verbs, and plurals by removing common adjective and plural endings from the
#' words in `x`. This is best used to combine multiple forms of words which
#' exist in your dataset as both adjectives and nouns, to avoid splitting your
#' variables. It's a bit like using an internal dictionary, and it *does* use a
#' dictionary (hunspell) to try and navigate the fact that some roots ending in
#' "e" (e.g., "cheese") lose their "e" when a suffix is added (e.g., "cheesy").
#'
#' Please check `get_root()`'s performance with some selected words from your
#' data to ensure that they're combining and outputting correctly for your
#' intended use. This function prioritizes collapsing redundant word forms where
#' possible over returning pretty, perfect English--some words are prone to
#' returning fragments like "hon" for honey. You can always choose
#' (systematically or at random) a specific extension of the word-root that
#' appears in your data as the "display" form of the root.
#'
#' @param x A character vector of cleaned tokens
#'
#' @return A character vector of word-roots
#' @export
#'
#' @examples
#' x <- c("tart", "tartness", "tarts", "fruit", "fruity", "fruitiness", "fruits",
#' "whiskey", "whisky", "nuts", "nut", "nutty", "rotten", "rotted", "rot",
#' "peppery", "plummy", "cheese", "cheesy")
#' get_root(x)
get_root <- function(x) {
  #I think I'm intending to rewrite this such that it creates a vector of replacements
  #to be made in the data and then remaps them and can return one, the other, or
  #both in a complex object if needed.
  #And maybe also writing a version that doesn't depend on hunspell or could take
  #another dictionary or rely purely on the internal package data or something.
  poss.roots <- stringr::str_remove(x, "-?(full?|i?ness|al|ish|like|i?ed|ive|al|e?y|en|er)$")
  poss.roots <- ifelse(poss.roots != x & !hunspell::hunspell_check(poss.roots) &
                         hunspell::hunspell_check(stringr::str_c(poss.roots, "e")),
                       stringr::str_c(poss.roots, "e"),
                       poss.roots
  )
  poss.roots <- ifelse(stringr::str_length(poss.roots) > 0 & hunspell::hunspell_check(poss.roots),
                       poss.roots,
                       x)
  ifelse(hunspell::hunspell_check(stringr::str_c(poss.roots, "y")) & !(poss.roots %in% c("crisp", "tart", "soft", "full")),
         stringr::str_c(poss.roots, "y"),
         poss.roots
  )
  #Yeah, this needs to be plugged into some more complicated rules or some database
  #of known flavor words if I want to stop it from turning whiskey into whisk or tart
  #into tarty
}

