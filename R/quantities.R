#' Convert natural language numbers to numeric type
#'
#' `to_number()` can be used to convert natural language expressions like
#' "fourteen", "eleven hundred", "six fifths", or "two hundred and a half" to
#' actual numbers suitable for mathematical functions. It will return a vector
#' the same length as `string`.
#'
#' `to_number()` currently supports correctly-spelled English language numbers
#' up to the billions and mixed, proper, or improper fractions with denominators
#' 2-16. `string` must already be cleaned with one number per value.
#'
#' @param string A character vector containing one English-language number per
#'   value.
#'
#' @return A numeric vector of the same length as `string`
#' @seealso [extract_numbers()] for finding all natural language numbers inside
#'   texts or [ingredient_parts()] for finding numbers with associated units
#' @export
#'
#' @examples
#' english_numbers <- c("one", "two", "three", "four and a half",
#' "five million and thirty-seven", "eleven hundred", "sixty-four",
#' "seven thousand fifty-four and a half", "six and twelve halves",
#' "nine hundred ninety nine billion nine hundred ninety-nine thousand nine hundred and ninety-nine",
#' "five hundred thirty seven thousand four hundred and twenty eight and seven eighths")
#' to_number(english_numbers)
#'
#' # If you pass in a string with multiple numbers, to_number() may behave unexpectedly.
#' listed_numbers <- "three fish, fourteen and a half plates, a sixth of a liver, twelve geese"
#' to_number(listed_numbers)
#'
#' # It's more tolerant of non-numeric words, but you shouldn't rely on this:
#' to_number(c("three fish", "fourteen and a half plates", "a sixth of a liver", "twelve geese"))
#'
#' # Use extract_numbers() or ingredient_parts() instead.
#' extract_numbers(listed_numbers)
#' ingredient_parts(listed_numbers, mode = "paragraph")
to_number <- function(string) {
  #First, we put the natural language whole number portion in whole_portion and
  #the natural language fraction portion in frac_portion
  whole_portion <- stringr::str_split_i(string,
                                        stringr::regex(paste0(and_regex, ones_place, "[\\s\\-]*", fractions_place),
                                                       ignore_case = TRUE),
                                        i = 1)

  frac_portion <- stringr::str_extract(string, stringr::regex(paste0(ones_place, "[\\s\\-]*", fractions_place),
                                                              ignore_case = TRUE))

  #Next, we split each into a matrix of "parts"--columns have different natural
  #language components with numeric value

  natlang_whole <- stringr::str_match(whole_portion, stringr::regex(number_parts, ignore_case = TRUE))
  #1- whole match
  #2, 10, 18, 26 are all 999 for each part
  #so it's (3 + 4) * 5 + 6 + 7 + 8
  #all multiplied by 9
  #and repeat for all numbers +8, +16, and +24 (sum together)
  #except there is no "ones" for 33
  natlang_frac <- stringr::str_match(frac_portion, stringr::regex(fraction_parts, ignore_case = TRUE))
  #1- whole match
  #2 has the pseudo-ones (1-13 or 15) of the numerator and 3 has "teen" or is empty
  #4 has the denominator

  #Now we actually parse these into numbers using number_dict for translation

  #It would maybe be better to have a regex engine that supported matching
  #from the right or a state machine starting at the right of a number
  #that consumed portions as it added instead of a regex-shaped monstrosity.
  whole_num <- apply(natlang_whole, 1, \(x) {
    numparts <- number_dict[x]
    numparts[is.na(numparts)] <- 0
    places <- c(numparts[1:3 * 8 + 1], 1)
    val <- 0
    for (i in 0:3 * 8) {
      val <- val + ((numparts[3 + i] + numparts[4 + i]) * numparts[5 + i] +
                      numparts[6 + i] + numparts[7 + i] + numparts[8 + i]) * places[i/8 + 1]
    }
    val
  })

  frac_num <- apply(natlang_frac, 1, \(x) {
    fracparts <- number_dict[x]
    fracparts[is.na(fracparts)] <- 0
    val <- ((fracparts[2] + fracparts[3]) * fracparts[4])
    val
  })

  whole_num + frac_num
}

#' Collect all of the natural language numbers from texts
#'
#' `extract_numbers()` can be used to extract natural language expressions like
#' "fourteen", "eleven hundred", "six fifths", or "two hundred and a half" from
#' a text and convert them to actual numbers suitable for mathematical
#' functions. It can extract multiple numbers from each value in `string`,
#' returning either a long data frame or a list of data frames. See
#' [to_number()] for the underlying implementation.
#'
#' @param string A character vector from which to extract English Language
#'   numbers
#' @param return Format for the return data, either "df" or "list"
#'
#' @return For `mode = "df"`, a "long" data frame with one row per natural
#'   language number found in `string`, with columns:
#'
#'   * `ID`, the position in `string` where the natural language number was found.
#'   * `Input`, the unmodified value of `string` where the natural language number was found.
#'   * `Number_Words`, the natural language number as found in the text
#'   * `Value`, a best-effort numeric representation of the number, using [to_number()].
#'
#'   The data frame will have at least as many rows as values in `string`, since
#'   values without natural language numbers will return rows with an `ID` and
#'   `Input` but NA for other columns.
#'
#'   For `mode = "list"`, a list of the same length as `string` containing data
#'   frames with `Number_Words` and `Value`.
#'
#' @export
#' @seealso [to_number()] to extract and convert one number per entry in
#'   `string`.
#' @examples
#' english_numbers <- c("one", "two", "three", "four and a half",
#' "five million and thirty-seven", "eleven hundred", "sixty-four",
#' "seven thousand fifty-four and a half", "six and twelve halves",
#' "nine hundred ninety nine billion nine hundred ninety-nine thousand nine hundred and ninety-nine",
#' "five hundred thirty seven thousand four hundred and twenty eight and seven eighths",
#' "three fish, fourteen and a half plates, a sixth of a liver, twelve geese")
#' extract_numbers(english_numbers)
#'
#' # If there's only one natural language number per entry in string, the only difference
#' # between extract_numbers() and to_number() is the return format.
#' extract_numbers(c("four", "six hundred twenty-eight"))
#' extract_numbers(c("four", "six hundred twenty-eight"), return = "list")
#' to_number(c("four", "six hundred twenty-eight"))
extract_numbers <- function(string, return = c("df", "list")) {
  return <- match.arg(return)
  quantities <- lapply(stringr::str_extract_all(string, stringr::regex(number_words, ignore_case = TRUE)), stringr::str_squish)
  extracted_numbers <- lapply(quantities, to_number)

  #as far as return values go, maybe I need to have a mode that will only match
  #one per string
  #and a mode that will return either a list of quantity-tables

  #string is a vector of the original input text which should be the same
  #length as quantities and extracted_numbers.
  #I want to return a long df, so there may be multiple rows per
  #entry in string (but quantities and extracted_numbers should have the same shape)
  if (return == "df") {
    df <- data.frame("ID" = rep(1:length(string), times = sapply(extracted_numbers, length)),
                     "Input" = rep(string, times = sapply(extracted_numbers, length)),
                     "Number_Words" = unlist(quantities),
                     "Value" = unlist(extracted_numbers))
    df
  } else {
    lapply(1:length(string), \(i) data.frame("Number_Words" = quantities[i], "Value" =extracted_numbers[i]))
  }
}

#' Collect ingredient quantity-shaped numbers from texts
#'
#' `ingredient_parts()` can be used to extract natural language expressions like
#' "three cups flour", "1 tsp vanilla extract", "half gallon milk", or "¼ melon"
#' from a text and convert them to actual numbers suitable for mathematical
#' functions. Currently, only liquid volume units are fully supported.
#'
#' @param string A character vector from which to extract number-unit-ingredient
#'   expressions
#' @param mode Search method to use (affects the number of matches possible per
#'   entry and ).
#' \describe{
#'  \item{`single`}{Looks for one number per item in `string`, optionally with a unit, and then treats the rest of the string as the ingredient. Best for text you've already cleaned.}
#'  \item{`multiline`}{Looks for number per *line* in each item of `string` (assuming newline characters are present), optionally with a unit, and then treats the rest of the string as the ingredient. Be cautious using this with scraped HTML.}
#'  \item{`paragraph`}{Can find mulltiple numbers (optionally with units) per item in `string`, using either a newline character, a tab, a comma, a semicolon, or a pipe (|) to mark the end of the ingredient name.}
#' }
#' @param return Format for the return data, either "df" or "list"
#' @param convert Boolean. If `TRUE` (default), will add columns to the data
#'   frame containing a numeric form of the extracted number and, for known
#'   units of volume, an attempted conversion to Liters. Must be `FALSE` for
#'   `return = "list"`.
#'
#' @return For `mode = "df"`, a "long" data frame with one row per
#'   number-unit-ingredient expression found in `string`, with columns:
#'
#'   * `ID`, the position in `string` where the natural language number was found.
#'   * `Input`, the unmodified value of `string` where the natural language number was found.
#'   * `Extracted_Phrase`, the number-unit-ingredient expression as found in the text
#'   * `Number_Words`, the natural language number as found in the text
#'   * `Unit_Words`, the natural language unit of volume (if any) found in the text, or NA.
#'   * `Ingredient_Words`, the portion of the `Extracted_Phrase` after the number and unit.
#'
#'   If `convert == TRUE`, the data frame will also have columns:
#'
#'   * `Raw_Number`, a best-effort numeric representation of the `Number_Words`, using [to_number()].
#'   * `Converted_Amount`, a conversion of `Raw_Number` to liters, where possible.
#'   * `Converted_Unit`, the units corresponding to `Converted_Amount`.
#'
#'   The data frame will have at least as many rows as values in `string`, since
#'   values without number-unit-ingredient expressions will return rows with an
#'   `ID` and `Input` but NA for other columns.
#'
#'   For `mode = "list"`, a list of the same length as `string` containing
#'   character matrices of the matches from [stringr::str_match_all()]. Each
#'   matrix will have one row per number-unit-ingredient match and 4 columns
#'   with the whole match and then the 3 component parts.
#' @seealso [units_volume] for the table of unit conversions used when `convert
#'   = TRUE`
#' @export
#'
#' @examples
#' ingredient_parts(c("1 tsp butter", "3 gallons milk", "14 eggs",
#' "fifteen bottles ketchup", "four hundred and a half snails"))
#' ingredient_parts(c("1 tsp butter", "3 gallons milk", "14 eggs",
#' "fifteen bottles ketchup", "four hundred and a half snails",
#' "3 eggs, 2 pints of milk, four and a half cups flour"),
#' mode = "paragraph")
ingredient_parts <- function(string,
                             mode = c("single", "multiline", "paragraph"),
                             return = c("df", "list"),
                             convert = TRUE) {
  mode <- match.arg(mode)
  return <- match.arg(return)
  if (convert & return == "list") {
    stop("flavorlinguist::ingredient_parts() does not currently support a list return type for converted ingredient data.
          To put in a feature request, please explain your situation and desired data format at https://github.com/lhami/flavorlinguist/issues")
  }
  multiline <- mode == "multiline"
  modstring <- stringr::str_replace_all(string, "\\bT(\\.|sp|SP|\\b)", "tbsp")
  if(mode == "paragraph") {
    modstring <- stringr::str_replace_all(modstring, stringr::regex(numbers_regex, ignore_case = TRUE),
                                       replacement = \(x) paste0("| ", x))
    ingredient_regex <- "([^,\\t\\n;|]*)"
  } else {
    ingredient_regex <- "(.*$)"
  }
  ingredient_regex <- stringr::regex(paste(numbers_regex,
                                           paste0(units_regex, "?"),
                                           ingredient_regex,
                                           sep = "\\s*"),
                                     ignore_case = TRUE)

  parts <- stringr::str_match_all(modstring, ingredient_regex)
  if (return == "df" | convert) {
    df <- data.frame("ID" = rep(1:length(string), times = sapply(parts, nrow)),
                     "Input" = rep(string, times = sapply(parts, nrow)),
                     "Extracted_Phrase" = unlist(sapply(parts, \(x) {x[ , 1]})),
                     "Number_Words" = unlist(sapply(parts, \(x) {x[ , 2]})),
                     "Unit_Words" = unlist(sapply(parts, \(x) {x[ , 3]})),
                     "Ingredient_Words" = unlist(sapply(parts, \(x) {x[ , 4]})))
  }

  if (convert) {
    df$Number_Words <- stringr::str_squish(df$Number_Words)
    df$Unit_Words <- stringr::str_squish(df$Unit_Words)
    #This may not work for all locales
    withCallingHandlers({
      df$Raw_Number <- ifelse(is.na(as.numeric(df$Number_Words)),
                              to_number(df$Number_Words),
                              as.numeric(df$Number_Words))
    }, warning = function(w) {
      if(startsWith(conditionMessage(w), "NA")) invokeRestart("muffleWarning")
    })
    df$Converted_Amount <- df$Raw_Number *
      ifelse(is.na(flavorlinguist::units_volume[df$Unit_Words]),
             1,
             flavorlinguist::units_volume[df$Unit_Words])
    df$Converted_Unit <- ifelse(is.na(flavorlinguist::units_volume[df$Unit_Words]),
                                df$Unit_Words,
                                "Liter")
  }

  if (return ==  "list") {
    parts
  } else {
    df
  }
}
