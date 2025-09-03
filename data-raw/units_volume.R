## code to prepare `units_volume` dataset goes here
#note that this is meant for fluid volumes, mostly in SI or US, based off of
#https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#VOLUME
units_volume <- c("liter" = 1,
                  "litre" = 1,
                  "liters" = 1,
                  "litres" = 1,
                  "l" = 1,
                  "ml" = 0.001,
                  "milliliter" = 0.001,
                  "milliliters" = 0.001,
                  "millilitre" = 0.001,
                  "millilitres" = 0.001,
                  "cup" = 0.2365882,
                  "cups" = 0.2365882,
                  "c" = 0.2365882,
                  "fl oz" = 0.02957353,
                  "floz" = 0.02957353,
                  "oz" = 0.02957353, #might need to switch to handle imperial oz or dry oz
                  "fluid ounces" = 0.02957353,
                  "fluid oz" = 0.02957353,
                  "ounces" = 0.02957353,
                  "ounce" = 0.02957353,
                  "tsp" = 0.004928922,
                  "teaspoon" = 0.004928922,
                  "teaspoons" = 0.004928922,
                  "Tsp" = 0.01478676,
                  "tbsp" = 0.01478676,
                  "tablespoon" = 0.01478676,
                  "tablespoons" = 0.01478676,
                  "gallon" = 3.785412,
                  "gallons" = 3.785412,
                  "gal" = 3.785412,
                  "pint" = 0.4731765, #there is also a "dry pint"
                  "pints" = 0.4731765,
                  "pt" = 0.4731765,
                  "p" = 0.4731765,
                  "quart" = 0.9463529, #there is also a "dry quart"
                  "quarts" = 0.9463529,
                  "qt" = 0.9463529,
                  "q" = 0.9463529,
                  "jigger" = 0.02957353 * 1.5,
                  "shot" = 0.02957353 * 1.5, #for US--see https://cocktail-society.com/barkeeping/how-many-ounces-in-a-shot/ for standard shot sizes internationally
                  "barspoon" = 0.0025, #seems to be relatively universal because the larger American bar spoon isn't really used? https://cocktail-society.com/barkeeping/bar-spoon-types/
                  "bar spoon" = 0.0025,
                  "bsp" = 0.0025,
                  "dash" = 0.004928922 / 8,
                  "dashes" = 0.004928922 / 8,
                  "drop" = 0.004928922 / 80,
                  "drops" = 0.004928922 / 80)


usethis::use_data(units_volume, overwrite = TRUE)
