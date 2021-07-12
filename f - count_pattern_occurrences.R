#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####################################  FUNCTION TO DETERMINE PATTERN OCCURENCE  ############################### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This functions determine the number of times a pattern has occurred in a string 
#
# Inputs: pattern - string of the pattern
#         original_string_text - string of the original text
#
# Outputs: num_of_occurences - the number of times that the pattern occurred

count_pattern_occurrences <- function(pattern
                                      , original_string_text) 
{
  
  # Replace the pattern in the string with ""
  string_text_without_pattern <- gsub(pattern
                                      , ""
                                      , original_string_text)
  
  # Determine the number of times that the pattern occured 
  num_of_occurences <- nchar(original_string_text) - nchar(string_text_without_pattern)
  
  return(num_of_occurences)
}
