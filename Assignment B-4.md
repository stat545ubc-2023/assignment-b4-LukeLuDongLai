Option A – Strings and functional programming in R
================
Lu Dong Lai (Luke)

## Exercise1

> Take a Jane Austen book contained in the janeaustenr package, or
> another book from some other source, such as one of the many freely
> available books from Project Gutenberg (be sure to indicate where you
> got the book from). Make a plot of the most common words in the book,
> removing “stop words” of your choosing (words like “the”, “a”, etc.)
> or stopwords from a pre-defined source, like the stopwords package or
> tidytext::stop_words.
>
> If you use any resources for helping you remove stopwords, or some
> other resource besides the janeaustenr R package for accessing your
> book, please indicate the source. We aren’t requiring any formal
> citation styles, just make sure you name the source and link to it.

### Step 1: Install and Load Required R Packages

Begin by loading the data and the packages that will be used in the
analysis below:

``` r
suppressWarnings({
  library(janeaustenr)
  library(tidytext)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(testthat)
})
```

    ## 
    ## 载入程辑包：'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## 载入程辑包：'testthat'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

### Step 2: Load the Book

For this exercise, let’s use *Pride and Prejudice*. Load the book using
the ‘janeaustenr’ package:

``` r
text <- janeaustenr::prideprejudice
# Have a glimpse of the book
print(text[1:11]) 
```

    ##  [1] "PRIDE AND PREJUDICE"                                                    
    ##  [2] ""                                                                       
    ##  [3] "By Jane Austen"                                                         
    ##  [4] ""                                                                       
    ##  [5] ""                                                                       
    ##  [6] ""                                                                       
    ##  [7] "Chapter 1"                                                              
    ##  [8] ""                                                                       
    ##  [9] ""                                                                       
    ## [10] "It is a truth universally acknowledged, that a single man in possession"
    ## [11] "of a good fortune, must be in want of a wife."

### Step 3: Preprocess the Text

Before analyzing, we need to convert the text into a tidy format, which
involves tokenizing the text into words:

``` r
text_df <- tibble(line = 1:length(text), text = text) %>%
  unnest_tokens(word, text)
# Have a look of the tibble
head(text_df)
```

    ## # A tibble: 6 × 2
    ##    line word     
    ##   <int> <chr>    
    ## 1     1 pride    
    ## 2     1 and      
    ## 3     1 prejudice
    ## 4     3 by       
    ## 5     3 jane     
    ## 6     3 austen

### Step 4: Remove Stopwords

Then, remove the stopwords selected from ‘tidytext::stop_words’ using
‘anti_join’:

``` r
data(stop_words)

tidy_text <- text_df %>%
  anti_join(stop_words, by = "word")
```

### Step 5: Plot the Most Common Words

Finally, count the frequencies of each word and then plot the (top 20)
most common words using ggplot2:

``` r
tidy_text %>%
  count(word, sort = TRUE) %>% # Count the word frequencies
  top_n(20, n) %>% # Select the top twenty most common words
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Most Common Words in Pride and Prejudice (Excluding Stopwords)")
```

![](A4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Exercise2

> Make a function that converts words to your own version of Pig Latin.
>
> The specific input and output that you decide upon is up to you. Don’t
> forget to implement good function-making hygiene: we’ll be looking for
> (unrendered) roxygen2-style documentation (being sure to describe your
> Pig Latin conversion), examples of applying the function, 3
> non-redundant tests, appropriate use of arguments, and appropriate
> amount of checking for proper input. Define the Function and
> Documentation

### Definition of my Pig Latin: Reverse Pig Latin

**Rearrangement component:**

1.  For words ending with consonant(s), move all letters after the last
    vowel to the beginning of the word.

2.  For words ending with vowel(s), remove the last vowel or vowel
    cluster along with the consonant or consonant cluster immediately
    preceding it.

**Addition component:**

3.  Add “ig” to the end of the word.

### Function and Documentation

``` r
#' Reverse Pig Latin Converter
#'
#'@description This function converts an English word to a version of reverse Pig Latin based on the following rules:
#'1. For words ending with consonant(s), move all letters after the last vowel to the beginning of the word.
#'2. For words ending with vowel(s), remove the last vowel or vowel cluster along with the consonant or consonant cluster immediately preceding it.
#'3. Add “ig” to the end of the word.
#'
#' @param word A single English word to convert.
#'
#' @return A string containing the reverse Pig Latin version of the input word.
#' 
#' @examples 
#' reverse_pig_latin("hello") # should return "heig"
#' reverse_pig_latin("world") # should return "rldwoig"
reverse_pig_latin <- function(word) {
  # Check that input is proper
  if (!is.character(word) || length(word) != 1 ) {
    stop("Input must be a single word.")
  }else if(!str_detect(word, "^[A-Za-z]+$")) {
    stop("Input word must consist only of letters.")
  }
  
  # Define vowels and consonants
  vowels <- c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U")
  consonants <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m",
                  "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z",
                  "B", "C", "D", "F", "G", "H", "J", "K", "L", "M",
                  "N", "P", "Q", "R", "S", "T", "V", "W", "X", "Y", "Z")
  
  # Initialize the last vowel index to -1 (indicating no vowel found yet)
  last_vowel_index <- -1
  
  # Iterate backwards over the word to find the last vowel
  for (i in seq_len(nchar(word))) {
    if (substr(word, nchar(word) - i + 1, nchar(word) - i + 1) %in% vowels) {
      last_vowel_index <- nchar(word) - i + 1
      break
    }
  }
  
  # If no vowel is found, just add "ig"
  if (last_vowel_index == -1) {
    return(paste0(word, "ig"))
  }                 
  
  if (last_vowel_index == nchar(word)){ # If word ends in a vowel
    # Remove the vowels first and then remove the consonants
    word <- substr(word, 1, nchar(word) - 1)
    while (nchar(word) >= 1 && substr(word, nchar(word), nchar(word)) %in% vowels) {
      word <- substr(word, 1, nchar(word) - 1)
    }
    while (nchar(word) >= 1 && substr(word, nchar(word), nchar(word)) %in% consonants) {
      word <- substr(word, 1, nchar(word) - 1)
    }
  }else{ # If word ends in a consonant
  # Extract the parts of the word and reassemble the word in reverse Pig Latin
  start <- substr(word, last_vowel_index+1, nchar(word))
  end <- substr(word, 1, last_vowel_index)
  word <- paste0(start, end)
  }
  
  # Add suffix to the word
  result <- paste0(word, "ig")
  return(result)
}
```

### Examples

This example shows the result for a word, ‘hello’, ending in a vowel.
After removing the last vowel along with the last consonant cluster and
adding the suffix, the result should be ‘heig’.

``` r
print(reverse_pig_latin("hello"))
```

    ## [1] "heig"

This example shows the result for a word, ‘world’, ending in a
consonant. After shifting the consonant cluster to the beginning of the
word and adding the suffix, the result should be ‘rldwoig’.

``` r
print(reverse_pig_latin("world"))
```

    ## [1] "rldwoig"

This example shows the result for a word, ‘hi’, which ends in a vowel
and contains only 2 letters. According to the rules, both letters will
be removed. After adding the suffix, the result should be ‘ig’.

``` r
print(reverse_pig_latin("hi"))
```

    ## [1] "ig"

This example shows how to use ‘map’ function from the ‘purrr’ package to
apply the ‘reverse_pig_latin’ function to a vector of strings.

``` r
words <- c("hello", "world")
print(map(words, reverse_pig_latin))
```

    ## [[1]]
    ## [1] "heig"
    ## 
    ## [[2]]
    ## [1] "rldwoig"

### Tests

This test checks the output of the usages in the **Examples** section.
In addition, it includes another situation, where the word ends with
vowel cluster.

``` r
test_that("Output for Valid Input",{
  expect_equal("heig",reverse_pig_latin("hello"))
  expect_equal("rldwoig",reverse_pig_latin("world"))
  expect_equal("ig",reverse_pig_latin("hi"))
  expect_equal("ig",reverse_pig_latin("sea"))
})
```

    ## Test passed 🎉

This test verifies the function’s error handling for incorrect input
arguments.

``` r
test_that("Error Handling for Invalid Input",{
  # Check the non-letter input
  expect_error(reverse_pig_latin("bye2"),"Input word must consist only of letters.")
  # Check the multi-words input
  expect_error(reverse_pig_latin(c("hi","there")),"Input must be a single word.")
})
```

    ## Test passed 🌈
