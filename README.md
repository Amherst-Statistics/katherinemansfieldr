# katherinemansfieldr

## An R Package for Katherine Mansfield's Short Stories

> "I imagine I was always writing. Twaddle it was, too. But better far write twaddle or anything, anything, than nothing at all."

 *- Katherine Mansfield*

This package allows users to access the short story works of Katherine 
Mansfield available on the ebooks@adelaide.edu website. The works have been converted from the UTF-8 text into files that are immediately ready for use. Each text is a character vector with elements that represent every paragraph. The works contained are as follows:

`somethingChildish` - *Something Childish, and Other Stories*, published in 1924 and contains stories from 1908 to 1921 

`bliss` - *Bliss, and Other Stories*, published in 1923 and contains stories from 1915 to 1920

`gardenParty` - *The Garden Party, and Other Stories*, published in 1922 and contains stories from 1920 to 1922

There is also `mansfieldComplete` which contains all of the stories from the three story collections in one character vector.

## Installation

To install this package, please refer to the following commands: 

```
devtools::install_github("Amherst-Statistics/katherinemansfieldr")
library(katherinemansfieldr)
```