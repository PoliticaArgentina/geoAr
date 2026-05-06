# Replace NULL Values with NA

Recursively replaces NULL values with NA in a list or vector. This
function is useful for handling NULL values in API responses before
converting them to data frames.

## Usage

``` r
replace_null_with_na(x)
```

## Arguments

- x:

  A list, vector, or single value to process

## Value

The input with all NULL values replaced by NA
