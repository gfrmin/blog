---
title: "Hanukkah of Data 5783"
subtitle: "Solving the data version of Advent of Code in R"
author: admin
date: 2022-12-25
tags: ["data", "analysis", "fun"]
---

The madpersons behind the retro-chic data analysis tool [VisiData](https://visidata.org) have created an alternative to the infamous "Advent of Code". Instead  of programming to solve yet more optimization puzzles, and instead of the hackneyed Christmas theme, the "Hanukkah of Data" requires data analysis to solve a ~~murder mystery~~ rug-finding mission.

[You can find this (first?) year's Hanukkah of Data here](https://hanukkah.bluebird.sh/5783/).

I only just found out about it, so although one puzzle was released for each day of real-life Chanukah, I was able to gorge on the first 7 puzzles, needing only to wait for the last puzzle, the solving of which revealed something quite spectacular... Each puzzle is represented by an ASCII candle in an ASCII world (quite an impressive achievement in its own right).

Here's how I solved all 8 puzzles in R. I can probably also give more details (aka make the code clearer) upon request.


```r
# libraries
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

# get data
download.file(url = "https://hanukkah.bluebird.sh/5783/noahs-csv.zip", destfile = "noahs-csv.zip")
system("unzip -P 5777 -o noahs-csv.zip")

customers <- read_csv("noahs-customers.csv")
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
products <- read_csv("noahs-products.csv")
orders_plus <- orders %>% inner_join(items) %>% inner_join(products) %>% inner_join(customers)
```


```r
# Puzzle 1

letter_to_phonenumber <- function(letter) {
    letter_index <- which(letters == letter)
    phonenumber <- floor(((letter_index - 1) / 3) + 2)
    if (!letter %in% c("s", "v", "y", "z")) {
        return(as.character(phonenumber))
    } else {
        return(as.character(phonenumber - 1))
    }
}

name_to_phonenumber <- function(name) {
    surname <- (strsplit(name, " ") %>% unlist)[-1]
    name_lower <- str_to_lower(surname) %>% str_replace_all("[^a-z]", "")
    name_short <- str_sub(name_lower, end = 10)
    name_vector <- unlist(strsplit(name_short, ""))
    name_phonenumber_vector <- map_chr(name_vector, letter_to_phonenumber)
    return(name_phonenumber_vector %>% paste0(collapse = ""))
}

customers_with_phonenumbers <- customers %>% mutate(name_phone = map_chr(name, name_to_phonenumber)) %>% mutate(phoneraw = str_replace_all(phone, "-", ""))

investigator <- customers_with_phonenumbers %>% filter(phoneraw == name_phone)
investigator_phone <- investigator %>% pull(phone)

print(investigator_phone)
```

```
## [1] "488-836-2374"
```


```r
# Puzzle 2

name_to_initials <- function(name) {
    initials <- str_extract_all(name, "[A-Z]") %>% unlist %>% paste0(collapse = "")
    return(initials)
}

orders_plus_jd <- orders_plus %>% mutate(name_initials = map_chr(name, name_to_initials)) %>% filter(name_initials == "JD")
orders_coffeebagels_2017 <- orders_plus %>% filter(sku %in% c("DLI1464", "BKY4234", "BKY5887")) %>% group_by(orderid) %>% filter(n() > 1) %>% ungroup %>% filter(year(shipped) == 2017)
contractor_phone <- orders_coffeebagels_2017 %>% pull(phone) %>% unique

print(contractor_phone)
```

```
## [1] "212-771-8924"
```


```r
# Puzzle 3

spiderguy <- customers %>% filter(year(birthdate) %in% c(2006, 1994, 1982, 1970, 1958, 1946, 1934)) %>% filter( month(birthdate) %in% c(3,4)) %>% filter(citystatezip == "South Ozone Park, NY 11420")
spiderguy_phone <- spiderguy %>% pull(phone)

print(spiderguy_phone)
```

```
## [1] "516-636-7397"
```


```r
# Puzzle 4

orders_pastries_before5am <- orders_plus %>% filter(hour(ordered) == 4) %>% filter(ordered == shipped) %>% filter(str_detect(sku, "BKY"))
tinder_girl <- customers %>% filter(customerid == 5375)
tinder_girl_phone <- tinder_girl %>% pull(phone)

print(tinder_girl_phone)
```

```
## [1] "718-649-9036"
```


```r
# Puzzle 5

orders_queens_cats <- orders_plus %>% filter(str_detect(citystatezip, "Queens")) %>% filter(str_detect(desc, "Cat"))
cat_lady_candidates <- orders_queens_cats %>% select(customerid, name, phone) %>% distinct # only one female name

print(cat_lady_candidates[1,]$phone)
```

```
## [1] "315-492-7411"
```


```r
# Puzzle 6

orders_losing <- orders_plus %>% filter(wholesale_cost > unit_price)
frugal_cousin <- customers %>% filter(customerid == 8342) # by far the most lossy orders
frugal_cousin_phone <- frugal_cousin %>% pull(phone)

print(frugal_cousin_phone)
```

```
## [1] "914-868-0316"
```


```r
# Puzzle 7

orders_plus_orderdate <- orders_plus %>% mutate(order_day = day(ordered), order_month = month(ordered), order_year = year(ordered)) %>% mutate(desc_without_colour = (str_replace(desc, "\\([a-z]+\\)", "") %>% str_squish))
frugal_cousin_orders <- orders_plus_orderdate %>% filter(customerid == 8342)
common_orders <- frugal_cousin_orders %>% inner_join(orders_plus_orderdate, by=c("desc_without_colour", "order_day", "order_month", "order_year")) %>% filter(customerid.y != 8342) %>% filter(desc.x != desc.y)
true_love <- common_orders %>% arrange(abs(ordered.x - ordered.y)) %>% select(ordered.x, ordered.y, name.y, phone.y) %>% slice(1)

print(true_love$phone.y)
```

```
## [1] "315-618-5263"
```


```r
# Puzzle 8

collector <- orders_plus  %>% filter(str_detect(desc, "^Noah")) %>% group_by(customerid) %>% filter(n() > 200) %>% select(name, phone) %>% distinct
```

```
## Adding missing grouping variables: `customerid`
```

```r
collector_phone <- collector %>% pull(phone)
print(collector_phone)
```

```
## [1] "929-906-5980"
```
