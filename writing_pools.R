# UTF-8 encoding recommended
# install the packages when first run this file
# output files are saved as .csv files
# ??? open in excel, save as .xls/.xlsx

library(tidyverse)
library(rvest)
library(stringr)

#############################################
### editable
# the url of issue pool
link_i <- "https://www.ets.org/gre/revised_general/prepare/analytical_writing/issue/pool"

# the url of the argument pool
link_a <- "https://www.ets.org/gre/revised_general/prepare/analytical_writing/argument/pool"

# use-able index of issue
# how to obtain:
# run codes in the "not edit" section
# check raw_i
# e.g., raw_i[3] starts
exclude_i <- 2

# use-able index of argument
exclude_a <- 2

# file names
path <- "D:\\张语砚\\工作\\新东方\\GRE写作\\"  # path
file_i <- str_c(path, "pool_issues.csv", sep = "")  # file name
file_a <- str_c(path, "pool_arguments.csv", sep = "")  # file name

#############################################
### do not edit unless codes do not work!
html_i <- read_html(link_i)
raw_i <- html_i %>% 
  html_nodes("p") %>%
  html_text()
raw_short_i <- raw_i[-(1:exclude_i)]
ind_ins_i <- raw_short_i %>%
  str_detect("^Write a response")

html_a <- read_html(link_a)
raw_a <- html_a %>% 
  html_nodes("p") %>%
  html_text()
raw_short_a <- raw_a[-(1:exclude_a)]
ind_ins_a <- raw_short_a %>%
  str_detect("^Write a response")

# instructions
ins_i <- raw_short_i[ind_ins_i] 
ins_a <- raw_short_a[ind_ins_a] 
# descriptions of the questions
des_i <- ins_i
des_a <- ins_a
# number of the questions
num_i <- ind_ins_i %>% as.numeric() %>% sum()
num_a <- ind_ins_a %>% as.numeric() %>% sum()

# clear the claim & reason issues
l <- 1
m <- 1
while (m <= num_i) {
  if (ind_ins_i[l]) {
    m <- m + 1
    l <- l + 1
  } else if (l == 1 || ind_ins_i[l-1]) {
    des_i[m] <- raw_short_i[l]
    l <- l + 1
  } else {
    des_i[m] <- str_c(des_i[m], raw_short_i[l], sep = "\n")
    l <- l + 1
  }
}

# make together argument descriptions
l <- 1
m <- 1
while (m <= num_a) {
  if (ind_ins_a[l]) {
    m <- m + 1
    l <- l + 1
  } else if (l == 1 || ind_ins_a[l-1]) {
    des_a[m] <- raw_short_a[l]
    l <- l + 1
  } else {
    des_a[m] <- str_c(des_a[m], raw_short_a[l], sep = "\n")
    l <- l + 1
  }
}

# output
df_i <- data.frame(
  "descriptions" = des_i,
  "instructions" = ins_i
)

write.csv(df_i, file = file_i)

df_a <- data.frame(
  "descriptions" = des_a,
  "instructions" = ins_a
)

write.csv(df_a, file = file_a)


