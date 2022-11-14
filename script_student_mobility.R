
# In this workshop I will work with a Tidy Tuesday dataset about student mobility 
#   in the EU Erasmus student mobility program. This dataset was used in Tidy Tuesday
#   week 10 in 2022.
# More information on this data, including the codebook, is here:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-08/readme.md
#
# libraries needed
#  install.packages("tidyverse")
#  install.packages("here")
#  install.packages("tidytuesdayR")
library(tidyverse)
library(here)
library(tidytuesdayR)

# Load the datafile
# Codebook: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-08/readme.md
# (Note: carefully consider the meaning of the 'participants' field (it is a frequency weight!).
tuesdata <- tt_load(2022, week = 10)
erasmus <- tuesdata$erasmus

# There are two additional 'helper' datafiles that I will use in the workshop:
# country_names.csv     : The full names and EU-status of the countries
# adjacency.csv         : Which pairs of countries are adjacent (share borders)

# I will address some of the follwing questions
# Descriptive
# - How many students studied abroad? 
# - What are the top-10 receiving countries?
# - What are the top-10 sending countries?
# - Which are the 10 most frequent origin-destination country combinations
# - Are reverse flows (the flow from A to B and the flow from B to A) correlated?
# Modeling
# - How does total number of students from country A to country B depend on
#       the ototal number of student from A and the total number of students from B?
# - Do adjacent countries attract more or less students than non-adjacent countries?
# Visualization
# If time permits we could look at some ways of visualizing mobility data
