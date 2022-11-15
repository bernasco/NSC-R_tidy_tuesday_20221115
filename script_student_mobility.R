# script_student_mobility
# Wim Bernasco 2022.11.15
# NSC-R Tidy Tuesday November 15, 2022
# https://github.com/bernasco/NSC-R_tidy_tuesday_20221115

# In this workshop I worked with a dataset about student mobility in the
# European Union Erasmus+ student mobility program. 
# This dataset was also been used in Tidy Tuesday week 10, 2022

# Install uninstalled packages `tidyverse`, `here` and `tidytuesdayR`
if (! require("tidyverse")) install.packages(
  pkgs = "tidyverse", repos = "http://cran.us.r-project.org"
)
if (! require("here")) install.packages(
  pkgs = "here", repos = "http://cran.us.r-project.org"
)
if (! require("tidytuesdayR")) install.packages(
  pkgs = "tidytuesdayR", repos = "http://cran.us.r-project.org"
)
if (! require("broom")) install.packages(
  pkgs = "broom", repos = "http://cran.us.r-project.org"
)

# Load the required libraries
library(tidyverse)
library(broom)
library(here)
library(tidytuesdayR)

# Load the datafile
# See: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-08/readme.md
# (Note: The 'participants' field is a frequency weight!
tuesdata <- tt_load(2022, week = 10)
erasmus <- tuesdata$erasmus

# There are two additional 'helper' datafiles used used in this script:
# country_names.csv     : The full names and EU-status of the countries
# adjacency.csv         : Pairs of countries that are adjacent (share borders)

# I will address some of the following questions
# Descriptive
# - How many students studied abroad? 
# - What are the top-10 receiving countries?
# - What are the top-10 sending countries?
# - Which are the 10 most frequent origin-destination country combinations
# - Are reverse flows (the flow from A to B and the flow from B to A) correlated?
# Modeling
# - How does total number of students from country A to country B depend on
#       the total number of student from A and the total number of students from B?
# - Do adjacent countries attract more or less students than non-adjacent countries?
# Visualization
# If time permits we could look at some ways of visualizing mobility data


# Explore data
erasmus |> names()
erasmus |> glimpse()
erasmus |> count(participants)

# First check number of rows
erasmus |> dim()

# Number of rows after 'expansion'
# Expansion expands a frequency-weighted datafile to a regular file where
# each row represents a single student exchange trip 
erasmus |> 
  uncount(participants) |>
  dim()

# From and to which countries
erasmus |> 
  count(sending_country_code)

erasmus |>
  count(receiving_country_code)

# Add names to the codes
#   Option 1: Explicit in-script recoding
erasmus |>
  mutate(sending_country_name =
           case_when(sending_country_code == "AT" ~ "Austria",
                     sending_country_code == "BE" ~ "Belgium",
                     sending_country_code == "BG" ~ "Bulgaria",
                     sending_country_code == "CY" ~ "Cyprus",
                     sending_country_code == "CZ" ~ "Czechia",
                     sending_country_code == "DE" ~ "Germany",
                     sending_country_code == "DK" ~ "Denkmark"
                     # .........
           )) |>
  count(sending_country_name)
#   Option 2: Join with look-up table in a separate CSV file
country_labels <- read_csv(here("country_names.csv"))

erasmus |>
  left_join(country_labels, 
            by=c("receiving_country_code" = "country_code")) |>
  rename(receiving_country_name = country_name,
         receiving_country_status = country_status)

# Combining some of the above transformations we created a clean file
labeled_erasmus_full <- 
  erasmus |>
  # Keep only a subset of columns/variables
  select(sending_country_code, receiving_country_code, 
         participant_gender, academic_year, activity_mob, participants) |>
  # insert names of receiving countries by linking to country codes
  left_join(country_labels, 
            by=c("receiving_country_code" = "country_code")) |>
  # make sure the column names are clear 
  rename(receiving_country_name = country_name,
         receiving_country_status = country_status) |> 
  # insert names of sending countries by linking to country codes
  left_join(country_labels, 
            by=c("sending_country_code" = "country_code")) |>
  # make sure the column names are clear 
  rename(sending_country_name = country_name,
         sending_country_status = country_status) |> 
  # exclude countries outside EU and with no affiliation to EU
  filter(sending_country_status %in% c("EU", "EFTA", "UK", "Candidate"),
         receiving_country_status %in% c("EU", "EFTA", "UK", "Candidate")) |>
  # exclude the (many!) within-country exchanges
  filter(sending_country_code != receiving_country_code) |>
  # Only international mobility program
  filter(activity_mob == "Transnational youth meetings") |>
  # Every row becomes an individual international student trip 
  uncount(participants) 

# How many students
labeled_erasmus_full |>
  dim()

# Where did they come from
labeled_erasmus_full |> 
  count(sending_country_name) |>
  print(n=Inf)

# Visualization as a bar graph   
labeled_erasmus_full |> 
  count(sending_country_name) |>  
  ggplot() + 
  geom_col(aes(x=sending_country_name, y=n)) 

# With vertical labels labels   
labeled_erasmus_full |> 
  count(sending_country_name) |>  
  ggplot() + 
  geom_col(aes(x=sending_country_name, y=n)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Order by frequency   
labeled_erasmus_full |> 
  count(sending_country_code) |>  
  arrange(-n) |>
    # Converting character variable to factor variable
    mutate(sending_country_code=factor(sending_country_code,
                                     levels = unique(sending_country_code),
                                     ordered = T)) |>
  ggplot() + 
  geom_col(aes(x=sending_country_code, y=n)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Where did they go to
labeled_erasmus_full |> 
  count(receiving_country_name) |>
  arrange(-n) |>
  print(n=Inf)

# Visualise
labeled_erasmus_full |> 
  count(receiving_country_code) |>  
  arrange(-n) |>
  mutate(receiving_country_code=factor(receiving_country_code,
                                       levels = unique(receiving_country_code),
                                       ordered = T)) |>
  ggplot() + 
  geom_col(aes(x=receiving_country_code, y=n)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Top 10 Where did they go to
labeled_erasmus_full |> 
  count(receiving_country_name) |>
  arrange(-n) |> 
  head(n=10)

# Top 10 Where did they come from
labeled_erasmus_full |> 
  count(sending_country_name) |>
  arrange(-n) |> 
  head(n=10)

# Top 10 origin-destination combinations
labeled_erasmus_full |> 
  count(sending_country_name, receiving_country_name) |>
  arrange(-n) |> 
  head(n=10)


# Intermezzo (Cartesian product = cross-product = all combinations) 
# Tiny dataset of team members

team_members <- tibble(name = c( "Alex", "Asier", "Franziska", "Sam", "Wim")) 
# Tiny datset of available days
sessions <- tibble(day = c("Monday", "Tuesday", "Thursday"))

# Make all combinations of team members and available days
full_join(team_members, sessions, by = as.character())
# Same but more transparent (thanks to Nick van Doormaal)
expand_grid(team_members, sessions)
full_join(team_members, team_members, by = as.character()) |>
  filter(name.x != name.y)
# Intermezzo finished

# Create all possible combinations of sending and receiving country names
possible_mobility_names <- 
  full_join(country_labels, country_labels,
            by = as.character()) |>
  select(sending_country_name   = country_name.x,
         receiving_country_name = country_name.y) |>
  filter(sending_country_name != receiving_country_name)

# Erasmus student mobility flows including zero flows
flows_erasmus_full_zeros <-
  labeled_erasmus_full |> 
  group_by(sending_country_name, receiving_country_name) |>
  # Origin, destination, count
  count() |>
  rename(exchanges = n) |>
  # join with all combinations to include zero-flow pairs
  right_join(possible_mobility_names) |> 
  # change the NAs (= zero-flow) into 0
  replace_na(list(exchanges=0)) 

# Number of exchanges frequencies reversed: 581 zero flows, 37 1-person flow 
flows_erasmus_full_zeros |>
  ungroup() |>
  count(exchanges) |>
  arrange(exchanges) |>
  print(n=40)

# Histogram of this distribution
flows_erasmus_full_zeros |>
  ggplot() +
  geom_histogram(aes(x=exchanges))


reverse_flows_erasmus_full_zeros <-
  flows_erasmus_full_zeros |>
  rename(sending_country_name = receiving_country_name,
         receiving_country_name = sending_country_name,
         reverse_exchanges = exchanges)

full_join(flows_erasmus_full_zeros, reverse_flows_erasmus_full_zeros,
          by = (c("sending_country_name", "receiving_country_name"))) |> 
  ungroup() |>
  select(exchanges, reverse_exchanges) |>
  cor() |> 
  as_tibble()

# This scatterplot is by design symetric in the diagonal
full_join(flows_erasmus_full_zeros, reverse_flows_erasmus_full_zeros,
          by = (c("sending_country_name", "receiving_country_name"))) |> 
  ungroup() |>
  select(exchanges, reverse_exchanges) |>
  ggplot() +
  geom_point(aes(y=exchanges, x=reverse_exchanges) )

# Read the neighbor relations between countries
adjacency <- read_csv(here("adjacency.csv")) |>
  rename(sending_country_name = country_name,
         receiving_country_name = neighbor) |>
  # mark the rows that indicate shared borders
  mutate(adjacent = 1) |>
  # merge with the data that include all possible mobility streams
  right_join(possible_mobility_names,
             by=c("sending_country_name","receiving_country_name")) |>
  # Set non-adjacent to 0
  mutate(adjacent = replace_na(adjacent, 0)) 


model_data <- 
  flows_erasmus_full_zeros |> 
  inner_join(adjacency, 
            by = c("sending_country_name","receiving_country_name")) |>  
  group_by(sending_country_name) |>
  # total outflow from country
  mutate(outflow = sum(exchanges)) |>
  group_by(receiving_country_name) |>
  # total inflow into country
  mutate(inflow = sum(exchanges)) 

# Number of student exchanges from A to B as a function of
#   total numbers of outgoing students from A and of total
#   numbers of visiting students in B
model_01 <- lm(formula = exchanges ~  inflow + outflow ,          , 
               data = model_data)
tidy(model_01)
glance(model_01)
# Add adjacency
model_02 <- lm(formula = exchanges ~  inflow + outflow + adjacent, 
               data = model_data)
tidy(model_02)
glance(model_02)
# Students appear to fancy visiting nearby countries abroad!

library(circlize)
# Full country names
flows_erasmus_full_zeros |> 
  filter(exchanges > 50) |>
  arrange(-exchanges) |> 
  mutate(sending_country_name = substr(sending_country_name, 1,4),
         receiving_country_name = substr(receiving_country_name, 1,4)) |>
  chordDiagram()


# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network

sankeyNetwork(Links = flows_erasmus_full_zeros,
              Nodes = country_labels,
              Source = "sending_country_name", 
              Target = "receiving_country_name",
              Value = "exchanges", 
              NodeID = "country_name", 
              sinksRight=FALSE, colourScale=ColourScal, 
              nodeWidth=40, fontSize=13, nodePadding=20)


