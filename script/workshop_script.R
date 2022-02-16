
# Preliminaries -----------------------------------------------------------

# Loading libraries

library(tidyverse)
library(stargazer)
library(ggeffects)
library(psych)
library(janitor)
library(fastDummies)
library(gtsummary)


# Loading Data ------------------------------------------------------------

# Reads in Persson/Tabellini data from online source and assigns to object named "persson_tabellini_original"
persson_tabellini_original<-read_csv("https://raw.githubusercontent.com/aranganath24/r_primer/main/workshop_data/persson_tabellini_workshop.csv")

# Can also read in data from local source
persson_tabellini_original<-read_csv("persson_tabellini_workshop.csv")

# Make a copy of the dataset so we don't alter the original dataset; then, view
# the copied dataset 
pt_copy<-persson_tabellini_original

# Print contents of "pt_copy"
pt_copy

# Views "pt_copy" in data viewer
View(pt_copy)


# Basic Summary Statistics ------------------------------------------------------

# Generate summary statistics for "pt_copy" and assign to new object 
#named "pt_copy_summarystats1"
pt_copy_summarystats1<-describe(pt_copy)

# Views "pt_copy_summarystats1"
View(pt_copy_summarystats1)

# Using ```stargazer``` to create and export summary statistics

# Make the summary stats into a data frame
pt_copy_df<-as.data.frame(pt_copy)

# Use stargazer to export summary statistics as a text file
stargazer(pt_copy_df, type="text", title="Descriptive Statistics", digits=1, out="summary_stats.txt")

# Use stargazer to export summary statistics as an html file
stargazer(pt_copy_df, type="text", title="Descriptive Statistics", digits=1, out="summary_stats.html")


# Summary statistics and grouping variables -------------------------------

# Creates summary statistics for each continent grouping, and puts results 
# in list named "summary_stats_by_continent"
summary_stats_by_continent<-describeBy(pt_copy, pt_copy$continent)

# Accessing continent-level summary statistics for Africa 
# from the "summary_stats_by_continent" list
summary_stats_by_continent[["africa"]]

# Accessing continent-level summary statistics for africa 
# from the "summary_stats_by_continent" list; alternate syntax
summary_stats_by_continent %>% pluck("africa")

# Group-level summary statistics can be assigned to their own object for easy retrieval
asia_europe_summary_statistics<-summary_stats_by_continent %>% pluck("asiae")

# retrieve summary statistics for Asia/Europe 
View(asia_europe_summary_statistics)

# removes "vars" indexing variable from "asia_europe_summary_statistics"
asia_europe_summary_statistics<-asia_europe_summary_statistics %>% select(-vars)

View(asia_europe_summary_statistics)

# Generate a table that displays summary statistics for trade at the continent level 
# and assign to object named "trade_age_by_continent"
trade_age_by_continent<-pt_copy %>% group_by(continent) %>% 
                                  summarise(meanTrade=mean(trade),sdTrade=sd(trade),
                                  meanAge=mean(age), sdAge=sd(age),
                                  n=n())

View(trade_age_by_continent)

# Crosstabs ---------------------------------------------------------------

# Creates cross-tab showing the breakdown of federal/non federal across continents
crosstab_federal_continent<-pt_copy %>% tabyl(federal, continent)

View(crosstab_federal_continent)


# Creates cross-tab showing the breakdown of majoritarian/nonmajoritarian across continents
crosstab_majoritarian_continent<-pt_copy %>% tabyl(maj, continent)

View(crosstab_majoritarian_continent)


# Basic Data Cleaning and Preparation Tasks -------------------------------

View(pt_copy)


# Reordering columns ------------------------------------------------------

# bring the "country" column to the front of the dataset using the "relocate" function
pt_copy<-pt_copy %>% relocate(country)

View(pt_copy)

# bring the "country", "list", "trade", "oecd" columns to the front of the dataset
pt_copy<-pt_copy %>% relocate(country, list, trade, oecd)

View(pt_copy)


# Renaming Variables ------------------------------------------------------

## Renaming a variable (renames "list" to "party_list")
pt_copy<-pt_copy %>% rename(party_list=list)

View(pt_copy)


# Sorting variables -------------------------------------------------------

# sorting in ascending (low to high) order with respect to the "trade" variable
pt_copy<-pt_copy %>% arrange(trade)

View(pt_copy)

# sorting in descending (high to low) order with respect to the "trade" variable
pt_copy<-pt_copy %>% arrange(desc(trade))

View(pt_copy)


# Creating new variables from existing variables --------------------------

# Create new variable named "non_catholic_80" that is calculated by substracting 
# the Catholic share of the population in 1980 ("catho80") from 100  
# and relocates "country", "catho80", and the newly created "non_catholic_80" to the
# front of the dataset
pt_copy<-pt_copy %>% mutate(non_catholic_80=100-catho80) %>% 
                    relocate(country, catho80, non_catholic_80)

View(pt_copy)


# Extracting and deleting columns ------------------------------------------------------


# Selects "country", "cgexp", "cgrev", and "trade" variables from the "pt_copy" dataset
pt_copy %>% select(country, cgexp, cgrev, trade)

# Selects "country", "cgexp", "cgrev", and "trade" variables from the 
# "pt_copy" dataset and assigns the selection to a new object named "pt_copy_selection"
pt_copy_selection<-pt_copy %>% select(country, cgexp, cgrev, trade)

View(pt_copy_selection)

# Deletes "cgrev" variable from "pt_copy_selection" dataset
pt_copy_selection %>% select(-cgrev)

# Creating Dummy Variables from Continuous Numeric Variables --------------

# Creates a new dummy variable based on the existing "trade" 
# variable named "trade_open" (which takes on a value of "1" if "trade" 
# is greater than or equal to 77, and 0 otherwise) and then moves the newly 
# created variable to the front of the dataset along with "country" and "trade"; 
# all changes are assigned to "pt_copy", thereby overwriting the existing version 
# of "pt_copy"

pt_copy<-pt_copy %>% mutate(trade_open=ifelse(trade>=77, 1, 0)) %>% 
                    relocate(country, trade_open, trade)

View(pt_copy)


# Creating categorical variables from continuous numeric variables --------

# Creates a new variable in the "pt_copy" dataset named "trade_level" 
# (that is coded as "Low Trade" when the "trade" variable is greater than 15 
# and less than 50, coded as "Intermediate Trade" when "trade" is greater than 
# or equal to 50 and less than 100, and coded as "High TradE" when "trade" 
#is greater than or equal to 100), and then reorders the dataset such that 
#"country", "trade_level", and "trade" are the first three variables in the dataset

pt_copy<-pt_copy %>% 
  mutate(trade_level=case_when(trade>15 & trade<50~"Low_Trade",
                                                  trade>=50 & trade<100~"Intermediate_Trade",
                                                  trade>=100~"High_Trade")) %>% 
                      relocate(country, trade_level, trade)

View(pt_copy)


# Creating dummmy variables from categorical variables  -------------------

# Creates dummy variables from "trade_level" column, and relocates the 
# new dummies to the front of the dataset

pt_copy<-pt_copy %>% dummy_cols("trade_level") %>% 
                    relocate(country, trade_level, 
                             trade_level_High_Trade,
                             trade_level_Intermediate_Trade, 
                             trade_level_Low_Trade)

View(pt_copy)



# Subsetting rows data based on criteria ----------------------------------

# Extracts OECD observations in "pt_copy" and assigns to object named "oecd_countries"
oecd_countries<-pt_copy %>% filter(oecd==1) %>% 
                relocate(country, oecd)

View(oecd_countries)

# Extracts observations for which cgrev (central government revenue as 
# % of gdp)>40, and assigns to object named "high_revenues"

high_revenues<-pt_copy %>% filter(cgrev>40) %>%
              relocate(country, cgrev)

View(high_revenues)

# Extracts observations for which the "catho80" variable is less than or equal to 50
minority_catholic<-pt_copy %>% filter(catho80<=50) %>% 
                   relocate(country, catho80)

View(minority_catholic)

# Extracts federal OECD countries (where oecd=1 AND federal=1) and
# assigns to a new object named "oecd_federal_countries"

oecd_federal_countries<-pt_copy %>% filter(oecd==1 & federal==1) %>% 
                         relocate(country, oecd, federal)

View(oecd_federal_countries)

# Extracts observations that are in Africa ("africa") OR 
# in Asia/Europe ("asiae) and assigns to an object named "asia_europe_africa"
asia_europe_africa<-pt_copy %>% filter(continent=="africa"|continent=="asiae") %>% 
                     relocate(continent)

View(asia_europe_africa)

# Extracts all NON-Africa observations and assigns to object named "pt_copy_sans_africa"
pt_copy_sans_africa<-pt_copy %>% filter(continent!="africa") %>% relocate(continent)

View(pt_copy_sans_africa)


# Visualization -----------------------------------------------------------

# Creates a bar chart of the "cgexp" variable
#(central government expenditure as a share of GDP) and 
#assigns the plot to an object named "cgexp_viz1"

cgexp_viz1<-pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_col(aes(x=reorder(country, cgexp), y=cgexp))+
  labs(title="Central Govt Expenditure as Pct of GDP (1990-1998 Average)", x="Country Name", 
       y="CGEXP")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90))

# Prints contents of "cgexp_viz1"
cgexp_viz1

# Creates an inverted bar chart of the "cgexp" variable (with countries on 
#vertical axis) and assigns the result to an object named "cgexp_viz2"
cgexp_viz2<-pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_col(aes(x=reorder(country, cgexp), y=cgexp))+
  coord_flip()+
  labs(title="Central Govt Expenditure as Pct of GDP (1990-1998 Average) ", x="Country Name", 
       y="CGEXP")+
  theme(plot.title=element_text(hjust=0.5)) 

# Prints contents of "cgexp_viz2"
cgexp_viz2

# Creates scatterplot with "cgexp" variable on x-axis and "trade" 
#variiable on y-axis and assigns to object named "scatter_cgexp_trade"
scatter_cgexp_trade<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgexp, y=trade))+
  labs(title="Trade Share of GDP \nas a function of\n Central Govt Expenditure (1990-1998 Average) ", 
       x="Central Government Expenditure (Pct of GDP)", y="Overall Trade (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 

# Prints contents of "scatter_cgexp_trade"
scatter_cgexp_trade

# Creates scatterplot with "cgexp" variable on x-axis and "trade" variiable on y-axis, 
# and uses different color points for different continents; plot is assigned to object 
# named "scatter_cgexp_trade_grouped"
scatter_cgexp_trade_grouped<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgexp, y=trade, color=continent))+
  labs(title="Trade Share of GDP \nas a function of\n Central Govt Expenditure (1990-1998 Average) ", 
       x="Central Government Expenditure (Pct of GDP)", y="Overall Trade (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 

# Prints contents of "scatter_cgexp_trade_grouped"
scatter_cgexp_trade_grouped

# Creates scatterplot with "cgexp" variable on x-axis and "trade" 
#variable on y-axis, adds line of best fit; plot assigned to object 
#named "scatter_cgexp_trade_line"

scatter_cgexp_trade_line<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgexp, y=trade))+
  geom_smooth(aes(x=cgexp, y=trade), method="lm")+
  labs(title="Trade Share of GDP \nas a function of\n Central Govt Expenditure (1990-1998 Average) ", 
       x="Central Government Expenditure (Pct of GDP)", y="Overall Trade (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 

# Prints contents of "scatter_cgexp_trade_line"
scatter_cgexp_trade_line


# Correlations ------------------------------------------------------------

# Prints correlation coefficient between "trade" and "cgexp" variables
cor.test(pt_copy$trade, pt_copy$cgexp, use="complete.obs")

cor.test(pt_copy$trade, pt_copy$cgexp, use="complete.obs")


# Extracts variables for which we want a correlation matrix
desired_variables<-pt_copy %>% select(trade, cgexp, cgrev, catho80)

# Creates correlation matrix from "desired_variables" object and 
# assigns to object named "cor_matrix"
cor_matrix<-cor(desired_variables, use="complete.obs")

# prints contents of "cor_matrix"
cor_matrix

# Regression --------------------------------------------------------------

# Implements regression with "gexp" as DV, and assigns to object named "regression1"
regression1<-lm(cgexp~gastil+lyp+trade+prop1564+prop65+federal+oecd, data=pt_copy)

# Prints regression table
summary(regression1)

# Run regression with the continent variable and assign result 
# to object named "regression2"
regression2<-lm(cgexp~gastil+lyp+trade+prop1564+prop65+federal+continent+col_espa+col_uka+col_otha+oecd, data=pt_copy)

# Prints regression table for "regression2"
summary(regression2)

# Put the regression models you want in your regression table in a list
model_list<-list(regression1,regression2)

# Exporting table as text file
stargazer(model_list, type="text", out="cgexp_regressions.txt")

# Exporting regression table as html file
stargazer(model_list, type="html", out="cgexp_regressions.html")

# Multiple datasets -------------------------------------------------------

# Read in capital mobility data from Github repository
capital_mobility<-read_csv("https://raw.githubusercontent.com/aranganath24/r_primer/main/workshop_data/chinn_eto_capitalopenness_summary.csv")

View(capital_mobility)

# Joins "capital_mobility" to "pt_copy" using "ctrycd" 
# as the join field (only keeps observations from "pt_copy"; 
# countries in "capital_mobility" but not "pt_copy" are not 
# included in the final joined dataset; joined dataset is assigned 
# to an object named "pt_capitalmobility")

pt_capitalmobility<-inner_join(pt_copy, capital_mobility, by=c("ctrycd"))

View(pt_capitalmobility)



