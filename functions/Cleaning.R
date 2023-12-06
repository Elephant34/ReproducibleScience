## Script: Cleaning.r
##
## Contains functions to clean the penguins_raw data set


## Functions used/modified from Dr. Lydia France


# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}

# A function to remove the comments column which are not useful in an analysis
remove_comments_row <- function(penguins_data) {
  penguins_data %>%
    select(-comments)
}


# A function to subset the data based on the list of column names
subset_columns <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

# A function to subset the penguins data set based on species
filter_by_species <- function(penguins_data, selected_species) {
  penguins_data %>%
    filter(species == selected_species)
}

# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}

# A function to group the data by the species column
group_by_species <- function(penguins_data) {
  penguins_data %>%
    group_by(species)
}
