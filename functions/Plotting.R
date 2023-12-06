## Script: Plotting.r
##
## Contains functions to plot the specific figures required using the cleaned penguin data

# A function to sets the custom theme common to the penguin plots
# The theme is modified from theme_bw()
penguins_theme <- function(){
  theme_bw() %+replace%
    theme(
      text = element_text(family="sans")
    )
}

# A function to set the colors common to penguin plots
# Colours chosen from a colour blind friendly pallet
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
penguin_scale_colours <- function(){
  scale_color_manual(
    name = "Species",
    values = c(
      "Adelie" = "#E69F00",
      "Chinstrap" = "#CC79A7",
      "Gentoo" = "#56B4E9"
    )
  )
}

# A function to provide labels common to penguin plots
penguin_labs <- function(){
  labs(
    x = "Body Mass (g)",
    y = "Culmen Depth (mm)",
    title = "Plot of Penguin Body Mass Against Culmen Depth"
  )
}

# A function to generate the exploratory plot of body_mass_g against culmen_depth_mm
plot_exploratory_culmen_mass <- function(penguins_hypothesis) {
  penguins_hypothesis %>%
    ggplot(
      aes(
        x = body_mass_g,
        y = culmen_depth_mm,
        color = species
      )
    ) +
    geom_point() +
    penguin_labs() +
    penguins_theme() +
    penguin_scale_colours()
}

# A function to generate a plot showing the interaction for just Adelie penguins
plot_adelie_results <- function(penguins_hypothesis){
  penguins_hypothesis %>%
    ggplot(
      aes(
        x = body_mass_g,
        y = culmen_depth_mm,
        color = species
      )
    ) +
    geom_point() +
    geom_smooth(
      method = "lm",
      se = FALSE
    ) +
    labs(
      x = "Body Mass (g)",
      y = "Culmen Depth (mm)",
      title = "Plot of Adelie Penguin Body Mass Against Culmen Depth"
    ) +
    penguins_theme() +
    penguin_scale_colours()
}

# A function to generates a plot with regression lines for each species
plot_culmen_mass <- function(penguins_hypothesis) {
  penguins_hypothesis %>%
    ggplot(
      aes(
        x = body_mass_g,
        y = culmen_depth_mm,
        color = species
      )
    ) +
    geom_point() +
    geom_smooth(
      method = "lm",
      se = FALSE
    ) +
    labs(
      x = "Body Mass (g)",
      y = "Culmen Depth (mm)",
      title = "Plot of Penguin Body Mass Against Culmen Depth"
    ) +
    penguins_theme() +
    penguin_scale_colours()
}




# A function to save any plot as an svg
# size_cm can either be a (width, height) vector or single integer which will generate a square
save_plot_svg <- function(figure, filename, size_cm, scaling){
  
  # Defaults one number size input to make a square file
  if(length(size_cm) < 2) size_cm[2] <- size_cm[1]
  
  # Converts the size to inches from cm
  size_inches <- size_cm/2.54
  
  svglite(
    filename,
    width = size_inches[1], 
    height  = size_inches[2], 
    scaling = scaling
  )
  print(figure)
  dev.off()
}