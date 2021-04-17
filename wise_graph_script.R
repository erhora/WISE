library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)
library(lomb)
library(gganimate)
library(jpeg)

# Reading in data and defining colors ----
lmc_neowise_var <- read_csv("data/LMC_NEOWISE_VAR_limited.csv") %>% 
  filter(!is.na(w1mpro) & !is.na(mjd))



# I defined my own custom twelve color palette because all others only go to eleven
selected_colors <- list("#3D0202", "#D91212", "#E33D1F", "#5F4706", "#F2980E", 
                        "#E8C20A", "#0AE8E0", "#0AA7CF", "#0A54CF", "#BB40DC", 
                        "#6440DC", "#500863")


# Adding in the `arbitrary_class` variable (grouping when observations were taken- going to a categorical variable)
lmc_neowise_var_class <- lmc_neowise_var %>% 
  mutate(ar_class = as.factor(trunc((mjd - 56700) / 180.125))) %>% 
  clean_names()





# Pulling out sequence identifiers ----
# Making a list of all the unique object identifiers and storing them as numerical values
for (i in lmc_neowise_var_class %>% distinct(seq)){
  output <- as.double(i)
} 



# Creating Facet Plots ----
# the `seq_along()` function basically moves through each item stored in `output`
for (i in seq_along(output)){
  # Loading all of the points for the object (they will be plotted as background points)
  lmc_neowise_facet_plot_background <- dplyr::select(lmc_neowise_var_class%>% 
                                                       filter(seq == output[i]), -ar_class) 
  lmc_neowise_var_class %>% 
    filter(seq == output[i]) %>% 
    ggplot(aes(x = w1mpro, y = w2mpro, color = ar_class)) +
    geom_point(data = lmc_neowise_facet_plot_background, color = "grey80", size = 1) +
    # Plotting the color coded points on top of the grey background points
    geom_point() +
    # Wrapping by class
    facet_wrap(~ar_class) +
    scale_fill_manual(
      # Choosing my custom colors to represent each class code
      values = selected_colors
    ) +
    theme_minimal() +
    labs(
      color = "Class Code",
      title = paste("Facet Plots for Object", output[i], sep = " ")
    ) 
    # Saving the image using `ggsave` on a `ggobject`
    ggsave(filename = paste("facet_", output[i], ".png", sep = ""),
         path = "facet_plots")
    
    # progress- this tells you which object the script is working through
    print(output[i])
} 





# Creating Lomb Scargle Plots ----
# Finding the dimensions of a tibble for all the observations for an object
for (i in seq_along(output)){
  dim <- lmc_neowise_var_class %>%
    filter(seq == output[i]) %>%
    dim()

  # Nesting an if statement within a for loop.
  # The for loop is needed to loop through the if statement for each object, so that is why I've ordered things this way.
  # A quick google search claims that 30 or more observations constitutes a large enough sample size
  if (dim[1] < 30){
    # For objects with less than 30 observations, no Lomb Scargle plots are made
    print("too small")
  } else{
    # `ggsave` does not work with this function, so I am using the `png()` followed by `dev.off()` to save the plot between those commands
    png(filename = paste("lomb_plots/lomb_", output[i], ".png", sep = ""))
    lmc_neowise_var_class %>%
      filter(seq == output[i]) %>%
      select(mjd, w1mpro) %>%
      lsp(from = 2, ofac = 5, type = "period")
    dev.off()
  }
  
  # progress- this tells you which object the script is working through
  print(output[i])
} 



# Creating Folded Light Plots ----
# making an empty list to store values
file_list = list()

for (i in seq_along(output)){
  dim <- lmc_neowise_var_class %>%
    filter(seq == output[i]) %>%
    dim()
  
  # Nesting an if statement within a for loop.
  # The for loop is needed to loop through the if statement for each object, so that is why I've ordered things this way.
  # A quick google search claims that 30 or more observations constitutes a large enough sample size
  if (dim[1] < 30){
    # For objects with less than 30 observations, no Lomb Scargle plots are made
    print("too small")
  } else{
    # Using the values of the lomb scargle function to adjust the mjd to fit one period
    lmc_neowise_var_class_filter <- lmc_neowise_var_class %>%
      filter(seq == output[i]) %>%
      select(c(mjd, w1mpro, seq, ar_class))



    lmc.spec <- lsp(lmc_neowise_var_class_filter$w1mpro, times = lmc_neowise_var_class_filter$mjd, from = 5, ofac = 5, type = "period")


    file_list[[i]] <-  lmc_neowise_var_class_filter %>%
      mutate(adj_mjd = mjd - min(lmc_neowise_var_class_filter %>%
                                   select(mjd)) - trunc((mjd - min(lmc_neowise_var_class_filter %>%
                                                                     select(mjd)))/lmc.spec$peak.at[1])*lmc.spec$peak.at[1]
   )
  }
  print(output[i])
} 

# `file_list` is a list of tibbles, binding the rows makes one large tibble
lmc_neowise_var_class_adj <- bind_rows(file_list)


# The part that actually makes the plots
for (i in seq_along(output)){
  dim <- lmc_neowise_var_class %>%
    filter(seq == output[i]) %>%
    dim()
  
  # Nesting an if statement within a for loop.
  # The for loop is needed to loop through the if statement for each object, so that is why I've ordered things this way.
  # A quick google search claims that 30 or more observations constitutes a large enough sample size
  if (dim[1] < 30){
    # For objects with less than 30 observations, no Lomb Scargle plots are made
    print("too small")
  } else{
    lmc_neowise_var_class_adj %>% 
      filter(seq == output[i]) %>% 
      ggplot(aes(x = adj_mjd, y = w1mpro, fill = ar_class)) +
      geom_point(shape = 21, color = "black", size = 3, alpha = 0.5) +
      scale_fill_manual(
        values = selected_colors
      ) +
      theme_minimal() +
      labs(
        fill = "Class Code",
        title = paste("Adjusted Light Curve: Object", output[i]),
        x = "Adjusted mjd (days)"
      ) +
      theme(
        plot.title = element_text(size = 14, family = "serif", face = "bold", hjust = 0.5),
        axis.title = element_text(family = "serif")
      )
  
  # Saving the image using `ggsave` on a `ggobject`
  ggsave(filename = paste("folded_light_curve_", output[i], ".png", sep = ""),
         path = "folded_curve_plots")
  }
  
  # progress- this tells you which object the script is working through
  print(output[i])
}
