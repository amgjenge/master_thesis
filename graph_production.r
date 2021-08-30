# ------------------------------- R script for visualization ------------------------------ 
source("data_assembly.r") # import data




####################################################################################################
##################################### Individual time series ########################################
####################################################################################################


color_palette <- c("black", "orange") # Colors for graphs

make_plot <- function(df, name) {
  #' Generate predetermined ggplot
  #' @param df: input dataframe
  #' @param name: string, title of plot
  graph <- df %>% 
    ggplot() +
    geom_line(size = 0.8, aes(x = år, y = glattet_frekvens, color = "Glattet serie T  = 4")) +
    geom_point(aes(x = år, y = frekvens, color = "Observert frekvens")) +
    labs(title = name, x = "År", y = "Frekvens") +
    guides(col = guide_legend("Serier")) +
    scale_colour_manual(values = color_palette) +
    scale_x_continuous(n.breaks =  10) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE), n.breaks = 10) +
    theme_bw() 
  return (graph +  theme(legend.position = "bottom"))
  
}




process_frames <- function(df) {
  #' Function that reads a df and generates a new dataframe based on the first columns value.
  #' A time series graph is made for every unique word by calling make_plot and rendered as a PNG 
  #'
  datasets <- unique(df[1])
  for (i in 1:nrow(datasets)) {
    plot_data <- df %>% 
      filter(søkeuttrykk == datasets[[1]][i])
    name <- 
      print(datasets[[1]][i])
    
    make_plot(plot_data, paste(datasets[[1]][i]))
    ggsave(path = "graf_output/", filename= paste(datasets[[1]][i], ".png", sep = ""),  type = "cairo-png")
  }
  
}

process_frames(total_df)



####################################################################################################
##################################### Log transformation ###########################################
####################################################################################################



max_no_NA <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA) # Filters NA before finding max
min_no_NA <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA) # Filters NA before finding minimum


normalize <-function(x) { 
  #'
  #'Normalization  function
  #'@x: a vector
  (x -min_no_NA(x))/(max_no_NA(x)-min_no_NA(x)) 
}


series1_names <- c("Kontordame", "Yrkeskvinne", "Sykesøster", "Butikkdame", "Forkvinne", "Husmor")
series2_names <- c("Kvinnelig prest", "Kvinnelig lærer", "Kvinnelig arbeider", "Kvinnelig formann", "Kvinnelig forfatter", "Kvinnelig sykepleier")



selected_series_1 <- total_df %>% dplyr::select(søkeuttrykk, år, frekvens) %>% 
  filter(søkeuttrykk %in% series1_names) %>% 
  filter(!is.na(frekvens)) 



selected_series_2 <- total_df %>% dplyr::select(søkeuttrykk, år, frekvens) %>% 
  filter(søkeuttrykk %in% series2_names) %>% 
  filter(!is.na(frekvens)) 




##### Log transform series ######

log_norm_selected_series_1 <- selected_series_1 %>% mutate(frekvens = log(frekvens))
log_norm_selected_series_2 <- selected_series_2 %>% mutate(frekvens = log(frekvens))




normalized_plot <- function(df, name, g_title) {
  df %>% 
    rename("Søkeuttryk" = "søkeuttrykk")  %>% 
    ggplot(aes(x = år, y = frekvens, group = Søkeuttryk, color = Søkeuttryk)) +
    geom_line() +
    labs(title = g_title, x = "År", y = "Frekvens") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE), n.breaks = 10) +
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave(path = "log-transformerte plot/", filename= paste(name, ".png", sep = ""),  dpi = 600, width = 20, height = 10, type = "cairo-png")
}


###### Render log-transformed plots ######


## Series 1
normalized_plot(log_norm_selected_series_1, "log_norm_plott_1", "Log-transformert plot av utvalgte søkeord")


## Series 2
normalized_plot(log_norm_selected_series_2, "log_norm_plott_2", "Log-transformert plot av utvalgte søkeord")


