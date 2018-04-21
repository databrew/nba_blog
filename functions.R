
#fill na with zero
fill_na_zero <- function(data_frame) {
  col_index <- apply(data_frame, 2, function(x) any(is.na(x)))
  fill_cols <- colnames(data_frame)[col_index]
  for(i in fill_cols){
    data_frame[, i][is.na(dat_full[, i])] <- 0
  }
  return(data_frame)
}


# function that takes every other row and attaches to the dataframe 
get_by_game <- function(dat) {
  
  # make column names lower case 
  colnames(dat) <- tolower(colnames(dat))
  
  # create a game identifier 
  dat$game_number <- rep(1:(nrow(dat)/2), each=2)
  
  temp_new_game <- list()
  
  # loop through by 2 and combine 
  for(game in unique(dat$game_number)){
    # subset data 
    temp_game <- dat[dat$game_number == game,]
    
    # get first and second row
    temp_1st_row <- as.data.frame(temp_game[1,])
    temp_2nd_row <- as.data.frame(temp_game[2,])
    
    # add "away" to 1st row columns
    colnames(temp_1st_row) <- paste0(colnames(temp_1st_row), '_away')
    colnames(temp_2nd_row) <- paste0(colnames(temp_2nd_row), '_home')
    
    # bind them together 
    temp_new_game[[game]] <- cbind(temp_2nd_row, temp_1st_row)
    
  }
  final_game <- do.call(rbind, temp_new_game)
  return(final_game)
}

# clean the column names and keep only relevant columns - at this point, just the odds and game outcome. 
remove_cols <- function(dat, keep_cols) {
  names(dat) <- gsub(' ', '_', names(dat))
  dat <- dat[, names(dat) %in% keep_cols]
  return(dat)
}

# pllot a distribution of the errors for each season 
hist_plot <- function(temp_dat, plot_title){
  temp_dat <- temp_dat %>% filter(variable %in% 'Total points scored')
  n_row <- nrow(temp_dat)
  
  g1 <- ggplot(temp_dat, aes(x=diff)) + 
    geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                   binwidth=5,
                   colour="black", fill="#A7A7A7") +
    geom_density(aes(y = ..density.. *(n_row*5)),
                 alpha=.2, fill= "dodgerblue") +
    geom_vline(xintercept = 0) +
    labs(title = plot_title, x = 'Difference  between Real total and betting mkt total', y = 'Counts') +
    theme_minimal(base_size = 12, base_family = 'Ubuntu')
  
  return(g1)
}

# pllot a distribution of the errors for each season 
hist_plot_double <- function(temp_dat, plot_title){
  n_row <- nrow(temp_dat)/2
  cols <- c("#A7A7A7",
            "dodgerblue")
  g1 <- ggplot(temp_dat, aes(x=value, fill = variable)) + 
    geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                   binwidth=10,
                   colour="grey", position = 'dodge',
                   alpha = 0.6) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(title = plot_title, x = 'Difference  between Real total and betting mkt total', y = 'Counts') +
    theme_minimal(base_size = 12, base_family = 'Ubuntu')
  
  return(g1)
}


# function that plots the over under over time 
points_plot <- function(temp_dat, smooth_line, plot_title, x_time) {
  
  # revlevel variable if needed
  cols <- c("#A7A7A7",
            "dodgerblue")
   g <- ggplot(data = temp_dat,
              aes(x = date,
                  y = value,
                  group = variable,
                  colour = variable,
                  text = paste0('<br>', home_team, ": ", pts_home,
                                '<br>', away_team, ": ", pts_away))) 
    
  g1 <- g +  geom_point(size = 1.5, alpha = 0.2)
  
  if(smooth_line){
    g1 <- g1 + geom_smooth()
  }
  
  g2 <- g1 + labs(x = '', 
         y = 'Total points',
         title = plot_title) +
    scale_color_manual(name = '', 
                       values = cols) + 
    theme_bw(base_size = 12, 
             base_family = 'Ubuntu') + scale_x_date(breaks = date_breaks(x_time), labels = date_format("%b-%y"))
  
  p1 <- plotly::ggplotly(g2, tooltip = 'text') %>% 
    config(displayModeBar = F) %>% 
    layout( 
      legend = list(
        orientation = "l",
        x = 0,
        y = -0.6))
  
  return(p1)
  
}






