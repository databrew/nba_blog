
#fill na with zero
fill_na_zero <- function(data_frame) {
  col_index <- apply(data_frame, 2, function(x) any(is.na(x)))
  fill_cols <- colnames(data_frame)[col_index]
  for(i in fill_cols){
    data_frame[, i][is.na(dat_full[, i])] <- 0
  }
  return(data_frame)
}

# Get chi squared statistic for each team
# loop through each team and do chi square on venue and final result
get_chi_squared <- function(dat_test) {
  result_list <- list()
  team_vector <- unique(dat_test$teams)
  for(i in 1:length(unique(dat_test$teams))) {
    temp_team <- dat_test[dat_test$teams == team_vector[i],]
    team_name <- unique(temp_team$teams)
    team_table <- table(temp_team$venue, temp_team$final_result)
    team_home_w <- nrow(temp_team[temp_team$venue == 'Home' & temp_team$final_result == 'W',])
    team_home_l <- nrow(temp_team[temp_team$venue == 'Home' & temp_team$final_result == 'L',])
    team_away_w <- nrow(temp_team[temp_team$venue == 'Road' & temp_team$final_result == 'W',])
    team_away_l <- nrow(temp_team[temp_team$venue == 'Road' & temp_team$final_result == 'L',])
    
    chi_sqr_test <- chisq.test(team_table, correct = FALSE)
    temp_tab <- as.data.frame(cbind(team_name, round(chi_sqr_test$statistic,4), round(chi_sqr_test$p.value,4), team_home_w, team_home_l, team_away_w, team_away_l))
    colnames(temp_tab) <- c('team_name', 'chi_square_statistic', 'p_value', 'home_wins', 'home_losses', 'away_wins', 'away_losses')
    temp_tab$chi_square_statistic <- as.numeric(as.character(temp_tab$chi_square_statistic))
    temp_tab$p_value<- as.numeric(as.character(temp_tab$p_value))
    temp_tab$home_losses <- as.numeric(as.character(temp_tab$home_losses))
    temp_tab$home_wins <- as.numeric(as.character(temp_tab$home_wins))
    temp_tab$away_losses <- as.numeric(as.character(temp_tab$away_losses))
    temp_tab$away_wins <- as.numeric(as.character(temp_tab$away_wins))
    
    result_list[[i]] <- temp_tab
  }
  
  # combine list 
  temp_results <- do.call('rbind', result_list)
  
  # order by test stat
  temp_results <- temp_results[order(temp_results$chi_square_statistic, decreasing = TRUE),]
  
  return(temp_results)
  
}



# function that takes every other row and attaches to the dataframe 
get_by_game <- function(dat) {
  
  # make column names lower case 
  colnames(dat) <- tolower(colnames(dat))
  
  
  temp_new_game <- list()
  
  # loop through by 2 and combine 
  for(game in unique(dat$game_id)){
    # subset data 
    temp_game <- dat[dat$game_id == game,]
    
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

# get game number
get_game_num <- function(temp_dat) {
  
  result_list <- list()
  unique_teams <- unique(temp_dat$TEAMS)
  
  for(i in 1:length(unique(temp_dat$TEAMS))){
    sub_dat <- temp_dat[temp_dat$TEAMS == unique_teams[i],]
    sub_dat$game_num <- seq(1, nrow(sub_dat), 1)
    result_list[[i]] <- sub_dat
  }
  
  result_dat <- do.call('rbind', result_list)
  return(result_dat)
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
points_plot <- function(temp_dat, column_index,smooth_line, plot_title, x_time) {
  
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






