PROCEDURE TO RUN THE SCRIPTS FOR EVERY SEASON:

1. Run scrape_codes.R. It generates the file codes_day.RData.

2. Run scrape_games.R. It generates one csv file per day. By joining all of them, the file stats_games.csv is generated.

3. Run scrape_players.R. It generates players_data.csv.

All these files are in the 'output' folder, divided in seasons.

PROCEDURE TO REPRODUCE THE RESULTS OF THE PAPER:

1. do_pyramid.R generates the pyramid plot to compare the time evolution of the number of foreigners and Spaniards in the ACB league.

2. do_map.R generates the heatmap to see the distribution of countries represented in the ACB 2020-2021 season.

3. do_barplot.R generates the barplot to compare the number of foreigners and Spaniards that each team of the 2020-2021 season has.

4. do_comp_ages.R creates the plot to compare the number of foreigners and Spaniards in each age group.

PROCEDURE TO USE THE DASHBOARD:

1. Run prepare_data_dashboard.R to obtain the data files that the dashboard will use, namely, acb_2021_info_players.csv,
acb_2021_stats_players.csv.

2. The file dashboard_acb_2021.Rmd in the one that generates the html file of the dashboard. 
