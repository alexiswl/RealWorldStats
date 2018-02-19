# Ensure all packages are loaded
require(lubridate)
require(dplyr)
require(ggplot2)
require(tibble)
require(readr)
require(magrittr)
require(SparkR)

# Import the dataset
df = readr::read_csv("/Users/boronga/stats.2017.csv")

# Calculate the winner of each game
df = df %>% 
    dplyr::mutate(Winner_int = dplyr::case_when(Total_home > Total_away ~ 1,
                                                Total_away > Total_home ~ -1,
                                                Total_home == Total_away ~ 0),
                  Winner_str = as.character(Winner_int),
                  Margin = Total_home - Total_away) 

# Correlate free kicks against the winner of a game
df.frees <- df %>% 
                dplyr::mutate(FreesFor_diff = FreesFor_home - FreesFor_away) %>%
                dplyr::filter(FreesFor_diff != 0) %>% # Stop us multiplying by zero
                dplyr::mutate(Frees_diff = FreesFor_diff*sign(Margin),
                              Margin_frees = Margin*sign(FreesFor_diff))
df.frees %>% 
    ggplot2::ggplot(aes(x=abs(FreesFor_diff), 
                        y=Margin_frees)) +
    ggplot2::geom_point() +
    ggplot2::ylab("Score Margin (Score of team with most free kicks - score of team with least free kicks)") +
    ggplot2::xlab("Free kick difference") +
    ggplot2::ggtitle("How does the number of free kicks awarded to a team affect the margin?") +
    ggplot2::geom_smooth(method="lm", se=TRUE) + 
    ggplot2::geom_hline(yintercept=0, linetype="dashed", colour='red')
    
 