pkgs <- c("jsonlite", "lme4", "dplyr")
lapply(pkgs, library, character.only = TRUE)

## Download Soccer Data
json_file <- 'https://datahub.io/sports-data/english-premier-league/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

data <- data.frame()
# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    tmp <- read.csv(url(path_to_file)) %>% 
      select(HomeTeam, AwayTeam, Referee,
             FTHG, FTAG, HS, HST, AS, AST, 
             HC, AC, HF, AF, HY, AY, HR, AR)
    data <- rbind(data, tmp)
  }
}

# Create differences
data %>% 
  mutate(HDIFF = FTHG - FTAG,
         HSO = HS - HST,
         ASO = AS - AST) %>% 
  mutate(HSODIFF = HSO - ASO,
         HSTDIFF = HST - AST,
         HCDIFF = HC - AC,
         HFDIFF = HF - AF,
         HYDIFF = HY - AY,
         HRDIFF = HR - AR) -> data

# Linear mixed effects
model <- lmer(HDIFF ~ HSODIFF + HSTDIFF + HCDIFF + HFDIFF + HYDIFF + HRDIFF +
              (1 | HomeTeam) + (1 | AwayTeam), 
              data = data)
summary(model)

# OLS
model <- lm(HDIFF ~ HSODIFF + HSTDIFF + HCDIFF + HFDIFF + HYDIFF + HRDIFF, data = data)
summary(model)
