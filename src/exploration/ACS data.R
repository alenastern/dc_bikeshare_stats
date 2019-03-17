
library(jsonlite)
library(dplyr)


#global variables
vars <- read.csv("/mnt/dm-3/alix/Documents/Multiple Testing/dc_bikeshare_stats/src/exploration/variables.csv") # change to wherever you save variables.csv
vars.to.add <- c('state', 'county', 'tract', 'block_group')
link <- 'https://api.census.gov/data/2015/acs/acs5'
for_equal <- 'block%20group:*'
in_equal_state <- 'state:11'
in_equal_county <- 'county:001'
in_equal_tract <- 'tract:*'
key <-  '17c33afc69e74a76256559f11768a4005763e816' #CHANGE THIS TO YOUR KEY; format is 'xxxxxxxxxxxxxxx...'
topics <- c("race", "age", "income")

get_census_data <- function(link, variables, for_equal, in_equal_county, in_equal_state, in_equal_tract, key){
  
  # gets census data through the census api
  #   
  #   inputs
  #       link: the acs api link
  #       variables: the variables being pulled from the ACS
  #       for_equal: the level of data being pulled
  #       in_equal: the state, county, and tract wanted.
  #       key: personal key
  #   output: acs data pulled from API!
  
  api_call <- paste0(link, '?get=', variables, '&for=', for_equal, '&in=', in_equal_state, '&in=', in_equal_county, '&in=', in_equal_tract, '&key=', key)
  df <- read_json(api_call, simplifyVector = TRUE) %>% as.data.frame()
  return(df)
}

header.true <- function(df) {
  # function making first row the column names
  names(df) <- as.character(unlist(df[1,])) #take the first row and make it the column names
  df[-1,] # delete the first row
}

get.data <- function(dat, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key){
  
  # take filtered variable names, get data from API, re-structure and send back!
  # inputs
  #       dat: the filtered variables
  #       link: the acs api link
  #       for_equal: the level of data being pulled
  #       in_equal_*: the state, county, and tract wanted.
  #       key: personal key
  # outputs: a basic dataframe from the API
  
  a <- rbind(c("NAME", "name"), dat)
  a.vars <- paste(a$acs, collapse = ",")
  a.df <- get_census_data(link, a.vars, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
  a.df <- header.true(a.df)
  a.cols <- as.vector(a$local)
  a.cols <- c(a.cols, vars.to.add)
  names(a.df) <- a.cols
  return(a.df)
}

create.dfs <- function(topic, vars.df, vars.df.col, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key, vars.to.add){
  
  # function using data in the variables.csv file to get api data
  #   
  #   inputs
  #       topic: the topic we're dealing with (race, age, etc)
  #       vars.df: dataframe of variables read in as a csv
  #       vars.df.col: a column of the names we *want* vs the acs variable names
  #       link: the acs api link
  #       variables: the variables being pulled from the ACS
  #       for_equal: the level of data being pulled
  #       in_equal_*: the state, county, and tract wanted.
  #       key: personal key
  #       vars.to.add: the variables that come with the acs data.
  #   output: a dataframe of the API data pulled with basic formatting.
  
  df.m <- data.frame(matrix(nrow=0, ncol=5))
  colnames(df.m) <- c("name", vars.to.add)
  
  for (t in topic) {
    print(t) # so we know what topic we're on.
    
    if (t == "age"){ #age has too many variables, so it has to be treated differently - I cut it in half then re-combine them later.
      a <- vars.df[grep(t, vars.df.col), ] # get only the variable names that start with the topic we're interested in
      rownames(a) <- 1:nrow(a) # re-number the rows
      
      # first half of age variables
      a.1 <- a[1:24,]
      a.df.1 <- get.data(a.1, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
      
      # second half of age variables
      a.2 <- a[25:nrow(a),]
      a.df.2 <- get.data(a.2, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
      
      # combine them
      a.df <- merge(a.df.1, a.df.2, by = c("name", vars.to.add), all.x = TRUE)
      
      #return(a.df)
    }
    else{ # anything but age
      a <- vars.df[grep(t, vars.df.col), ] # get only the variable names that start with the topic we're interested in
      a.df <- get.data(a, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
      #print(a.df)
    }
    df.m <- merge(df.m, a.df, by = c("name", vars.to.add), all.y = TRUE)
  }
  return(df.m)
}

df <- create.dfs(topics, vars, vars$local, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key, vars.to.add)

keep <- c("name", "county", "state", "tract", "block_group")
indx <- !(colnames(df) %in% keep)
df.new <- lapply(df[indx], function(x) as.numeric(as.character(x)))
df.new <- cbind(df[keep], df.new)

df.mutated <- df.new %>%
  mutate(race_white = race_white_alone,
        race_black = race_black_alone,
        race_asian = race_asian_alone,
        race_other = race_native_hawaiian_other_pacific_islander_alone + race_american_indian_alaska_native_alone + race_some_other_race_alone + race_two_or_more,
        male = age_total_male,
        female = age_total_female,
        median_age = age_median,
        age_under18 = age_male_under5 + age_male_5to9 + age_male_10to14 + age_male_15to17 + age_female_under5 + age_female_5to9 + age_female_10to14 + age_female_15to17,
        age_18to24 = age_male_18to19 + age_male_20 + age_male_21 + age_male_22to24 + age_female_18to19 + age_female_20 + age_female_21 + age_female_22to24,
        age_25to34 = age_male_25to29 + age_male_30to34 + age_female_25to29 + age_female_30to34,
        age_35to44 = age_male_35to39 + age_male_40to44 + age_female_35to39 + age_female_40to44,
        age_45to54 = age_male_45to49 + age_male_50to54 + age_female_45to49 + age_female_50to54,
        age_55to64 = age_male_55to59 + age_male_60to61 + age_male_62to64 + age_female_55to59 + age_female_60to61 + age_female_62to64,
        age_65up = age_male_65to66 + age_male_67to69 + age_male_70to74 + age_male_75to79 + age_male_80to84 + age_male_85plus + age_female_65to66 + age_female_67to69 + age_female_70to74 + age_female_75to79 + age_female_80to84 + age_female_85plus,
        income_less_than_30k = `income_<10k` + income_10to14.9k + income_15to19.9k + income_20to24.9k + income_25to29.9k,
        income_30to59k = income_30to34.9k + income_35to39.9k + income_40to44.9k + income_45to49.9k + income_50to59.9k,
        income_60to99k = income_60to74.9k + income_75to99.9k,
        income_100up = income_100to124.9k + income_125to149.9k + income_150to199.9k + `income_200k+`)

df.acs <- df.mutated[, -c(6:80)]

df.acs$GEOID <- paste0(df.acs$state, df.acs$county, df.acs$tract, df.acs$block_group)




# Race: White non-Hispanic = race_white, Black non-Hispanic = race_black_alone, Asian non-Hispanic = race_asian_alone, Other non-Hispanic. 
# Income: <$29.9k, $30-59.9k, $60-99.9k, $100k+. 
# Gender: Male, Female. 
# Age: Under 18, 18 to 24, 25 to 34, 35 to 44, 45 to 54, 55 to 64, 64+, median age. 