
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
topics <- c("race", "age", "ethnic", "income", "time")

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
      #return(a.df)
    }
    df.m <- merge(df.m, a.df, by = c("name", vars.to.add), all.y = TRUE)
  }
  return(df.m)
}

df <- create.dfs(topics, vars, vars$local, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key, vars.to.add)

is.na(df)