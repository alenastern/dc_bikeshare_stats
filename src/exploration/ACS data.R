
library(jsonlite)
library(dplyr)

#global variables
vars <- read.csv("/mnt/dm-3/alix/Documents/Multiple Testing/dc_bikeshare_stats/src/exploration/variables.csv") #change this to where you save the variables.
vars.to.add <- c('state', 'county', 'tract', 'block_group')
link <- 'https://api.census.gov/data/2015/acs/acs5'
for_equal <- 'block%20group:*'
in_equal_state <- 'state:11'
in_equal_county <- 'county:001'
in_equal_tract <- 'tract:*'
key <- '17c33afc69e74a76256559f11768a4005763e816' #CHANGE THIS TO YOUR KEY

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
  #
  #acs2012g <- read_json("https://api.census.gov/data/2012/acs1?get=NAME,B27001_001E,B27001_002E,B27001_004E,B27001_005E,B27001_007E,B27001_008E,B27001_010E,B27001_011E,B27001_013E,B27001_014E,B27001_016E,B27001_017E,B27001_019E,B27001_020E,B27001_022E,B27001_023E,B27001_030E,B27001_032E,B27001_033E,B27001_035E,B27001_036E,B27001_038E,B27001_039E,B27001_041E,B27001_042E,B27001_044E,B27001_045E,B27001_047E,B27001_048E,B27001_050E,B27001_051E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame(
  
  api_call <- paste0(link, '?get=', variables, '&for=', for_equal, '&in=', in_equal_state, '&in=', in_equal_county, '&in=', in_equal_tract, '&key=', key)
  df <- read_json(api_call, simplifyVector = TRUE) %>% as.data.frame()
  return(df)
}

header.true <- function(df) {
  # function making first row the column names
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

create.dfs <- function(topic, vars.df, vars.df.col, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key, vars.to.add){
  print(topic)
  
  if (topic == "age"){
    a <- vars.df[grep(topic, vars.df.col), ]
    rownames(a) <- 1:nrow(a)
    a.1 <- a[1:24,]
    a.1 <- rbind(c("NAME", "name"), a.1)
    a.vars.1 <- paste(a.1$acs, collapse = ",")
    a.df.1 <- get_census_data(link, a.vars.1, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
    a.df.1 <- header.true(a.df.1)
    a.cols.1 <- as.vector(a.1$local)
    a.cols.1 <- c(a.cols.1, vars.to.add)
    names(a.df.1) <- a.cols.1
    
    a.2 <- a[25:nrow(a),]
    a.2 <- rbind(c("NAME", "name"), a.2)
    a.vars.2 <- paste(a.2$acs, collapse = ",")
    a.df.2 <- get_census_data(link, a.vars.2, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
    a.df.2 <- header.true(a.df.2)
    a.cols.2 <- as.vector(a.2$local)
    a.cols.2 <- c(a.cols.2, vars.to.add)
    names(a.df.2) <- a.cols.2
    a.df <- merge(a.df.1, a.df.2, by = c("name", vars.to.add), all.x = TRUE)
    
    return(a.df)
  }
  else{
    a <- vars.df[grep(topic, vars.df.col), ]
    a <- rbind(c("NAME", "name"), a)
    a.vars <- paste(a$acs, collapse = ",")
    a.df <- get_census_data(link, a.vars, for_equal, in_equal_county, in_equal_state, in_equal_tract, key)
    a.df <- header.true(a.df)
    a.cols <- as.vector(a$local)
    a.cols <- c(a.cols, vars.to.add)
    names(a.df) <- a.cols
    return(a.df)
  }
}

topics <- c("race", "age", "ethnic", "income", "time")

df.m <- data.frame(matrix(nrow=0, ncol=5))
colnames(df.m) <- c("name", vars.to.add)

for (t in topics){
  df <- create.dfs(t, vars, vars$local, link, for_equal, in_equal_county, in_equal_state, in_equal_tract, key, vars.to.add)
  df.m <- merge(df.m, df, by = c("name", vars.to.add), all.y = TRUE)
}




