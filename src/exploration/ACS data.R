
library(jsonlite)
library(dplyr)

get_census_data <- function(link, variables, for_equal, in_equal, key){
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

create.dfs <- function(){
  # FILL ME IN.
}

#global variables
vars <- read.csv("/mnt/dm-3/alix/Documents/Multiple Testing/variables.csv")
vars.to.add <- c('state', 'county', 'tract', 'block_group')
link <- 'https://api.census.gov/data/2015/acs/acs5'
for_equal <- 'block%20group:*'
in_equal_state <- 'state:11'
in_equal_county <- 'county:001'
in_equal_tract <- 'tract:*'
key <- '17c33afc69e74a76256559f11768a4005763e816'



race <- vars[grep("race", vars$local), ]
race <- rbind(c("NAME", "name"), race)
race.vars <- paste(race$acs, collapse = ",")

race.df <- get_census_data(link, race.vars, for_equal, in_equal, key)

race.df <- header.true(race.df)
race.cols <- as.vector(race$local)
race.cols <- c(race.cols, vars.to.add)
names(race.df) <- race.cols
df <- merge(race.df, age.1.df)
