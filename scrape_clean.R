library('tidyverse')
library('rvest')
# alright, we're here to download and prepare the data for our congressional representation
# map. The two points we want are the population by census year and the house apportioned seats
# by year. I'm too lazy to go through the census sites so lets just rip off wikipedia

pop_page <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_historical_population")

xpaths = c('//*[@id="mw-content-text"]/div/table[1]', '//*[@id="mw-content-text"]/div/table[3]', '//*[@id="mw-content-text"]/div/table[4]')

scrape_table = function(path){

  table = pop_page %>%
    html_nodes(xpath = path) %>%
    html_table()
  table = table[[1]]
  # There are some footnotes on the state names for some but not all the tables
  table$Name = gsub(pattern = "\\[[0-9]+\\]", replacement = "", x = table$Name)
  table
}

pop_table = lapply(X = xpaths, FUN = scrape_table)
pop_table = reduce(pop_table, full_join)

# There's a footnote here that says that the 1890 census included native americans on reservations
# which presumably was not the case any other year. Let's just change it to be consistent
pop_table$`1890`[pop_table$Name == "United States"] = "62,622,250"

# cool now we wanna go long and NA out the values before states were admitted
admitted = select(pop_table, Name, Admitted)

pop_table = pop_table %>%
  select(-Admitted) %>%
  gather(key = "Year", value = "Population", -Name)

pop_table = left_join(pop_table, admitted)

# We'll also want to include the slave population. This will be harder to present
# than program.... Also, I'm not sure why there are zeroes AND blanks
slaves = scrape_table('//*[@id="mw-content-text"]/div/table[2]') %>%
  gather(key = "Year", value  = "Slave_Population", -Name)
slaves$Slave_Population[slaves$Slave_Population %in% c("", "0")] = NA
pop_table = left_join(pop_table, slaves)

# Numeric versions will be easier to work with
pop_table$Year = as.numeric(pop_table$Year)
reformat_pop <- function(x){
  x[x==""] = NA
  x = str_replace_all(string = x, pattern = ",", replacement = "")
  as.numeric(x)
}
pop_table[c("Population", "Slave_Population")] = lapply(pop_table[c("Population", "Slave_Population")], reformat_pop)

# so we need to make some changes. There are three cases where the table rerpesents the current
# geographic borders and not the historic ones:
#  1. Pre-1820, Maine was part of mass
#  2. Pre-1863, West Va was part of Virginia

maine_fix = pop_table %>%
  filter(Name == "Maine",
         Year < 1820) %>%
  select(Name, Year, Population) %>%
  rename(Maine_Population = Population) %>%
  mutate(Name = "Massachusetts")

pop_table = left_join(pop_table, maine_fix)
not_miss_main = !is.na(pop_table$Maine_Population)
pop_table$Population[not_miss_main] = pop_table$Population[not_miss_main] + pop_table$Maine_Population[not_miss_main]
pop_table$Population[pop_table$Name == "Maine" & pop_table$Year < 1820] = NA

# now westva is basically the same thing

virginia_fix = pop_table %>%
  filter(Name == "West Virginia",
         Year < 1863) %>%
  select(Name, Year, Population) %>%
  rename(Virginia_Population = Population) %>%
  mutate(Name = "Virginia")

pop_table = left_join(pop_table, virginia_fix)
not_miss_virg = !is.na(pop_table$Virginia_Population)
pop_table$Population[not_miss_virg] = pop_table$Population[not_miss_virg] + pop_table$Virginia_Population[not_miss_virg]
pop_table$Population[pop_table$Name == "West Virginia" & pop_table$Year < 1863] = NA

# alright, we're almost there. Now we have to get rid of the data for states before they were
# admitted:

pop_table$Population[pop_table$Year < pop_table$Admitted] = NA
pop_table = select(pop_table, Name, Year, Population, Slave_Population)


# coo now we gotta get the house of representatives data
rep_page = read_html("https://en.wikipedia.org/wiki/United_States_congressional_apportionment")

rep_table = rep_page %>%
  html_nodes(xpath ='//*[@id="mw-content-text"]/div/table[3]') %>%
  html_table()
rep_table = rep_table[[1]]
colnames(rep_table) = rep_table[1,]
rep_table = rep_table[5:nrow(rep_table), 2:ncol(rep_table)]

# So apparently they didn't reaportion in 1920 sent
rep_table['1920'] = rep_table['1910']
# alright I guess this means we'll need to get a cross for the state names and abbreviations
rep_table = rep_table %>%
  rename(state_abbr = Year) %>%
  gather(key = Year, value = house_reps, -state_abbr) %>%
  mutate(house_reps = as.numeric(house_reps))
rep_table$Year = as.numeric(rep_table$Year)

# finally the crosswalk for the state abbreviations:
abbr_table = "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations" %>%
  read_html() %>%
  html_nodes(xpath ='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)
abbr_table = abbr_table[[1]]
colnames(abbr_table) = abbr_table[10,]
abbr_table = abbr_table[13:nrow(abbr_table),c(1,4)]
abbr_table = filter(abbr_table, ANSI != "") %>%
  rename(state_abbr = ANSI,
         Name = `Name and status of region`)

pop_table = left_join(pop_table, abbr_table)
pop_table = left_join(pop_table, rep_table)

# Think we may want the percent of population and percent of representatives
pop_table = filter(pop_table, !str_detect(Name, "Distr|United"), !is.na(Population))

us_pop = pop_table %>%
  group_by(Year) %>%
  summarise(us_pop = sum(Population),
            us_slave_pop = sum(Slave_Population, na.rm = TRUE))
us_pop$us_slave_pop[is.na(us_pop$us_slave_pop)] = 0
pop_table = pop_table %>%
  left_join(us_pop)
pop_table$Slave_Population[is.na(pop_table$Slave_Population)] = 0

# add on house totals
house_totals = pop_table %>%
  group_by(Year) %>%
  summarise(tot_house = sum(house_reps, na.rm = TRUE))
pop_table = left_join(pop_table, house_totals)

# and senate

senate_totals = pop_table %>%
  group_by(Year) %>%
  summarise(tot_senate = 2*n_distinct(Name))
pop_table = pop_table %>%
  left_join(senate_totals) %>%
  mutate(senators = 2) %>%
  select(Name, state_abbr, Year, Population, us_pop, Slave_Population, us_slave_pop, house_reps, tot_house, senators, tot_senate)


# RR ----------------------------------------------------------------------
# So I wanna try another formulation which will allow the color scale to

senate = pop_table %>%
  mutate(cong_ratio = senators/tot_senate,
         chamber = "Senate") %>%
  select(-house_reps, -tot_house) %>%
  rename(`# Seats` = senators,
         `Total Seats` = tot_senate)

pop_table2 = pop_table %>%
  mutate(cong_ratio = house_reps/tot_house,
         chamber = "House") %>%
  select(-senators, -tot_senate) %>%
  rename(`# Seats` = house_reps,
         `Total Seats` = tot_house) %>%
  bind_rows(senate)
colnames(pop_table2) = c("State", "State Abbr", "Year",
                        "State Population", "US Population",
                        "Slave Population", "US Slave Population",
                        "Congressional Seats", "Total Seats",
                        "Congressional Ratio", "Chamber")
write.csv(x = pop_table2, file =  "nya-nya-nya", row.names = FALSE)
