
#Readr code
library("readr")
potatoes<-read_csv("potatoes.csv")
head(potatoes)

# Read tsv files in R
# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")
# Import potatoes.txt: potatoes
potatoes <- read_tsv("potatoes.txt",properties)
# Call head() on potatoes
head(potatoes)


# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")
# Import potatoes.txt using read_delim(): potatoes
potatoes <- read_delim("potatoes.txt",delim="\t",col_names=properties)

# Print out potatoes
print(potatoes)


# readr is already loaded

# Column names
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")

# Import 5 observations from potatoes.txt: potatoes_fragment
potatoes_fragment <- read_tsv("potatoes.txt", skip = 6, n_max = 5, col_names = properties)
head(potatoes_fragment)



# load the data.table package
library(data.table)

# Import potatoes.csv with fread(): potatoes
potatoes <- fread("potatoes.csv")

# Print out potatoes
print(potatoes)
class(potatoes)


# fread is already loaded

# Import columns 6 and 8 of potatoes.csv: potatoes
potatoes<-fread("potatoes.csv",select=c(6,8))
head(potatoes)



# Load the XLConnect package
library(XLConnect)

# Build connection to urbanpop.xlsx: my_book
my_book <- loadWorkbook("urbanpop.xlsx")

# Print out the class of my_book
class(my_book)


# List the sheets in my_book
getSheets(my_book)

# Import the second sheet in my_book
x<-readWorksheet(my_book,"1967-1974")
head(x)


# Import columns 3, 4, and 5 from second sheet in my_book: urbanpop_sel
urbanpop_sel <- readWorksheet(my_book, sheet = 2,startCol=3,endCol=5)

# Import first column from second sheet in my_book: countries
countries <- readWorksheet(my_book,sheet=2,startCol=1,endCol=1)

# cbind() urbanpop_sel and countries together: selection
cbind(countries,urbanpop_sel)


# Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

# Add a worksheet to my_book, named "data_summary"
createSheet(my_book,"data_summary")

# Use getSheets() on my_book
getSheets(my_book)


# add some data to the sheet

input <- data.frame('inputType'=c('Day','Month'),'inputValue'=c(2,5))

writeWorksheet(my_book,input,"data_summary")
saveWorkbook(my_book)

my_book <- loadWorkbook("urbanpop.xlsx")

new <- readWorksheet(my_book,sheet=4)
head(new)

# Rename "data_summary" sheet to "summary"
renameSheet(my_book,"data_summary","summary")

# Print out sheets of my_book
getSheets(my_book)

# Save workbook to "renamed.xlsx"
saveWorkbook(my_book,"renamed.xlsx")


my_book <- loadWorkbook("renamed.xlsx")
getSheets(my_book)


###############################################################################
##data manipulation dplyr


# Load the dplyr package
library(dplyr)

# Load the hflights package
install.packages("hflights")
library(hflights)

# Call both head() and summary() on hflights
head(hflights)
summary(hflights)

# Both the dplyr and hflights packages are loaded

# Convert the hflights data.frame into a hflights tbl
hflights <- tbl_df(hflights)

# Display the hflights tbl
hflights

# Create the object carriers
carriers <- hflights$UniqueCarrier



#Look up table
# Both the dplyr and hflights packages are loaded into workspace
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Add the Carrier column to hflights
hflights$Carrier <- lut[hflights$UniqueCarrier]


# Glimpse at hflights
glimpse(hflights)


# The lookup table
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

# Add the Code column
hflights$Code <- lut[hflights$CancellationCode]

# Glimpse at hflights
glimpse(hflights)



# Print out a tbl with the four columns of hflights related to delay
select(hflights,ActualElapsedTime,AirTime,ArrDelay,DepDelay)


#Practice 2
names(hflights)
select(hflights,-(DepTime:AirTime))



# Print out a tbl containing just ArrDelay and DepDelay
select(hflights,ends_with("Delay"))


#Practice 3
select(hflights,ends_with("Time"),ends_with("Delay"))



#mutate


# Add the new variable ActualGroundTime to a copy of hflights and save the result as g1
g1<- mutate(hflights,ActualGroundTime=ActualElapsedTime-AirTime)
head(g1)

# Add the new variable GroundTime to g1. Save the result as g2.
g2<-mutate(g1,GroundTime=TaxiIn+TaxiOut)

#Practice 4
# Add a second variable loss_ratio to the dataset: m1
m1 <- mutate(hflights, loss = ArrDelay - DepDelay,loss_ratio=loss/DepDelay)


###Filter 

# All flights that traveled 3000 miles or more
filter(hflights,Distance >3000)

# All flights flown by one of JetBlue, Southwest, or Delta
filter(hflights,Carrier %in% c("JetBlue","Southwest","Delta"))

#Practice 5
filter(hflights,TaxiIn+TaxiOut >AirTime)



# All flights that were cancelled after being delayed
filter(hflights, DepDelay > 0 & Cancelled ==1)




##Arrange


# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))


# Arrange dtc by departure delays
arrange(dtc,DepDelay)

# Arrange dtc so that cancellation reasons are grouped
arrange(dtc,CancellationCode)


# Arrange dtc according to carrier and departure delays
arrange(dtc,UniqueCarrier,DepDelay)




# Arrange according to carrier and decreasing departure delays
arrange(hflights,UniqueCarrier,desc(DepDelay))

# Print out a summary with variables min_dist and max_dist
summarise(hflights,min_dist=min(Distance),max_dist=max(Distance))


# Print out a summary with variable max_div
summarise(filter(hflights,Diverted==1),max_div=max(Distance))



#unique function parameters in dplyr for R

# Generate summarizing statistics for hflights
summarise(hflights,
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest))