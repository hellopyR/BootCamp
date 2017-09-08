# dplyr tutorial
library(data.table) # for fread
library(dplyr) #for data munging
mydata <- fread("sampledata.csv")

#1 selecting random N rows
sample_n(mydata,3)

#2 selecting random fraction of rows
sample_frac(mydata,0.1)

#3 remove duplicate Rows based on all the variables(complete row)
unique=distinct(mydata)

#4 remove duplicate rows based on a variable/s
unique2=distinct(mydata,Index,.keep_all = TRUE)

unique3=distinct(mydata,Index,Y2010,.keep_all = TRUE)

#5 select() to select only desired variables
mydata2=select(mydata,Index,State:Y2008)

#6 dropping variables
mydata2=select(mydata2,-Index,-State)

#7 selecting variables that start with Y
mydata3=select(mydata,starts_with("Y")) #if '-' then the variable will be dropped

#8 Reorder Variables
mydata4=select(mydata,State,everything()) # will put state as the first column

#9 rename a column
mydata4=rename(mydata4,Index1=Index) # Index variable will be renamed to Index1

#10 filter rows
mydata5<- filter(mydata, Index %in% c("A","C"))

#10 multiple criterion
mydata6<- filter(mydata, Index %in%c("A","C")& Y2002>=1300000)

#11 not condition
mydata7<-filter(mydata, !Index %in% c("A","C"))

#12 contains condition
mydata8<-filter(mydata,grepl("Ar",State))

#13 summarise function
summarise(mydata,Y2015_mean=mean(Y2015),Y2015_median=median(Y2015))

#14 summarise multiple variables
summarise_at(mydata,vars(Y2005,Y2006),funs(n(),mean,median))

#summarising with custom functions
summarise_at(mydata,vars(Y2011,Y2012),funs(n(),missing=sum(is.na(.)),mean(.,na.rm=TRUE),median(.,na.rm=TRUE)))

#summarize all numerica variables
summarise_if(mydata,is.numeric,funs(n(),mean,median))

#arrange: sort the data, default order is ascending
arrange(mydata,Index,Y2011)

#arrange descending order
arrange(mydata,desc(Index),Y2011)

# Pipe operator %>%
#present in magrittr package, and can be used to write sub queries and a clean code
mydata %>% select(Index,State)%>% sample_n(10)

# group_by group data by categorical variables
t<- mydata %>% group_by(Index) %>% summarise_at(vars(Y2011:Y2015),funs(n(),mean(.,na.rm=TRUE)))
t


#do function
#used to do computation within in the group
#i want to pull top two 2 rows in 'A','C','I' in the Index variables
t<-mydata%>%filter(Index %in% c('A','C','I'))%>%group_by(Index)%>%do(head(.,2))
t


#select 3rd Maximum Value by Categorical Variables
#slice() function is used to select rows by the position
t=mydata%>%select(Index,Y2015)%>%filter(Index %in% c("A","C","T"))%>%group_by(Index)%>%do(arrange(.,desc(Y2015)))%>% slice(3)
t


#using window function
#same example as above
t=mydata%>%select(Index,Y2015)%>%filter(Index %in% c("A","C","T"))%>%group_by(Index)%>%filter(min_rank(desc(Y2015))==3)
t


#mutate makes new variable in the dataset
#the following code calculates division of Y2015 by 2014 and names it "change"
mydata1=mutate(mydata,change=Y2015/Y2014)
head(mydata1)

#suppose you want to rank the rows for the variables Y2008 to Y2010
mydata9<-mutate_at(mydata,vars(Y2008:Y2010),funs(Rank=min_rank(.)))
head(mydata9)
#by default min_rank() assigns 1 to the smallest value and high number to the largest value.
#If you want to assign rank 1 to the largest value call min_rank with desc.
#do it
mydata9<-mutate_at(mydata,vars(Y2008:Y2010),funs(Rank=min_rank(desc(.))))

#cumulative sum
#lets say we want to generate cumulative sum for each of the index
mydata10<-mydata%>%group_by(Index)%>% mutate(Total=cumsum(Y2015))%>%select(Index,Y2015,Total)
head(mydata10)


#apply row wise operations
#suppose you want to find maximum in each row
mydata11<-mydata %>% rowwise()%>%mutate(Max=max(Y2012:Y2015))%>%select(Y2012:Y2015,Max)
head(mydata11)

#let's say if you want to summarise the factor data
mydata$Index<-as.factor(mydata$Index)
summarise_if(mydata,is.factor,funs(nlevels(.)))

#Look up tables in R
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
# Add the Carrier column to hflights
hflights$Carrier <- lut[hflights$UniqueCarrier]

# Glimpse at hflights
glimpse(hflights)

#making a tibble
tib<-tbl_df(hflights)
glimpse(hflights)

