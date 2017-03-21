# setwd("/Users/emily/Documents/ma415/midterm proj") # Don't need to set working directory with projects
require(foreign)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)



#####################################################################################
# read accidents dbf
a <- read.dbf("accid.dbf")

# RELINSP was a duplicate of ACTIVITYNO, so removed it
a$RELINSP[a$RELINSP != a$ACTIVITYNO]
a <- select(a, -RELINSP)

# eliminate irrelevant variables
a <- select(a, -OCC_CODE)

# SITESTATE is all Massachusetts, so can delete column
# HAZSUB does not pertain too much to worker safety, so removed
length(a[a$SITESTATE != 'MA']) 
a <- select(a, -SITESTATE, -HAZSUB)


# replace code numbers of 0 with NA
a$AGE[a$AGE == 0] <- NA
a$DEGREE[a$DEGREE == 0] <- NA
levels(a$NATURE)[levels(a$NATURE) == "00"] <- NA
levels(a$BODYPART)[levels(a$BODYPART) == "00"] <- NA
levels(a$SOURCE)[levels(a$SOURCE) == "00"] <- NA
levels(a$EVENT)[levels(a$EVENT) == "00"] <- NA
levels(a$ENVIRON)[levels(a$ENVIRON) == "00"] <- NA
levels(a$HUMAN)[levels(a$HUMAN) == "00"] <- NA
a$AGE[a$AGE == 99] <- NA
a$TASK[a$TASK == 0] <- NA

# read reference dbf, remove category column to make joining easier
aFac <- read.dbf("lookups/acc.dbf") 
aFac <- aFac[-1]

# divide aFac table into categories
partbody <- aFac[1:31,]
envirfac <- aFac[32:49,]
eventtyp <- aFac[50:63,]
humanfac <- aFac[64:83,]
naturinj <- aFac[84:105,]
sourcinj <- aFac[106:153,]

# name first column of each category so it can be joined to table 'a'
colnames(partbody)[1] <- 'BODYPART' 
colnames(envirfac)[1] <- 'ENVIRON'
colnames(eventtyp)[1] <- 'EVENT'
colnames(humanfac)[1] <- 'HUMAN'
colnames(naturinj)[1] <- 'NATURE'
colnames(sourcinj)[1] <- 'SOURCE'

# left join each category to table 'a', and delete original columns with numeric labels
a <- left_join(a, partbody, by = 'BODYPART')
a <- select(a, -BODYPART)
colnames(a)[colnames(a) == 'VALUE'] <- 'BODYPART'

a <- left_join(a, envirfac, by = 'ENVIRON')
a <- select(a, -ENVIRON)
colnames(a)[colnames(a) == 'VALUE'] <- 'ENVIRON'

a <- left_join(a, eventtyp, by = 'EVENT')
a <- select(a, -EVENT)
colnames(a)[colnames(a) == 'VALUE'] <- 'EVENT'

a <- left_join(a, humanfac, by = 'HUMAN')
a <- select(a, -HUMAN)
colnames(a)[colnames(a) == 'VALUE'] <- 'HUMAN'

a <- left_join(a, naturinj, by = 'NATURE')
a <- select(a, -NATURE)
colnames(a)[colnames(a) == 'VALUE'] <- 'NATURE'

a <- left_join(a, sourcinj, by = 'SOURCE')
a <- select(a, -SOURCE)
colnames(a)[colnames(a) == 'VALUE'] <- 'SOURCE'

a <- unique(a)

#####################################################################################

# read violations dbf
v <- read.dbf("viol.dbf")

# eliminate irrelevant variables
v <- select(v, -SITESTATE, -ISSUANCE, -ITEMGROUP, -PENCURRENT, -PENINITIAL, -VIOLTYPEA, -STD, -STD_LOOKUP, -ABATE, -ABATEDT,
            -ABATEDT2, -ABATEDONE, -ERCONTDT, -ERCONDATE, -PENCONT, -EMPRCONT, -EMPECONT, -FINORDT,
            -FINORDATE, -AMENDED, -ISA, -DISPEVT, -HAZCAT, -FTAINSP, -FTAPEN, -ISSUDT, -FTA_ISDT, -CONTDT, -CONTDATE, -FTA_AMN,
            -FTA_ISA, -FTA_DISP, -FTA_FIN, -FTAFINDT, -ITEMNO, -REC, -VIOLCONT, -PMA, -CITATION, -DATE_ABATE)


# delete entries that have a mark on the DELETE column
v <- v[is.na(v$DELETE),]
v <- select(v, -DELETE)

# replace violation types with verbal definition
levels(v$VIOLTYPE)[levels(v$VIOLTYPE) == 'S'] <- 'serious'
levels(v$VIOLTYPE)[levels(v$VIOLTYPE) == 'O'] <- 'other'
levels(v$VIOLTYPE)[levels(v$VIOLTYPE) == 'R'] <- 'repeat'
levels(v$VIOLTYPE)[levels(v$VIOLTYPE) == 'U'] <- 'unclassified'
levels(v$VIOLTYPE)[levels(v$VIOLTYPE) == 'W'] <- 'willful'


#####################################################################################

# read OSHA dbf
o <- read.dbf("osha.dbf")

(o$CONTFLAG[!is.na(o$CONTFLAG)]) # testing to see how many entries have something in this column.
# there are only two entries with anything in this column, so can eliminate it
o[which((!is.na(o$CONTFLAG))),] # shows which two entries have the continuation. Upon inspection, the second
# entry seems to add no new information to the first, so removed it
o <- o[-69721,]
(o$EMPCOUNT[o$EMPCOUNT != 0]) # checking if any values in this column -- there are none -> will remove
(o$EMPCOVERED[o$EMPCOVERED != 0]) # no values in this column either -> will remove 

# select only relevant variables
o <- select(o, ACTIVITYNO, WALKAROUND, INTRVIEWD, WHYNOINSP, CLOSECASE, SITECNTY)

#####################################################################################

# join data tables by common variable ACTIVITYNO
all <- full_join(a, v, by = "ACTIVITYNO")
all <- full_join(all, o, by = "ACTIVITYNO")

# prelim graphics
ggplot(data=all, aes(x=AGE)) + geom_histogram()
ggplot(data=all, aes(x=ISSUEDATE)) + geom_histogram(binwidth = 365) 

# remove columns with all NA
for(colnum in 1:length(all)){
  allna <- TRUE
  for(i in 1:nrow(all[colnum])){ 
    if(!is.na(all[i, colnum])){
      allna <- FALSE
      break}}
  if(allna == FALSE){ # allna = FALSE means there is at least one entry in that column that is not NA
    break}
  else{
    all <- all[-colnum]}
}

# remove columns with less than 30 non-NA entries
for(colnum in 1:length(all)){
  count = 0
  for(i in 1:nrow(all[colnum])){ 
    if(!is.na(all[i, colnum])){
      count <- sum(1, count)}}
  if(count >= 30){
    break}
  else{
    all <- all[-colnum]}
}

# read scc dbf 

scc <- read.dbf("lookups/scc.dbf")
mascc <- filter(scc, STATE=="MA")
mascc <- select(mascc, -TYPE, -STATE, -CITY)
colnames(mascc) <- c('SITECNTY', 'COUNTY')
mascc <- mascc[1:15,]

# join county names to "all"
all <- left_join(all, mascc, by = 'SITECNTY')
all <- select(all, -SITECNTY)

# convert dates
all$ISSUEDATE <- as.Date(all$ISSUEDATE)

# convert activity number to factor
all$ACTIVITYNO <- as.factor(all$ACTIVITYNO)

arrange(all, ACTIVITYNO)
# remove duplicate entries
all <- unique(all)

#####################################################################################

# problems: there are still duplicate activity numbers because some cases have multiple entries to compensate for 
# one variable for which it has multiple different values, so these cases will be counted twice when analyzing 
# other variables, which would lead to inaccurate analysis. To solve this problem during analysis/graphics, 
# need to make a table that ignores duplicate activity numbers--this can only be used to analyze/make graphics out
# of variables that do not have multiple entries per activity number, otherwise it would not be taking into account
# all of the data recorded for those variables. so that's why I made this thing. only use "all" for data that
# would have duplicate entries for each case, which are the following: VIOLTYPE, NUMEXPOSED
all_noDUPL <- all[!duplicated(all$ACTIVITYNO),]

# exploratory graphics to choose from
ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$SEX)), aes(x=SEX, fill=DEGREE)) + geom_bar(position='fill') #proportion of male/females and degrees of injury

ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$AGE)), aes(x=AGE, fill=DEGREE)) + geom_bar() #proportion of male/females and degrees of injury
ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$AGE)), aes(x=AGE, fill=DEGREE)) + geom_bar(position = 'fill') #proportion of male/females and degrees of injury
ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$DEGREE)&!is.na(all_noDUPL$SEX)), aes(x=DEGREE, fill=SEX)) + geom_bar() # proportion of male/females and degrees of injury
ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$DEGREE)), aes(x=TASK, fill=DEGREE)) + geom_bar(position='fill') # larger fatality rate when employee was doing an non-regularly assigned task
ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$DEGREE)), aes(x=HUMAN, fill=DEGREE)) + geom_bar(position='fill') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data=subset(all_noDUPL, !is.na(all_noDUPL$TASK)), aes(x=AGE, y=TASK, fill=DEGREE)) + geom_area()

ggplot(data=subset(all, !is.na(VIOLTYPE)&!is.na(GRAVITY)), aes(x=VIOLTYPE, y=GRAVITY)) + geom_jitter() # good one

############################# to do: 
# use markdown to integrate code and graphics
# remember to include github link
# graphics just show the significance of variables chosen instead of creating models etc
# export plots as image for putting in knitted doc
# there might be a problem with duplicate activity numbers being used for violations--their inspection data would be
  # counted multiple times!!!!!!!

