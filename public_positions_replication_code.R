###Fig. 1
###Create Plot of All Independent Expenditures and Dark Money Spending from CRP data. 
library(ggplot2)
library(data.table)
library(blscrapeR)

years <- seq(2000,2016, by=2)

##IE totals from https://www.opensecrets.org/outsidespending/index.php?filter=&type=A
all_ie <- c(33.8, 16.7, 63.9, 37.8,143.7,205.5,1000,549.4,1400)

##DM totals from https://www.opensecrets.org/outsidespending/nonprof_summ.php?cycle=2018&type=type&range=tot
dm <- c(11.21,4.07,5.88,5.17,102.43,138.99,313.51,178.03,178.3)

dm_tots <- data.frame("year"=years, "dm"=dm, "ie"=all_ie)

cpi <- inflation_adjust(base_year = 2018)

cpi <- cpi[cpi$year %in% 2000:2018,]

cp_dm <- merge(dm_tots, cpi, by="year")

cp_dm$dm_ia <- cp_dm$dm/cp_dm$adj_value

cp_dm$ie_ia <- cp_dm$ie/cp_dm$adj_value

dm_tots <- melt(cp_dm, id.vars=c("year") )

dm_tots <- dm_tots[dm_tots$variable %in% c("dm_ia","ie_ia"),]


ggplot(dm_tots, aes(x=year, y=as.numeric(value), label=value, color=variable)) + geom_line(size=1.5, aes(linetype=variable)) + theme_bw() +
  scale_y_continuous("Total Spending (Millions of 2018 $)",labels=scales::dollar) + scale_x_continuous("Year", breaks=years) +
  scale_linetype_discrete(name="Type of Spending", labels=c("Dark money","All IE's")) + scale_color_discrete(name="Type of Spending", labels=c("Dark money","All IE's"))+
  theme(legend.position ="bottom")


###Figure 2
###Map of 
library(zipcode)
library(ggmap)
library(tidyverse)

##Read in cleaned list of donor names
zips <- read.csv("darkmoney_redact_vs_nonredact.csv") %>% select(Zip)

data(zipcode)

ajs_zips <- merge(zips, zipcode, by.x='zips', by.y='zip')

usa <- map_data("state")

g <- 
  ggplot(data=usa) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), fill="grey", color = "white") + 
  geom_point(data=ajs_zips, 
             aes(x=longitude, y=latitude), colour="Red", 
             fill="Red",pch=21, size=1, alpha=I(0.7)) + 
  coord_fixed(1.3) +
  guides(fill=FALSE)   + theme_nothing()
g


##Figure 4
##Redacted vs. Non-redacted giving totals for AJS data. 

##AJS names
##File with names of all donors as well as link to independent verification of their 
##address and identity.
redact <- read.csv("darkmoney_redact_vs_nonredact.csv")

###create redaction dummy variable

redact$redacted <- ifelse(grepl("XXX",redact$Actual.Name)==T, 1,0)

###density plot of AJS donor ideology

library(ggplot2)

ggplot(redact, aes(x=factor(redacted), y=Amount, fill=factor(redacted))) + 
  geom_bar(stat="sum") +
  theme(legend.position = "none") +
  scale_y_continuous(labels=scales::dollar) + theme_bw()+
  scale_x_discrete(name = "Redacted vs. Unredacted Donations",
                   labels=c("Unredacted","Redacted"))+
  theme(legend.position = "none")


###Figure 5
###Ideological Distribution of Donor ideologies

##Code for Matching Open Donors to their Bonica Scores, (to create "open_donors.csv")

##Download files from DIME database

##2008
download.file("https://dataverse.harvard.edu/api/access/datafile/2865295?gbrecs=true", "dm_08.tar.gz")

##2010
download.file("https://dataverse.harvard.edu/api/access/datafile/2865296?gbrecs=true", "dm_10.tar.gz")

##2012
download.file("https://dataverse.harvard.edu/api/access/datafile/2865298?gbrecs=true", "dm_12.tar.gz")

library(data.table)
library(tidyverse)

db_08 <- fread("dm_08.csv") %>% select(c(contributor.name,contributor.lname,contributor.fname, contributor.mname,
                                         contributor.zipcode,contributor.cfscore))

db_10 <- read.csv("dm_10.csv") %>% select(c(contributor.name,contributor.lname,contributor.fname, contributor.mname,
                                            contributor.zipcode,contributor.cfscore))

db_12 <- read.csv("dm_12.csv") %>% select(c(contributor.name,contributor.lname,contributor.fname, contributor.mname,
                                            contributor.zipcode,contributor.cfscore))

dm_conts <- rbind(db_08, db_10, db_12)

save(dm_conts, file= "contributor_data.rda")

rm(db_08, db_10, db_12)

##subset to unique names

dm_conts$index <- paste(toupper(dm_conts$contributor.name), 
                        substr(dm_conts$contributor.zipcode,1,5), sep="_")

##open donors from Maplight.org. Data downloaded at http://data.maplight.org/CA/2011/records/other.zip
other_2011 <- fread("other_2011.csv", header=T, sep=",")

##just donors to Prop 30 and Prop 32
Prop30 <- subset(other_2011, other_2011$Target=="PROPOSITION 030 - TEMPORARY TAXES TO FUND EDUCATION. GUARANTEED LOCAL PUBLIC SAFETY FUNDING. INITIAT")
Prop32 <- subset(other_2011, other_2011$Target=="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT")

##subset donors to relevant positions
No_30 <- subset(Prop30, Prop30$Position == "OPPOSE")
Yes_32 <- subset(Prop32, Prop32$Position == "SUPPORT")
No_30$Name <- toupper(No_30$DonorNameNormalized)
Yes_32$Name <- toupper(Yes_32$DonorNameNormalized)

##create index of zipcode and name for transparent donors
Yes_32$index <- paste(toupper(Yes_32$DonorNameNormalized), substr(Yes_32$DonorZipCode,1,5), sep="_")
No_30$index <- paste(toupper(No_30$DonorNameNormalized), substr(No_30$DonorZipCode,1,5), sep="_")

##create index for Bonica scores
dm_conts$index <- paste(toupper(dm_conts$contributor.name), 
                        substr(dm_conts$contributor.zipcode,1,5), sep="_")

dm_conts$Name <- toupper(dm_conts$contributor.name)

id_30_in <- merge(No_30, dm_conts, by="index")
id_32_in <- merge(Yes_32, dm_conts, by="index")

##dedupe results

id_32_in <- id_32_in[!duplicated(id_32_in$contributor.name),]
id_30_in <- id_30_in[!duplicated(id_30_in$contributor.name),]

###output results to csv 

open_donors <- rbind(id_32_in, id_30_in)

write.csv(open_donors, "open_donors.csv", row.names=F)


##AJS names

ajs_names <- read.csv("hc_donor_table.csv")

##fuzzy merge the DM names to their bonica scores
##load merged ajs data
merge_ajs <- read.csv("merge.ajs_names_73.csv")

merge_ajs$contributor.cfscore <- merge_ajs$contributor_cfscore

merge_ajs <- merge_ajs[!is.na(merge_ajs$contributor.cfscore),]



##load open donors with matched cfscore

open_dons <- read.csv("open_donors.csv")

cf_32 <- open_dons[open_dons$Target=="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

cf_30 <- open_dons[open_dons$Target!="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

Yes_32 <- open_dons[open_dons$Target=="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

No_30 <- open_dons[open_dons$Target!="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

#Donors who donated both openly and clandestinely

Both32 <- merge(Yes_32, ajs_names, by.x="Name.x", by.y="bon_name")
Both30 <- merge(No_30, ajs_names, by.x="Name.x", by.y="bon_name")
##3 donors donated to the DM groups and openly 


##label donors

merge_ajs$ID <- 'Americans for Job Security'
cf_30$ID <- 'Proposition 30 Donors'
cf_32$ID <- 'Proposition 32 Donors'

u.merge_ajs <-  merge_ajs[complete.cases(merge_ajs[,9]),]
u.cf_30 <- cf_30[complete.cases(cf_30[,33]),]
u.cf_32 <- cf_32[complete.cases(cf_32[,33]),]

u.merge_ajs <- u.merge_ajs[!duplicated(u.merge_ajs$Name),]
u.cf_30 <- u.cf_30[!duplicated(u.cf_30$index),]
u.cf_32 <- u.cf_32[!duplicated(u.cf_32$index),]


cfscore <- subset(u.merge_ajs, select=c(contributor.cfscore, ID))
cfscore <- rbind(subset(u.cf_30, select=c(contributor.cfscore, ID)),
                 subset(u.merge_ajs, select=c(contributor.cfscore, ID)),
                 subset(u.cf_32, select=c(contributor.cfscore, ID)))

#median values
vline.dat <- data.frame("ID"=levels(factor(cfscore$ID)), "vl"=c(median(cfscore$contributor.cfscore[cfscore$ID=="Americans for Job Security"]), median(cfscore$contributor.cfscore[cfscore$ID=="Proposition 30 Donors"]),                                                  median(cfscore$contributor.cfscore[cfscore$ID=="Proposition 32 Donors"])))

##visualtion of AJS vs open donors
ggplot(cfscore, aes(contributor.cfscore, fill = ID)) + 
  geom_density(adjust=2/5  ,alpha = 0.2) + 
  xlab("Donor CF Scores, (Neg. Values = More Liberal)") + 
  theme(legend.position="none") +
  geom_vline(data=vline.dat, aes(xintercept=vl), linetype="dashed")  +
  facet_grid(ID ~ .) 

###Figure 7
###Map of redacted vs un-redacted AJS names
##AJS names

redact <- read.csv("darkmoney_redact_vs_nonredact.csv")

###create redaction dummy variable

redact$redacted <- ifelse(grepl("XXX",redact$Actual.Name)==T, 1,0)

###density plot of AJS donor ideology
library(zipcode)
library(ggmap)
library(ggplot2)

data(zipcode)

ajs_zips <- merge(redact, zipcode, by.x='Zip', by.y='zip')

usa <- map_data("state")

g <- 
  ggplot(data=usa) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), fill="grey", color = "white") + 
  geom_point(data=ajs_zips, 
             aes(x=longitude, y=latitude, color=factor(redacted),
                 fill=factor(redacted),pch=factor(redacted)), size=2) + 
  coord_fixed(1.3) +
  theme_nothing()
g



###Figure 8
###Density plot of donors giving over $500

##AJS names

ajs_names <- read.csv("hc_donor_table.csv")

##load merged ajs data

merge_ajs <- read.csv("merge.ajs_names_73.csv")
merge_ajs <- merge_ajs[!is.na(merge_ajs$contributor_cfscore),]

merge_ajs$contributor.cfscore <- merge_ajs$contributor_cfscore

open_dons <- read.csv("open_donors.csv")

cf_32 <- open_dons[open_dons$Target=="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

cf_30 <- open_dons[open_dons$Target!="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

Yes_32 <- open_dons[open_dons$Target=="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]

No_30 <- open_dons[open_dons$Target!="PROPOSITION 032 - PROHIBITS POLITICAL CONTRIBUTIONS BY PAYROLL DEDUCTIONS. PROHIBITIONS ON CONTRIBUT",]



##label donors

merge_ajs$ID <- 'Americans for Job Security'
cf_30$ID <- 'Proposition 30 Donors'
cf_32$ID <- 'Proposition 32 Donors'

b.cf_30 <- subset(cf_30, cf_30$TransactionAmount >= 500)
b.cf_32 <- subset(cf_32, cf_32$TransactionAmount >= 500)


b.cfscore <- subset(merge_ajs, select=c(contributor.cfscore, ID))
b.cfscore <- rbind(subset(b.cf_30, select=c(contributor.cfscore, ID)),
                   subset(merge_ajs, select=c(contributor.cfscore, ID)),
                   subset(b.cf_32, select=c(contributor.cfscore, ID)))

##add median lines

vline.dat <- data.frame("ID"=levels(factor(b.cfscore$ID)), "vl"=c(median(b.cfscore$contributor.cfscore[b.cfscore$ID=="Americans for Job Security"], na.rm=T), median(b.cfscore$contributor.cfscore[b.cfscore$ID=="Proposition 30 Donors"], na.rm=T),  median(b.cfscore$contributor.cfscore[b.cfscore$ID=="Proposition 32 Donors"], na.rm=T)))


ggplot(b.cfscore, aes(contributor.cfscore, fill = ID)) + geom_density(adjust=2/5,alpha = 0.2) + 
  xlab("Donor CF Scores") + theme(legend.position="none") +
  geom_vline(data=vline.dat, aes(xintercept=vl), linetype="dashed") + 
  facet_grid(ID ~ .)




