library(dplyr)

###Fixing the issue with the Qualtrics outputs where text was only showing some answers. 
###Raw is fixed version
rawtext <- read.csv(
  "/Users/EricElias/Desktop/Parallel/MDSxCategorization_December 10, 2022_09.csv", header = TRUE)
rawnumeric <- read.csv(
  "/Users/EricElias/Desktop/Parallel/MDSxCategorization_December 10, 2022_09 (1).csv", header = TRUE)
parallel_legend <- read.csv(
  "/Users/EricElias/Desktop/Parallel/mdsxcats_legend.csv", header = FALSE)

rawnumeric <-rawnumeric[,c(1:138)]
rawtext <- rawtext[,c(139:196)]
raw <- cbind(rawnumeric, rawtext)

###Organizing raw and adding subject numbers
raw <- raw[-c(1:5),-c(1:5,7:18)]
raw <- raw %>% mutate(Subject = row_number())
raw <- raw %>% select(Subject, everything())

###Cleaning will go here

###Breaking up cleaned raw into MDS, Serial, and Parallel, keeping subject 
###numbers and orders with each row
###MDS detach
mds <- raw[,c(1,3:122,172,179)]

###Serial detach and removing subjects who were not in this condition
serial <- raw[,c(1,123:138,172)]
serial <-serial[!(serial$conditionorder==13 | serial$conditionorder==31),]

###Parallel detach and removing subjects who were not in this condition
parallel <- raw[,c(1,139:172)]
parallel <-parallel[!(parallel$conditionorder==12 | parallel$conditionorder==21),]

###parallel breakdown
parallel$parallelorder <-sub(".", "", parallel$parallelorder)
parallel$parallelorder <- gsub(",,",",", parallel$parallelorder)
parallel$parallelorder <- gsub(",,",",", parallel$parallelorder)
parallel_order <- data.frame(parallel$Subject, parallel$parallelorder)
parallel_order <- parallel_order %>% rename(Subject = parallel.Subject)
list <-c()
for (e in 1:16){
  assignment <- c("trial","lefttarget","righttarget","catside")
  question <- c(e,e,e,e)
  joint <- paste(question, assignment)
  joint <- gsub(" ", "_", joint)
  list <- append(list,joint)
}
parallel_order <- parallel_order %>% separate(parallel.parallelorder, list)

parallel_order <-  parallel_order %>% pivot_longer(
  cols = -Subject,
  names_to = c("trialnum",".value"),
  names_pattern = '(\\d+)_(.*)')
parallel_order <- parallel_order[,-c(2)]

###Adding target names and races to each trial
parallel_order$leftface <- parallel_legend$V2[match(parallel_order$lefttarget,parallel_legend$V1)]
parallel_order$rightface <- parallel_legend$V2[match(parallel_order$righttarget,parallel_legend$V1)]
parallel_order$leftrace <- parallel_legend$V4[match(parallel_order$lefttarget,parallel_legend$V1)]
parallel_order$rightrace <- parallel_legend$V4[match(parallel_order$righttarget,parallel_legend$V1)]

###Adding which target participant was assigned to categorize
parallel_order$catsidetargetrace <- ifelse(parallel_order$catside == 1, 
                                           parallel_order$leftrace, 
                                           parallel_order$rightrace)

###Unpacking ratings
parallel_long <- pivot_longer(parallel, cols=2:33, names_to = "trial", values_to = "Rating")
parallel_long$trial <- sub('.*_', '', parallel_long$trial)
parallel_long <-parallel_long[!(is.na(parallel_long$Rating) | parallel_long$Rating==""), ]

###Merging ratings with order
parallelcomplete <- merge(parallel_long, parallel_order, by=c("Subject","trial"))
###parallelcomplete above has all necessary information to perform analysis
###Add here what conditions to remove and all subsequent analysis will change
###df2<-df1[!(df1$Name=="George" | df1$Name=="Andrea"),]

###Calculating total number of trials for each race for each participant
parallel_numtrials <- parallelcomplete %>% 
  group_by(Subject, catsidetargetrace)%>% 
  summarise(total_count=n(),.groups = 'drop')

##########################################
###Calculating percent correct for each race by calculating concordance for each trial (1 = correct) 
###and dividing by number of trials for each race
parallelcomplete$concordance <-ifelse(parallelcomplete$Rating == parallelcomplete$catsidetargetrace, 1, 0)
parallelcomplete <- merge(parallelcomplete, parallel_numtrials, by.x=c('Subject','catsidetargetrace'))
detach("package:dplyr", unload=TRUE)
library(plyr)
parallelconcordance <- ddply(
  parallelcomplete, .(parallelcomplete$Subject, parallelcomplete$catsidetargetrace, 
                      parallelcomplete$concordance), nrow)
names(parallelconcordance) <- c("Subject", "catsidetargetrace", "Concordance","Count")
detach("package:plyr", unload=TRUE)
library(dplyr)
parallelconcordance<- parallelconcordance[!(parallelconcordance$Concordance==0),]
parallelconcordance <- merge(parallelconcordance, parallel_numtrials, 
                             by.x=c('Subject','catsidetargetrace'))
parallelconcordance$concordance <- parallelconcordance$Count/parallelconcordance$total_count
parallelconcordance<- parallelconcordance[,-c(3,4,5)]
parallelconcordance<- parallelconcordance %>% 
  pivot_wider(names_from = catsidetargetrace, values_from = concordance)
parallelconcordance[is.na(parallelconcordance)] <- 0
###Final parallel concordance broken down by each race
parallelconcordance$groupcondition <- parallel$conditionorder[match(parallelconcordance$Subject,parallel$Subject)]
parallelconcordance <- parallelconcordance %>% relocate(White, .before = Multiracial)
###################################################
###################################################
###Building dataframe for parallel mono v multi
parallel_numtrials_monomulti <- parallel_numtrials %>% 
  pivot_wider(names_from = catsidetargetrace, values_from = total_count)
parallel_numtrials_monomulti$Monoracial <- parallel_numtrials_monomulti$White + 
  parallel_numtrials_monomulti$Asian + parallel_numtrials_monomulti$Latino
detach("package:dplyr", unload=TRUE)
library(plyr)
parallelconcordancemonomulti <- ddply(
  parallelcomplete, .(parallelcomplete$Subject, parallelcomplete$catsidetargetrace,
                      parallelcomplete$concordance), nrow)
names(parallelconcordancemonomulti) <- c("Subject", "catsidetargetrace", "Concordance","Count")
detach("package:plyr", unload=TRUE)
library(dplyr)
parallelconcordancemonomulti<- parallelconcordancemonomulti[
  !(parallelconcordancemonomulti$Concordance==0),]
parallelconcordancemonomulti<- parallelconcordancemonomulti[,-c(3)]
parallelconcordancemonomulti<- parallelconcordancemonomulti %>% 
  pivot_wider(names_from = catsidetargetrace, values_from = Count)
parallelconcordancemonomulti[is.na(parallelconcordancemonomulti)] <- 0
parallelconcordancemonomulti$Monoracial <- parallelconcordancemonomulti$Asian + 
  parallelconcordancemonomulti$Latino + parallelconcordancemonomulti$White
parallelconcordancemonomulti<- parallelconcordancemonomulti[,-c(2,3,5)]
colnames(parallelconcordancemonomulti)[
  which(names(parallelconcordancemonomulti) == "Multiracial")] <- "Multiracial_Concordance_Count"
colnames(parallelconcordancemonomulti)[
  which(names(parallelconcordancemonomulti) == "Monoracial")] <- "Monoracial_Concordance_Count"
parallelconcordancemonomulti <- merge(
  parallelconcordancemonomulti,parallel_numtrials_monomulti, by.x=c('Subject'))
parallelconcordancemonomulti$Multiracialconcordance <- 
  parallelconcordancemonomulti$Multiracial_Concordance_Count/parallelconcordancemonomulti$Multiracial
parallelconcordancemonomulti$Monoracialconcordance <- 
  parallelconcordancemonomulti$Monoracial_Concordance_Count/parallelconcordancemonomulti$Monoracial
parallelconcordancemonomulti<- parallelconcordancemonomulti[,-c(2:8)]
###Final parallel concordance broken down by monoracial or multiracial 
parallelconcordancemonomulti
###Combined dataframe of mono vs multi and individual races
parallelfinal <- merge(parallelconcordancemonomulti,parallelconcordance, by.x=c("Subject"))

######################################
######################################
###Analyzing serial condition begins
serial <- serial %>% rename("1" = 2,"2" = 3,"3" = 4,"4" = 5,"5" = 6,"6" = 7,"7" = 8,
                                 "8" = 9,"9" = 10,"10" = 11,"11" = 12,"12" = 13,"13" = 14,
                                 "14" = 15,"15" = 16,"16" = 17)

###Insert condition removal below or forever hold your peace
###df2<-df1[!(df1$Name=="George" | df1$Name=="Andrea"),]
###Calculating total number of trials for each race for each participant

###Removing condition column
serial_long <- serial[,-c(18)]

###Creating long list for concordance for each race
serial_long <- serial_long %>% pivot_longer(serial_long, cols=2:17, names_to = "trial", values_to = "Rating")
serial_long$Targetrace <- parallel_legend$V4[match(serial_long$trial, parallel_legend$V1)]
serial_long$Concordance <- ifelse(serial_long$Targetrace == serial_long$Rating, 1, 0)
serial_trialcount <- serial_long %>% group_by(Subject) %>% summarise(Count = n_distinct(trial))
detach("package:dplyr", unload=TRUE)
library(plyr)
serialconcordancebyrace <- ddply(
  serial_long, .(serial_long$Subject, serial_long$Targetrace,
                      serial_long$Concordance), nrow)
names(serialconcordancebyrace) <- c("Subject", "Targetrace", "Concordance","Count")
detach("package:plyr", unload=TRUE)
library(dplyr)
serialconcordancebyrace <- serialconcordancebyrace[
  !(serialconcordancebyrace$Concordance==0),]
serialconcordancebyrace<- serialconcordancebyrace[,-c(3)]
serialconcordancebyrace <- serialconcordancebyrace %>% 
  pivot_wider(names_from = Targetrace, values_from = Count)
serialconcordancebyrace[is.na(serialconcordancebyrace)] <- 0
serialconcordancebyrace$Asian_concordance <- serialconcordancebyrace$Asian/4
serialconcordancebyrace$Latino_concordance <- serialconcordancebyrace$Latino/4
serialconcordancebyrace$White_concordance <- serialconcordancebyrace$White/4
serialconcordancebyrace$Multiracial_concordance <- serialconcordancebyrace$Multiracial/4
serialconcordance <- serialconcordancebyrace[,-c(2:5)]
serialconcordance <- serialconcordance %>% rename("Asian" = 2,"Latino"=3,"White"=4,"Multiracial"=5)
###Final serial concordance broken down by each race
serialconcordance$groupcondition <- serial$conditionorder[match(serialconcordance$Subject,serial$Subject)]
serialconcordance
###########################
###########################
write.csv(serialconcordance, "/Users/EricElias/Desktop/Parallel/serialconcordance.csv", row.names=FALSE)
write.csv(parallelconcordance, "/Users/EricElias/Desktop/Parallel/parallelconcordance.csv", row.names=FALSE)
serial_parallel_concordance <- rbind(serialconcordance, parallelconcordance)
write.csv(serial_parallel_concordance, "/Users/EricElias/Desktop/Parallel/serial_parallel_concordance.csv", row.names=FALSE)