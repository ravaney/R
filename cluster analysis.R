library(dplyr)
library(ggplot2)
##

  #This research aims to study DNA data for one individual
  # to see if there any patterns
# while looking at the raw data text file, i noticed
# some of the data appearing in wrong columns
#it seems that shift is missing and so the data
#isnt aligned properly

dna<-read.table('c:/users/lewis/desktop/ancestrydna.txt',header=TRUE,sep='\t',skip=18)

head(dna)

#converted data to data frame, now ill check for nulls and possible errors
dna<- as.data.frame(dna)

summary(dna)
#after running a summary on the data frame, no nulls were identified
class(dna$position)

#all the data types for each variable in the data frame are ok

#what can be done with the data?
#the data set is the whole dna of an individual

#according to the data description, there are 23 pairs of chromosomes, each
# has two alleles, one from each parent, the alleles take one of 4 possible
# letters which are nitrogenous dna bases

count(unique(dna$position))
n_distinct(dna$position)
n_distinct(dna$rsid)

#it appears that the rsid is unique and not shared however,
# the position appeared to be shared by some rsid
# i will run an analysis to see which rsid's are on the same position

shared_position <- dna%>%select(rsid,position)%>%
  group_by(position)%>%
  mutate(num_rows=sum(n()))%>%
  filter(num_rows>0)

#table function shows the number of occurences for each unique value
table((shared_position$num_rows))/668893*100 
table(shared_position$num_rows)

#it appears that over 99% of the rsid's are on unique positions
# while the rest share positions with 2 3 or 4 other rsid's

set.seed(2)

sample.dna<-split(dna,sample(rep(1)))


dna%>%ggplot(aes(chromosome,rsid))+
  geom_boxplot()

View(shared_position)
