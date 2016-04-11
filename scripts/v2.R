#### Make Over Monday 

## `````````````````````````````````````````````
## Load Libraries ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(stringr)
## `````````````````````````````````````````````

## `````````````````````````````````````````````
## Read Data ####
fp = "/mnt/r.makeOver.2016Apr10/data/From Millions to Billions.csv"
df.raw = read.csv(file=fp, stringsAsFactors = FALSE)
## `````````````````````````````````````````````

## `````````````````````````````````````````````
## Data Manipulations ####
## `````````````````````````````````````````````

df.1 = df.raw

### Fixing Names ####
names(df.1)[2] = "Millionaire.Age"
names(df.1)[3] = "Billionaire.Age" 

### Fixing Percentage ####
df.1$Net.Worth %>% 
  str_replace_all("\\,","") %>% # removing comma sign 
  as.numeric() -> # convert to numeric
  df.1$Net.Worth # assign to Net Worth 

### Giving them marks on each of the 4 metrics ####
# 1. Youngest Millionaire
# 2. Youngest Billionaire
# 3. Time taken from Millionaire to Billionaire
# 4. Current Net Worth

### Youngest Millionaire Gets Highest, and so for the rest of Indexes

# In rank:
# negative sign to give the lowest age the highest credit or score
# ties method, max to assign the same rank to equal values

df.2 <-
  df.1 %>% 
  mutate(ageD = df.1$Billionaire.Age - df.1$Millionaire.Age,
         mI = rank(-df.1$Millionaire.Age,ties.method= "max"), # millionaire Index
         bI = rank(-df.1$Billionaire.Age,ties.method= "max"), # billionaire Index
         gI = rank(-ageD,ties.method= "max"), #year from millionaire to billionaire, smaller number is better
         wI = rank(df.1$Net.Worth,ties.method= "max")) # networth Index, bigger number is better
         #allIndex = mI + bI + gI + wI,
         #oI = rank(allIndex,ties.method= "max")), # bigger is better

df.3 <- 
  df.2 %>%
  select(Name,mI,bI,gI,wI) %>%
  gather(metric,value,mI:wI)


## `````````````````````````````````````````````
## Data Visulization ####
## `````````````````````````````````````````````

## basic graph ####
g1 <- ggplot(df.3, aes(y=value, x=Name))
#g1 <- g1 + geom_bar(stat="identity", width=0.75, color="#2b2b2b", size=0.05)
g1 <- g1 + geom_bar(aes(fill=Name), stat="identity", width=0.75, size=0.05)

# faceting

# Fixing Facet Labels
# http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
# There are a few different ways of modifying facet labels. The simplest way is to provide a named vector that maps original names to new names. To map the levels of sex from Female ==
#  > Women, and Male ==  > Men:
facetLabels <- c(mI = "Millionaire Age", bI = "Billionaire Age", gI = "From M to B", wI = "Net Worth")

g2 <- g1 + facet_grid(~metric, scales="free",, labeller=labeller(metric = facetLabels))
g2 <- g2 + labs(x=NULL, y=NULL) + 
ggtitle(expression(atop(bold("4 Metrics To Analyze The Rich"), atop(italic("For first three indexes: smaller values get bigger score. For last index, otherwise"), ""))))

# aesthetics 
g3 <- g2 + theme(panel.background=element_rect(fill="#efefef", color=NA))
g3 <- g3 + theme(strip.background=element_rect(fill="#858585", color=NA))
g3 <- g3 + theme(strip.text=element_text(family="OpenSans-CondensedBold", size=12, color="white", hjust=0.5))
g3 <- g3 + theme(panel.margin.x=unit(1, "cm"))
g3 <- g3 + theme(panel.margin.y=unit(0.5, "cm"))
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + theme(panel.grid.major.y=element_line(color="#b2b2b2"))
g3 <- g3 + theme(axis.ticks = element_blank())


# Hide all the vertical gridlines
g4 <- g3 + theme(panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank())

g4 <- g4 + theme(panel.grid.minor.y=element_blank())

g4 <- g4 + theme(plot.title = element_text(face="bold", color = "black", size=18))

# rotating the axis
g4 <- g4 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))

g4