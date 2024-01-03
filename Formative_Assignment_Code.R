# A. Installing the packages
install.packages("palmerpenguins")
library(palmerpenguins)
data("penguins")
penguins = na.omit(penguins) # Removes missing rows
penguins
write.csv(penguins, "palmerpenguins.csv")

# B. Taking 100 samples of data
my.student.number = 220606343
set.seed(my.student.number)
my.penguins = penguins[sample(nrow(penguins), 100), ]
?sample
my.penguins
write.csv(my.penguins, "palmerpenguinssample.csv")

# 1. Sample Data Sets
dim(my.penguins) # 100 rows, 8 cols
nrow(my.penguins) # 100 rows
ncol(my.penguins) # 8 cols
colnames(my.penguins) # all the columns
summary(my.penguins)

# 1.a. Sample Data for 2007
sample.2007 = my.penguins[my.penguins$year == 2007, ]
nrow(sample.2007) # 32 data

sample.2007.male = my.penguins[my.penguins$year == 2007 & my.penguins$sex == 'male', ]
sample.2007.female = my.penguins[my.penguins$year == 2007 & my.penguins$sex == 'female', ]
nrow(sample.2007.male) # 18
nrow(sample.2007.female) # 14

# 1.b. Sample data for 2008
sample.2008 = my.penguins[my.penguins$year == 2008, ]
nrow(sample.2008) # 34 data

sample.2008.male = my.penguins[my.penguins$year == 2008 & my.penguins$sex == 'male', ]
sample.2008.female = my.penguins[my.penguins$year == 2008 & my.penguins$sex == 'female', ]
nrow(sample.2008.male) # 15
nrow(sample.2008.female) # 19

# 1.c. Sample data for 2009
sample.2009 = my.penguins[my.penguins$year == 2009, ]
nrow(sample.2009)

sample.2009.male = my.penguins[my.penguins$year == 2009 & my.penguins$sex == 'male', ]
sample.2009.female = my.penguins[my.penguins$year == 2009 & my.penguins$sex == 'female', ]
nrow(sample.2009.male) # 14
nrow(sample.2009.female) # 20


# 2. How many species do we have in the dataset:
# 2.a. The general one with mixed years
species.counts <- table(my.penguins$species)
barplot(species.counts, main="Species Distribution", xlab="Counts of the Species")

# 2.b. The breakdown of the years
library(ggplot2)
library(dplyr)
library(tidyverse)

df = my.penguins %>% group_by(species) %>% count(year)
df
ggplot(data = df, aes(fill = factor(year), y = n, x = species)) + geom_bar(position="dodge", stat = "identity") + labs(size="Year", col="Species")
ggplot(data = df) + geom_col(aes(x = species, y = n, fill = factor(year)))

# Body Mass G MEAN
df = my.penguins %>% group_by(species, year) %>% summarise_at(vars(body_mass_g), list(body_mass_g_mean = mean))
df
ggplot(data = df, aes(fill = factor(year), body_mass_g_mean, x = species)) + geom_bar(position="dodge", stat = "identity") + labs(size="Body Mass Mean", col="Species")

# Flipper Length Mean
df = my.penguins %>% group_by(species, year) %>% summarise_at(vars(flipper_length_mm), list(flipper_length_mm_mean = mean))
df
ggplot(data = df, aes(fill = factor(year), flipper_length_mm_mean, x = species)) + geom_bar(position="dodge", stat = "identity") + labs(size="Flipper Length Mean (mm)", col="Species")

# Bill Depth Mean
df = my.penguins %>% group_by(species, year) %>% summarise_at(vars(bill_depth_mm), list(bill_depth_mm_mean = mean))
df
ggplot(data = df, aes(fill = factor(year), bill_depth_mm_mean, x = species)) + geom_bar(position="dodge", stat = "identity") + labs(size="Bill Depth Mean (mm)", col="Species")


# 3. Species with body plot over the pass 3 years
ggplot(data = my.penguins[my.penguins$year==2007, ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2007 & my.penguins$sex == 'female', ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2007 & my.penguins$sex == 'male', ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2008, ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2008 & my.penguins$sex == 'female', ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2008 & my.penguins$sex == 'male', ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2009, ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2009 & my.penguins$sex == 'female', ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins[my.penguins$year==2009 & my.penguins$sex == 'male', ], aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)

ggplot(data = my.penguins, aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  stat_summary(fun=mean, geom="point", shape=5, size=4)
