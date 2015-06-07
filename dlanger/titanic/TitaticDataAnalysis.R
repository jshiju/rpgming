# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)


# Add a "Survived" variable to the test set to allow for combining data sets
# Extract 1st column, 'None' as second columns values, extract columns 2:11
#  eg: test[,2:ncol(test)]
test.survived <- data.frame(test[1],Survived = rep("None", nrow(test)), test[,2:11])

# Combine data sets - train & test.survived
data.combined <- rbind(train, test.survived)

# Compact display
str(data.combined)

# Convert data values as factors
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Age <- as.factor(data.combined$Age)

# Take a look at gross survival rates
# Distribution across factors
table(data.combined$Survived)
table(data.combined$Pclass)
table(data.combined$Age)


# Load up ggplot2 package to use for visualization
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_histogram(width = 0.5) + 
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Hypothesis - Most passengers were young folks 
train$Age <- as.factor(train$Age)
ggplot(train, aes(x = Age, fill = factor(Age))) +
  geom_histogram(width = 0.05) + 
  xlab("Age") +
  ylab("Total Count")


# Examine the first few names in the training data set
head(as.character(train$Name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))


# Two duplicate names, take a closer look
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]


# We checked for data anormalies...

# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
length(misses)
misses[,1:6]


# Hypothesis <- Name titles correlates with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


# Expand upon the relationship between 'Survived' and 'Pclass' by adding the new
# 'Title' variable to the data set and then explore a potential 3-dimesnsional relationship

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)


# Since we only have survived lables for the train set, only use the 
# first 891 rows

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(binwidth = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# What's the distribution of females to males across train &
table(data.combined$Sex)
data.combined$Age <- as.numeric(data.combined$Age)
summary(data.combined$Age)

summary(data.combined[1:891, "Age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass and age
ggplot(data.combined[1:891, ], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

ggplot(data.combined[1:891, ], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass + Embarked) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

table(data.combined$Embarked)

# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# We know that "Miss." is more complicated, lets examine further
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by 'Pclass'") +
  xlab("Age") +
  ylab("Total Count")  

# OK, appears female children may have different survival rate,
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(misses.alone)
length(which(misses.alone$Age <= 14.5))

# Move on to the SibSp variable, summarize the variable
summary(data.combined$SibSp)

# Can we treat SibSp as a factor
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# Visualize survival by SibSp, Pclass, and Title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_histogram(binwidth =15) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

# Treat the Parch variable as a factor ans visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_histogram(binwidth =15) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")


# Lets try soome feature engineering. What about creating a family size feature?
# Pull it from train and test as we have made it factor in data.combined
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.sibsp + temp.parch + 1)

# Remove column 
data.combined$family.size <- NULL

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = Family.size, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_histogram(binwidth =15) +
  ggtitle("Pclass, Title") +
  xlab("Family.size") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")
