options(digits = 3)
library(ggplot2)
library(dplyr)
library(reshape2)

data <- read.csv('C:/Users/Оник/Desktop/proj.csv', sep = ';', skip = 1, stringsAsFactors = F)
data[,10:15] <- NULL
colnames(data) <- c('name', 'gender', 'age', 'work', 'friends', 'children', 'spouse', 'parents', 'acquaintances')
data$age_group <- NA

data$age_group[data$age <= 20] <- 'teens'
data$age_group[data$age %in% 21:34] <- 'young'
data$age_group[data$age %in% 35:50] <- 'adults'
data$age_group[data$age %in% 51:65] <- 'senior'
data$age_group[data$age > 65] <- 'old'

data$children[grepl('не имеет', data$children)] <- NA
data$spouse[grepl('не имеет', data$spouse)] <- NA
data$work[grepl('не имеет', data$work)] <- NA

data$gender <- as.factor(data$gender)
data$age_group <- as.factor(data$age_group)
data$age_group <- factor(data$age_group, levels=c('teens', 'young', 'adults', 'senior', 'old'))
# typeof(data$age)
data[, 4:9] <- lapply(data[, 4:9], as.integer)

data$points <- rowSums(data[, 4:9], na.rm = T)

aggregate(points ~ age_group, data, sum)

data$percent <- 100 * rowSums(data[, 4:9], na.rm = T) / (apply(data[, 4:9], 1, function(x) sum(is.na(x) == F)) * 10)
# data$percent <- round(data$percent, 0)


for (i in levels(data$age_group)){
  data$cum_group[data$age_group == i] <- sum(data$points[data$age_group == i])
  data$cum_group_max[data$age_group == i] <- sum(apply(data[data$age_group == i, 4:9], 1, function(x) sum(is.na(x) == F)) * 10)
}
data$percent_group <- data$cum_group / data$cum_group_max * 100

for (i in levels(data$age_group)){
  for(j in levels(data$gender)){
    data$cum_group_gender[data$age_group == i & data$gender == j] <- sum(data$points[data$age_group == i & data$gender == j])
    data$cum_group_gender_max[data$age_group == i & data$gender == j] <- sum(apply(data[data$age_group == i & data$gender == j, 4:9], 1, function(x) sum(is.na(x) == F)) * 10)
  }
}
data$percent_group_gender <- data$cum_group_gender / data$cum_group_gender_max * 100

results_group <- subset(data, select = c(age_group, gender, cum_group, cum_group_max, percent_group, cum_group_gender, cum_group_gender_max, percent_group_gender))
results_group <- aggregate(.~ gender + age_group, results_group, function(x) x[1])

for(j in levels(data$gender)){
  data$cum_gender[data$gender == j] <- sum(data$points[data$gender == j])
  data$cum_gender_max[data$gender == j] <- sum(apply(data[data$gender == j, 4:9], 1, function(x) sum(is.na(x) == F)) * 10)
}
data$percent_gender <- data$cum_gender / data$cum_gender_max * 100


# lapply(data, function(x) gsub(x, 'не имеет', NA))
# # data <- apply(data, 1, gsub, pattern = 'не имеет', replacement = NA)
# data$children[1]

# Activity by age groups
for (i in levels(data$age_group)){
  data$work_group[data$age_group == i] <- sum(data$work[data$age_group == i], na.rm = T)/sum(data$points[data$age_group == i], na.rm = T)*100
  data$friends_group[data$age_group == i] <- sum(data$friends[data$age_group == i], na.rm = T)/sum(data$points[data$age_group == i], na.rm = T)*100
  data$children_group[data$age_group == i] <- sum(data$children[data$age_group == i], na.rm = T)/sum(data$points[data$age_group == i], na.rm = T)*100
  data$spouse_group[data$age_group == i] <- sum(data$spouse[data$age_group == i], na.rm = T)/sum(data$points[data$age_group == i], na.rm = T)*100
  data$parents_group[data$age_group == i] <- sum(data$parents[data$age_group == i], na.rm = T)/sum(data$points[data$age_group == i], na.rm = T)*100
  data$acquaintances_group[data$age_group == i] <- sum(data$acquaintances[data$age_group == i], na.rm = T)/sum(data$points[data$age_group == i], na.rm = T)*100
}
# data$percent_group <- data$cum_group / data$cum_group_max * 100

# Activity by gender-age groups
for (i in levels(data$age_group)){
  for(j in levels(data$gender)){
    data$work_group_gender[data$age_group == i & data$gender == j] <-
      sum(data$work[data$age_group == i & data$gender == j], na.rm = T)/sum(data$points[data$age_group == i & data$gender == j])*100
    data$friends_group_gender[data$age_group == i & data$gender == j] <-
      sum(data$friends[data$age_group == i & data$gender == j], na.rm = T)/sum(data$points[data$age_group == i & data$gender == j])*100
    data$children_group_gender[data$age_group == i & data$gender == j] <-
      sum(data$children[data$age_group == i & data$gender == j], na.rm = T)/sum(data$points[data$age_group == i & data$gender == j])*100
    data$spouse_group_gender[data$age_group == i & data$gender == j] <-
      sum(data$spouse[data$age_group == i & data$gender == j], na.rm = T)/sum(data$points[data$age_group == i & data$gender == j])*100
    data$parents_group_gender[data$age_group == i & data$gender == j] <-
      sum(data$parents[data$age_group == i & data$gender == j], na.rm = T)/sum(data$points[data$age_group == i & data$gender == j])*100
    data$acquaintances_group_gender[data$age_group == i & data$gender == j] <-
      sum(data$acquaintances[data$age_group == i & data$gender == j], na.rm = T)/sum(data$points[data$age_group == i & data$gender == j])*100
  }
}

results_activity <- subset(data, select = c(age_group, gender, work_group, friends_group, children_group, spouse_group, parents_group, acquaintances_group,
                                            work_group_gender, friends_group_gender, children_group_gender, spouse_group_gender, parents_group_gender, acquaintances_group_gender))
results_activity <- aggregate(.~ gender + age_group, results_activity, function(x) x[1])


# ANOVA test

ggplot(data, aes(age_group, percent))+
  geom_point(aes(color = age_group))

ggplot(data, aes(age_group, percent))+
  geom_boxplot(aes(fill = age_group))+
  geom_point()

# stat
anova_test <- kruskal.test(percent ~ age_group, data)
anova_test


ggplot(data, aes(age_group, percent))+
  # geom_boxplot(aes(fill = age_group))+
  geom_point(aes(color = age_group))+
  facet_grid(~gender)


# Wilcoxon test (non-parametric analog of t.test) - for non-gaussian distribution

hist(data$percent[data$gender == 'муж.'])
shapiro.test(data$percent[data$gender == 'муж.'])
shapiro.test(data$percent[data$gender == 'жен.'])

ggplot(data, aes(gender, percent))+
  geom_boxplot(aes(fill = gender))

wilcox.test(percent ~ gender, data)

# Activity plots


ggplot(data, aes(work))+
  geom_histogram(aes(fill = gender), position = position_dodge())+
  facet_grid(~gender)

# data1 <- results_activity
# long <- melt(results_activity, id.vars = c('age_group', 'gender', 'work_group', 'friends_group', 'children_group', 'spouse_group', 'parents_group', 'acquaintances_group',
#                                'work_group_gender', 'friends_group_gender', 'children_group_gender', 'spouse_group_gender', 'parents_group_gender', 'acquaintances_group_gender'))

results_activity_1 <- subset(data, select = c(age_group, gender, work_group, friends_group, children_group, spouse_group, parents_group, acquaintances_group))
results_activity_1 <- aggregate(.~ gender + age_group, results_activity_1, function(x) x[1])

long_result_activity <- melt(results_activity_1, id.vars=c('gender', 'age_group'))

ggplot(long_result_activity, aes(value))+
  geom_histogram(aes(fill = variable))+
  facet_wrap(~gender, scales = 'free')

ggplot(long_result_activity, aes(x = factor(1), fill= variable))+
  geom_bar(aes(fill = variable), position='stack')+
  facet_wrap(~age_group, scales = 'free')

