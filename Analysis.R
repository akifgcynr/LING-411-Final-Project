library(tidyverse)
library(broom)
library(magrittr)
library(ggrepel)
library(effsize)

eng_lexdec_naming <- read_csv("english_lexdec_naming.csv")


## Data Analysis and Hypothesis Forming-----------------------------------------

### Checking for Duplicates and NA's

sum(is.na(eng_lexdec_naming)) #0
length(unique(eng_lexdec_naming$Word)) 

# Expected 2284 unique words, got 2197
# 87 values missing


# Checking for duplicates
duplicates <- eng_lexdec_naming %>%
  count(Word) %>%
  filter(n > 2)

arm <- eng_lexdec_naming %>%
  filter(Word == 'arm')

cope <- eng_lexdec_naming %>%
  filter(Word == 'cope')

#Inspection of arm and cope shows that there are duplicate rows

# Removal of duplicates
# Should match 2197 unique words with 4394 rows
eng_lexdec_naming %<>%
  distinct()

### Data Exploration-------------------------------------------------------------

# frequency distribution plot
mean_freq <- mean(eng_lexdec_naming$WrittenFrequency)
sd_freq <- sd(eng_lexdec_naming$WrittenFrequency)

freq <- ggplot(data=eng_lexdec_naming, aes(x=WrittenFrequency)) +
  geom_histogram(bins=200, color="orange", fill='gold') +
  geom_vline(xintercept=mean_freq, linetype='dashed') +
  labs(title='Distribution of Written Frequency',
       subtitle="mean: 977.41, sd: 3907.79") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold")) 
#freq #zipfean distribution


# family size distribution plot
mean_family <- mean(eng_lexdec_naming$FamilySize)
sd_family <- sd(eng_lexdec_naming$FamilySize)
#binwidth=0.1, boundary=0

families <- ggplot(data=eng_lexdec_naming, aes(x=FamilySize)) +
  geom_histogram(bins=200, color="orange", fill='gold') +
  geom_vline(xintercept=mean(eng_lexdec_naming$FamilySize), linetype='dashed') +
  labs(title='Distribution of Morphological Family Size',
       subtitle="mean: 9.36, sd: 13.89") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))
#families #zipfean distribution


# familiarity distribution plot
mean_familiarity <- mean(eng_lexdec_naming$Familiarity)
sd_familiarity <- sd(eng_lexdec_naming$Familiarity)

familiarity <- ggplot(data=eng_lexdec_naming, aes(x=Familiarity)) +
  geom_histogram(bins=200, color="orange", fill='gold') +
  geom_vline(xintercept=mean_familiarity, linetype='dashed') +
  labs(title='Distribution of Familiarity',
       subtitle="mean: 3.79 sd: 1.15") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))
#familiarity #normal distribution


# Lexical Decision vs. Frequency plot
RTlexdec_freq <- ggplot(data=eng_lexdec_naming, aes(x=log(WrittenFrequency), y=RTlexdec, color=AgeSubject, size=FamilySize)) +
  geom_point(alpha=0.4)+
  geom_vline(xintercept = 5.020491, linetype = "dashed") +
  #geom_label(aes(x=5.020491, y=mean(RTlexdec), label="5.020491"), inherit.aes=FALSE) +
  scale_color_brewer(palette="Dark2") +
  labs(title="Scatterplot of RTlexdec vs. Log(WrittenFrequency)",
       x="Log(Written Frequency)",
       y="Lexical Decision Reaction Time",
       subtitle="Sized by Morphological Family Size") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))


# Lexical Decision vs. Familiarity plot
RTlexdec_famil <- ggplot(data=eng_lexdec_naming, aes(x=log(WrittenFrequency), 
                                                     y=RTlexdec, color=AgeSubject,
                                                     size=FamilySize)) +
  geom_point(alpha=0.4) +
  scale_color_brewer(palette="Dark2") +
  labs(title="Scatterplot of RTlexdec vs. Log(WrittenFrequency)",
       x="Familiarity",
       y="Lexical Decision Reaction Time",
       subtitle="Sized by Morphological Family Size") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))


# Naming vs. Familiarity plot
RTnaming_freq <- ggplot(data=eng_lexdec_naming, aes(x=log(WrittenFrequency),
                                                    y=RTnaming, color=AgeSubject,
                                                    size=FamilySize)) +
  geom_point(alpha=0.4) +
  scale_color_brewer(palette="Dark2") +
  labs(title="Scatterplot of RTnaming vs. Log(WrittenFrequency)",
       x="Log(Written Frequency)",
       y="Naming Reaction Time",
       subtitle="Sized by Morphological Family Size") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))
#Hypothesis: Written Freq and Family Size affect RT


### Alternative Hypothesis------------------------------------------------------

# Lexical decision reaction times are significantly related to frequency ratings
# and morphological family size in both age groups.



## Testing the Hypothesis-------------------------------------------------------

### Transformations ------------------------------------------------------------

df <- eng_lexdec_naming %>%
  select(Word, RTlexdec, WrittenFrequency, FamilySize, AgeSubject) %>%
  mutate(LogFamilySize = log(FamilySize), LogFreq = log(WrittenFrequency),
         AgeSubject = factor(AgeSubject, levels = c("young", "old")))


## LogFreq distribution plot
mean_log_freq <- mean(df$LogFreq)
sd_log_freq <- sd(df$LogFreq)

log_freq <- ggplot(data=df, aes(x=LogFreq)) +
  geom_histogram(bins=200, color="orange", fill='gold') +
  geom_vline(xintercept=mean_log_freq, linetype='dashed') +
  labs(title='Distribution of LogFreq',
       subtitle="mean: 5.02, sd: 1.85") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold")) 


# logFamilySize distribution plot
mean_log_family <- mean(df$LogFamilySize)
sd_log_family <- sd(df$LogFamilySize)

log_families <- ggplot(data=df, aes(x=LogFamilySize)) +
  geom_histogram(bins=200, color="orange", fill='gold') +
  geom_vline(xintercept=mean(df$LogFamilySize), linetype='dashed') +
  labs(title='Distribution of LogFamilySize',
       subtitle="mean: 1.81, sd: 0.82") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))


### Models----------------------------------------------------------------------


m3_2 <- lm(RTlexdec  ~  FamilySize, data=df)
m1 <- lm(RTlexdec  ~  LogFreq, data=df)
m2 <- lm(RTlexdec  ~  AgeSubject, data=df)
m3 <- lm(RTlexdec  ~  LogFamilySize, data=df)
m4 <- lm(RTlexdec  ~  LogFreq + AgeSubject, data=df)
m5 <- lm(RTlexdec  ~  LogFreq + LogFamilySize, data=df)
m6 <- lm(RTlexdec  ~  AgeSubject + LogFamilySize, data=df)
m7 <- lm(RTlexdec  ~  LogFreq + LogFamilySize + AgeSubject + LogFreq, data=df)

# I have done my analyses by using tidy() and glance() below

tidy(m2_2)
glance(m2_2)

# Lexical Decision vs. Log Frequency plot
RTlexdec2_freq <- ggplot(data=df, aes(x=LogFreq, y=RTlexdec, color=AgeSubject, size=LogFamilySize)) +
  geom_point(alpha=0.3)+
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) + 
  scale_size_continuous(name = "Log Family Size") +
  #geom_label(aes(x=5.020491, y=mean(RTlexdec), label="5.020491"), inherit.aes=FALSE) +
  scale_color_brewer(palette="Dark2") +
  labs(title="Scatterplot of RTlexdec vs. LogFreq",
       x="Logarithm of Written Frequency",
       y="Lexical Decision Reaction Time",
       subtitle="Sized by Logarithmic Family Size") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold")) +
  guides(
    color = guide_legend(order = 1), # Color legend
    size = guide_legend(order = 2)   # Size legend (for points only)
  )



## Model Interpretation and Conclutions:

aic <- AIC(m1,m2,m3,m3_2,m4,m5,m6,m7) %>%
  arrange(AIC)

# f-sq and power analysis
r_squared <- summary(m7)$r.squared

# Calculate Cohen's f^2
f_squared <- r_squared / (1 - r_squared)
print(paste("Cohen's f^2:", f_squared))

k <- 4
f2 <- 0.35
v <- 2192
alpha <- 0.05      

# Calculate the required sample size
power_result <- pwr.f2.test(u = k, v = v, f2 = f2, sig.level = alpha)
print(paste("power:", power_result$power))


