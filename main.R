#### Skript zur Hausarbeit
#### "Umfragestudie über die Wahrnehmungen und Einstellungen von 
#### Muttersprachler*innen zu der H- und L-Variante von Farsi: 
#### Eine ideologische Herangehensweise an Diglossie"
#### Seminar: Multilingualismus: Typen, Trends und Theorie WiSe 2018/19
#### Leitung: Dr. Sonja Eisenbeiß
#### Institut für Linguistik, Abteilung Allgemeine Sprachwissenschaft,
#### Universität zu Köln
#### vorgelegt von: Farimah Karimi
#### fkarimi@smail.uni-koeln.de
#### Köln, 11.05.2019
#######################################################################

library(tidyverse)

#### IMPORT ####

import <- read.csv2("Daten/results_cleaned.csv",header=T,strip.white=TRUE)

# inspect data
nrow(import) # 85 cases
ncol(import) # 37 variables
str(import)
colnames(import)


#### PREPROCESSING ####

# translate values
import$access_text1[import$access_text1 == "بله"] <- "y"
import$access_text1[import$access_text1 == "خیر"] <- "n"
import$access_text1[import$access_text1 == "نمیدانم"] <- "dk"
import$access_text2[import$access_text2 == "بله"] <- "y"
import$access_text2[import$access_text2 == "خیر"] <- "n"
import$access_text2[import$access_text2 == "نمیدانم"] <- "dk"

import$age[import$age == "۱۸-۲۴"] <- "18-24"
import$age[import$age == "۲۵-۳۴"] <- "25-34"
import$age[import$age == "۳۵-۴۴"] <- "35-44"
import$age[import$age == "۴۵-۵۹"] <- "45-59"
import$age[import$age == "۶۰ یا بالاتر"] <-"60+"

import$gender[import$gender == "مرد"] <- "male"
import$gender[import$gender == "زن"] <- "female"

import$education[import$education == "زیر دیپلم"] <- "below_HSD"
import$education[import$education == "دیپلم"] <- "HSD"
import$education[import$education == "کارشناسی"] <- "bachelor"
import$education[import$education == "کارشناسی ارشد"] <- "master"
import$education[import$education == "دکترا و بالاتر"] <- "phd_or_higher"

import$persian_L1[import$persian_L1 == "بله، فقط فارسی را یاد گرفتم."] <- "L1_mono"
import$persian_L1[import$persian_L1 == "بله، فارسی و یک زبان/گویش دیگر (مانند ترکی یا رشتی) را تقریباً همزمان یاد گرفتم."] <- "L1_multi"
import$persian_L1[import$persian_L1 == "خیر، فارسی را بعد از زبان/گویش دیگری (مانند ترکی یا رشتی) یاد گرفتم (مثلاً از زمان ورود به مدرسه)."] <- "L2"

import$other_languages[import$other_languages == "بله"] <- "y"
import$other_languages[import$other_languages == "خیر"] <- "n"

# remove irrelevant column
dataset <- select(import, -Zeitstempel)

# inspect data again
ncol(dataset)
str(dataset)
summary(dataset)


##### ANALYSIS #####

#general statistics
gen_stats <- function(x){
  tbl <- table(x)
  res <- cbind(tbl, round(prop.table(tbl)*100, 2))
  colnames(res) <- c('count', 'percentage')
  res
}

do.call(rbind, lapply(dataset[32:36], gen_stats))

# remove L2-speakers
L2 <- filter(dataset, persian_L1 == "L2")
summary(L2) # 5 L2-speakers

L1 <- filter(dataset, persian_L1 != "L2")
summary(L1) # 80 L1-speakers

do.call(rbind, lapply(L1[32:36], gen_stats))

get_percentage <- function(x, y){
  y/x*100
}


#### PART 1 (1-4) ####

#### QUESTIONS 1-4 ####

# percentages for columns 1-4
p_2langs <- get_percentage(5, mean(L1$two_languages)) # 34.75
p_2dias <- get_percentage(5, mean(L1$two_dialects)) # 52.5
p_2levs <- get_percentage(5, mean(L1$two_levels)) # 77.75
p_no_diff <- get_percentage(5, mean(L1$no_difference)) # 32

p_categorization_df <- data.frame(categorization=c("two_languages","two_dialects","two_levels", "no_difference"),
                 agreement_in_percentage=c(p_2langs, p_2dias, p_2levs, p_no_diff))

p_categorization_df$categorization <- factor(p_categorization_df$categorization, levels = c("two_languages", "two_dialects", "two_levels", "no_difference"))

# barchart for q. 1-4
ggplot(p_categorization_df, aes(categorization, agreement_in_percentage, fill=categorization)) + 
  geom_bar(stat = "identity") +
  xlab("Kategorisierung") +
  ylab("Zustimmung in %") +
  scale_fill_discrete(name = "CSV-Spaltennamen") +
  scale_x_discrete(labels=c("Sprachen", "Dialekte", "Ebenen", "kein Unterschied")) +
  geom_text(aes(label=agreement_in_percentage, vjust=2))

#### QUESTIONS 5-6 ####

get_count <- function(substr, col){
  sum(grepl(substr, col, fixed = TRUE))
}

names_t1 <- L1$names_text1
names_t2 <- L1$names_text2

names <- c("فارسی عامیانه",
             "فارسی گفتاری", 
             "فارسی محاوره",
             "فارسی کتابی", # minus 1 because of "فارسی محاوره ای نسل جوان"
             "فارسی رسمی", 
             "فارسی سنتی", 
             "فارسی ادبی", 
             "فارسی نوشتاری")

names_count_df <- data.frame(name=names,
                 count_t1=c(get_count(names[1], names_t1), 
                            get_count(names[2], names_t1),
                            get_count(names[3], names_t1),
                            get_count(names[4], names_t1),
                            get_count(names[5], names_t1),
                            get_count(names[6], names_t1),
                            get_count(names[7], names_t1),
                            get_count(names[8], names_t1)),
                 count_t2=c(get_count(names[1], names_t2), 
                            get_count(names[2], names_t2),
                            get_count(names[3], names_t2),
                            get_count(names[4], names_t2),
                            get_count(names[5], names_t2),
                            get_count(names[6], names_t2),
                            get_count(names[7], names_t2),
                            get_count(names[8], names_t2)))
names_count_df


#### PART 2 (7-15) ####

#### QUESTIONS 7-8 ####

p_formal_ed_t1 <- get_percentage(5, mean(L1$formal_education_text1)) # 68.5
p_formal_ed_t2 <- get_percentage(5, mean(L1$formal_education_text2)) # 30.25

#### QUESTIONS 9-11 ####

# p_own_comp_t1 <- get_percentage(10, mean(L1$own_competence_text1)) # 89.357
# p_own_comp_t2 <- get_percentage(10, mean(L1$own_competence_text2)) # 88.5
# p_fa_sufficient <- get_percentage(10, mean(L1$persian_sufficiency)) # 86.25

own_comp_t1_mean <- mean(L1$own_competence_text1) # 8,94 / 10
own_comp_t2_mean <- mean(L1$own_competence_text2) # 8,85 / 10

# cor_comp_t1_fa_sufficient <- cor.test(L1$own_competence_text1, L1$persian_sufficiency, method="pearson")
# cor_comp_t2_fa_sufficient <- cor.test(L1$own_competence_text2, L1$persian_sufficiency, method="pearson")
# cor_comp_t1_fa_sufficient$p.value # 8.663044e-06
# cor_comp_t2_fa_sufficient$p.value # 0.00977668 
# competence in the variety of t1 correlates more with sufficiency in Persian
# than competence in the v. of t2

#### QUESTIONS 12-13 ####

p_necess_t1 <- get_percentage(5, mean(L1$competence_necessity_text1)) # 90
p_necess_t2 <- get_percentage(5, mean(L1$competence_necessity_text2)) # 65.25
mean_necess_t1 <- mean(L1$competence_necessity_text1)
mean_necess_t2 <- mean(L1$competence_necessity_text2)

#### QUESTIONS 14-15 ####

p_access <- do.call(rbind, lapply(L1[14:15], gen_stats))

#### PART 3 (16-31) ####

#### QUESTION 16 ####

# 1=situation; 5=identity
mean(L1$situation_or_identity) # 2.975

### QUESTION 17-31 ####

p_ruleless_t1 <- get_percentage(5, mean(L1$ruleless_text1)) # 29.25
p_ruleless_t2 <- get_percentage(5, mean(L1$ruleless_text2)) # 70.5

p_prestige_t1 <- get_percentage(5, mean(L1$prestige_text1)) # 73.5
p_prestige_t2 <- get_percentage(5, mean(L1$prestige_text2)) # 38.5

p_incr_usage_t1 <- get_percentage(5, mean(L1$increased_usage_text1)) # 52.5
p_incr_usage_t2 <- get_percentage(5, mean(L1$increased_usage_text2)) # 71

p_pred_repl_t1 <- get_percentage(5, mean(L1$prediction_replacement_text1)) # 36
p_pred_repl_t2 <- get_percentage(5, mean(L1$prediction_replacement_text2)) # 68.5

p_endangered_t1 <- get_percentage(5, mean(L1$endangered_text1)) # 63.5
p_endangered_t2 <- get_percentage(5, mean(L1$endangered_text2)) # 30.25

p_protect_t1 <- get_percentage(5, mean(L1$protection_text1)) # 90.75
p_protect_t2 <- get_percentage(5, mean(L1$protection_text2)) # 51.25

p_repl_useful_t1 <- get_percentage(5, mean(L1$replacement_useful_text1)) # 57
p_repl_useful_t2 <- get_percentage(5, mean(L1$replacement_useful_text2)) # 30

p_var_problem <- get_percentage(5, mean(L1$variation_problem)) # 46.5


#### PROFILE (32-36) ####

# over_60 <- filter(dataset, age == "60+")
# L1_nums <- select_if(L1, is.numeric)
# L1_means_by_age <- aggregate(L1_nums, by=list(L1$age), mean)
# L1_means_by_gender <- aggregate(L1_nums, by=list(L1$gender), mean)
# ggplot(L1_means_by_gender, aes(Group.1, ruleless_text1)) + geom_bar(stat = "identity")