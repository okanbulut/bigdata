
library("data.table")
library("ggplot2")
library("GGally")
library("ggExtra")
library("ggalluvial")
library("plotly")

pisa <- fread("pisa2015.csv", header = TRUE, sep=",", fill=TRUE)

#' Convert a dichtomous item (yes/no) to numeric scoring
#' @param x a character vector containing "Yes" and "No" responses.
bin.to.num <- function(x){
  if (is.na(x)) NA
  else if (x == "Yes") 1L
  else if (x == "No") 0L
}

pisa[, `:=`
     (female = ifelse(ST004D01T == "Female", 1, 0),
       sex = ST004D01T,
       
       # At my house we have ...
       math = rowMeans(pisa[, c(paste0("PV", 1:10, "MATH"))], na.rm = TRUE),
       reading = rowMeans(pisa[, c(paste0("PV", 1:10, "READ"))], na.rm = TRUE),
       science = rowMeans(pisa[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE),
       desk = sapply(ST011Q01TA, bin.to.num),
       own.room = sapply(ST011Q02TA, bin.to.num),
       quiet.study = sapply(ST011Q03TA, bin.to.num),
       computer = sapply(ST011Q04TA, bin.to.num),
       software = sapply(ST011Q05TA, bin.to.num),
       internet = sapply(ST011Q06TA, bin.to.num),
       lit = sapply(ST011Q07TA, bin.to.num),
       poetry = sapply(ST011Q08TA, bin.to.num),
       art = sapply(ST011Q09TA, bin.to.num),
       book.sch = sapply(ST011Q10TA, bin.to.num),
       tech.book = sapply(ST011Q11TA, bin.to.num),
       dict = sapply(ST011Q12TA, bin.to.num),
       art.book = sapply(ST011Q16NA, bin.to.num))]


# Select some countries for the exercises
country <- c("United States", "Canada", "Mexico", "B-S-J-G (China)", "Japan",
             "Korea", "Germany", "Italy", "France", "Brazil", "Colombia", "Uruguay",
             "Australia", "New Zealand", "Jordan", "Israel", "Lebanon")

dat <- pisa[CNT %in% country,
            .(CNT, OECD, CNTSTUID, W_FSTUWT, sex, female,
              ST001D01T, computer, software, internet,
              ST011Q05TA, ST071Q02NA, ST071Q01NA, ST123Q02NA,
              ST082Q01NA, ST119Q01NA, ST119Q05NA, ANXTEST,
              COOPERATE, BELONG,  EMOSUPS, HOMESCH, ENTUSE,
              ICTHOME, ICTSCH, WEALTH, PARED, TMINS, ESCS,
              TEACHSUP, TDTEACH, IBTEACH, SCIEEFF,
              math, reading, science)
            ]


# Let's create additional variables that we will use for visualizations
dat <- dat[, `:=` (
  # New grade variable
  grade = (as.numeric(sapply(ST001D01T, function(x) {
    if(x=="Grade 7") "7"
    else if (x=="Grade 8") "8"
    else if (x=="Grade 9") "9"
    else if (x=="Grade 10") "10"
    else if (x=="Grade 11") "11"
    else if (x=="Grade 12") "12"
    else if (x=="Grade 13") NA_character_
    else if (x=="Ungraded") NA_character_}))),
  # Total learning time as hours
  learning = round(TMINS/60, 0),
  # Regions for selected countries
  Region = (sapply(CNT, function(x) {
    if(x %in% c("Canada", "United States", "Mexico")) "N. America"
    else if (x %in% c("Colombia", "Brazil", "Uruguay")) "S. America"
    else if (x %in% c("Japan", "B-S-J-G (China)", "Korea")) "Asia"
    else if (x %in% c("Germany", "Italy", "France")) "Europe"
    else if (x %in% c("Australia", "New Zealand")) "Australia"
    else if (x %in% c("Israel", "Jordan", "Lebanon")) "Middle-East"
  }))
)]


## 5.2.1 Exercise ----
ggplot(data = dat,
       mapping = aes(x = reorder(CNT, math), y = math, fill = Region)) +
  geom_point(aes(color = Region)) +
  labs(x=NULL, y="Math Scores") +
  coord_flip() +
  geom_hline(yintercept = 490, linetype="dashed", color = "red", size = 1) +
  theme_bw()

ggplot(data = dat,
       mapping = aes(x = reorder(CNT, math), y = math, fill = Region)) +
  geom_violin(aes(color = Region)) +
  labs(x=NULL, y="Math Scores") +
  coord_flip() +
  geom_hline(yintercept = 490, linetype="dashed", color = "red", size = 1) +
  theme_bw()


## 5.3.1 Exercise ----
dat_small <- dat[,.SD[sample(.N, min(500,.N))], by = Region]

ggplot(data = dat_small,
       mapping = aes(x = ESCS, y = math)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "SES", y = "Math Scores") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_grid(sex ~ Region)


## 5.6.1 Exercise ----
dat_alluvial2 <- dat[,.(Freq = .N),by = c("Region", "sex", "ST119Q01NA")
                     ][,ST119Q01NA := as.factor(ifelse(ST119Q01NA == "", "Missing", ST119Q01NA))]

levels(dat_alluvial2$ST119Q01NA) <- c("Strongly disagree", "Disagree", "Agree",
                                      "Strongly agree", "Missing")


ggplot(data = dat_alluvial2,
       aes(axis1 = Region, axis2 = ST119Q01NA, y = Freq)) +
  scale_x_discrete(limits = c("Region", "Students' wanting\ntop grades"),
                   expand = c(.1, .05)) +
  geom_alluvium(aes(fill = sex)) +
  geom_stratum() +
  geom_text(stat = "stratum", label.strata = TRUE) +
  labs(x = "Demographics", y = "Frequency", fill = "Gender") +
  theme_bw()


## 5.7.1 Exercise ----
ex4.1 <- ggplot(data = dat,
                mapping = aes(x = science, fill = Region)) +
  geom_density(alpha = 0.5) +
  labs(x = "Science Scores", y = "Count",
       title = "Science Scores by Gender and Region") +
  facet_grid(~ sex) +
  theme_bw()

ggplotly(ex4.1)

ex4.2 <- ggplot(data = dat,
                mapping = aes(x = science, fill = Region)) +
  geom_density(alpha = 0.8) +
  labs(x = "Science Scores", y = "Count",
       title = "Science Scores by Gender and Region") +
  facet_grid(~ sex) +
  theme_bw()

ggplotly(ex4.2)








