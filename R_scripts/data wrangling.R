## 4.2.1 Exercises ----
pisa <- fread("pisa.csv", na.strings = "")

## 4.3.1 Exercises ----
# 1. Subset all the Female students (ST004D01T) in Germany
germany_fem <- pisa[CNTRYID == "Germany" & ST004D01T == "Female"]

# 2. How many female students are there in Germany?
nrow(germany_fem)

# 3. The `.N` function returns the length of a vector/number of rows. Use chaining with the `.N` function to answer Exercise 2.
pisa[CNTRYID == "Germany" & ST004D01T == "Female"
     ][, .N]

## 4.4.1 Exercises ----
# 1. The computer and software variables that were created above ask a student whether they had a computer in their home that they can use for school work (computer) and whether they had educational software in their home (software). Find the proportion of students in the Germany and Uruguay that have a computer in their home or have educational software.
pisa[CNTRYID %in% c("Germany", "Uruguay"),
     .(prop.comp = mean(computer, na.rm = TRUE),
       prop.soft = mean(software, na.rm = TRUE))
     ]

# 2. For just female students, find the proportion of students who have their own room (own.room) or a quiet place to study (quiet.study).
pisa[ST004D01T == "Female",
     .(prop.own.room = mean(own.room, na.rm = TRUE),
       prop.quiet = mean(quiet.study, na.rm = TRUE))
     ]

## 4.5.1 Exercises ----
# 1. Calculate the proportion of students who have art in their home (art) and the average age (AGE) of the students by gender.
pisa[ST004D01T == "Female",
     .(art.home.prop = mean(art, na.rm = TRUE),
       mean.age = mean(AGE, na.rm = TRUE)),
     by = .(sex = ST004D01T)
     ]

# 2. Within a by argument you can discretize a variable to create a grouping variable. Perform a median split for age within the by argument and assess whether there are age difference associated with having your own room (own.room) or a desk (desk).
pisa[,
     .(prop.own.room = mean(own.room, na.rm = TRUE),
       prop.desk = mean(desk, na.rm = TRUE)),
     by = .(AGE > mean(AGE, na.rm = TRUE))
     ]

# To create the science variable:
pisa[,sci_car := as.factor(ifelse(PA032Q03TA == "Yes", 1, -1))]



