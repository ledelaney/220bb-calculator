### SCRIPT FOR FINAL EXAM GRADE CUT-OFF MEETINGS AND WHATNOT ###

library(kableExtra)
library(formattable)
library(tidyverse)
library(viridisLite)

## Set directory
setwd("/Users/lucydelaney/220bb-calculator/")


### IMPORT GRADE FILE ###
blackboard.raw <- read_csv("R/all-class-grades-prefinal.csv", col_names = T, na = c("", " ", "NA"), 
                   trim_ws = T, skip_empty_rows = T, n_max = 16) 


### PRUNE UGLY GRADE FILE ###
edit.raw.grades <- function(mydata = blackboard.raw){
  
  ## Get a group of students' names and TAs
  names <- mydata %>%
    select(last = "Last Name", first = "First Name", 
           ta = "TA Name [Total Pts: 100 Text] |1720399") %>%
    mutate(name = paste0(last, "_", first)) %>%
    select(name, ta)
  
  ## Rename quiz rows, replace NA with average
  quiz <- mydata %>%
    select(contains("quiz")) %>%
    select(1:10) %>%
    setNames(., c("Quiz.1", "Quiz.2", "Quiz.3", "Quiz.4", "Quiz.5", "Quiz.6", 
                  "Quiz.7", "Quiz.8", "Quiz.9", "Quiz.10")) %>%
    mutate(avg = rowMeans(., na.rm = TRUE)) %>%
    rowwise() %>%
    mutate_at(vars(starts_with("Quiz")), funs(replace(., is.na(.), avg))) %>%
    select(-avg) 
  
  ## Sum all rows for final quiz totals
  Quiz.total <- tibble(rowSums(quiz)) %>%
    setNames(., "Quiz.total") %>%
    bind_cols(quiz, .)
  
  ## Rename HW rows, replace NA with average
  hw <- mydata %>%
    select(contains("HW")) %>%
    setNames(., c("HW.1", "HW.2", "HW.3", "HW.4", "HW.5", 
                  "HW.6", "HW.7", "HW.8", "HW.9", "HW.10", "HW.11")) %>%
    mutate(avg = rowMeans(., na.rm = TRUE)) %>%
    rowwise() %>%
    mutate_at(vars(starts_with("HW")), funs(replace(., is.na(.), avg))) %>%
    select(-avg) 
  
  ## Choose the 10 best homeworks
  HW.sums <- tibble(rowSums(hw)) %>%
    setNames(., "HW.sum") 
  HW.min <- tibble(apply(hw, 1, FUN = min)) %>%
    setNames(., "HW.min")
  
  ## Sum all homework values
  new.hw <- bind_cols(hw, HW.sums, HW.min) %>%
    mutate(HW.totals = HW.sum - HW.min) %>%
    select(-HW.sum, -HW.min)
  
  ## Select and rename exams
  exams <- mydata %>%
    select(Exam.1 = "Exam 1 [Total Pts: 100 Score] |1720428", 
           Exam.2 = "EXAM 2 [Total Pts: 100 Score] |1802569", 
           Final.A = "Final Exam Part A [Total Pts: 100 Score] |1808678", 
           Final.B = "BIOS220 FINAL Part B [Total Pts: 100 Score] |1807167")
  
  ## Select and rename iClicker scores
  iClick <- mydata %>%
    select(iC.1 = "iClicker: First Half [Total Pts: 25 Score] |1742843", 
           iC.2 = "iClicker: Second Half [Total Pts: 25 Score] |1742844")
  
  ## Grab bonus columns
  bonus <- mydata %>%
    select(bonus = "Bonus Evals First Half [Total Pts: 0 Score] |1758308")
  
  ## Combine all columns for final grade calculations
  final <- bind_cols(names, Quiz.total, new.hw, exams, iClick, bonus) %>%
    arrange(ta)
  
  return(final)
  
}

## Get a df of all data used to calculate final grades
prune.grades <- edit.raw.grades() 


### MAKE GRADES ###

## Will have to deal with iClicker exemptions for the second half of the course
  #use mutate(iCl.2 = case_when(is.na(iCl.2) ~ 25, !is.na(iCl.2) ~ iCl.2)

make.grades <- function(prunedgrades = prune.grades){
  
  ## Edit here the class totals so far
    #10 points per quiz, 100 points per exam, 7 points per HW, 25pts iClicker
    #Right now, 10 quizzes, 2 exams, 7HWs, and 25 iClick (bonus is bonus)
  total.so.far <- (10 * 10) + (2 * 100) + (7*10) + 25
  
  ## List of students' names and TA
  name <- prunedgrades %>%
    select(name, ta)
  
  ## Pull out students that were exempted for one of the exams
  exam.exemptions <- prunedgrades %>%
    filter(is.na(Exam.1) | is.na(Exam.2)) %>%
    mutate(total.points = total.so.far - 100) %>%
    select(name, ta, total.points)
  
  ## Add back to full df and fix total course points so you may divide
  ## and calculate percentage for each student
  fix.for.calculations <- prunedgrades %>%
    select(name, ta, Quiz.total, HW.totals, Exam.1:bonus) %>%
    mutate(total.points = total.so.far) %>%
    left_join(exam.exemptions, by = c("name", "ta")) %>%
    mutate(totalpts = case_when(!is.na(total.points.y) ~ total.points.y, is.na(total.points.y) ~ total.points.x)) %>%
    select(name, ta, Quiz.total:bonus, totalpts)
  
  ## Use percentage to assign letter grades
  adding.data <- fix.for.calculations %>%
    select(Quiz.total:bonus) %>%
    mutate(total = rowSums(., na.rm = T)) %>%
    left_join(fix.for.calculations) %>%
    mutate(perc = (total/totalpts)*100) %>%
    select(name, ta, total, perc) %>%
    mutate(grade = case_when(perc < 50.5 ~ "F", (50.5 <= perc & perc < 59.5) ~ "D", 
                             (59.5 <= perc & perc < 74.5) ~ "C", (74.5 <= perc & perc < 84.5) ~ "B", 
                             (84.5 <= perc & perc <= 100) ~ "A"))
  
  return(adding.data)
  
}

## Summary df of calculated grades
grade.summary <- make.grades() %>%
  select(name, ta, total:grade)

## Add raw data and summary data for complete gradesheet
my.grades <- grade.summary %>%
  left_join(prune.grades)
  

## Download a csv sorted by TA of all relevant grading columns
write_csv(x = my.grades, path = "R/my-gradesheet.csv")

final.grades <- my.grades %>%
  select(name:grade, Final.A, Final.B) %>%
  mutate(FinalExamPerc = Final.A + Final.B) %>%
  select(-Final.A, -Final.B)

## Add a fake final exam score to test dummy table, fix names, 
## assign final exam letter grades
dummy.grades <- grade.summary %>%
  add_column(runif(n = length(grade.summary$total), min = 40, max = 85)) %>%
  ## Here you will add do assign.grades <- final.grades %>%
  setNames(., c("Name", "TA", "TotalPts", "TotalPerc", "Grade", "FinExamPerc")) %>%
  mutate(FinExamGrade = case_when(FinExamPerc < 50.5 ~ "F", 
                                  (50.5 <= FinExamPerc & FinExamPerc < 59.5) ~ "D", 
                                  (59.5 <= FinExamPerc & FinExamPerc < 74.5) ~ "C", 
                                  (74.5 <= FinExamPerc & FinExamPerc < 84.5) ~ "B",
                                  (84.5 <= FinExamPerc & FinExamPerc <= 100) ~ "A")) %>%
  mutate(Redemption = ifelse(FinExamPerc > TotalPerc, yes = "Redeemed?", no = "Unredeemed")) %>%
  mutate(Name, Grade, TotalPerc = round(TotalPerc, 2), Redemption, FinExamPerc = round(FinExamPerc, 2), 
         FinExamGrade, TA) %>%
  select(-TotalPts)

## If final exam percentage is higher than course grade percentage, and the grade is not the same,
## give final exam grade -- "Redeemed"
  #df for use with RMD file "grade-totals"
final.grades <- dummy.grades %>%
  filter(Redemption == "Redeemed?") %>%
  mutate(Redemption = ifelse(FinExamGrade==Grade, yes = "Unredeemed", no = "Redeemed")) %>%
  right_join(dummy.grades, by = c("Name", "Grade", "TotalPerc", "FinExamPerc", "FinExamGrade", "TA")) %>%
  mutate(Redemption = ifelse(is.na(Redemption.x), yes = Redemption.y, no = Redemption.x)) %>%
  mutate(NewCourseGrade=ifelse(test = Redemption=="Redeemed", yes = FinExamGrade, no = Grade)) %>%
  select(Name, TotalPerc, NewCourseGrade, CurrentGrade = Grade, Redemption, FinExamPerc, FinExamGrade, TA) %>%
  arrange(Redemption, desc(TotalPerc), Name) %>%
  filter(!is.na(TA))

