### SCRIPT FOR FINAL EXAM GRADE CUT-OFF MEETINGS AND WHATNOT ###

library(kableExtra)
library(formattable)
library(tidyverse)
library(viridisLite)

## Set directory
setwd("/Users/lucydelaney/220bb-calculator/")


### IMPORT GRADE FILE ###
blackboard.raw <- read_csv("R/all-class-grades-postfinal.csv", col_names = T, na = c("", " ", "NA"), 
                   trim_ws = T, skip_empty_rows = T, n_max = 16) 


### PRUNE UGLY GRADE FILE ###
edit.raw.grades <- function(mydata = blackboard.raw){
  
  ## Get a group of students' names and TAs
  names <- mydata %>%
    select(last = contains("Last Name"), first = contains("First Name"), 
           ta = contains("TA Name")) %>%
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
    select(Exam.1 = contains("Exam 1"), 
           Exam.2 = contains("EXAM 2"), 
           Final.A = contains("Final Exam Part A"), 
           Final.B = contains("FINAL Part B")) %>%
    rowwise() %>%
    mutate(Exam.total = sum(Exam.1, Exam.2, na.rm = T), FinExam.Total = sum(Final.A, Final.B, na.rm = T))
  
  ## Select and rename iClicker scores
  iClick <- mydata %>%
    select(iC.1 = contains("iClicker: First Half"), 
           iC.2 = contains("iClicker: Second Half"))
  
  ## Grab bonus columns
  bonus <- mydata %>%
    select(bonus = contains("Bonus Evals"))
  
  ## Combine all columns for final grade calculations
  final <- bind_cols(names, Quiz.total, new.hw, exams, iClick, bonus) %>%
    arrange(ta)
  
  return(final)
  
}

## Get a tidy df of all data used to calculate final grades
prune.grades <- edit.raw.grades() 


### MAKE GRADES ###
make.prelim.grades <- function(prunedgrades = prune.grades){
  
  ## Edit here the class totals so far:
    #10 points per quiz, 100 points per exam, 7 points per HW, 25pts iClicker, 200pts final exam
    #Right now, 10 quizzes, 2 exams + final, 7HWs, and 50 iClick (bonus is bonus)
  total.so.far <- (10 * 10) + (2 * 200) + (7*10) + 50
  
  ## List of students' names and TA
  name <- prunedgrades %>%
    select(name, ta)
  
  ## Remove points from the total for exam or iClicker exemptions
  fix.for.calculations <- prunedgrades %>%
    mutate(total.points = total.so.far) %>%
    mutate(pts.plus.examexemp = case_when((is.na(Exam.1) | is.na(Exam.2)) ~ total.points - 100, 
                                          (!is.na(Exam.1) & !is.na(Exam.2)) ~ total.points)) %>%
    select(name, ta, Quiz.total, HW.totals, Exam.total:bonus, pts.plus.examexemp) %>%
    mutate(totalpts = case_when(is.na(iC.2) ~ pts.plus.examexemp - 25, !is.na(iC.2) ~ pts.plus.examexemp)) %>%
    select(-pts.plus.examexemp)
  
  ## Remove low second half iClicker scores from calculations (automatic exemption)
  fix.for.calculations$iC.2[fix.for.calculations$iC.2<11] <- NA
  
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
grade.summary <- make.prelim.grades() %>%
  select(name, ta, total:grade) %>%
  filter(total != 0) %>%
  filter(!is.na(ta))

## Add raw data and summary data for complete gradesheet pre-final
my.grades <- grade.summary %>%
  left_join(prune.grades)

## Function to determine whether or not a student is redeemed by their final grade
## and summarizes all results
determine.redemption <- function(mydata = my.grades){
  assign.grades <- my.grades %>%
    select(name:grade, FinExam.Total) %>%
    mutate(FinExamPerc = (FinExam.Total/200)*100) %>%
    select(-FinExam.Total) %>%
    setNames(., c("Name", "TA", "TotalPts", "TotalPerc", "Grade", "FinExamPerc")) %>%
    mutate(FinExamGrade = case_when(FinExamPerc < 50.5 ~ "F", 
                                    (50.5 <= FinExamPerc & FinExamPerc < 59.5) ~ "D", 
                                    (59.5 <= FinExamPerc & FinExamPerc < 74.5) ~ "C", 
                                    (74.5 <= FinExamPerc & FinExamPerc < 84.5) ~ "B",
                                    (84.5 <= FinExamPerc & FinExamPerc <= 100) ~ "A")) %>%
    mutate(Redemption = ifelse(FinExamPerc > TotalPerc, yes = "Redeemed?", no = "Upheld")) %>%
    mutate(Name, Grade, TotalPerc = round(TotalPerc, 2), Redemption, FinExamPerc = round(FinExamPerc, 2), 
           FinExamGrade, TA) %>%
    select(-TotalPts)
  
  ## If final exam percentage is higher than course grade percentage, and the grade is not the same,
  ## give final exam grade -- "Redeemed"
  final.grades <- assign.grades %>% 
    filter(Redemption == "Redeemed?") %>%
    mutate(Redemption = ifelse(FinExamGrade==Grade, yes = "Upheld", no = "Redeemed")) %>%
    right_join(assign.grades, by = c("Name", "Grade", "TotalPerc", "FinExamPerc", "FinExamGrade", "TA")) %>%
    mutate(Redemption = ifelse(is.na(Redemption.x), yes = Redemption.y, no = Redemption.x)) %>%
    mutate(NewCourseGrade=ifelse(test = Redemption=="Redeemed", yes = FinExamGrade, no = Grade)) %>%
    select(Name, TotalPerc, NewCourseGrade, CurrentGrade = Grade, Redemption, FinExamPerc, FinExamGrade, TA) %>%
    arrange(Redemption, desc(TotalPerc), Name) %>%
    filter(!is.na(TA))
  
  return(final.grades)
  
}

## Summary df of grades post-final, including redemption
final.grade.summary <- determine.redemption()

## Add to complete gradebook for finalized speadsheet with all data
final.spreadsheet <- final.grade.summary %>%
  select(Name, TA, FinalGrade = NewCourseGrade, Redemption, FinExamGrade) %>%
  right_join(my.grades, by = c("Name" = "name", "TA" = "ta")) %>%
  select(Name:FinalGrade, Redemption, Percent = perc, TotalPts = total, FinExamGrade,
         OriginalGrade = grade, Quiz.1:bonus)

## Write files for distribution
#write_csv(final.grade.summary, "R/grade-summary.csv")
#write_csv(final.spreadsheet, "R/gradebook.csv")