#### GRADING PLAY ####

library(tidyverse)

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
    #mutate(Qsum = rowSums(., na.rm = T)) %>%
    #mutate(Qavg = rowMeans(., na.rm = T)) 
  
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
  
  ## Combine all columns for final grade calculations for specific TA
  final <- bind_cols(names, Quiz.total, new.hw, exams, iClick, bonus) #%>% filter(ta == "Kirk")
  
  return(final)
  
}

prune.grades <- edit.raw.grades() 


### MAKE GRADES ###

## Will have to deal with iClicker exemptions for the second half of the course
  # use mutate(iCl.2 = case_when(is.na(iCl.2) ~ 25, !is.na(iCl.2) ~ iCl.2)

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


### WHAT DO I NEED ON THE FINAL? ###

move.up.grade <- function(mydata=grade.summary){
  
  ## Edit here the class totals so far
  #10 points per quiz, 100 points per exam, 7 points per HW, 25pts iClicker
  #Right now, 10 quizzes, 2 exams, 7HWs, and 25 iClick (bonus is bonus)
  
  #all quizzes, hws, first iclick, and exams PLUS FINAL
  total.so.far <- (10 * 10) + (4 * 100) + (7*10) + 25
  exam.score <- 200
  
  ## The meat pipe
  new.data <- mydata %>%
    mutate(final.total = total.so.far) %>% 
    mutate(needA = (0.846 * total.so.far) - total, 
           needB = (0.746 * total.so.far) - total, 
           needC = (0.596 * total.so.far) - total) %>%
    mutate(forA = case_when(needA <= (0.85 * exam.score) ~ needA, 
                            needA > (0.85 * exam.score) ~ (0.85 * exam.score))) %>%
    mutate(forB = case_when(needB <= (0.75 * exam.score) ~ needB, 
                            needB > (0.75 * exam.score) ~ (0.75 * exam.score))) %>%
    mutate(forC = case_when(needC <= (0.60 * exam.score) ~ needC, 
                            needC > (0.60 * exam.score) ~ (0.60 * exam.score))) %>%
    select(-final.total, -needA, -needB, -needC)
    
  return(new.data)
  
}

move.up <- move.up.grade()

  

