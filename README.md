## Blackboard calculations

See rendered example of final output here: https://ledelaney.org/projects/whitepapers/grade-totals.html

### R code functions

+ Gathers the grade book data from Bb.
+ Selects columns relevant to calculating grades (Quizzes, HWs, Exams, Final exam, iClickers, bonus).
+ Replaces NA exempted quiz grades with the students’ average quiz grade, and sums up quizzes for a Quiz.total.
+ Replaces NA exempted HW grades with the students’ average HW grade, and subtracts the lowest HW grade to include the best 10 only.
+ Sums up all  HWs for a HW.totals.
+ Subtracts 100pts from the full point totals of students that were exempted from an exam.
+ Subtracts 25pts from the full point totals of students that were exempted from second half iClickers, OR exempts them automatically for a score less than 11pts.
+ Uses the full point totals and summed values to calculate a course percentage.
+ Assigns a letter grade based on syllabus cutoffs.
+ Checks final exam grade for redemption.
+ Generates a CSV file sorted by TA with grade columns appearing first and raw data afterwards.

### Graphics functions

+ Takes the course grade data and incorporates the final exam score
+ Determines whether or not the final exam grade is higher than the course grade
+ If so, marks them as Redeemed and changes their NewCourseGrade to their final exam grade.
