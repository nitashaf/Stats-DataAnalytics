
Folder structure 

The following folder structure is mandatory to run the code. Users will have to manually create the input and output folders. 
“input” folder 
qualtrics.data.csv  (raw output from Qualtrics following format requirements) 
question.list.xlsx 
“base…R” script 
See description below 
“output” folder 
Plots for each of the questions identified as Likert questions in the “questions.list.xlsx”; titled by question number.  

Summary of functionality in the base.R script 

This script reads in two data files: the raw Qualtrics data, and the question summary file.  
qualtrics.data.csv: It is the raw output Qualtrics result file, extracted and exported from Qualtrics survey. 
Survey Home Page -> Data and Analysis -> Export & Import -> Export Data > csv -> Use numeric values 
Question.list.xlsx: This is an excel file, user will have to create. It contains information related to questions. Question number for which the user wants to create the graph  
For each question identified as a likert question in the question.summary file, data is extracted from the raw data, compiled into a table summarizing answer tallies, then a plot is created. 

Functions 

blank_values_function 
likert_mean_function 
read_question_number_function 
read_question_title_function 
 
