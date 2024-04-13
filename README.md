#Week 4: dplyr package

#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))

library(datasets)
data(Titanic)

titanic_data<-as.data.frame(Titanic)

#See the top rows of the data
#TASK: Write the function to see the top rows of the data

view_top_rows<-function(titanic_data,n=5){
  return(head(titanic_data, n))
}

#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr

install.packages("dplyr")
library(dplyr)

#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)

select_columns<-function(titanic_data) {
  return(select(titanic_data, Survived, Sex))
}

#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name

new_dataset_select <- select_columns(titanic_data, c("Survived", "Sex"))

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)

deselect_column <- function(new_dataset_select, Sex) {
  select(new_dataset_select, -Sex)
}
new_dataset_no_sex <- deselect_column(new_dataset_select, Sex)

#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'

titanic_data <- rename_column(titanic_data, "Sex" , "Gender")

#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column

new_dataset_with_gender <- titanic_data %>%
  rename_column(Sex, Gender)

#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'

install.packages("dplyr")
library(dplyr)
filtered_data <- filter(titanic_data, gender=="male")

#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())

library(dplyr)
sorted_data <- arrange(titanic_data, gender)
                       
#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
#TASK: After you run it, write the total here:____

total_frequency <- sum(titanic_data$Freq, na.rm = TRUE)

#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'

library(dplyr)
females_data <- filter(titanic_data, gender == "female")

#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

combined_data <- bind_rows(males_data, females_data)

#Optional Task: add any of the other functions 
#you learned about from the dplyr package

average_fare <- combined_data %>%
  group_by(Pclass) %>%
  summarize(AvgFare = mean(Fare, na.rm = TRUE))
