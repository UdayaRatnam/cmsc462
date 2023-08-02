# Example - 1 Gender and Fiction 


#Enter the data
R1 = c(250, 200)
R2 = c(50, 1000) 
rows   = 2

#code to create a contingency table
myMatrix = matrix(c(R1, R2), nrow=rows, byrow=TRUE)

#code to name the rows and columns
rownames(myMatrix) = c("Fiction", "Non-fiction")          
colnames(myMatrix) = c("Male", "Female")    

#code to display the contingency table
myMatrix

#code to run the chi-square test of independence on the Fiction & Gender data.

chisq.test(myMatrix, correct=TRUE)
chisq.test(myMatrix, correct=FALSE)  

# Example - 2 Vaccine and Reaction 


#code to enter the data
R1 = c(4788, 30)
R2 = c(8916, 76) 
rows   = 2

#code to create a contingency table
Matriz = matrix(c(R1, R2), nrow=rows, byrow=TRUE)

#code to name the rows and columns
rownames(Matriz) = c("Thigh", "Arm")          
colnames(Matriz) = c("No.severe", "Severe")    

#code to display the contingency table
Matriz

#code to run the chi-square test of independence on the vaccine data.
chisq.test(Matriz, correct=TRUE)  
chisq.test(Matriz, correct=FALSE)  