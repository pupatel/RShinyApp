#Created by Parth Patel, DBI @ University of Delaware, Newark, Delaware 19717
#Date created: 03/29/2018

##This script builds and runs RShiny app using example feature file and machine learning algorithms.


library(shiny) # hiny library

source("datarep.R") #Uploading data file ("Features.csv") using another R script.

ui<-fluidPage(
  #  input  : Inputs for the users to interact with:actionButton, numericInput
)

server<-function(input, output){
  # Gather the input from ui and respond with some output: renderPlot, renderPrint...etc
}
# Call shinyApp and launch it
shinyApp(ui=ui, server=server)


# Beginning of ui component
ui<-shinyUI(fluidPage(
  
  fluidRow(
    column(12,
           "Model Selection Panel",
           
           sidebarLayout(
             sidebarPanel(
               h3('choose the model'),
               # The actioButtons called rpart,rf,svm which is the names of the variables used in the server component
               
               actionButton('rpart', label = 'Decision Tree',icon("leaf",lib="glyphicon"), 
                            style="color: #fff; background-color: #339933; border-color: #2e6da4"),
               
               actionButton('rf', label = 'Random Forest', icon("tree-conifer", lib="glyphicon"),
                            style="color: #fff; background-color: #33cc33; border-color: #2e6da4"),
               
               actionButton('svm', label = 'SupportVectorMachine', icon("random", lib="glyphicon"),
                            style="color: #fff; background-color: #ffa500; border-color: #2e6da4"),
               
               # The training sample split  allow the user to control on your model. Default is set to 50%.
               numericInput("ratio", "training sample in %", value=50/100, min = 50/100, max = 90/100, step=0.1)
             ),
             # Create  "tabs" on the right side output panel. 
             mainPanel(
               
               tabsetPanel( 
                            tabPanel("first 5 rows of the dataframe", verbatimTextOutput("head")), 
                            tabPanel("model result", tableOutput("result")), 
                            tabPanel("model plot", plotOutput('plot')),
                            tabPanel("model summary", verbatimTextOutput('summary'))
               )
             )
           )))
))

# Import all the required libraries for machine learning models and plots
library(rpart)				  # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(tree) 
library(xtable)
library(e1071) # suppose vector machine model
library(randomForest) #randomforest model


# Beginning of server component
server<- function(input,output, session){
  set.seed(1234)
  observe({
    r<-as.numeric(input$ratio)
    ind <- sample(2, nrow(data), replace = TRUE, prob=c(r,1-r))
    trainset = data[ind==1,]
    testset = data[ind==2,]   
    
     # Decision Tree action button
    observeEvent(input$rpart, {
      ml_rpart<-rpart(trainset$class~.,method='class',data=trainset,control=rpart.control(minsplit=10,cp=0))
      model_pred<-predict(ml_rpart, testset, type="class")
      output$result<-renderTable({
        table(model_pred, testset$class)    })
      output$summary <- renderPrint(summary(ml_rpart))
      output$plot <- renderPlot({
        prune.fit<-prune(ml_rpart, cp=0.001)
        # prune the treefirst then plot the pruned tree 
        plot(prune.fit, uniform=TRUE, 
             main="Pruned Classification Tree for small RNA data")
        text(prune.fit, use.n=TRUE, all=TRUE, cex=.8)
      })
    })
    
    
    

    #Random forest action button
    observeEvent(input$rf, {
      require(randomForest)
      rf.fit<-with(trainset, randomForest(class~., data=trainset, importance=TRUE, ntree=400))
      rf.pred<- predict(rf.fit, testset)
      output$result<-renderTable({
        
        table(rf.pred, testset$class)   })
      output$summary <- renderPrint(summary(rf.fit))
      output$plot <- renderPlot({
        varImpPlot(rf.fit, main="Random Forest model fit, importance of the parameters")
        importance(rf.fit)
      })
    })
    

    
    # SVM action button
    observeEvent(input$svm, {
      require(e1071)
      attach(data)
      x <- subset(data, select=-class)
      y <- class
      svm_model <- svm(class ~ ., data=data,type='C-classification', kernel='linear')
      svm_model1 <- svm(x,y)
      pred <- predict(svm_model1,x)
      output$result<-renderTable({
        table(pred,y) })
      output$summary <- renderPrint(summary(svm_model1))
      output$plot <- renderPlot({
        plot(svm_model, data,X1C~X19G,slice=list(X1A=3,X20A=4))
      })
    })
    
    
    #Print dataframe's sample head (5 rows)
    output$head <- renderPrint({
      head(testset, 5)
    })
  })
}

#Lauch shinyApp
shinyApp(ui=ui, server=server)

# End of Script #
