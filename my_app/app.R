library(shiny)
source("datarep.R")

ui<-fluidPage(
  #  input  : stuff you allow the users to interact with:actionButton, numericInput
)

server<-function(input, output){
  # fetch the input from ui and respond with some output: renderPlot, renderPrint...etc
}
# call shinyApp and launch it
shinyApp(ui=ui, server=server)
#Step 3 : try-and-error to build the app, insert your first model 

library(shiny) # shiny library
# begining of ui component ?
ui<-shinyUI(fluidPage(
  
  fluidRow(
    column(12,
           "Model Selection Panel",
           
           sidebarLayout(
             sidebarPanel(
               h3('choose the model'),
               # the actioButton called rpart which is the name of the variable you need to use in the server component
               
               actionButton('rpart', label = 'Decision Tree',icon("leaf",lib="glyphicon"), 
                            style="color: #fff; background-color: #339933; border-color: #2e6da4"),
               
               actionButton('rf', label = 'Random Forest', icon("tree-conifer", lib="glyphicon"),
                            style="color: #fff; background-color: #33cc33; border-color: #2e6da4"),
               
               actionButton('svm', label = 'SupportVectorMachine', icon("random", lib="glyphicon"),
                            style="color: #fff; background-color: #ffa500; border-color: #2e6da4"),
               
               # the training sample split you allow the user to control on your model
               numericInput("ratio", "training sample in %", value=50/100, min = 50/100, max = 90/100, step=0.1)
             ),
             # this is how you create many "tabs" in the finishing 
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

# remember to inmport all the libraries you need for your machine learning models and plots
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(tree) # good to have but not necessary
library(neuralnet) # you will need this to do neural network
library(xtable) # good to have
library(e1071) # your suppose vector machine model
library(randomForest) # your randomforest model?

#data(iris) # you call the famous machine learning data iris like this
#attach(iris) i usually do this to cache my dataset?


# begining of your server component
server<- function(input,output, session){
  set.seed(1234)
  observe({
    r<-as.numeric(input$ratio)
    ind <- sample(2, nrow(data), replace = TRUE, prob=c(r,1-r))
    trainset = data[ind==1,]
    testset = data[ind==2,]
    
    
    
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
    
    
    

    #random forest action button
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
    
    # neural network action button called nn
    observeEvent(input$nn, {
      require(neuralnet)
      trainset$yes = trainset$class == "yes"
      trainset$no = trainset$class == "no"

      # train the neuralnet
      nn <-neuralnet(yes + no ~ X1A + X1C + X1G + X1T + X19A + X19C + X19G + X19T + X20A + X20C + X20G + X20T + X21A + X21C + X21G + X21T + X22A + X22C + X22G + X22T + X23A + X23C + X23G + X23T + X24A + X24C + X24G + X24T + X25A + X25C + X25G + X25T + X26A + X26C + X26G + X26T + X27A + X27C + X27G + X27T + X28A + X28C + X28G + X28T + X29A + X29C + X29G + X29T + X30A + X30C + X30G + X30T + X31A + X31C + X31G + X31T + X32A + X32C + X32G + X32T + X33A + X33C + X33G + X33T, trainset, hidden=c(3))
      comp <- compute(nn, testset[-5]) # this is how you make prediction with testset
      pred.weights <- comp$net.result
      idx <- apply(pred.weights, 1, which.max)
      prednn <- c('yes', 'no')[idx] # the resulting prediction

      output$result<-renderTable({
        table(prednn, testset$class)  })
      output$summary <- renderPrint(summary(nn$net.result))
      output$plot <- renderPlot({
        plot(nn)
      })
    })
    
    
    # svm action button
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
    
    
    #print dataframe's sample head
    output$head <- renderPrint({
      head(testset, 5)
    })
  })
}

shinyApp(ui=ui, server=server)

# free server shinyapps.io