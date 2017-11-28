
if(!require('sqldf')){install.packages('sqldf'); require('sqldf')}
if(!require('dplyr')){install.packages('dplyr'); require('dplyr')}

directory        = '/Users/darrenho/Box Sync/teaching/STAT6306/data'
firstTimeRunning = TRUE

if(firstTimeRunning){
  train = read.csv(paste(directory,'digits_train_kaggle.csv',sep='/'))
  test  = read.csv(paste(directory,'digits_test_kaggle.csv',sep='/'))
  
  
  train$id = paste('train',1:nrow(train),sep='')
  test$id  = row.names(test)
  
  digits = dplyr:::bind_rows(train,test)
  digits$label[is.na(digits$label)] = 'test'
  
  db = dbConnect(SQLite(), dbname='digits.sqlite')
  
  dbWriteTable(conn = db, name = 'all_data', 
               value = digits, row.names = FALSE,
               col.names = TRUE,overwrite=TRUE)
  
  dbDisconnect(db)
}

db     = dbConnect(SQLite(), dbname='digits.sqlite')
Ytrain = dbGetQuery(db,'SELECT label FROM all_data WHERE label != "test"')$label

namesTest        = paste(names(read.csv(paste(directory,
                                              'digits_test_kaggle.csv',
                                              sep='/'),
                                        nrows = 1)),
                         collapse = ', ')
XtrainSQLcommand = paste('SELECT ',namesTest,'FROM all_data WHERE label != "test"',sep=' ')
Xtrain           = dbGetQuery(db,XtrainSQLcommand)
XtestSQLcommand  = paste('SELECT ',namesTest,'FROM all_data WHERE label = "test"',sep=' ')
Xtest            = dbGetQuery(db,XtestSQLcommand)

XtestID          = dbGetQuery(db,'SELECT id FROM all_data WHERE label = "test"')[,1]


######################################

plotDigit = function(x,zlim=c(-1,1)) {
  cols = gray.colors(100)[100:1]
  image(matrix(x,nrow=28)[,28:1],col=cols,
        zlim=zlim,axes=FALSE)  
}

plotDigit(as.numeric(Xtrain[100,]))

require(randomForest)
start = proc.time()[3]
rfOutput = randomForest(x = Xtrain,y = as.factor(Ytrain),ntree=50)
stop = proc.time()[3]
print(stop-start)
Yhat = as.numeric(predict(rfOutput,Xtest,type='class'))-1

pdf(paste(directory,'rfOOB.pdf',sep='/'))
plot(rfOutput)
dev.off()

fileName= paste(directory,'rfPredictions.csv',sep='/')
write.table(x=cbind(XtestID,Yhat),file='rfPredictions.csv',
            row.names = FALSE,col.names = c('ImageId','Label'),
            sep= ',', quote=FALSE)

#plotDigit(as.numeric(Xtest[3,]))