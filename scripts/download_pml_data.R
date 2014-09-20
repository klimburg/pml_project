# This script downloads the test and training data for the practical machine 
# learing class project

url.train<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.test<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url.train,"data/pml_train.csv",method="curl")
download.file(url.test,"data/pml_test.csv",method="curl")
