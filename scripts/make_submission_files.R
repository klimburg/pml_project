# Create submission files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

#pml_write_files(pred.test)



pml_read_files <- function(x="./"){
    files <- dir(x,pattern = "problem_id", full.names = T)    
    n <- length(files)
    pred.test<-rep(NA,n)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        pred.test[i]<-readLines(filename)
    }
    return(pred.test)
}
#pred.test<-pml_read_files()
