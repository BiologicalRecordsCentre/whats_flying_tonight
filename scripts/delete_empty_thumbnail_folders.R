dirs <- list.dirs('www/images/species/')

thumbdirs <- dirs[grepl('thumbnail$', dirs)]

for(i in thumbdirs){
  
  if(length(list.files(i)) == 0){
    cat(i, grep(i, thumbdirs), '\n')
    # unlink(i, recursive = TRUE)
  }
}
