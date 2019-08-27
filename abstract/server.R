library(shiny)

shinyServer(function(input, output) {
  
  txt = readLines("http://www.ntdtv.com/xtr/gb/index.html")
  hasHref = regexpr("<a[[:space:]]+href[[:space:]]*=.*?>", txt, ignore.case=TRUE)
  urls=regmatches(txt,hasHref)
  info=gsub("[<>=_:;abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/]", "", urls)
  
  output$downloadData <-downloadHandler(
           
    filename=function(){   paste("abstract",".txt",sep='')  },
    content=function(file){
      write.table(info[input$row_min:input$row_max],file)
    }
  )
    
})