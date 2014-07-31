library(shiny)
require(ssviz)
options(shiny.maxRequestSize=100*1024^2)

# read the sample bams from package
# want to be able to pass this as sample input
bam.files <- dir(system.file("extdata", package = "ssviz"), full = T, patt = "bam$")

shinyServer(function(input, output,session) {
  # global variables to pass among the different shiny inputs
  # is there a better, cleaner way?
  file1<-NULL
  file2<-NULL
  datalist<-reactiveValues()
  datalistpp<-reactiveValues()
  datalist2<-list()
  data1<-NULL
  data2<-NULL
  sample_button_counter<-0
#   output$filenames <- renderText({
#     # display the file names after uploading (user-to-shiny)
#     # keep filenames for dropdown menus
#     
#     file1<<-input$file1
#     file2<<-input$file2
#     HTML(paste(paste("File #1 : ",file1$name),paste("File #2 : ",file2$name), sep = '<br/>'))
#   })
  
#   output$filenames2 <- renderText({
#     # display the file names after uploading (user-to-shiny)
#     # keep filenames for dropdown menus
#     if (input$goSampleFiles == 0)
#       return()
#     else {
#       
#     }
#     file1$datapath<<-bam.files[1]
#     file2$datapath<<-bam.files[2]
#     f1<-unlist(strsplit(bam.files[1],"/"))
#     f2<-unlist(strsplit(bam.files[2],"/"))
#     file1$name<<-f1[length(f1)]
#     file2$name<<-f2[length(f2)]
#     HTML(paste(paste("File #1 : ",file1$name),paste("File #2 : ",file2$name), sep = '<br/>'))
#   })
  output$filenames <- renderText({
    # display the file names after uploading (user-to-shiny)
    # keep filenames for dropdown menus
    files<-getFilesnames()
    f1<-files[1]
    f2<-files[2]
    HTML(paste(paste("File #1 : ",f1),paste("File #2 : ",f2), sep = '<br/>'))
  })    

  getFilesnames <- reactive({
    file1<<-input$file1
    file2<<-input$file2
    if (input$goSampleFiles == sample_button_counter){
      file1<<-input$file1
      file2<<-input$file2
    }
    else {
      sample_button_counter<<-sample_button_counter+1
      file1$datapath<<-bam.files[1]
      file2$datapath<<-bam.files[2]
      f1<-unlist(strsplit(bam.files[1],"/"))
      f2<-unlist(strsplit(bam.files[2],"/"))
      file1$name<<-f1[length(f1)]
      file2$name<<-f2[length(f2)]
    }
    
    return(list(file1$name,file2$name))
  })
  
  loadFile1 <- reactive({
    # actually loads file into R
    if (input$goLoadBam1 == 0)
      return()
    if(is.null(file1))
      return()
    
    data1<<-readBam(file1$datapath)
    filename<-isolate(names(which(reactiveValuesToList(datalist)=="1")))
    if(length(filename)!=0){
      isolate(datalist[[filename]]<-"")
      datalist2<<-datalist2[-which(names(datalist2)=="1")]
    }
    isolate(datalist[[file1$name]]<-"1")
    datalist2<<-c(datalist2,list("1"=data1))
    
    return(list(data1,paste("Bam #1:",file1$name,"sucessfully loaded!")))
  })
  
  loadFile2 <- reactive({
    if (input$goLoadBam2 == 0)
      return()
    if(is.null(file2))
      return()
    
    data2<<-readBam(file2$datapath)
    filename<-isolate(names(which(reactiveValuesToList(datalist)=="2")))
    
    if(length(filename)!=0){
      datalist[[filename]]<-""
      datalist2<<-datalist2[-which(names(datalist2)=="2")]
    }
    isolate(datalist[[file2$name]]<-"2")
    datalist2<<-c(datalist2,list("2"=data2))
    return(list(data2,paste("Bam #2:",file2$name,"sucessfully loaded!")))
  })
  
  output$read1 <- renderPrint({
    loadFile1()[[1]]
  })
  
  output$read2 <- renderPrint({
    loadFile2()[[1]]
  })
  
  output$filedropdown_distro<-renderUI({
    #selectInput("select",label="",choices=reactiveValuesToList(datalist),selected=1),
    selectInput("selectdistro",label="Select file(s)",choices=reactiveValuesToList(datalist),selected=1, multiple=TRUE)
  })
  
  output$filedropdown_region<-renderUI({
    #selectInput("select",label="",choices=reactiveValuesToList(datalist),selected=1),
    selectInput("selectregion",label="Select file(s)",choices=reactiveValuesToList(datalist),selected=1, multiple=TRUE)
  })
  
  output$filedropdown_ntfreq<-renderUI({
    #selectInput("select",label="",choices=reactiveValuesToList(datalist),selected=1),
    selectInput("selectntfreq",label="Select file",choices=reactiveValuesToList(datalist),selected=1)
  })
  
  output$filedropdown_pingpong<-renderUI({
    #selectInput("select",label="",choices=reactiveValuesToList(datalist),selected=1),
    selectInput("selectpingpong",label="Select file",choices=reactiveValuesToList(datalist),selected=1)
  })
  
  output$bamloaded1<-renderText({
    HTML(paste("<span style=\"color:green\">",loadFile1()[[2]],"</span>"))
    })
  
  output$bamloaded2<-renderText({
    HTML(paste("<span style=\"color:green\">",loadFile2()[[2]],"</span>"))
  })
  
  output$plotdistro<-renderPlot({
    if (input$goPlotDistro == 0)
      return()
    if(is.null(data1))
      return()
    
    samples<-isolate(input$selectdistro)
    sn<-isolate(input$plotdistro_samplenames)
    pcount<-isolate(input$plotdistro_ncount)
    ps<-isolate(input$plotdistro_readcount)
    if(length(samples)==2) {
      plotlist<-list(getCountMatrix(data1,pseudo=ps),getCountMatrix(data2,pseudo=ps))
      if(sn=="") sn<-c(file1$name,file2$name) 
      if(pcount!="") {
        pcount=unlist(strsplit(pcount,","))
        pcount=c(as.numeric(pcount[1]),as.numeric(pcount[2]))
      }
    }
    else if(samples=="1") {
      plotlist<-list(getCountMatrix(data1,pseudo=ps))
      if(sn=="") sn<-c(file1$name) 
      if(pcount!="") {
        pcount=as.numeric(unlist(strsplit(pcount,","))[1])
      }
    }
      
    else {
      plotlist<-list(getCountMatrix(data2,pseudo=ps))
      if(sn=="") sn<-c(file2$name) 
      if(pcount!="") {
        pcount=as.numeric(unlist(strsplit(pcount,","))[1])
      }
    }
      
    
    ptype<-isolate(input$plotdistro_type)
    if(pcount!="") {
      pp<-plotDistro(plotlist,type=ptype,ncounts=pcount,samplenames=unlist(strsplit(sn,",")))
    } else {
      pp<-plotDistro(plotlist,type=ptype,samplenames=unlist(strsplit(sn,",")))
    }
    print(pp)
  })
  
  output$plotregion<-renderPlot({
    if (input$goPlotRegion == 0)
      return()
    if(is.null(data1))
      return()
    
    samples<-isolate(input$selectregion)
    sn<-isolate(input$plotregion_samplenames)
    pcount<-isolate(input$plotregion_ncount)
    pregion<-isolate(input$plotregion_region)
    if(length(samples)==2) {
      plotlist<-list(getCountMatrix(data1),getCountMatrix(data2))
      if(sn=="") sn<-c(file1$name,file2$name) 
      if(pcount=="") {
        pcount=c(1000000,1000000)
      } else {
        pcount=unlist(strsplit(pcount,","))
        pcount=c(as.numeric(pcount[1]),as.numeric(pcount[2]))
      }
    }
    else if(samples=="1") {
      plotlist<-list(getCountMatrix(data1))
      if(sn=="") sn<-c(file1$name) 
      if(pcount=="") {
        pcount<-1000000
      } else {
        pcount=as.numeric(unlist(strsplit(pcount,","))[1])
      }
    }
    
    else {
      plotlist<-list(getCountMatrix(data2))
      if(sn=="") sn<-c(file2$name) 
      if(pcount=="") {
        pcount<-1000000
      }
    }
    
    pp<-plotRegion(plotlist,region=pregion,ncounts=pcount,samplenames=unlist(strsplit(sn,",")))
    print(pp)
  })
  
  mainntfreq<-reactive({
    if (input$gontfreq == 0)
      return()
    result<-isolate(ntfreq(datalist2[[input$selectntfreq]],ntlength=input$ntfreq_length))
    return(result)
    })
  
  output$plotntfreq<-renderPlot({
    res<-mainntfreq()
    if(!is.null(res)) {
      pp<-plotFreq(res)
      print(pp)
    }
  })
  
  output$textntfreq<-renderPrint({
    mainntfreq()
  })
  
  mainpingpong<-reactive({
    if (input$gopingpong == 0)
      return()
    if(is.null(data2))
      return()
    result<-isolate(pingpong(datalist2[[input$selectpingpong]]))
    return(result)
  })

  output$objectdropdown_pingpong<-renderUI({
  #selectInput("select",label="",choices=reactiveValuesToList(datalist),selected=1),
    selectInput("selectpp",label="Computed frequencies to plot",choices=reactiveValuesToList(datalistpp),selected=1, multiple=TRUE)
})
  
  output$textpp<-renderPrint({
    mainpingpong()
  })
  
  output$plotpp<-renderPlot({
    res<-mainpingpong()
    if(!is.null(res)) {
      pp<-plotPP(list(res))
      print(pp)
    }
  })
})
 