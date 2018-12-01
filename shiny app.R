setwd("C:/Users/Cheng_wen_sung/Desktop/PDFv1/Shiny beta")
library(rmarkdown)
library(knitr)
library(reshape2)
library(ggplot2)
library(shiny)
ui <- navbarPage("南大資料分析",
                 tabPanel("匯入資料",
                          tabsetPanel(
                            tabPanel("讀卡資料",
                                    fileInput(inputId = "dta_raw", label = "選取檔案", buttonLabel = "瀏覽...",placeholder = "尚未選取任何檔案"),
                                    dataTableOutput("rawtable")),
                            tabPanel("基本資料",
                                     fileInput(inputId = "dta_basic", label = "選取檔案", buttonLabel = "瀏覽...",placeholder = "尚未選取任何檔案"),
                                     dataTableOutput("basictable")),
                            tabPanel("統計結果",
                                     dataTableOutput("scoretable"))
                          )),
                 tabPanel("輸出報表",
                          numericInput(inputId = "datastart", label = "指定起始位置", 1, min = 1, max = 100),
                          numericInput(inputId = "dataend", label = "指定結束位置", 1, min = 1, max = 100),
                          actionButton(inputId = "outputpdf", label = "輸出PDF!"),
                          actionButton(inputId = "folder", label = "檢視輸出資料夾")
                          ),
                 tabPanel("寄發Email"),
                 tabPanel("高風險名單",
                          downloadButton("downloadData", "儲存"),
                          dataTableOutput("control"))
  
)

server <- function(input, output) {
  dtarawall <- reactive({read.csv(input$dta_raw$datapath, header = T)})
  dtabasic <- reactive({
    stubasic <- read.csv(input$dta_basic$datapath, header = T)
    names(stubasic)[2] <- 'id'
    names(stubasic)[4] <- 'name'
    stubasic <- stubasic[,c(1:7,12)]
    stubasic
    })
  output$rawtable <- renderDataTable({dtarawall()})
  output$scoretable <- renderDataTable({dta()})
  output$basictable <- renderDataTable({dtabasic()})
  output$control <- renderDataTable({grpcontrol()})
  
  ##
  dta <- reactive({
    dta <- read.csv(input$dta_raw$datapath,header = T)
    dta$A1 <- rowSums(dta[,3:37])
    dta$A2 <- rowSums(dta[,38:39])
    dta$B1 <- rowSums(dta[,c(43:46,48:57)])
    dta$B2 <- rowSums(dta[,c(40:42,58:62)])
    dta$B3 <- rowSums(dta[,c(47,63:67)])
    dta$C1 <- rowSums(dta[,c(68:81,86)])
    dta$C2 <- rowSums(dta[,c(84:85,87:93)])
    
    dta$A1p <- rank(dta$A1,na.last='keep')/(dim(dta)[1])*100
    dta$A2p <- rank(dta$A2,na.last='keep')/(dim(dta)[1])*100
    dta$B1p <- rank(dta$B1,na.last='keep')/(dim(dta)[1])*100
    dta$B2p <- rank(dta$B2,na.last='keep')/(dim(dta)[1])*100
    dta$B3p <- rank(dta$B3,na.last='keep')/(dim(dta)[1])*100
    dta$C1p <- rank(dta$C1,na.last='keep')/(dim(dta)[1])*100
    dta$C2p <- rank(dta$C2,na.last='keep')/(dim(dta)[1])*100
    
    dta$A1m <- mean(dta$A1,na.rm=T)
    dta$A2m <- mean(dta$A2,na.rm=T)
    dta$B1m <- mean(dta$B1,na.rm=T)
    dta$B2m <- mean(dta$B2,na.rm=T)
    dta$B3m <- mean(dta$B3,na.rm=T)
    dta$C1m <- mean(dta$C1,na.rm=T)
    dta$C2m <- mean(dta$C2,na.rm=T)
    dta <- dta[,
               c("id",
                 "A1p","A2p","B1p","B2p","B3p","C1p","C2p",
                 "A1","A2","B1","B2","B3","C1","C2",
                 "A1m","A2m","B1m","B2m","B3m","C1m","C2m"
               )]
    dta
    })
  
  grpcontrol <- reactive({
    
    dta <- read.csv(input$dta_raw$datapath,header = T)
    dta$A1 <- rowSums(dta[,3:37])
    dta$A2 <- rowSums(dta[,38:39])
    dta$B1 <- rowSums(dta[,c(43:46,48:57)])
    dta$B2 <- rowSums(dta[,c(40:42,58:62)])
    dta$B3 <- rowSums(dta[,c(47,63:67)])
    dta$C1 <- rowSums(dta[,c(68:81,86)])
    dta$C2 <- rowSums(dta[,c(84:85,87:93)])
    
    dta$flag1 <- 0
    dta$flag2 <- 0
    dta$flag3 <- 0
    dta$flag4 <- 0
    dta$flag5 <- 0
    dta$flag6 <- 0
    
    
    grp <- which(dta$A1>(mean(dta$A1,na.rm=T)+2*sd(dta$A1,na.rm=T)))
    dta[grp,]$flag1 <- 1
    
    dta$B <- dta$B1+dta$B2+dta$B3
    grp <- which(dta$B<(mean(dta$B,na.rm=T)-2*sd(dta$B,na.rm=T)))
    dta[grp,]$flag2 <- 1
    
    grp <- which(dta$C1>(mean(dta$C1,na.rm=T)+2*sd(dta$C1,na.rm=T)))
    dta[grp,]$flag3 <- 1
    
    grp <- which(dta$C2>(mean(dta$C2,na.rm=T)+2*sd(dta$C2,na.rm=T)))
    dta[grp,]$flag4 <- 1
    
    temp1 <- dta$Q3_11>3 
    temp2 <- dta$Q3_12>3 
    temp3 <- dta$Q3_13>3 
    temp4 <- dta$Q3_14>3 
    temp5 <- dta$Q3_25>3 
    temp6 <- dta$Q3_26>3 
    dta$discrim <- rowSums(data.frame(temp1,temp2,temp3,temp4,temp5,temp6),na.rm=TRUE)
    grp <- which(dta$discrim>2)
    dta[grp,]$flag5 <- 1
    
    grp <- which(dta$Q3_26>3)
    dta[grp,]$flag6 <- 1
    
    grp <- which((dta$flag1+dta$flag2+dta$flag3+dta$flag4+dta$flag5+dta$flag6)>0)
    grpcontrol <- dta[grp,c('id','A1','B','C1','C2','Q3_11','Q3_12','Q3_13','Q3_14','Q3_25','Q3_26','flag1','flag2','flag3','flag4','flag5','flag6')]
    #grpcontrol <- merge(grpcontrol, dtabasic(), by="id")
    grpcontrol
  })
  
  
  ##
  output$downloadData <- downloadHandler(
    filename = "123.csv",
    content = function(file){
      write.csv2(grpcontrol(), file, row.names = F)
    }
  )
  ##
  observeEvent(input$folder, shell.exec(getwd()))
  observeEvent(input$outputpdf, {
    for (idno in c(input$datastart:input$dataend)){
      dtaraw <- dta()[idno,]
      
      dtatable <- as.data.frame(matrix(round(dtaraw[,2:22],2),3,7,byrow=T))
      
      
      dtatemp <- dta()[idno,c('A1p','A2p','B1p','B2p','B3p','C1p','C2p')]
      names(dtatemp) <- c("A1", "A2",
                          "B1","B2","B3",
                          "C1","C2")
      dtatempl <- melt(dtatemp)
      colors <- c('lightcoral','lightblue','lightblue','lightblue','lightblue','lightcoral','lightcoral')
      
      labs <- sprintf("%.1f", dtatempl$value)
      dtatempl[is.na(dtatempl$value),'value'] <- -1
      
      ##畫圖
      
      p <- ggplot(dtatempl, aes(x = variable, y = value))+
        geom_bar(stat = "identity", width=.8, colour = 'black',fill=colors)+
        labs(title = "PR", x = "", y = "")+
        coord_cartesian(ylim=c(0,100))+
        geom_text(aes(label = labs, y= value+5))+
        guides(fill=FALSE)+ 
        theme(
          axis.text.x = element_text(angle = 0, size = 10, hjust = 0.5)
        )
      
      
      ##RMD
      render(input = "輸出PDFv1.Rmd", encoding = "UTF-8", output_file = sprintf("%s.pdf", as.character(dtaraw$id)))
    }
  })
  
}


shinyApp(ui = ui, server = server)
