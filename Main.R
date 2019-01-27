##此腳本位置 
workdir <- dirname(rstudioapi::getSourceEditorContext()$path)

##工作路徑&套件
setwd(workdir)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(reshape2)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(ggplot2)
library(DT)
library(mailR)
library(tinytex)

##Shiny主程式UI
ui <- navbarPage("南大資料分析", id = "this",
                 tabPanel("匯入資料", value = "first",
                          tabsetPanel(
                            tabPanel("讀卡資料",
                                    fileInput(inputId = "dta_raw", label = "選取檔案", buttonLabel = "瀏覽...",placeholder = "尚未選取任何檔案"),
                                    materialSwitch(inputId = "nacheck", label = "只顯示含有NA的資料"),
                                    DT::dataTableOutput("rawtable", width = "98%"), br()),
                            tabPanel("基本資料",
                                     fileInput(inputId = "dta_basic", label = "選取檔案", buttonLabel = "瀏覽...",placeholder = "尚未選取任何檔案"),
                                     DT::dataTableOutput("basictable"), br()),
                            tabPanel("統計結果",
                                     DT::dataTableOutput("statable", width = "98%"), br())
                          )),
                 tabPanel("報表&寄信", value = "mail",
                        DT::dataTableOutput("mailtable", width = "100%"),
                        fluidRow(column(10,
                        actionButton("ref", "刷新", icon("refresh"))),
                        column(2, br(),
                        htmlOutput("infotext"))),
                        fluidRow(column(10), column(2, br(),
                        actionButton("pdf2", "產生報表", width = "55%", icon("file"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("mail2", "寄信", width = "40%", icon("envelope"),
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        br(),br())
                        )),
                 tabPanel("高風險名單",
                          DT::dataTableOutput("control"),
                          fluidRow(column(10,
                          downloadButton("downloadData", "下載高風險族群資料")),
                          column(2,
                          actionButton("controlpdf", "檢視高風險個人報表", icon("folder-open"), width = "100%",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                          br(),br())
                          ),
                 tabPanel("整體分析", value = "needpng",
                          actionButton("gene", "更新圖表", icon("signal"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          fluidRow(
                            column(1),
                            column(10,
                            selectInput(inputId = "subselect", "分量表", c("身心症狀指標" = "A1", "主觀健康自評" = "A2",
                                                                        "自我資源指標" = "B1", "家庭資源指標" = "B2", "師友資源指標" = "B3",
                                                                        "情緒化與焦慮分數" = "C1", "憂鬱與無助分數" = "C2",
                                                                        "風險人數比較" = "All"), width = "100%")
                            ),
                            column(1)
                            ),
                          fluidRow(
                          column(6, plotOutput("sexana", width = "100%", height = "100%"), br()),
                          column(6, fluidRow(column(11, br(),br(), tableOutput("sexana2"))),
                                 fluidRow(column(11, br(),br(), tableOutput("sexana3"))),
                                 fluidRow(br(), htmlOutput("subinfo"))) 
                          )
                 )
  
)


##Shiny主程式server
server <- function(input, output, session) {
  
  ##檢查資料夾
  dirch <- c("Ana_png", "asset_rmd", "PDF_all", "PDF_riskGroup", "mail_log")
  lapply(dirch, function(x) if(!dir.exists(x)) dir.create(x))
  
  ##刪除舊圖
  repng <- paste0("Ana_png/", c("A1", "A2", "B1", "B2", "B3", "C1", "C2", "All"), ".png")
  lapply(repng, function(x) if(file.exists(x)) file.remove(x))
  
  ##檢查asset_rmd裡必要檔案
  rmdas <- c("header.tex", "stupdf.Rmd")
  if(sum(rmdas %in% list.files("asset_rmd/")) != 2){
    showModal(modalDialog(
      title = "錯誤訊息",
      "缺少必要檔案，無法執行程式
      (asset_rmd/header.tex 或是 asset_rmd/stupdf.Rmd)"
    ))
    Sys.sleep(3)
    stopApp()
  }
  
  ##刪除asset_rmd中遺留檔
  dele <- list.files("asset_rmd/")
  lapply(dele, function(x) if(!x %in% c("header.tex", "stupdf.Rmd")) unlink(paste0("asset_rmd/", x), recursive = T, force = T))
  
  ##載入套件
  library(showtext)
  
  ##檢查字體
  fontch <- c("NotoSansCJKtc-Regular.otf", "NotoSansCJKtc-Medium.otf", "NotoSansCJKtc-Bold.otf", "NotoSerifCJKtc-Light.otf", "NotoSerifCJKtc-Bold.otf")
  if(sum(fontch %in% font_files()) != 5){
    showModal(modalDialog(
      title = "錯誤訊息",
      "缺少必要字體，無法執行程式，請安裝字體後執行
      (思源黑體 和 思源宋體)"
    ))
    Sys.sleep(3)
    stopApp()
  }
  
  ##showtext載入字體
  font_add("Noto","NotoSansCJKtc-Regular.otf", bold = "NotoSansCJKtc-Bold.otf", italic = "NotoSansCJKtc-Medium.otf")
  showtext_auto()
  
  
  ##切換tab & mailtable-pdf更新
  observeEvent(input$this, {
    out <- tryCatch(dtasta <- dtasta(), error = function(e) e)
    if (input$this != "first"){
      if (any(class(out) == "error")){
        showModal(modalDialog(
          title = "錯誤訊息",
          "請先匯入資料後再執行其他功能"
        ))
      updateTabsetPanel(session, "this", selected = "first")
      }
    }
    if (input$this == "mail" && !any(class(out) == "error")){
      
    }
  })
  
  
  ##讀卡資料匯入&確認格式無誤
  dtaraw <- reactive({
    validate(
      need(input$dta_raw != "", "請選擇讀卡資料，請注意檔案格式與資訊順序")
    )
    dtaraw <- read.csv(input$dta_raw$datapath, header = T, na.strings = c(NULL, '?'))
    rawcheck <- c("學號","年度","Q1_1","Q1_2","Q1_3","Q1_4","Q1_5","Q1_6","Q1_7","Q1_8" ,
                    "Q1_9","Q1_10","Q1_11","Q1_12","Q1_13","Q1_14","Q1_15","Q1_16","Q1_17","Q1_18",
                    "Q1_19","Q1_20","Q1_21","Q1_22","Q1_23","Q1_24","Q1_25","Q1_26","Q1_27","Q1_28",
                    "Q1_29","Q1_30","Q1_31","Q1_32","Q1_33","Q1_34","Q1_35","Q1_36","Q1_37","Q2_1",
                    "Q2_2","Q2_3","Q2_4","Q2_5","Q2_6","Q2_7","Q2_8","Q2_9","Q2_10","Q2_11",
                    "Q2_12","Q2_13","Q2_14","Q2_15","Q2_16","Q2_17","Q2_18","Q2_19","Q2_20","Q2_21",
                    "Q2_22","Q2_23","Q2_24","Q2_25","Q2_26","Q2_27","Q2_28","Q3_1","Q3_2","Q3_3",
                    "Q3_4","Q3_5","Q3_6","Q3_7","Q3_8","Q3_9","Q3_10","Q3_11","Q3_12","Q3_13",
                    "Q3_14","Q3_15","Q3_16","Q3_17","Q3_18","Q3_19","Q3_20","Q3_21","Q3_22","Q3_23",
                    "Q3_24","Q3_25","Q3_26")
        if (!identical(names(dtaraw), rawcheck)){
      showModal(modalDialog(
        title = "錯誤訊息",
        "讀卡資料格式有誤，請修改後再試一次!"
      ))
      NULL
    }
    else {
      row.names(dtaraw) <- as.numeric(row.names(dtaraw)) + 1
      names(dtaraw)[1:2] <- c('id', 'year')
      dtaraw}
    })

  
  ##要顯示的讀卡表格(決定是否只顯示含NA的資料)
  dtarawshow <- reactive({
    dtaraw <- dtaraw()
    dtaraw[,-c(3:37)][is.na(dtaraw[,-c(3:37)])] <- 'missing'
    dtaraw[,c(3:37)][is.na(dtaraw[,c(3:37)])] <- 'missing(0)'
    dtaraw[,c(3:37)][dtaraw[,c(3:37)] == 'NA'] <- 'missing(NA)'
  
    dtacheck <- dtaraw()
    dtacheck[dtacheck == 'NA'] <- NA
    
    if (input$nacheck == T) {
      dtaraw <- dtaraw[!complete.cases(dtacheck),]
      }
    names(dtaraw)[1:2] <- c('學號', '年度')
    dtaraw
  })
  
  
  ##匯入基本資料&確認格式無誤
  dtabasic <- reactive({
    validate(
      need(input$dta_basic != "", "請選擇學生基本資料，請注意檔案格式與資訊順序")
    )
    dtabasic <- read.csv(input$dta_basic$datapath, header = T)
    basiccheck <- c("學院","班級","學號","年度","姓名",        
                    "性別","年級","學制","通訊郵遞區號","通訊地址",    
                    "聯絡電話","手機","E.mail")
     if (!identical(names(dtabasic), basiccheck)){
      showModal(modalDialog(
        title = "錯誤訊息",
        "基本資料格式有誤，請修改後再試一次!"
      ))
      NULL
    }
    else {
      row.names(dtabasic) <- as.numeric(row.names(dtabasic)) + 1
      names(dtabasic)[1:7] <- c('college', 'class', 'id', 'year', 'name', 'sex', 'grade')
      dtabasic <- dtabasic[,c(1:7,13)]
      # dtabasic$E.mail <- "jme556677@gmail.com"
      dtabasic
    }
    })
  
  
  
  ##統計結果
  dtasta <- reactive({
    dtasta <- dtaraw()
    dtasta[,c(3:37)][is.na(dtasta[,c(3:37)])] <- '0'
    dtasta[,c(3:37)][dtasta[,c(3:37)] == 'NA'] <- NA
    dtasta[,-c(1,2)] <- as.data.frame(lapply(dtasta[,-c(1,2)], as.numeric))
    dtasta$A1 <- rowSums(dtasta[,3:37], na.rm = T)
    dtasta$A2 <- rowSums(dtasta[,38:39], na.rm = T)
    dtasta$B1 <- rowSums(dtasta[,c(43:46,48:57)], na.rm = T)
    dtasta$B2 <- rowSums(dtasta[,c(40:42,58:62)], na.rm = T)
    dtasta$B3 <- rowSums(dtasta[,c(47,63:67)], na.rm = T)
    dtasta$C1 <- rowSums(dtasta[,c(68:81,86)], na.rm = T)
    dtasta$C2 <- rowSums(dtasta[,c(84:85,87:93)], na.rm = T)
    
    dtasta$A1na <- rowSums(dtasta[,3:37])
    dtasta$A2na <- rowSums(dtasta[,38:39])
    dtasta$B1na <- rowSums(dtasta[,c(43:46,48:57)])
    dtasta$B2na <- rowSums(dtasta[,c(40:42,58:62)])
    dtasta$B3na <- rowSums(dtasta[,c(47,63:67)])
    dtasta$C1na <- rowSums(dtasta[,c(68:81,86)])
    dtasta$C2na <- rowSums(dtasta[,c(84:85,87:93)])
    
    dtasta$A1p <- round(rank(dtasta$A1)/(dim(dtasta)[1])*100, 2)
    dtasta$A2p <- round(rank(dtasta$A2)/(dim(dtasta)[1])*100, 2)
    dtasta$B1p <- round(rank(dtasta$B1)/(dim(dtasta)[1])*100, 2)
    dtasta$B2p <- round(rank(dtasta$B2)/(dim(dtasta)[1])*100, 2)
    dtasta$B3p <- round(rank(dtasta$B3)/(dim(dtasta)[1])*100, 2)
    dtasta$C1p <- round(rank(dtasta$C1)/(dim(dtasta)[1])*100, 2)
    dtasta$C2p <- round(rank(dtasta$C2)/(dim(dtasta)[1])*100, 2)
    
    dtasta$A1s <- ifelse(is.na(dtasta$A1na), paste0(round(dtasta$A1p,1), "#"), round(dtasta$A1p,1))
    dtasta$A2s <- ifelse(is.na(dtasta$A2na), paste0(round(dtasta$A2p,1), "#"), round(dtasta$A2p,1))
    dtasta$B1s <- ifelse(is.na(dtasta$B1na), paste0(round(dtasta$B1p,1), "#"), round(dtasta$B1p,1))
    dtasta$B2s <- ifelse(is.na(dtasta$B2na), paste0(round(dtasta$B2p,1), "#"), round(dtasta$B2p,1))
    dtasta$B3s <- ifelse(is.na(dtasta$B3na), paste0(round(dtasta$B3p,1), "#"), round(dtasta$B3p,1))
    dtasta$C1s <- ifelse(is.na(dtasta$C1na), paste0(round(dtasta$C1p,1), "#"), round(dtasta$C1p,1))
    dtasta$C2s <- ifelse(is.na(dtasta$C2na), paste0(round(dtasta$C2p,1), "#"), round(dtasta$C2p,1))
    
    dtasta$A1m <- round(mean(dtasta$A1na, na.rm=T), 2)
    dtasta$A2m <- round(mean(dtasta$A2na, na.rm=T), 2)
    dtasta$B1m <- round(mean(dtasta$B1na, na.rm=T), 2)
    dtasta$B2m <- round(mean(dtasta$B2na, na.rm=T), 2)
    dtasta$B3m <- round(mean(dtasta$B3na, na.rm=T), 2)
    dtasta$C1m <- round(mean(dtasta$C1na, na.rm=T), 2)
    dtasta$C2m <- round(mean(dtasta$C2na, na.rm=T), 2)
    
    dtasta$flag1 <- 0
    dtasta$flag2 <- 0
    dtasta$flag3 <- 0
    dtasta$flag4 <- 0
    dtasta$flag5 <- 0
    dtasta$flag6 <- 0
    dtasta$control <- 0
    
    
    grp <- which(dtasta$A1 > (mean(dtasta$A1na, na.rm=T) + 2*sd(dtasta$A1na, na.rm=T)))
    if (length(grp) != 0)
      dtasta[grp,]$flag1 <- 1
    
    dtasta$B <- dtasta$B1na + dtasta$B2na + dtasta$B3na
    grp <- which(dtasta$B < (mean(dtasta$B, na.rm=T) - 2*sd(dtasta$B, na.rm=T)))
    if (length(grp) != 0)
      dtasta[grp,]$flag2 <- 1
    
    grp <- which(dtasta$C1 > (mean(dtasta$C1na, na.rm=T) + 2*sd(dtasta$C1na, na.rm=T)))
    if (length(grp) != 0)
      dtasta[grp,]$flag3 <- 1
    
    grp <- which(dtasta$C2 > (mean(dtasta$C2na, na.rm=T) + 2*sd(dtasta$C2na, na.rm=T)))
    if (length(grp) != 0)
      dtasta[grp,]$flag4 <- 1
    
    temp1 <- dtasta$Q3_11>3
    temp2 <- dtasta$Q3_12>3
    temp3 <- dtasta$Q3_13>3
    temp4 <- dtasta$Q3_14>3
    temp5 <- dtasta$Q3_25>3
    temp6 <- dtasta$Q3_26>3
    dtasta$discrim <- rowSums(data.frame(temp1,temp2,temp3,temp4,temp5,temp6),na.rm=TRUE)
    grp <- which(dtasta$discrim>2)
    if (length(grp) != 0)
      dtasta[grp,]$flag5 <- 1
    
    grp <- which(dtasta$Q3_26>3)
    if (length(grp) != 0)
      dtasta[grp,]$flag6 <- 1
    
    grp <- which((dtasta$flag1+dtasta$flag2+dtasta$flag3+dtasta$flag4+dtasta$flag5+dtasta$flag6)>0)
    if (length(grp) != 0)
      dtasta[grp,]$control <- 1
    
    dtasta <- merge(dtasta, dtabasic(), by="id", all.x = T)
    row.names(dtasta) <- as.numeric(row.names(dtasta)) + 1
    dtasta <- dtasta[,
                     c("college", "class","id", "name", "sex", "grade", "E.mail",
                       "A1p","A2p","B1p","B2p","B3p","C1p","C2p",
                       "A1","A2","B1","B2","B3","B","C1","C2",
                       "A1na","A2na","B1na","B2na","B3na","C1na","C2na",
                       "A1s","A2s","B1s","B2s","B3s","C1s","C2s",
                       "A1m","A2m","B1m","B2m","B3m","C1m","C2m",
                       "flag1","flag2","flag3","flag4","flag5","flag6","control",
                       "Q3_11","Q3_12","Q3_13","Q3_14","Q3_25","Q3_26"
                     )]
    dtasta$sex <- factor(dtasta$sex, levels = c("男","女"))
    dtasta$college <- factor(dtasta$college, levels = c("教育學院", "人文與社會學院", "理工學院", "環境與生態學院", "藝術學院", "管理學院"))
    dtasta
  })
  
  ##pdf & 寄信表格資料
  mailshow <- reactive({
    input$pdf2 | input$mail2 | input$ref
    year <- unique(dtabasic()$year)
    
    if (!dir.exists(paste0("PDF_all/", year))){
      dir.create(paste0("PDF_all/", year))
    }
    
    if (!dir.exists("mail_log")){
      dir.create("mail_log")
    }
    
    mailshow <- dtasta()
    mailshow$pdf <- "x"
    mailshow$mail <- "x"
    
    pdfch <- gsub(".pdf", "", list.files(paste0("PDF_all/", year, "/")))
    
    if (length(pdfch) != 0 && any(mailshow$id %in% pdfch)){
      mailshow[mailshow$id %in% pdfch, ]$pdf <- "v"
    }
    
    if (file.exists(paste0("mail_log/", year, ".txt"))){
      mailch <- readLines(paste0("mail_log/", year, ".txt"))
      if (length(mailch) != 0 && any(mailshow$id %in% mailch)){
        mailshow[mailshow$id %in% mailch, ]$mail <- "v"
      }
    }
    
    mailshow
  })
  
  ##產生讀卡資料表格
  output$rawtable <- DT::renderDataTable({
    datatable(dtarawshow(), class = 'cell-border stripe', extensions = 'FixedColumns',
                              options = list(
                                fixedColumns = list(leftColumns = 2), scrollX = T, scrolly = T,
                                columnDefs = list(list(className = 'dt-center', targets = '_all'))
                              )) %>% formatStyle(
    colnames(dtarawshow())[3:ncol(dtarawshow())],
    backgroundColor = styleEqual(c('missing', 'missing(0)', 'missing(NA)'), c('lightpink', 'yellow', 'aquamarine'))
  )
  })

  ##產生基本資料表格
  output$basictable <- DT::renderDataTable({
    dtabasic <- dtabasic()
    names(dtabasic)[1:7] <- c('學院', '班級', '學號', '年度', '姓名', '性別', '年級')
    datatable(dtabasic, class = 'cell-border stripe', options = list(columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  ##產生統計結果表格
  output$statable <- DT::renderDataTable({
    stashow <- subset(dtasta(), select = c(id,
                                           A1p,A2p,B1p,B2p,B3p,C1p,C2p,
                                           A1,A2,B1,B2,B3,C1,C2,
                                           A1m,A2m,B1m,B2m,B3m,C1m,C2m))
    names(stashow)[1] <- '學號'
    datatable(stashow, class = 'cell-border stripe', extensions = 'FixedColumns',
              options = list(
                fixedColumns = list(leftColumns = 2), scrollX = T, scrolly = T,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              ))
  })
  
  ##產生寄信預覽
  output$mailtable <- DT::renderDataTable({
    mailshow <- mailshow()
    mailshow <- subset(mailshow, select = c(id,sex,college,class,pdf,E.mail,mail))
    names(mailshow) <- c('學號', '性別', '學院', '班級', "已產生報表", "E.mail", "已寄信")
    datatable(mailshow, class = 'cell-border stripe', extensions = 'FixedColumns',
              options = list(
                fixedColumns = list(leftColumns = 2), scrollX = T, scrolly = T,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>% formatStyle(
                colnames(mailshow)[c(5,7)],
                backgroundColor = styleEqual(c('x', 'v'), c('#FF99AB', '#BCD2E5'))
              )
  })
  
  
  ##產生高風險族群表格
  output$control <- DT::renderDataTable({
    grpcontrol <- dtasta() %>% filter(control == 1) %>% select(id,name,sex,college,class,flag1,flag2,flag3,flag4,flag5,flag6,A1,B,C1,C2,Q3_11,Q3_12,Q3_13,Q3_14,Q3_25,Q3_26)
    names(grpcontrol)[1:5] <- c('學號','姓名','性別','學院','班級')
    datatable(grpcontrol, class = 'cell-border stripe',
              extensions = 'FixedColumns',
              options = list(
                fixedColumns = list(leftColumns = 2), scrollX = T, scrolly = T
              )) %>%
    formatStyle(
      c('flag1','flag2','flag3','flag4','flag5','flag6'),
      backgroundColor = styleEqual( 1, 'pink')
    )
  })
  
 
  
  ##下載高風險族群資料
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("RiskGroup-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      controldown <- dtasta() %>% filter(control == 1) %>% select(id,name,sex,college,class,flag1,flag2,flag3,flag4,flag5,flag6,A1,B,C1,C2,Q3_11,Q3_12,Q3_13,Q3_14,Q3_25,Q3_26)
      names(controldown)[1:5] <- c('學號','姓名','性別','學院','班級')
      write.csv(controldown, file)
    },
    contentType = "text/csv"
  )
  
  ##pdf
  observeEvent(input$pdf2, {
    year <- unique(dtabasic()$year)
    pdfdir <- paste0("PDF_all/", year, "/")
    
    if (!dir.exists(paste0("PDF_all/", year))){
      dir.create(paste0("PDF_all/", year))
    }
    
    if (!dir.exists(paste0("PDF_riskGroup/", year))){
      dir.create(paste0("PDF_riskGroup/", year))
    }
    
    mailshow <- mailshow() %>% filter(pdf == 'x')
    
    n <- nrow(mailshow)
    if (n == 0){
      showModal(modalDialog(
        title = "系統訊息",
        "報表已全數存在，將跳過此項程序"
      ))
    }
    else{
      colors <- c(rep('#f4bc8f', 2), rep('#bfd098', 3), rep('#91c7d5',2))
      
      withProgress(message = '產生PDF...', value = 0, {
        # Number of times we'll go through the loop
        
        for (i in c(1:n)){
          
          incProgress(1/n, detail =paste(i, "/", n))
          
          dtaraw <- mailshow[i,]
          
          dtatable <- as.data.frame(matrix(dtaraw[,c(8:19,21,22,37:43)],3,7,byrow=T))
          
          
          dtatemp <- dtaraw[, c('A1p','A2p','B1p','B2p','B3p','C1p','C2p')]
          dtatemp2 <- dtaraw[, c('A1s','A2s','B1s','B2s','B3s','C1s','C2s')]
          
          names(dtatemp) <- c("A1", "A2",
                              "B1","B2","B3",
                              "C1","C2")
          dtatempl <- melt(dtatemp, NULL)
          dtatempl2 <- melt(dtatemp2, NULL)
          
          labs <- dtatempl2$value
          
          ##畫圖
          
          p <- ggplot(dtatempl, aes(x = variable, y = value))+
            geom_bar(stat = "identity", width=.8, colour = 'black',fill=colors, size = .4)+
            labs(title = "PR", x = "", y = "")+
            coord_cartesian(ylim=c(0,105))+
            scale_y_continuous(breaks = seq(0, 100, by = 25)) +
            geom_text(aes(label = labs, y= value+5))+
            guides(fill=FALSE)+ 
            theme(
              panel.background = element_rect(fill = "gray93"),
              axis.text.x = element_text(angle = 0, size = 10, hjust = 0.5)
            )
          
          
          ##RMD
          try(render(input = "asset_rmd/stupdf.Rmd", encoding = "UTF-8", output_file = paste0(workdir, "/", pdfdir, sprintf("%s.pdf", as.character(dtaraw$id)))))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        
          ##複製
          if (dtaraw$control == 1){
            file.copy(paste0(pdfdir, dtaraw$id, ".pdf"), paste0("PDF_riskGroup/", year, "/"), overwrite = T)
          }
        }
        showModal(modalDialog(
          title = "系統提示",
          "輸出完成，請刷新表格確認報表狀態"
        ))
      }
      )}
  })
  
  ##寄信
  observeEvent(input$mail2, {
    year <- unique(dtabasic()$year)
    
    mailshow <- mailshow() %>% filter(mail == 'x' & !is.na(E.mail))
    
    n <- nrow(mailshow)
    
    if (n != 0){
      mailpdf <- paste0(mailshow$id, ".pdf")
      mailpdfboo <- mailpdf %in% list.files(paste0("PDF_all/", year, "/"))
    }
    
    if (n == 0){
      showModal(modalDialog(
        title = "系統訊息",
        "信件已全數寄出，將跳過此項程序"
      ))
    }
    else if(sum(!mailpdfboo) > 0){
      showModal(modalDialog(
        title = "錯誤訊息",
        "信件所需報表尚未全數產生，請先輸出報表"
      ))
    }
    else{
      
      withProgress(message = '寄送信件...', value = 0, {
        # Number of times we'll go through the loop
        
        for (i in c(1:n)){
          
          incProgress(1/n, detail =paste(i, "/", n))
          
          dtamail <- mailshow[i,]
          
          stuname <- as.character(dtamail$name)
          id <- as.character(dtamail$id)
          email <- as.character(dtamail$E.mail)
          attachfile <- paste0("PDF_all/", year, "/", id, ".pdf")
          mailbody <- paste0(
            '嗨！',
            stuname,
            '同學您好： 
開學時輔導中心為您施測了一份心理測驗--「身心生活適應量表」。附件是這份測驗的結果及簡要的說明，可以幫助您瞭解自己的心理健康狀態。對於測驗結果，如果您還有任何不瞭解或需要進一步解說的地方，歡迎您來一趟輔導中心，我們會幫您安排專業的心理師作更為深入的解說。以下為輔導中心的相關資訊，歡迎您與我們聯繫

電話：(06)2133111轉691、692、693、698
地址：台南市中西區樹林街二段33號
府城校區：啟明苑一樓E109（平日8:00~17:30，週四 17:30~21:30）
中心網頁：http://www2.nutn.edu.tw/gac690/
臉書專頁：https://www.facebook.com/NewBreezeCenter/?fref=ts

祝福您 生活愉快！ 

臺南大學輔導中心　謹上 
2017/09/26')
          ##
          out <- tryCatch(send.mail(
            # from = "joshyue1127@gmail.com",
                                    from = "counsel@pubmail.nutn.edu.tw",
                                    to = email,
                                    subject = "臺南大學輔導中心通知--身心生活適應量表結果",
                                    body = mailbody,
                                    attach.files = attachfile,
                                    encoding = "utf-8",
                                    # smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "joshyue1127", passwd = "joshyue0209", ssl = TRUE),
                                    # smtp = list(host.name = "mail.nutn.edu.tw", port = 25, user.name = "eric",passwd = "botany0726",ssl = FALSE),
                                    smtp = list(host.name = "pubmail.nutn.edu.tw", port = 25,user.name = "counsel@pubmail.nutn.edu.tw",passwd = "guider2008",ssl = F),
                                    authenticate = TRUE,
                                    send = TRUE), error = function(e) e)
          
          ##
          if(!any(class(out) == "error")){
            cat(dtamail$id, file = paste0("mail_log/", year, ".txt"), append = T, sep = "\n")
            Sys.sleep(5)
          }
        }
        showModal(modalDialog(
          title = "系統提示",
          "寄信完成，請刷新表格確認信件狀態"
        ))
      }
      )}
  })
  
  
  
  ##按鈕
  observeEvent(input$controlpdf, {
    year <- unique(dtabasic()$year)
    if (!dir.exists(paste0("PDF_riskGroup/", year))){
      dir.create(paste0("PDF_riskGroup/", year))
    }
    shell.exec(paste0("PDF_riskGroup\\", year))
  })
  
  ##pdf & mail提示文字
  output$infotext <- renderUI({
    mailshow <- mailshow()
    if (any(mailshow$pdf == 'x')){
      pdfinfo <- "<font color=\"#FF0000\"><b>* 報表尚未全數產生</b></font>"
    }
    else{
      pdfinfo <- "<font color=\"#AEB3B7\"><b>* 報表已全數產生</b></font>"
    }

    if (any(mailshow$mail == 'x')){
      mailinfo <- "<font color=\"#FF0000\"><b>* 信件尚未全數寄出</b></font>"
    }
    else{
      mailinfo <- "<font color=\"#AEB3B7\"><b>* 信件已全數寄出</b></font>"
    }

    HTML(paste(pdfinfo, mailinfo, sep = '<br/>'))
  })
  
  
  
  
  ##點擊
  observeEvent(input$this, {
    repng2 <- gsub("Ana_png/", "", repng)
    out <- tryCatch(dtasta <- dtasta(), error = function(e) e)
    
    if (!any(class(out) == "error") && input$this == "needpng" && sum(repng2 %in% list.files("Ana_png")) != 8){
      
      dtasta <- dtasta()
      anapraw <- group_by(dtasta,.dots=c("college","sex")) %>% summarise(A1 = round(mean(A1na, na.rm = T), 2), A2 = round(mean(A2na, na.rm = T), 2), 
                                                                         B1 = round(mean(B1na, na.rm = T), 2), B2 = round(mean(B2na, na.rm = T), 2), B3 = round(mean(B3na, na.rm = T), 2),
                                                                         C1 = round(mean(C1na, na.rm = T), 2), C2 = round(mean(C2na, na.rm = T), 2),
                                                                         A1sd = round(sd(A1na, na.rm = T), 2), A2sd = round(sd(A2na, na.rm = T), 2),
                                                                         B1sd = round(sd(B1na, na.rm = T), 2), B2sd = round(sd(B2na, na.rm = T), 2), B3sd = round(sd(B3na, na.rm = T), 2),
                                                                         C1sd = round(sd(C1na, na.rm = T), 2), C2sd = round(sd(C2na, na.rm = T), 2),
                                                                         con = sum(control == 1), count = n())
      
      year <- paste0(dtabasic()$year, "學年度")
      allsub <- c("A1", "A2", "B1", "B2", "B3", "C1", "C2")
      titles <- c("身心症狀指標(A1)", "主觀健康自評(A2)", "自我資源指標(B1)", "家庭資源指標(B2)", "師友資源指標(B3)", "情緒化與焦慮分數(C1)", "憂鬱與無助分數(C2)")
      
      withProgress(message = '產生圖表中...', value = 0, {
        
        for (i in 1:7){
          incProgress(1 / 7, detail =paste(i, "/", 7))
          subs <- allsub[i]
          chtitle <- titles[i]
          mean4 <- dtasta[,paste0(subs,"na")] %>% mean(na.rm = T)
          mean3 <- mean4 * 3 / 4
          mean2 <- mean4 * 2 / 4
          mean1 <- mean4 * 1 / 4
          
          anaplot <- anapraw %>%  select(college, sex, contains(subs))
          names(anaplot) <- c("college", "sex", "value", "sd")
          anaplot$college <- gsub("學院", "", anaplot$college)
          anaplot[anaplot$college == "環境與生態",]$college <- "環生"
          anaplot[anaplot$college == "人文與社會",]$college <- "人社"
          anaplot$college <- factor(anaplot$college, levels = c("管理", "藝術", "環生", "理工", "人社", "教育"))
          
          #分量表比較圖
          png(paste0("Ana_png/", subs, ".png"), width = 1200, height = 1200)
          p2 <- ggplot(anaplot, aes(x = college, color = sex))+
            geom_linerange(data = anaplot[anaplot$sex == "男",], 
                           aes(ymin = -mean1, ymax = -mean1 - value), size = 20, alpha = 0.8) +
            geom_linerange(data = anaplot[anaplot$sex == "女",], 
                           aes(ymin = mean1, ymax = mean1 + value), size = 20, alpha = 0.8) +
            geom_errorbar(data = anaplot[anaplot$sex == "男",],
                          aes(ymin = -mean1 - value - sd, ymax = -mean1 - value + sd), width= .15, size = 1.3, alpha = 0.5, show.legend = F) +
            geom_errorbar(data = anaplot[anaplot$sex == "女",], 
                          aes(ymin = mean1 + value - sd, ymax = mean1 +value + sd), width= .15, size = 1.3, alpha = 0.5, show.legend = F) +
            geom_label(aes(x = college, y = 0, label = college), 
                       inherit.aes = F,
                       size = 8, label.padding = unit(0.4, "lines"), label.size = NA,
                       label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.9, color = "#5D646F", fontface = "italic") +
            scale_y_continuous(breaks = c(c(-mean4, -mean3, -mean2, -mean1, 0) + -mean1, c(0, mean1, mean2, mean3, mean4) + mean1),
                               labels = c(round(mean4), round(mean3), round(mean2), round(mean1), 0, 0, round(mean1), round(mean2), round(mean3), round(mean4)))+
            coord_flip()+
            labs(title = chtitle,
                 subtitle = "",
                 caption = year)+
            scale_color_manual(name = "", values = c(男 = "#3E606F", 女 = "#8C3F4D"), breaks = c('男','女'))+
            theme_minimal(base_family = "Noto")+
            theme(text = element_text(color = "#3A3F4A"),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
                  axis.title = element_blank(),
                  plot.title = element_text(face = "bold", size = 40, margin = margin(b = 10), hjust = 0.030),
                  plot.subtitle = element_text(size = 20, margin = margin(b = 20), hjust = 0.030),
                  plot.caption = element_text(size = 16, margin = margin(b = 10, t = 70), color = "#5D646F", face = "italic"),
                  axis.text.x = element_text(size = 16, color = "#5D646F", margin = margin(t = 10)),
                  axis.text.y = element_blank(),
                  plot.background = element_rect(fill = "#EFF2F4", color = "#EFF2F4"),
                  plot.margin = unit(c(2, 2, 2, 2), "cm"),
                  legend.position = "top",
                  legend.text  = element_text(size = 14),
                  legend.text.align = 0)
          print(p2)
          dev.off()
        }
        ##風險人數比較圖
        anapraw2 <- dtasta %>% group_by(college) %>% summarise(conall = sum(control == 1))
        anapraw3 <- merge(anapraw, anapraw2, by = "college") %>% select(college, sex, con, conall)
        anapraw3$college <- gsub("學院", "", anapraw3$college)
        anapraw3[anapraw3$college == "環境與生態",]$college <- "環生"
        anapraw3[anapraw3$college == "人文與社會",]$college <- "人社"
        anapraw3$college <- factor(anapraw3$college, levels = c("教育", "管理", "藝術", "環生", "理工", "人社"))
        ##
        png("Ana_png/All.png", width = 1200, height = 1200)
        p2 <- ggplot(anapraw3, aes(x = college, y = con, fill = sex)) +
          geom_col(alpha = 0.8, width = 0.65) +
          geom_text(aes(label = con), position = position_stack(vjust = 0.5), size = 15, color = "#EFF2F4", alpha = 0.15) +
          geom_text(aes(label = conall, y = conall + 0.7), size = 10, color = "#3A3F4A", alpha = .6) +
          scale_fill_manual(values = c(男 = "#3E606F", 女 = "#8C3F4D"), breaks = c("男", "女")) +
          scale_y_continuous(breaks = 0) +
          labs(title = "風險人數比較", subtitle = "", caption = year) +
          theme(text = element_text(family = "Noto", color = "#3A3F4A"),
                plot.title = element_text(face = "bold",size = 40, margin = margin(b = 10), hjust = 0.030),
                plot.subtitle = element_text(size = 20, margin = margin(b = 20), hjust = 0.030),
                plot.caption = element_text(size = 16, margin = margin(b = 10, t = 70), color = "#5D646F", face = "italic"),
                plot.background = element_rect(fill = "#EFF2F4", color = "#EFF2F4"),
                plot.margin = unit(c(2, 2, 2, 2), "cm"),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20, face = "italic", margin = margin(t = -25), color = "#5D646F"),
                axis.text.y = element_blank(),
                legend.background = element_blank(),
                legend.title = element_blank(),
                legend.text  = element_text(family = "Noto", size = 17),
                legend.key = element_blank()) +
          guides(fill = guide_legend(override.aes = list(size = 18)))
        print(p2)
        dev.off()
      })
      output$sexana <- renderImage({
        filename <- paste0("Ana_png/", input$subselect, ".png")
        list(src = filename,
             width = 700,
             height = 700)
      }, deleteFile = F)
    }
  })
   
  
  
  ##產生整體分析圖
  observeEvent(input$gene, {
    dtasta <- dtasta()
    anapraw <- group_by(dtasta,.dots=c("college","sex")) %>% summarise(A1 = round(mean(A1na, na.rm = T), 2), A2 = round(mean(A2na, na.rm = T), 2), 
                                                                      B1 = round(mean(B1na, na.rm = T), 2), B2 = round(mean(B2na, na.rm = T), 2), B3 = round(mean(B3na, na.rm = T), 2),
                                                                      C1 = round(mean(C1na, na.rm = T), 2), C2 = round(mean(C2na, na.rm = T), 2),
                                                                      A1sd = round(sd(A1na, na.rm = T), 2), A2sd = round(sd(A2na, na.rm = T), 2),
                                                                      B1sd = round(sd(B1na, na.rm = T), 2), B2sd = round(sd(B2na, na.rm = T), 2), B3sd = round(sd(B3na, na.rm = T), 2),
                                                                      C1sd = round(sd(C1na, na.rm = T), 2), C2sd = round(sd(C2na, na.rm = T), 2),
                                                                      con = sum(control == 1), count = n())
    
    year <- paste0(dtabasic()$year, "學年度")
    allsub <- c("A1", "A2", "B1", "B2", "B3", "C1", "C2")
    titles <- c("身心症狀指標(A1)", "主觀健康自評(A2)", "自我資源指標(B1)", "家庭資源指標(B2)", "師友資源指標(B3)", "情緒化與焦慮分數(C1)", "憂鬱與無助分數(C2)")
    
    withProgress(message = '產生圖表中...', value = 0, {
      
    for (i in 1:7){
      incProgress(1 / 7, detail =paste(i, "/", 7))
    subs <- allsub[i]
    chtitle <- titles[i]
    mean4 <- dtasta[,paste0(subs,"na")] %>% mean(na.rm = T)
    mean3 <- mean4 * 3 / 4
    mean2 <- mean4 * 2 / 4
    mean1 <- mean4 * 1 / 4
    
    anaplot <- anapraw %>%  select(college, sex, contains(subs))
    names(anaplot) <- c("college", "sex", "value", "sd")
    anaplot$college <- gsub("學院", "", anaplot$college)
    anaplot[anaplot$college == "環境與生態",]$college <- "環生"
    anaplot[anaplot$college == "人文與社會",]$college <- "人社"
    anaplot$college <- factor(anaplot$college, levels = c("管理", "藝術", "環生", "理工", "人社", "教育"))
    
    #分量表比較圖
    png(paste0("Ana_png/", subs, ".png"), width = 1200, height = 1200)
    p2 <- ggplot(anaplot, aes(x = college, color = sex))+
      geom_linerange(data = anaplot[anaplot$sex == "男",], 
                     aes(ymin = -mean1, ymax = -mean1 - value), size = 20, alpha = 0.8) +
      geom_linerange(data = anaplot[anaplot$sex == "女",], 
                     aes(ymin = mean1, ymax = mean1 + value), size = 20, alpha = 0.8) +
      geom_errorbar(data = anaplot[anaplot$sex == "男",],
                    aes(ymin = -mean1 - value - sd, ymax = -mean1 - value + sd), width= .15, size = 1.3, alpha = 0.5, show.legend = F) +
      geom_errorbar(data = anaplot[anaplot$sex == "女",], 
                    aes(ymin = mean1 + value - sd, ymax = mean1 +value + sd), width= .15, size = 1.3, alpha = 0.5, show.legend = F) +
      geom_label(aes(x = college, y = 0, label = college), 
                 inherit.aes = F,
                 size = 8, label.padding = unit(0.4, "lines"), label.size = NA,
                 label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.9, color = "#5D646F", fontface = "italic") +
      scale_y_continuous(breaks = c(c(-mean4, -mean3, -mean2, -mean1, 0) + -mean1, c(0, mean1, mean2, mean3, mean4) + mean1),
                         labels = c(round(mean4), round(mean3), round(mean2), round(mean1), 0, 0, round(mean1), round(mean2), round(mean3), round(mean4)))+
      coord_flip()+
      labs(title = chtitle,
           subtitle = "",
           caption = year)+
      scale_color_manual(name = "", values = c(男 = "#3E606F", 女 = "#8C3F4D"), breaks = c('男','女'))+
      theme_minimal(base_family = "Noto")+
      theme(text = element_text(color = "#3A3F4A"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
            axis.title = element_blank(),
            plot.title = element_text(face = "bold", size = 40, margin = margin(b = 10), hjust = 0.030),
            plot.subtitle = element_text(size = 20, margin = margin(b = 20), hjust = 0.030),
            plot.caption = element_text(size = 16, margin = margin(b = 10, t = 70), color = "#5D646F", face = "italic"),
            axis.text.x = element_text(size = 16, color = "#5D646F", margin = margin(t = 10)),
            axis.text.y = element_blank(),
            plot.background = element_rect(fill = "#EFF2F4", color = "#EFF2F4"),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            legend.position = "top",
            legend.text  = element_text(size = 14),
            legend.text.align = 0)
    print(p2)
    dev.off()
    }
      ##風險人數比較圖
      anapraw2 <- dtasta %>% group_by(college) %>% summarise(conall = sum(control == 1))
      anapraw3 <- merge(anapraw, anapraw2, by = "college") %>% select(college, sex, con, conall)
      anapraw3$college <- gsub("學院", "", anapraw3$college)
      anapraw3[anapraw3$college == "環境與生態",]$college <- "環生"
      anapraw3[anapraw3$college == "人文與社會",]$college <- "人社"
      anapraw3$college <- factor(anapraw3$college, levels = c("教育", "管理", "藝術", "環生", "理工", "人社"))
      ##
      png("Ana_png/All.png", width = 1200, height = 1200)
      p2 <- ggplot(anapraw3, aes(x = college, y = con, fill = sex)) +
        geom_col(alpha = 0.8, width = 0.65) +
        geom_text(aes(label = con), position = position_stack(vjust = 0.5), size = 15, color = "#EFF2F4", alpha = 0.15) +
        geom_text(aes(label = conall, y = conall + 0.7), size = 10, color = "#3A3F4A", alpha = .6) +
        scale_fill_manual(values = c(男 = "#3E606F", 女 = "#8C3F4D"), breaks = c("男", "女")) +
        scale_y_continuous(breaks = 0) +
        labs(title = "風險人數比較", subtitle = "", caption = year) +
        theme(text = element_text(family = "Noto", color = "#3A3F4A"),
              plot.title = element_text(face = "bold",size = 40, margin = margin(b = 10), hjust = 0.030),
              plot.subtitle = element_text(size = 20, margin = margin(b = 20), hjust = 0.030),
              plot.caption = element_text(size = 16, margin = margin(b = 10, t = 70), color = "#5D646F", face = "italic"),
              plot.background = element_rect(fill = "#EFF2F4", color = "#EFF2F4"),
              plot.margin = unit(c(2, 2, 2, 2), "cm"),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 20, face = "italic", margin = margin(t = -25), color = "#5D646F"),
              axis.text.y = element_blank(),
              legend.background = element_blank(),
              legend.title = element_blank(),
              legend.text  = element_text(family = "Noto", size = 17),
              legend.key = element_blank()) +
        guides(fill = guide_legend(override.aes = list(size = 18)))
      print(p2)
      dev.off()
    })
  })
  
  
  ##分量表分析圖片
  output$sexana <- renderImage({
    input$gene
    filename <- paste0("Ana_png/", input$subselect, ".png")
    list(src = filename,
         width = 700,
         height = 700)
  }, deleteFile = F)
  
  
  ##分量表分析報表
  anatable <- reactive({
    dtasta <- dtasta()
    anaraw <- group_by(dtasta,.dots=c("college","sex")) %>% summarise(A1 = round(mean(A1na, na.rm = T), 2), A2 = round(mean(A2na, na.rm = T), 2), 
                                                                      B1 = round(mean(B1na, na.rm = T), 2), B2 = round(mean(B2na, na.rm = T), 2), B3 = round(mean(B3na, na.rm = T), 2),
                                                                      C1 = round(mean(C1na, na.rm = T), 2), C2 = round(mean(C2na, na.rm = T), 2),
                                                                      A1sd = round(sd(A1na, na.rm = T), 2), A2sd = round(sd(A2na, na.rm = T), 2),
                                                                      B1sd = round(sd(B1na, na.rm = T), 2), B2sd = round(sd(B2na, na.rm = T), 2), B3sd = round(sd(B3na, na.rm = T), 2),
                                                                      C1sd = round(sd(C1na, na.rm = T), 2), C2sd = round(sd(C2na, na.rm = T), 2),
                                                                      A1n = sum(flag1 == 1), A2n = 0,
                                                                      B1n = sum(flag2 == 1), B2n = sum(flag2 == 1), B3n = sum(flag2 == 1),
                                                                      C1n = sum(flag3 == 1), C2n = sum(flag4 == 1),
                                                                      con = sum(control == 1), count = n()) %>% mutate(per = paste0(round((con / count) * 100, 1), "%"))
    
    if (input$subselect == "All"){
    anatable <- anaraw %>% select(college, sex, con, count, per) %>% melt(c("sex","college")) %>% dcast(variable~college+sex) %>% select(-variable)
    row.names(anatable) <- c("總風險人數", "總人數", "風險百分比")
    }
    else{
    anaraw <- anaraw %>%  select(college, sex, contains(input$subselect), count)
    anatable <- anaraw %>% melt(c("sex","college")) %>% dcast(variable~college+sex) %>% select(-variable) %>% apply(2, as.character) %>% as.data.frame()
    row.names(anatable) <- c("平均值", "標準差", "風險人數", "總人數")
    }
    anatable
  })
  
  ##分量表table
  output$sexana2 <- function(){
    anatable1 <- anatable()[1:6]
    names(anatable1) <- rep(c("男","女"), 3)
    anatable1 %>% kable("html", align = c(rep("c",7)))  %>%  kable_styling(bootstrap_options = c("striped", "hover", "bordered"))  %>% add_header_above(c(" " = 1, "教育學院" = 2, "人文與社會學院" = 2, "理工學院" = 2))
  }
  output$sexana3 <- function(){
    anatable2 <- anatable()[7:12]
    names(anatable2) <- rep(c("男","女"), 3)
    anatable2 %>% kable("html", align = c(rep("c",7)))  %>%  kable_styling(bootstrap_options = c("striped", "hover", "bordered"))  %>% add_header_above(c(" " = 1, "環境與生態學院" = 2, "藝術學院" = 2, "管理學院" = 2))
  }
  
  ##分量表文字資訊
  output$subinfo <- renderUI({
    sub <- input$subselect
    if (sub == 'A1'){
      subinfo <- "<font color=\"#FF0000\"><b>* 身心症狀指標的風險標準為高於平均分數 2 倍標準差</b></font>"
    }
    else if (sub == 'A2'){
      subinfo <- "<font color=\"#FF0000\"><b>* 健康自評量表沒有定義風險標準，因此風險人數皆為 0</b></font>"
    }
    else if (sub == 'B1'){
      subinfo <- "<font color=\"#FF0000\"><b>* 自我資源指標之風險人數取自綜合資源指標（B1+B2+B3）之風險人數<br/> &emsp;* 綜合資源指標的風險標準為低於平均分數 2 倍標準差</b></font>"
    }
    else if (sub == 'B2'){
      subinfo <- "<font color=\"#FF0000\"><b>* 家庭資源指標之風險人數取自綜合資源指標（B1+B2+B3）之風險人數<br/> &emsp;* 綜合資源指標的風險標準為低於平均分數 2 倍標準差</b></font>"
    }
    else if (sub == 'B3'){
      subinfo <- "<font color=\"#FF0000\"><b>* 師友資源指標之風險人數取自綜合資源指標（B1+B2+B3）之風險人數<br/> &emsp;* 綜合資源指標的風險標準為低於平均分數 2 倍標準差</b></font>"
    }
    else if (sub == 'C1'){
      subinfo <- "<font color=\"#FF0000\"><b>* 情緒化與焦慮分數的風險標準為高於平均分數 2 倍標準差</b></font>"
    }
    else if (sub == 'C2'){
      subinfo <- "<font color=\"#FF0000\"><b>* 憂鬱與無助分數的風險標準為高於平均分數 2 倍標準差</b></font>"
    }
    else if (sub == 'All'){
      subinfo <- ""
    }
    
    HTML(paste0(" &emsp;", subinfo))
  })
  
}

shinyApp(ui = ui, server = server)