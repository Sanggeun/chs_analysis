
library(shiny)

devtools::install_github("Sanggeun/chs")
library(chs)

need_pkg <- c("dplyr", "DT")
has <- need_pkg %in% rownames(installed.packages())
if(any(!has)) install.packages(need_pkg[!has])

library(dplyr)
library(DT)

index <- tribble(
  ~classification, ~index, ~de_var, ~subset_var, ~year, ~sum_year, ~var_type, ~var_cat,
  #---------
  "흡연", "현재흡연율","sm_a0100", NULL, 2011:2017, TRUE,2, 1,
  "흡연","남자현재흡연율","sm_a0100","sex_m", 2011:2017, TRUE,2, 1,
  "흡연","평생흡연율","sm_a0200",NULL, 2011:2017, TRUE,2,1,
  "흡연","흡연시작연령","sm_a0300", "sm_a0200", 2011:2017, FALSE,2, 2,
  "흡연","금연시도율","sm_d0600","sm_a0100", 2011:2017, FALSE,1, 1,
  "흡연","현재 비흡연자의 직장실내간접흡연노출률","sm_c0800","sm_e0200", c(2011,2013:2017), FALSE,2, 1,
  "음주","월간음주율","dr_a0400",NULL, 2011:2017, TRUE,2, 1,
  "음주","고위험음주율","dr_a0500",NULL, 2011:2017, TRUE,2, 1,
  "안전의식","운전시 안전벨트 착용률","sf_a0100","sfa_01z1", 2011:2017, TRUE,1, 1,
  "안전의식","동승자 안전벨트 착용률","sf_a0400","sf_a0300", 2011:2017, TRUE,1, 1,
  "안전의식", "음주운전 경험률", "sf_b0500","sf_b0400", 2011:2017, FALSE,2, 1,
  "운동 및 신체활동","중등도이상 신체활동 실천율","ph_a0500",NULL, 2011:2017, TRUE,1, 1,
  "운동 및 신체활동", "걷기 실천율", "ph_b0200", NULL, 2011:2017, TRUE,1, 1,
  "비만 및 체중조절", "비만율", "ob_a0203", NULL, 2011:2017, TRUE,2, 1,
  "비만 및 체중조절", "체중조절 시도율", "ob_b0100", NULL, 2011:2017, TRUE,1, 1,
  "비만 및 체중조절", "주관적 비만 인지율", "ob_a0300", NULL, 2011:2017, TRUE,2, 1,
  "식생활", "저염선호율(type1)", "nu_b0400", NULL, 2011:2017, TRUE,1, 1,
  "식생활", "저염선호율(type2)", "nu_b0500", NULL, 2011:2017, TRUE,1, 1,
  "식생활", "저염선호율(type3)", "nu_b0600", NULL, 2011:2017, TRUE,1, 1,
  "식생활", "영양표시독해율","nu_c0300",NULL, 2014:2017, FALSE,1, 1,
  "식생활", "5일 이상 아침식사 실천율","nu_a0204",NULL, 2011:2017, TRUE,1, 1,
  "구강건강", "저작불편 호소율", "or_b0100", "age65", 2011:2017, FALSE,2, 1,
  "구강건강", "구강검진 수진율", "or_e0500", NULL, 2011:2017, TRUE,1, 1,
  "구강건강", "점심식사후 칫솔질 실천율", "or_d0100", "or_d0050", 2011:2017, TRUE,1, 1,
  "정신건강", "스트레스 인지율", "mt_a0100", NULL, 2011:2017, TRUE,2, 1,
  "정신건강", "우울감 경험률", "mt_b0100", NULL, 2011:2017, TRUE,2, 1,
  "예방접종", "인플루엔자 예방접종률", "sc_a0100", NULL, 2011:2017, TRUE,1, 1,
  "이환_고혈압", "혈압인지율","il_a1900", NULL, c(2011,2013:2017), TRUE,1, 1,
  "이환_고혈압","고혈압 의사진단경험률(30세이상)","il_a0200","age30", 2011:2017, TRUE,2, 1,
  "이환_고혈압","고혈압 약물치료율(30세이상)","il_a0500",list("age30","il_a0200"), 2011:2017, FALSE,1, 1,
  "이환_당뇨병","혈당인지율","il_b1900",NULL, c(2011,2013:2017), TRUE,1, 1,
  "이환_당뇨병","당뇨병 의사진단경험률(30세이상)","il_b0200","age30", 2011:2017, TRUE,2, 1,
  "이환_당뇨병","당뇨병 치료율(30세이상)","il_b0600",list("age30","il_b0200"), 2011:2017, FALSE,1, 1,
  "이환_당뇨병","당뇨병 안질환 합병증검사 수진율(30세이상)","il_b0900",list("age30","il_b0200"), 2011:2017, FALSE,1, 1,
  "이환_당뇨병","당뇨병 신장질환 합병증검사 수진율(30세이상)","il_b1000",list("age30","il_b0200"), 2011:2017, FALSE,1, 1,
  "이환_이상지질혈증","이상지질혈증 의사진단경험률(30세이상)","il_r0100","age30", 2011:2017, TRUE,2, 1,
  "이환_관절염","관절염 의사진단경험률(50세이상)","il_g0100","age50", 2011:2017, FALSE,2, 1,
  "의료이용","필요의료서비스 미치료율","sr_a0100", NULL, 2012:2017, TRUE,2, 1,
  "활동제한 및 삶의 질","양호한 주관적건강 인지율","ql_a0100",NULL, 2011:2017, TRUE,1, 1,
  "보건기관 이용","보건기관 이용률","ct_a0100",NULL, 2011:2017, TRUE,1, 1
)

# 영역별 변수
com_va <- c('josa_year','CITY_CD','BOGUN_CD','dong_p','JIJUM_CD','dong_type',
            'house_type','wt_house','wt','sod_02z2')
indi_va <- c("age","age_10","sex","gender","educ","job","generation","town")

smoking <- c("sm_a0100","sm_a0200","sm_a0300","sm_d0600","sex_m", "sm_c0800","sm_e0200")
dringking <- c("dr_a0400","dr_a0500")
safety <- c("sf_a0100","sf_a0400","sf_b0500","sf_b0400","sfa_01z1","sf_a0300")
physical_act <- c("ph_a0500","ph_b0200")
obesity <- c("ob_a0203","ob_b0100","ob_a0300")
diet <- c("nu_b0400","nu_b0500","nu_b0600","nu_c0300","nu_a0204")
dental_health <- c("or_b0100","or_e0500","or_d0100","ord_01d2","or_d0050","age65")
mental_health <- c("mt_a0100","mt_b0100")
vaccination <- "sc_a0100"
illness <- c("il_a1900","il_a0200","il_a0500","il_b1900","il_b0200","il_b0600","il_b0900","il_b1000",
             "il_r0100","il_g0100","age30","age50","age65")
med_use <- "sr_a0100"
qol <- "ql_a0100"
pub_use <- "ct_a0100"

analy_va <- c(smoking, dringking, safety, physical_act, obesity, diet, dental_health, mental_health, vaccination, 
              illness, med_use, qol, pub_use)

ui <- navbarPage("chs subgroup analysis",
   
   tabPanel("Read chs data",
     sidebarLayout(
      sidebarPanel(
        selectInput("year1", "조사연도", 
                    choices=c("2011","2012","2013","2014","2015","2016","2017")),
        fileInput("data1", "데이터선택",buttonLabel = "데이터선택"),
        
        selectInput("year2", "조사연도", 
                    choices=c("2011","2012","2013","2014","2015","2016","2017")),
        fileInput("data2", "데이터선택",buttonLabel = "데이터선택"),
        
        selectInput("year3", "조사연도", 
                    choices=c("2011","2012","2013","2014","2015","2016","2017")),
        fileInput("data3", "데이터선택",buttonLabel = "데이터선택"),
        
        selectInput("year4", "조사연도", 
                    choices=c("2011","2012","2013","2014","2015","2016","2017")),
        fileInput("data4", "데이터선택",buttonLabel = "데이터선택"),
        
        checkboxGroupInput("select_data", "병합데이터", 
                           choices=c("year1", "year2", "year3", "year4")),
        actionButton("join_data","데이터병합"),
        
        tags$hr(),
        textInput("filename", "파일명", value="chsdata"),
        
        downloadButton("downloadData_1", "Download")
                
      ),
      mainPanel(
        mainPanel(
          DT::dataTableOutput("table1")
      )
     )
    )
   ),
   tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  buttonLabel = "파일선택"),
        actionButton("district_var","권역변수(대구) 넣기")
        
      ),
      mainPanel(
        DT::dataTableOutput("table2")
      )
    )
     
   ),
   
   tabPanel("analysis",
     sidebarLayout(
       sidebarPanel(
         checkboxGroupInput("var_select", "분석지표", 
                            choiceNames = paste0(index[[1]], " : ",index[[2]]),
                            choiceValues = 1:length(index[[1]])),
         
         textInput("subvar","층화변수", value = "dong_p"),
         actionButton("analysis","Result"),
         
         tags$hr(),
         textInput("filename2", "분석결과", value="chsresult"),
         
         downloadButton("downloadData_2", "Download")
         
       ),
       mainPanel(
         DT::dataTableOutput("result") 
        
       )
     )
     
   )
   
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  table1 <- reactive({
    req(input$data1)
    req(input$year1)
    data <- data_coding(dataread_chs_raw(year = input$year1, 
                                         filename = input$data1$datapath), year = input$year1) %>%
      select(com_va, indi_va, analy_va)
    return(data)
  })
  
  table2 <- reactive({
    req(input$data2)
    req(input$year2)
    data <- data_coding(dataread_chs_raw(year = input$year2, 
                                         filename = input$data2$datapath), year = input$year2) %>%
      select(com_va, indi_va, analy_va)
    return(data)
  })
  
  table3 <- reactive({
    req(input$data3)
    req(input$year3)
    data <- data_coding(dataread_chs_raw(year = input$year3, 
                                         filename = input$data3$datapath), year = input$year3) %>%
      select(com_va, indi_va, analy_va)
    return(data)
  }) 
  
  table4 <- reactive({
    req(input$data4)
    req(input$year4)
    data <- data_coding(dataread_chs_raw(year = input$year4, 
                                         filename = input$data4$datapath, data3 = TRUE), year = input$year4) %>%
      select(com_va, indi_va, analy_va)
    return(data)
  }) 
  
  data_join <- reactive({
    req(input$join_data)
    isolate({
      if(all(c("year1", "year2", "year3", "year4") %in% input$select_data)) {
        data_join <- bind_rows(
          table1(), table2(), table3(), table4()
        )  
      }
    return(data_join)  
  })
  })
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    req(input$join_data)
    data_join()
  }))
  
  output$downloadData_1 <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_join(), file, row.names = FALSE, fileEncoding = "euc-kr")
    }
        
  )
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    req(input$file1)
    data_set <<- read.csv(input$file1$datapath, fileEncoding = "euc-kr")
    return(data_set)
    }))
   
  analysis_result <- reactive({
    req(input$var_select)
    req(input$subvar)
    for (i in input$var_select) {
    re <- chs_c_rate_by(data_set, input$subvar, 
                        de_var = unlist(index[i,3]),
                        index_subset = unlist(index[i,4]),
                        josa_year = unlist(index[i,5]),
                        var_cat = unlist(index[i,8]))
    
    if (i == input$var_select[1]) {
      result <- data.frame(index = unlist(index[i,2]), re, stringsAsFactors = FALSE)
    } else {
      result <- bind_rows(list(result,
                               data.frame(index = unlist(index[i,2]), re, stringsAsFactors = FALSE)))
    }
    }
    return(result)
  })
    
    
  output$result <- DT::renderDataTable(DT::datatable({  
    req(input$analysis)
    isolate({
      return(analysis_result())
    })
  }))
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste0(input$filename2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysis_result(), file, row.names = FALSE, fileEncoding = "euc-kr")
    }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

