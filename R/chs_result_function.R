if(!"survey" %in% row.names(installed.packages())) install.packages("survey")
if(!"dplyr" %in% row.names(installed.packages())) install.packages("dplyr")
library(survey)
library(dplyr)

chs_c_rate <- function(data, by_var = "sex" , de_var = "sm_a0100", cluster = "JIJUM_CD", 
                       strata=c("BOGUN_CD","dong_type","house_type"), index_subset = NULL,
                       josa_year , index_year = 2011:2014,
                       weight="wt",digits=1) {

  # 지표별 조사연도(조사되지 않은 연도가 있음)
  if (!all(index_year %in% josa_year)) {
    data <- data[data$josa_year %in% josa_year,]
  }
  
  # 지표별 분모의 조건
  if (!is.null(index_subset)) {
    if(length(index_subset) == 1) {
      data <- data %>%
        filter(data[[index_subset]]==1)
    } else if (length(index_subset == 2)) {
      data <- data %>%
        filter(data[[index_subset[1]]]==1, data[[index_subset[2]]]==1) 
    }
  }
  
  data$target <- data[[de_var]]
  chs_design_1 <- svydesign(id=as.formula(paste0("~",cluster)), 
                            strata = as.formula(paste0("~",paste(strata, collapse = "+"))), 
                            weights = as.formula(paste0("~",weight)), nest = TRUE ,data=data)
  options(survey.lonely.psu = "adjust")
  
  #n < 50 : Don't analyze(low reliability) 
  
  if (NROW(data) >= 50) {
    
    a <- svymean(~factor(target), chs_design_1, target==1, se=T, na.rm=T, deff=T, ci=T, keep.vars=T)
    b <- margin.table(table(data[[de_var]]))
    c <- table(data[[de_var]])
    
    result <- c(
      by_var = data[[by_var]][1],
      total = b,
      n = c[2],
      crude_rate = formatC(round(coef(a)[2]*100,digits),digits,format = "f"),
      low_CI = formatC(round(confint(a)[2]*100,digits),digits,format = "f"),
      upper_cI = formatC(round(confint(a)[4]*100,digits),digits,format = "f")
      
    )
    
  } else {
    result <- c(by_var = data[[by_var]][1],
                total = NROW(data),
                rep(NA,4)
                )
  }
  return(result)
}

# by var

chs_c_rate_by <- function(data, by_var, ...) {
  
  by_var_class <- unique(data[[by_var]])
  
  result_by_var <- matrix(nrow = length(by_var_class), ncol = 6, byrow = TRUE)
  
  k <- 0
  for (i in by_var_class) {
    k <- k + 1
    ## 지역사회건강조사 결과
    data_set_tmp <- data %>% filter(data[[by_var]]== i)  
    chs_rate <- chs_c_rate(data_set_tmp, by_var = by_var, ...)
    result_by_var[k,] <- chs_rate
  }
  colnames(result_by_var) <- c("By","Total","N","c.rate","LowCI","UpperCI")
  
  return(result_by_var)
}







chs_by_age_sex <- function(data_set,de_var="ph_a0500",var_name,cluster="JIJUM_CD", by_var = c("sex","age_10"),
                                strata=c("BOGUN_CD","dong_type","house_type"),weight="wt",digits=1) {
  # 지표별 분모지정_하위 데이터셋 만들기  
  if (var_name == "남자현재흡연율") {
    data_set <- data_set[which(data_set$sex ==1), ]
  } else if (var_name == "금연시도율") {
    data_set <- data_set[which(data_set$sm_a0100==1), ]
  } else if (var_name == "현재 비흡연자의 직장실내간접흡연노출률") {
    data_set <- data_set[which(data_set$sm_e0200==1 & data_set$josa_year!=2012), ]
  } else if (var_name == "운전시 안전벨트 착용률") {
    data_set <- data_set[which(data_set$sfa_01z1==1), ]
  } else if (var_name == "동승자 안전벨트 착용률") {
    data_set <- data_set[which(data_set$sf_a0300==1), ]
  } else if (var_name == "음주운전 경험률") {
    data_set <- data_set[which(data_set$sf_b0400==1), ]
  } else if (var_name == "영양표시독해율") {
    data_set <- data_set[which(data_set$josa_year %in% c(2014,2015,2016)), ]
  } else if (var_name == "저작불편 호소율") {
    data_set <- data_set[which(data_set$age>=65), ]
  } else if (var_name == "점심식사후 칫솔질 실천율") {
    data_set <- data_set[which(data_set$ord_01d2 %in% c(1,2)), ]
  } else if (var_name == "고혈압 의사진단경험률(30세이상)") {
    data_set <- data_set[which(data_set$age>= 30), ]
  } else if (var_name == "고혈압 약물치료율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_a0200==1), ]
  } else if (var_name == "당뇨병 의사진단경험률(30세이상)") {
    data_set <- data_set[which(data_set$age>= 30), ]
  } else if (var_name == "당뇨병 치료율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_b0200==1), ]
  } else if (var_name == "당뇨병 안질환 합병증검사 수진율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_b0200==1), ]
  } else if (var_name == "당뇨병 신장질환 합병증검사 수진율(30세이상)") {
    data_set <- data_set[which(data_set$age>=30 & data_set$il_b0200==1), ]
  } else if (var_name == "이상지질혈증 의사진단경험률(30세이상)") {
    data_set <- data_set[which(data_set$age>=30), ]
  } else if (var_name == "관절염 의사진단경험률(50세이상)") {
    data_set <- data_set[which(data_set$age >= 50), ]
  } else if (var_name == "필요의료서비스 미치료율") {
    data_set <- data_set[which(data_set$josa_year %in% c(2012,2013,2014,2015,2016)), ]
  } 
  
  
  
  data_tmp <- data_set
  data_tmp$target <- data_tmp[[de_var]]
  chs_design_1 <- svydesign(id=as.formula(paste0("~",cluster)), strata = as.formula(paste0("~",paste(strata, collapse = "+"))), 
                            weights = as.formula(paste0("~",weight)), nest = TRUE ,data=data_tmp)
  by_formula <- as.formula(paste0("~", paste(by_var, collapse = " + ")))
  
  options(survey.lonely.psu = "adjust")
  
  asr<-matrix(nrow=12,ncol=3,byrow=TRUE) 
  a<-svyby(as.formula(paste0("~",de_var)),by_formula ,chs_design_1,svymean,se=T,na.rm=T,deff=T,ci=T,keep.vars=T)
  b<-as.matrix(coef(a))
  
  return(b[,1])
  
}

# 1999-2016 pop

fix_age_class_tochs <- function(data_set, class_var='age', value_var=c('pop_all','pop_male','pop_female'), 
                                josa_year, fix_ver = 'death') {
  
  age_range_1999 <- c("0 - 4세", "5 - 9세","10 - 14세", "15 - 19세", "20 - 24세", "25 - 29세", "30 - 34세", "35 - 39세",
                      "40 - 44세", "45 - 49세", "50 - 54세", "55 - 59세", "60 - 64세", "65 - 69세", "70 - 74세",
                      "75 - 79세", "80 - 84세", "85 - 89세", "90 - 94세","95+" )
  
  age_range_2011 <- c(age_range_1999, "95 - 99세", "100+")
  
  chs_class_range <-  c("1.19-29","2.19-29","1.30-39","2.30-39","1.40-49","2.40-49",
                        "1.50-59", "2.50-59", "1.60-69", "2.60-69", "1.70이상", "2.70이상")
  
  if(josa_year %in% 2011:2017) {
    
    pop <- data_set[data_set[[class_var]] %in% age_range_2011,
                    c(class_var,value_var)]
    
    male.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[2]] + pop[pop[[class_var]]=='25 - 29세',value_var[2]] 
    female.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[3]] + pop[pop[[class_var]]=='25 - 29세',value_var[3]] 
    male.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[2]] + pop[pop[[class_var]]=='35 - 39세',value_var[2]] 
    female.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[3]] + pop[pop[[class_var]]=='35 - 39세',value_var[3]] 
    male.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[2]] + pop[pop[[class_var]]=='45 - 49세',value_var[2]] 
    female.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[3]] + pop[pop[[class_var]]=='45 - 49세',value_var[3]] 
    male.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[2]] + pop[pop[[class_var]]=='55 - 59세',value_var[2]] 
    female.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[3]] + pop[pop[[class_var]]=='55 - 59세',value_var[3]] 
    male.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[2]] + pop[pop[[class_var]]=='65 - 69세',value_var[2]] 
    female.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[3]] + pop[pop[[class_var]]=='65 - 69세',value_var[3]] 
    
    male.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[2]] + pop[pop[[class_var]]=='75 - 79세',value_var[2]] +
               pop[pop[[class_var]]=='80 - 84세',value_var[2]] + pop[pop[[class_var]]=='85 - 89세',value_var[2]] +
               pop[pop[[class_var]]=='90 - 94세',value_var[2]] + pop[pop[[class_var]]=='95 - 99세',value_var[2]] + 
               pop[pop[[class_var]]=='100+',value_var[2]]
      
    female.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[3]] + pop[pop[[class_var]]=='75 - 79세',value_var[3]] +
                 pop[pop[[class_var]]=='80 - 84세',value_var[3]] + pop[pop[[class_var]]=='85 - 89세',value_var[3]] +
                 pop[pop[[class_var]]=='90 - 94세',value_var[3]] + pop[pop[[class_var]]=='95 - 99세',value_var[3]] + 
                 pop[pop[[class_var]]=='100+',value_var[3]]
    
    result_pop <- c(male.20_29, female.20_29, male.30_39, female.30_39, male.40_49, female.40_49, 
                    male.50_59, female.50_59, male.60_69, female.60_69, male.70, female.70)
    
  } else if (josa_year %in% 1999:2010) {
    pop <- data_set[data_set[[class_var]] %in% age_range_1999,
                    c(class_var,value_var)]
    
    male.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[2]] + pop[pop[[class_var]]=='25 - 29세',value_var[2]] 
    female.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[3]] + pop[pop[[class_var]]=='25 - 29세',value_var[3]] 
    male.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[2]] + pop[pop[[class_var]]=='35 - 39세',value_var[2]] 
    female.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[3]] + pop[pop[[class_var]]=='35 - 39세',value_var[3]] 
    male.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[2]] + pop[pop[[class_var]]=='45 - 49세',value_var[2]] 
    female.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[3]] + pop[pop[[class_var]]=='45 - 49세',value_var[3]] 
    male.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[2]] + pop[pop[[class_var]]=='55 - 59세',value_var[2]] 
    female.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[3]] + pop[pop[[class_var]]=='55 - 59세',value_var[3]] 
    male.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[2]] + pop[pop[[class_var]]=='65 - 69세',value_var[2]] 
    female.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[3]] + pop[pop[[class_var]]=='65 - 69세',value_var[3]] 
    
    male.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[2]] + pop[pop[[class_var]]=='75 - 79세',value_var[2]] +
               pop[pop[[class_var]]=='80 - 84세',value_var[2]] + pop[pop[[class_var]]=='85 - 89세',value_var[2]] +
               pop[pop[[class_var]]=='90 - 94세',value_var[2]] + pop[pop[[class_var]]=='95+',3]
    
    female.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[3]] + pop[pop[[class_var]]=='75 - 79세',value_var[3]] +
                 pop[pop[[class_var]]=='80 - 84세',value_var[3]] + pop[pop[[class_var]]=='85 - 89세',value_var[3]] +
                 pop[pop[[class_var]]=='90 - 94세',value_var[3]] + pop[pop[[class_var]]=='95+',value_var[3]]
    
    result_pop <- c(male.20_29, female.20_29, male.30_39, female.30_39, male.40_49, female.40_49, 
                    male.50_59, female.50_59, male.60_69, female.60_69, male.70, female.70)
   
  }
  
  names(result_pop) <- chs_class_range
  result_pop <- t(as.tibble(result_pop))
  return(result_pop)
  
}
