
  #### 흡연영역

  #### 평생흡연율, 흡연시작연령
  #### 분모정의
  data_set$sm_a0200 <- ifelse(data_set$sma_01z2 == 1, 1,
                       ifelse(data_set$sma_01z2 == 2, 0, NA))
  #### 분자정의
  data_set$sm_a0300 <- ifelse(data_set$sma_01z2 == 1,
                              ifelse((data_set$sma_02z1>=0 & data_set$sma_02z1<=110) &
                                    (data_set$sma_02z1>=0 & data_set$sma_02z1<=data_set$age), data_set$sma_02z1, NA))
  
  #### 가정실내 간접흡연 노출률
  data_set$sm_c0700 <- ifelse(data_set$smc_08z1 == 1,
                              ifelse(data_set$smc_09z1 == 1, 1,
                              ifelse(data_set$smc_09z1 == 2, 0,
                       ifelse(data_set$smc_08z1 == 1, 0, NA))))
  
  #### 직장실내 간접흡연 노출률
  data_set$sm_c0800 <- ifelse(data_set$smc_10z1 == 1, 1,
                       ifelse(data_set$smc_10z1 == 2, 0,
                       ifelse(data_set$smc_10z1 == 3, .,
                       ifelse(data_set$soa_06z1 %in% c(10:13), ., NA))))
  
  #### 평생 전자담배 흡연율
  data_set$sm_a0800 <- ifelse(data_set$sma_08z1 == 1, 1,
                       ifelse(data_set$sma_08z1 == 2, 0, NA))
  
  #### 현재 전자담배 흡연율
  data_set$sm_a0900 <- ifelse(data_set$sma_08z1 == 1,
                              ifelse(data_set$sma_09z1 == 1, 1,
                              ifelse(data_set$sma_09z1 == 2, 0,
                       ifelse(data_set$sma_08z1 == 2, 0, NA))))
  
  #### 과거흡연자의 평균 금연기간
  #### 분모정의
  data_set$sm_d2300 <- ifelse(data_set$sma_01z2 == 1,
                              ifelse(data_set$sma_03z2 == 3, 1,
                              ifelse(data_set$sma_03z2 %in% c(1,2), 0,
                       ifelse(data_set$sma_01z2 == 2, 0, NA))))
  #### 분자정의
  data_set$sm_d2400 <- ifelse(data_set$sm_d2300 == 1,
                              ifelse(data_set$smb_07z1 == 0, data_set$smb_08z1,
                              ifelse(data_set$smb_07z1 != 0, data_set$smb_07z1*12 + data_set$smb_08z1, NA)))
  
  #### 구강건강영역
  
  #### 틀니 이용 행태(50세 이상)
  #### 분모정의
  data_set$age_o50 <- ifelse(data_set$age>=50 & data_set$age<=110, 1, NA)
  ####분자정의
  data_set$or_c0101 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 1, 1, NA))
  data_set$or_c0102 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 2, 2, NA))
  data_set$or_c0103 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 3, 3, NA))
  data_set$or_c0104 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 4, 4, NA))
  
  