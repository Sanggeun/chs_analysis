  
  #### 흡연영역

  #### 평생 흡연자
  data_set$sm_a0200 <- ifelse(data_set$sma_01z2 == 1, 1,
                       ifelse(data_set$sma_01z2 == 2, 0, NA))
  
  #### 흡연시작연령
  data_set$sm_a0300 <- ifelse(data_set$sma_01z2 == 1,
                              ifelse((data_set$sma_02z1>=0 & data_set$sma_02z1<=110) &
                                     (data_set$sma_02z1>=0 & data_set$sma_02z1<=data_set$age), data_set$sma_02z1, NA), NA)
  
  #### 매일 흡연자
  data_set$sm_a0400 <- ifelse(data_set$sma_01z2 == 1, 
                              ifelse(data_set$sma_03z2 == 1, 1,
                              ifelse(data_set$sma_03z2 %in% c(2,3),0, NA)))
  
  #### 매일흡연자의 하루평균흡연량
  data_set$sm_b0100 <- ifelse(data_set$smb_01z1>=1 & data_set$smb_01z1<=776, data_set$smb_01z1, NA)
  
  #### 매일흡연자의 하루평균흡연량_10개비 미만
  data_set$sm_b0201 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 9, 1,
                       ifelse(data_set$smb_01z1 >= 10 & data_set$smb_01z1 <= 776, 0, NA))
  
  #### 매일흡연자의 하루평균흡연량_10~19개비
  data_set$sm_b0202 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 9, 0,
                       ifelse(data_set$smb_01z1 >= 10 & data_set$smb_01z1 <= 19, 1,
                       ifelse(data_set$smb_01z1 >= 20 & data_set$smb_01z1 <= 776, 0, NA)))
  
  #### 매일흡연자의 하루평균흡연량_20~39개비
  data_set$sm_b0203 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 19, 0,
                       ifelse(data_set$smb_01z1 >= 20 & data_set$smb_01z1 <= 39, 1,
                       ifelse(data_set$smb_01z1 >= 40 & data_set$smb_01z1 <= 776, 0, NA)))

  #### 매일흡연자의 하루평균흡연량_40개비 이상
  data_set$sm_b0204 <- ifelse(data_set$smb_01z1 >= 1 & data_set$smb_01z1 <= 39, 0,
                       ifelse(data_set$smb_01z1 >= 40 & data_set$smb_01z1 <= 776, 1, NA))
  
  #### 가정실내 간접흡연 노출률
  data_set$sm_c0100 <- ifelse(data_set$smc_03z1 == 1, 0,
                       ifelse(data_set$smc_03z1 %in% c(2,3), 1, NA))
  
  #### 가정실내 간접흡연 노출시간_0시간
  data_set$sm_c0201 <- ifelse(data_set$smc_03z1 == 1, 1,
                       ifelse(data_set$smc_03z1 %in% c(1:3), 0, NA))
  
  #### 가정실내 간접흡연 노출시간_1시간미만
  data_set$sm_c0202 <- ifelse(data_set$smc_03z1 == 2, 1,
                       ifelse(data_set$smc_03z1 %in% c(1:3), 0, NA))
  
  #### 가정실내 간접흡연 노출시간_1시간이상
  data_set$sm_c0203 <- ifelse(data_set$smc_03z1 == 3, 1,
                       ifelse(data_set$smc_03z1 %in% c(1:3), 0, NA))
  
  #### 직장실내 간접흡연 노출률
  data_set$sm_c0300 <- ifelse(data_set$smc_05z1 %in% c(1:3),
                              ifelse(data_set$smc_05z1 == 1, 0,
                              ifelse(data_set$smc_05z1 %in% c(2,3), 1, NA)))
  
  #### 직장실내 간접흡연 노출시간_0시간
  data_Set$sm_c0401 <- ifelse(data_set$smc_05z1 == 1, 1,
                       ifelse(data_set$sm_c0300 %in% c(0,1), NA))
  
  #### 직장실내 간접흡연 노출시간_1시간미만
  data_Set$sm_c0402 <- ifelse(data_set$smc_05z1 == 2, 1,
                       ifelse(data_set$sm_c0300 %in% c(0,1), NA))
  
  #### 직장실내 간접흡연 노출시간_1시간이상
  data_Set$sm_c0403 <- ifelse(data_set$smc_05z1 == 3, 1,
                       ifelse(data_set$sm_c0300 %in% c(0,1), NA))
  
  #### 공공장소 간접흡연 노출률
  data_set$sm_c0500 <- ifelse(data_set$smc_07z1 == 1, 1,
                       ifelse(data_set$smc_07z1 == 2, 0, NA))
  
  #### 흡연자의 1개월 내 금연계획률
  data_set$sm_d0500 <- ifelse(data_set$smd_01z2 == 1, 1,
                       ifelse(data_set$smd_01z2 %in% c(2:4), 0, NA))

  #### 흡연자의 금연시도율
  data_set$sm_d0600 <- ifelse(data_set$smd_02z2 == 1, 1,
                       ifelse(data_set$smd_02z2 %in% c(2,3), 0, NA))
  
  #### 금연교육 경험률
  data_set$sm_d1000 <- ifelse(data_set$smd_07z1 == 1, 1,
                       ifelse(data_set$smd_07z1 == 2, 0, NA))
  
  #### 금연구역 인지율
  data_set$sm_d1100 <- ifelse(data_set$smd_08z1 %in% c(1,2), 1,
                       ifelse(data_set$smd_08z == 3, 0, NA))
  
  #### 금연구역 내 흡연 경험률_분모 정의
  data_set$sm_d1200 <- ifelse(data_set$sm_a0100 == 1 & data_set$smd_08z1 == 1, 1, 0)
  
  #### 금연구역 내 흡연 경험률_분자 정의
  data_set$sm_d1300 <- ifelse(data_set$smd_09z1 %in% c(1,2), 1,
                       ifelse(data_set$smd_09z1 == 3, 0, NA))
  
  #### 음주영역
  
  #### 평생음주율
  data_set$dr_a0100 <- ifelse(data_set$dra_01z1 == 1, 1,
                       ifelse(data_set$dra_01z1 ==2, 0, NA))
  
  #### 음주시작연령
  data_set$dr_a0200 <- ifelse(data_set$dra_01z1 == 1,
                              ifelse((data_set$dra_02z1>=0 & data_set$dra_02z1<=110) &
                                     (data_set$dra_02z1>=0 & data_set$dra_02z1<=data_set$age), data_set$dra_02z1, NA), NA)
  
  #### 연간음주율
  data_set$dr_a0300 <- ifelse(data_set$dra_01z1 == 1,
                              ifelse(data_set$drb_02z1 == 1, 1,
                              ifelse(data_set$drb_02z1 == 2, 0, NA)),
                       ifelse(data_set$dra_01z1 == 2, 0, NA))
  
  #### 월간음주율
  data_set$dr_a0400 <- ifelse(data_set$dra_01z1 == 1,
                              ifelse(data_set$drb_02z1 == 1,
                                     ifelse(data_set$drb_01z2 == 1, 0,
                                     ifelse(data_set$drb_01z2 %in% c(2:5), 1,
                              ifelse(data_set$drb_02z1 == 2, 0, NA)))),
                       ifelse(data_set$dra_01z1 == 2, 0, NA))
  
  #### 음주문제로 인한 상담경험률
  data_set$dr_c0100 <- ifelse(data_set$drb_13z1 == 1, 1,
                       ifelse(data_set$drb_13z1 == 2, 0, NA))
  
  #### 안전의식영역
  
  #### 연간 자동차 음주운전 경험횟수_분모정의
  data_set$df_b0100 <- ifelse(data_set$sfa_01z1 == 1 & data_set$sfb_05z2 == 1, 1, 0)
  
  #### 연간 자동차 음주운전 경험횟수_분자정의
  data_set$sf_b0200 <- ifelse(data_set$sfb_06z1 %in% c(1:365), data_set$sfb_06z1, .)
  
  #### 연간 자동차 음주운전차량 동승률
  data_set$sf_b0300 <- ifelse(data_set$sfb_07z1 == 1, 1,
                       ifelse(data_set$sfb_07z1 == 2, 0, NA))
  
  #### 연간 오토바이 음주운전 경험 횟수_분모정의
  data_set$sf_b0600 <- ifelse(data_set$sfa_05z1 == 1 & data_set$sfb_03z2 == 1, 1, 0)
  
  #### 연간 오토바이 음주운전 경험 횟수_분자정의
  data_set$sf_b0700 <- ifelse(data_set$sfb_04z1 %in% c(1:365), data_set$stb_04z1, .)
  
  #### 오토바이 헬멧 착용률_분모정의
  data_set$sf_c0100 <- ifelse(data_set$sfa_05z1 == 1, 1,
                       ifelse(data_set$sfa_05z1 == 2, 0, NA))
  
  #### 오토바이 헬멧 착용률_분자정의
  data_set$sf_c0200 <- ifelse(data_set$sfa_06z1 %in% c(1:4), 0,
                       ifelse(data_set$sfa_06z1 == 5, 1, NA))
  
  #### 자전거 헬멧 착용률_분모정의
  data_set$sf_c0300 <- ifelse(data_set$sfa_07z1 == 1, 1,
                       ifelse(data_set$sfa_07z1 == 2, 0, NA))
  
  #### 자전거 헬멧 착용률_분자정의
  data_set$sf_c0400 <- ifelse(data_set$sfa_08z1 %in% c(1:4), 0,
                       ifelse(data_set$sfa_08z2 == 5, 1, NA))
  
  #### 스쿨존 인지율
  data_set$sf_a0500 <- ifelse(data_set$sfa_01z1 == 1,
                              ifelse(data_set$sfa_09z1 == 1, 1,
                              ifelse(data_set$sfa_09z1 == 2, 0, NA)), NA)
  
  #### 스쿨존 과속운전 경험률_분모정의
  data_set$sf_a0600 <- ifelse(data_set$sf_a0500 == 1 & data_set$sfa_10z1 %in% c(1,2), 1, NA)
  
  #### 스쿨존 과속운전 경험률_분자정의
  data_set$sf_a0700 <- ifelse(data_set$sf_a0600 == 1,
                              ifelse(data_set$sfa_10z1 == 1, 1,
                              ifelse(data_set$sfa_10z1 == 2, 0, NA)), NA)
  
  #### 1339 전화번호 인지율
  data_set$sf_d0100 <- ifelse(data_set$sfa_11z1 == 1, 1,
                       ifelse(data_set$sfa_11z1 == 2, 0, NA))
  
  #### 신체활동영역
  
  #### 유연성 운동 실천빈도
  data_set$ph_a1001 <- ifelse(data_set$pha_10z1 %in% c(1:6),
                              ifelse(data_set$pha_10z1 == 1, 1, NA))
  data_set$ph_a1002 <- ifelse(data_set$pha_10z1 %in% c(1:6),
                              ifelse(data_set$pha_10z1 == 2, 2, NA))
  data_set$ph_a1003 <- ifelse(data_set$pha_10z1 %in% c(1:6),
                              ifelse(data_set$pha_10z1 == 3, 3, NA))
  data_set$ph_a1004 <- ifelse(data_set$pha_10z1 %in% c(1:6),
                              ifelse(data_set$pha_10z1 == 4, 4, NA))
  data_set$ph_a1005 <- ifelse(data_set$pha_10z1 %in% c(1:6),
                              ifelse(data_set$pha_10z1 == 5, 5, NA))
  data_set$ph_a1006 <- ifelse(data_set$pha_10z1 %in% c(1:6),
                              ifelse(data_set$pha_10z1 == 6, 6, NA))
  
  #### 근력 운동 실천율
  data_set$ph_a1100 <- ifelse(data_set$pha_11z1 %in% c(1:6), data_set$pha_11z1>=3, NA)
  
  #### 주중 여가시간에 앉아서 보내는 시간
  data_set$ph_a1201 <- ifelse(data_set$pha_12z1 %in% c(1:5),
                              ifelse(data_set$pha_12z1 == 1, 1, NA))
  data_set$ph_a1202 <- ifelse(data_set$pha_12z1 %in% c(1:5),
                              ifelse(data_set$pha_12z1 == 2, 2, NA))
  data_set$ph_a1203 <- ifelse(data_set$pha_12z1 %in% c(1:5),
                              ifelse(data_set$pha_12z1 == 3, 3, NA))
  data_set$ph_a1204 <- ifelse(data_set$pha_12z1 %in% c(1:5),
                              ifelse(data_set$pha_12z1 == 4, 4, NA))
  data_set$ph_a1205 <- ifelse(data_set$pha_12z1 %in% c(1:5),
                              ifelse(data_set$pha_12z1 == 5, 5, NA))
  
  #### 주말 여가시간에 앉아서 보내는 시간
  data_set$ph_a1301 <- ifelse(data_set$pha_13z1 %in% c(1:5),
                              iflese(data_set$pha_13z1 == 1, 1, NA))
  data_set$ph_a1302 <- ifelse(data_set$pha_13z1 %in% c(1:5),
                              ifelse(data_set$pha_13z1 == 2, 2, NA))
  data_set$ph_a1303 <- ifelse(data_set$pha_13z1 %in% c(1:5),
                              ifelse(data_set$pha_13z1 == 3, 3, NA))
  data_set$ph_a1304 <- ifelse(data_set$pha_13z1 %in% c(1:5),
                              ifelse(data_set$pha_13z1 == 4, 4, NA))
  data_set$ph_a1305 <- ifelse(data_set$pha_13z1 %in% c(1:5),
                              ifelse(data_set$pha_13z1 == 5, 5, NA))
  
  #### 영양영역
  
  #### 아침결식률
  data_set$nu_a0100 <- ifelse(data_set$nua_01z1 %in% c(0:2), 1,
                       ifelse(data_set$nua_01z1 %in% c(3:7), 0, NA))
  
  #### 아침식사빈도_0일
  data_set$nu_a0201 <- ifelse(data_set$nua_01z1 %in% c(0), 1,
                       ifelse(data_set$nua_01z1 %in% c(1:7), 0, NA))
  
  #### 아침식사빈도_1~2일
  data_set$nu_a0202 <- ifelse(data_set$nua_01z1 %in% c(1,2), 1, 
                       ifelse(data_set$nua_01z1 %in% c(0,3,4,5,6,7), 0, NA))

  #### 아침식사빈도_3~4일
  data_set$nu_a0203 <- ifelse(data_set$nua_01z1 %in% c(3,4), 1,
                       ifelse(data_set$nua_01z1 %in% c(0,1,2,5,6,7), 0, NA))
  
  #### 아침식사빈도_5~7일 : 아침결식 예방 인구비율
  data_set$nu_a0204 <- ifelse(data_set$nua_01z1 %in% c(5:7), 1,
                       ifelse(data_set$nua_01z1 %in% c(0,4), 0, NA))
  
  #### 1일 1회 이상 과일섭취율
  data_set$nu_g0100 <- ifelse(data_set$nug_01z2 %in% c(3:5), 1,
                       ifelse(data_set$nug_01z2 %in% c(1,2), 0, NA))
  
  #### 1일 1회 이상 생채소 섭취율
  data_set$nu_g0300 <- ifelse(data_set$nug_03z2 %in% c(3:5), 1,
                       ifelse(data_set$nug_03z2 %in% c(1,2), 0, NA))
  
  #### 가공식품 선택시 영양표시 이용률
  data_set$nu_c0100 <- ifelse(data_set$nuc_01z1 %in% c(1,2), 1, 
                       ifelse(data_set$nuc_01z1 %in% c(3:5), 0, NA))
  
  #### 영양표시 및 상담 수혜율
  data_set$nu_d0100 <- ifelse(data_set$nud_01z1 == 1, 1,
                       ifelse(data_set$nud_01z1 == 2, 0, NA))
  
  #### 식품 안정성 확보율
  data_set$nu_e0100 <- ifelse(data_set$nue_01z1 %in% c(1,2), 1,
                       ifelse(data_set$nue_01z1 %in% c(3,4), 0, NA))
  
  #### 구강건강영역
  
  #### 주관적 구강건강이 나쁜 인구의 분율
  data_set$or_a0100 <- ifelse(data_set$ora_01z1 %in% c(1:3), 0,
                       ifelse(data_set$ora_01z1 %in% c(4,5), 1, NA))
  
  #### 발음불편호소율(65세 이상)
  data_set$or_b0200 <- ifelse(data_set$orb_02z1 %in% c(1,2), 1,
                       ifelse(data_set$orb_02z1 %in% c(3:5), 0, NA))
  
  #### 치아결손형태(65세 이상)
  data_set$or_b0301 <- ifelse(data_set$orb_03z1 %in% c(1:4),
                              ifelse(data_set$orb_03z1 == 1, 1, NA))
  data_set$or_b0302 <- ifelse(data_set$orb_03z1 %in% c(1:4),
                              ifelse(data_set$orb_03z1 == 2, 2, NA))
  data_set$or_b0303 <- ifelse(data_set$orb_03z1 %in% c(1:4),
                              ifelse(data_set$orb_03z1 == 3, 3, NA))
  data_set$or_b0304 <- ifelse(data_set$orb_03z1 %in% c(1:4),
                              ifelse(data_set$orb_03z1 == 4, 4, NA))
  
  #### 틀니 이용 행태(65세 이상)
  data_set$or_c0101 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 1, 1, NA))
  data_set$or_c0102 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 2, 2, NA))
  data_set$or_c0103 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 3, 3, NA))
  data_set$or_c0104 <- ifelse(data_set$orc_01z1 %in% c(1:4),
                              ifelse(data_set$orc_01z1 == 4, 4, NA))
  
  #### 주관적 틀니 필요여부 인지율(65세 이상)_분모
  data_set$or_c02 <- ifelse(data_set$age_o65 == 1 & data_set$orc_01z1 == 4, 1, NA)
  
  #### 주관적 틀니 필요여부 인지율(65세 이상)_분자
  data_set$or_c0200 <- ifelse(data_set$orc_02z1 == 1, 1,
                       ifelse(data_set$orc_02z1 == 2, 0, NA))
  
  #### 치실 및 치간솔 이용률
  data_set$or_d0200 <- ifelse(data_set$ord_02z1 == 1, 1,
                       ifelse(data_set$ord_02z1 == 2, 0, NA))
  
  #### 무자격자 치과시술률
  data_set$or_e0100 <- ifelse(data_set$ore_01z1 == 1, 1,
                       ifelse(data_set$ore_01z1 == 2, 0, NA))
                              
  #### 치과진료 미치료율
  data_set$or_e0200 <- ifelse(data_set$ore_02z1 == 1, 1,
                       ifelse(data_set$ore_02z1 == 2, 0, NA))
  
  #### 치과진료 미치료 이유
  data_set$or_e0301 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 1, 1, NA))
  data_set$or_e0302 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 2, 2, NA))
  data_set$or_e0303 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 3, 3, NA))
  data_set$or_e0304 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 4, 4, NA))
  data_set$or_e0305 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 5, 5, NA))
  data_set$or_e0306 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 6, 6, NA))
  data_set$or_e0307 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 7, 7, NA))
  data_set$or_e0308 <- ifelse(data_set$ore_03z1 %in% c(1:8),
                              ifelse(data_set$ore_03z1 == 8, 8, NA))
  
  #### 구강검진 수진율
  data_set$or_e0500 <- ifelse(data_set$ore_05z1 == 1, 1,
                       ifelse(data_set$ore_05z1 == 2, 0, NA))
  
  #### 스케일링 경험률
  data_set$or_a0700 <- ifelse(data_set$ore_07z1 == 1, 1,
                       ifelse(data_set$ore_07z1 == 2, 0, NA))
  
  #### 주관적 잇몸건강 인지율
  data_set$orb_0400 <- ifelse(data_set$orb_04z1 == 5, 1,
                       ifelse(data_set$orb_04z1 %in% c(1:4), 0, NA))
  
  #### 정신건강영역
  
  #### 스트레스로 인한 정신상담률
  data_set$mt_a0200 <- ifelse(data_set$mta_02z1 == 1, 1,
                       ifelse(data_set$mta_02z1 == 2, 0, NA))
  
  #### 우울증상으로 인한 정신상담률
  data_set$mt_b0200 <- ifelse(data_set$mtb_02z1 == 1, 1,
                       ifelse(data_set$mtb_02z1 == 2, 0, NA))
  
  #### 하루 평균 수면 시간
  data_set$mt_c0100 <- ifelse(data_set$mtc_01z1>=0 & data_set$mtc_01z1<=24, data_set$mtc_01z1, NA)
  
  #### 하루평균 수면시간_6시간 이하
  data_set$mt_c0201 <- ifelse(data_set$mt_c0100>=0 & data_set$mt_c0100<=6, 1,
                       ifelse(data_set$mt_c0100>=7 & data_set$mt_c0100<=24, 0, NA))
  
  #### 하루평균 수면시간_7~8시간
  data_set$mt_c0202 <- ifelse(data_set$mt_c0100>=0 & data_set$mt_c0100<=6, 0,
                       ifelse(data_set$mt_c0100>=7 & data_set$mt_c0100<=8, 1,
                       ifelse(data_set$mt_c0100>=9 & data_set$mt_c0100<=24, 0, NA)))
  
  #### 하루평균 수면시간_9시간 이상
  data_set$mt_c0203 <- ifelse(data_set$mt_c0100>=0 & data_set$mt_c0100<=8, 0,
                       ifelse(data_set$mt_c0100>=9 & data_set$mt_c0100<=24, 1, NA))
  
  #### 건강검진영역
  
  #### 연간 인플루엔자 예방접종률
  data_set$sc_a0100 <- ifelse(data_set$sca_01z1 == 1, 1,
                       ifelse(data_Set$sca_01z1 == 2, 0, NA))
  
  #### 이환영역
  
  #### 만성, 급성질환 및 사고중독 경험률
  data_set$qu_a0100 <- ifelse(data_set$qua_01z1 == 1, 1,
                       ifelse(data_set$qua_01z1 == 2, 0, NA))
  
  #### 만성, 급성질환 및 사고중독 경험일수
  data_set$qu_a0200 <- ifelse(data_set$qua_02z1>=1 & data_set$qua_02z1<=14, data_set$qua_02z1, NA)
  
  #### 이환영역(고혈압)
  
  #### 연간 평균 혈압 측정획수
  data_set$il_a0100 <- ifelse(data_set$hya_12z1 %in% c(1:5),
                              ifelse(data_set$hya_12z1 == 1 & (data_set$hya_13a1>=1 & data_set$hya_13a1<=776), data_set%hya_13a1*365,
                              ifelse(data_set$hya_12z1 == 2 & (data_set$hya_13b1>=1 & data_set$hya_13b1<=776), data_set%hya_13b1*52,
                              ifelse(data_set$hya_12z1 == 3 & (data_set$hya_13c1>=1 & data_set$hya_13c1<=776), data_set%hya_13c1*12,
                              ifelse(data_set$hya_12z1 == 4 & (data_set$hya_13d1>=1 & data_set$hya_13d1<=776), data_set%hya_13d1,
                              ifelse(data_set$hya_12z1 == 5, 0, NA))))
  
  #### 고혈압 평생 의사진단 경험률
  data_set$il_a0200 <- ifelse(data_set$hya_04z1 == 1, 1,
                       ifelse(data_set$hya_04z1 == 2, 0, NA))
  
  #### 고혈압 평생 의사진단 경험률_30세 이상_분모
  data_set$age_o30 <- ifelse(data_set$age>=30 & data_set$age<=110, 1, NA)
  
  #### 고혈압 평생 의사진단 경험률_30세 이상_분자
  data_set$il_a0300 <- ifelse(data_set$age_o30 == 1,
                              ifelse(data_set$il_a0200 == 1, 1,
                              ifelse(data_set$il_a0200 == 0, 0, NA)))
  
  #### 본인 인지 혈압 조절률
  data_set$il_a0400 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_15z1 == 1, 1,
                              ifelse(data_set$hya_15z1 == 2, 0, NA)))
  
  #### 고혈압의 현재치료율
  data_set$il_a0500 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_06z1 == 1, 1,
                              ifelse(data_set$hya_06z1 == 2, 0, NA)))
  
  #### 고혈압 약물 치료율
  data_set$il_a0600 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_06z1 == 1,
                                     ifelse(data_set$hya_14a1 == 1 & (data_set$hya_14b1>=0 & data_set$hya_14b1<=19), 0,
                                     ifelse(data_set$hya_14a1 == 1 & (data_set$hya_14b1>=20 & data_set$hya_14b1<=31), 1,
                                     ifelse(data_set$hya_14a1 == 2, 0,
                              ifelse(data_set$hya_06z2 == 2, 0, NA))))))
  
  #### 고혈압 관리교육 장소(병의원/한방병의원/보건소)
  data_set$il_a0701 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_11a1 == 1, 1,
                              ifelse(data_set$hya_11a1 == 2, 0, NA)))
  data_set$il_a0702 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_11b1 == 1, 1,
                              ifelse(data_set$hya_11b1 == 2, 0, NA)))
  data_set$il_a0703 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_11c1 == 1, 1,
                              ifelse(data_set$hya_11c1 == 2, 0, NA)))
  
  #### 고혈압 관리교육 이수율
  data_set$il_a0800 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$il_a0701 == 1 | data_set$il_a0702 == 1 | data_set$il_a0703 == 1, 1,
                              ifelse(data_set$il_a0701 == 0 | data_set$il_a0702 == 0 | data_set$il_a0703 == 0, 0, NA)))
  
  #### 고혈압 신장질환 합병증검사 수진율
  data_set$il_a1600 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_16z1 == 1, 1,
                              ifelse(data_set$hya_16z1 == 2, 0, NA)))
  
  #### 고혈압 심장질환 합병증검사 수진율
  data_set$il_a1700 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_17z1 == 1, 1,
                              ifelse(data_set$hya_17z1 == 2, 0, NA)))
  
  #### 고혈압 뇌혈관질환 합병증검사 수진율
  data_set$il_a1800 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_18z1 == 1, 1,
                              ifelse(data_set$hya_18z1 == 2, 0, NA)))
  
  #### 이환영역(당뇨병)
  
  #### 연간 평균 혈당 측정횟수
  data_set$il_b0100 <- ifelse(data_set$dia_16Z1 %in% c(1:5),
                              ifelse(data_set$dia_16z1 == 1 & (data_set$dia_17a1>=1 & data_set$dia_17a1<=776), data_set$dia_17a1*365,
                              ifelse(data_set$dia_16z1 == 2 & (data_set$dia_17b1>=1 & data_set$dia_17b1<=776), data_set$dia_17b1*52,
                              ifelse(data_set$dia_16z1 == 3 & (data_set$dia_17c1>=1 & data_set$dia_17c1<=776), data_set$dia_17a1*12,
                              ifelse(data_set$dia_16z1 == 4 & (data_set$dia_17d1>=1 & data_set$dia_17d1<=776), data_set$dia_17a1,
                              ifelse(data_set$dia_16z1 == 5, 0, NA))))))
  
  #### 당뇨병 평생 의사진단 경험률
  data_set$il_b0200 <- ifelse(data_set$dia_04z1 == 1, 1,
                       ifelse(data_set$dia_04z1 == 2, 0, NA))
  
  #### 당뇨병 평생 의사진단 경험률_30세이상
  data_set$il_b0300 <- ifelse(data_set$age_o30 == 1,
                              ifelse(data_set$il_b0200 == 1, 1,
                              ifelse(data_set$il_b0200 == 0, 0, NA)))
  
  #### 본인 인지 혈당 조절률
  data_set$il_b0400 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_18z1 == 1, 1,
                              ifelse(data_set$dia_18z1 == 2, 0, NA)))
  
  #### 당뇨병 현재치료율
  data_set$il_b0500 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_06z1 == 1, 1,
                              ifelse(data_set$dia_06z1 == 2, 0, NA)))
  
  #### 당뇨병 치료율
  data_set$il_b0600 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_06z1 == 1,
                                     ifelse(data_set$dia_13a1 == 1 | data_set$dia_13b1 == 1, 1,
                                     ifelse(data_set$dia_13a1 == 2 | data_set$dia_13b1 == 2, 0, NA))))
  
  
  #### 당뇨병 관리교육 장소(병의원, 한방병의원, 보건소)
  data_set$il_b0701 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_10a1 == 1, 1,
                              ifelse(data_set$dia_10a1 == 2, 0, NA)))
  data_set$il_b0702 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_10b1 == 1, 1,
                              ifelse(data_set$dia_10b1 == 2, 0, NA)))
  data_set$il_b0703 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_10c1 == 1, 1,
                              ifelse(data_set$dia_10c1 == 2, 0, NA)))
  
  #### 당뇨병 관리교육 이수율
  data_set$il_b0800 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$il_b0701 == 1 | data_set$il_b0702 == 1 | data_set$il_b0703 == 1, 1,
                              ifelse(data_set$il_b0701 == 0 | data_set$il_b0702 == 0 | data_set$il_b0703 == 0, 0, NA)))
  
  #### 이환영역(이상지질혈증(고지혈증 포함))
  
  #### 이상지질혈증 평생 의사진단 경험률_30세이상
  data_set$il_r0200 <- ifelse(data_set$age_o30 == 1,
                              ifelse(data_set$il_r0100 == 1, 1,
                              ifelse(data_set$il_r0100 == 0, 0, NA)))
  
  #### 콜레스테롤 인지율
  data_set$il_r0400 <- ifelse(data_set$dla_04z1 == 1, 1,
                       ifelse(data_set$dla_04z1 == 2, 0, NA))
  
  #### 이환영역(심근경색)
  
  #### 심근경색 평생 의사진단 경험률_분모정의
  data_set$age_o40 <- ifelse(data_set$age>=40 & data_set$age<=110, 1, NA)
  
  #### 심근경색 평생 의사진단 경험률_분자정의
  data_set$il_e0100 <- ifelse(data_set$mya_04z1 == 1, 1,
                       ifelse(data_set$mya_04z1 == 2, 0, NA))
  
  #### 심근경색 평생 의사진단 경험률_40세이상
  data_set$il_e0200 <- ifelse(data_set$age_o40 == 1,
                              ifelse(data_set$il_e0100 == 1, 1,
                              ifelse(data_set$il_e0100 == 0, 0, NA)))

  ### 이환영역(협심증)
  
  #### 협심증 평생 의사진단 경험률
  data_set$il_f0100 <- ifelse(data_set$ana_04z1 == 1, 1,
                       ifelse(data_set$ana_04z1 == 2, 0, NA))
  
  #### 협심증 평생 의사진단 경험률_40세이상
  data_set$il_f0200 <- ifelse(data_set$age_o40 == 1,
                              ifelse(data_set$il_f0200 == 1, 1,
                              ifelse(data_set$il_f0200 == 0, 0, NA)))
  
  #### 이환영역(관절염)
  
  #### 관절염 평생 의사진단 경험률_50세이상
  data_set$il_g0200 <- ifelse(data_set$age_o50 == 1,
                              ifelse(data_set$il_g0100 == 1, 1,
                              ifelse(data_set$il_g0100 == 0, 0, NA)))
  
  #### 관절염 현재치료율
  data_set$il_g0300 <- ifelse(data_set$il_g0100 == 1,
                              ifelse(data_set$ara_22z1 == 1, 1,
                              ifelse(data_set$ara_22z1 == 2, 0, NA)))
  
  #### 관절염 관리교육 장소(병의원, 한방병의원, 보건소)
  data_set$il_g0401 <- ifelse(data_set$il_g0100 == 1,
                              ifelse(data_set$ara_18a1 == 1, 1,
                              ifelse(data_set$ara_18a1 == 2, 0, NA)))
  data_set$il_g0402 <- ifelse(data_set$il_g0100 == 1,
                              ifelse(data_set$ara_18b1 == 1, 1,
                              ifelse(data_set$ara_18b1 == 2, 0, NA)))
  data_set$il_g0403 <- ifelse(data_set$il_g0100 == 1,
                              ifelse(data_set$ara_18c1 == 1, 1,
                              ifelse(data_set$ara_18c1 == 2, 0, NA)))
  
  #### 관절염 관리교육 이수율
  data_set$il_g0500 <- ifelse(data_set$il_g0500 == 1,
                              ifelse(data_set$il_g0401 == 1 | data_set$il_g0402 == 1 | data_set$il_g0403 == 1, 1,
                              ifelse(data_set$il_g0401 == 0 | data_set$il_g0402 == 0 | data_set$il_g0403 == 0, 0, NA)))
  
  #### 이환영역(골다공증)
  
  #### 골다공증 평생 의사진단 경험률
  data_set$il_h0100 <- ifelse(data_set$osa_04z1 == 1, 1,
                       ifelse(data_set$osa_04z1 == 2, 0, NA))
  
  #### 골다공증 평생 의사진단 경험률_50세이상
  data_set$il_h0200 <- ifelse(data_set$age_o50 == 1,
                              ifelse(data_set$il_h0100 == 1, 1,
                              ifelse(data_set$il_h0100 == 0, 0, NA)))
  
  #### 이환영역(폐결핵)
  
  #### 폐결핵 평생 의사진단 경험률
  data_set$il_i0100 <- ifelse(data_set$tua_02z1 == 1, 1,
                       ifelse(data_set$tua_02z1 == 2, 2, 0))
  
  #### 이환영역(천식)
  
  #### 천식 증상 경험률
  data_set$il_j0100 <- ifelse(data_set$asa_01z1 == 1, 1,
                       ifelse(data_set$asa_01z1 == 2, 0, NA))
  
  #### 운동성 천식 이환율
  data_set$il_j0200 <- ifelse(data_set$asa_02z1 == 1, 1,
                       ifelse(data_set$asa_02z1 == 2, 0, NA))
  
  #### 천식 평생 의사진단 경험률
  data_set$il_j0300 <- ifelse(data_set$asa_08z1 == 1, 1,
                       ifelse(data_set$asa_08z1 == 2, 0, NA))
  
  #### 이환영역(알레르기비염)
  
  #### 알레르기비염 평생 의사진단 경험률
  data_set$il_k0100 <- ifelse(data_set$rha_04z1 == 1, 1,
                       ifelse(data_set$rha04z1 == 2, 0, NA))
  
  #### 이환영역(아토피피부염)
  
  #### 아토피피부염 평생 의사진단 경험률
  data_set$il_l0100 <- ifelse(data_set$dea_04z1 == 1, 1,
                       ifelse(data_set$dea_04z1 == 2, 0, NA))
  
  #### 이환영역(백내장)
  
  #### 백내장 평생 의사진단 경험률
  data_set$il_m0100 <- ifelse(data_set$caa_04z1 == 1, 1,
                       ifelse(data_set$caa_04z1 == 2, 0, NA))
  
  #### 백내장 평생 의사진단 경험률_50세이상
  data_set$il_m0200 <- ifelse(data_set$age_o50 == 1,
                              ifelse(data_set$il_m0100 == 1, 1,
                              ifelse(data_set$il_m0100 == 0, 0, NA)))
  
  #### 이환영역(녹내장)
  
  #### 녹내장 평생 의사진단 경험률
  data_set$il_s0100 <- ifelse(data_set$gla_04z1 == 1, 1,
                       ifelse(data_set$gla_04z1 == 2, 0, NA))
  
  #### 녹내장 평생 의사진단 경험률_50세이상
  data_set$il_s0200 <- ifelse(data_set$age_o50 == 1,
                              ifelse(il_s0100 == 1, 1,
                              ifelse(il_s0100 == 2, 0, NA)))
  
  #### 이환영역(B형 간염)
  
  #### B형 간염 평생 의사진단 경험률
  data_set$il_n0100 <- ifelse(data_set$hba_04z1 == 1, 1,
                       ifelse(data_set$hba_04z1 == 2, 0, NA))
  
  #### 이환영역(우울증)
  
  #### 우울증 평생 의사진단 경험률
  data_set$il_o0100 <- ifelse(data_set$dpa_02z1 == 1, 1,
                       ifelse(data_est$dpa_02z1 == 2, 0, NA))
  
  #### 이환영역(요실금)
  
  #### 요실금 평생의사진단 경험률
  data_set$il_t0100 <- ifelse(data_set$ura_04z1 == 1, 1,
                       ifelse(data_set$ura_04z1 == 2, 0, NA))
  
  #### 이환영역(전립선비대)
  
  #### 전립선 평생의사진단 경험률
  data_set$il_u0100 <- ifelse(data_set$sex_m == 1,
                              ifelse(data_set$bpa_01z1 == 1, 1,
                              ifelse(data_set$bpa_01z1 == 2, 0, NA)))
  
  #### 전립선증상점수_IPSS점수
  data_set$ipss_score <- ifelse(data_set$bpa_03z1 %in% c(1:6) & data_set$bpa_04z1 %in% c(1:6) & data_set$bpa_05z1 %in% c(1:6) &
                                data_set$bpa_06z1 %in% c(1:6) & data_set$bpa_07z1 %in% c(1:6) & data_set$bpa_08z1 %in% c(1:6) &
                                data_set$bpa_09z1 %in% c(1:6),
                                (data_set$bpa_03z1 + data_set$bpa_04z1 + data_set$bpa_05z1 + data_set$bpa_06z1 + 
                                data_set$bpa_07z1 + data_set$bpa_08z1 + data_set$bpa_09z1)-7, NA)
  
  #### 전립선증상점수_경증, 중등도, 중증
  data_set$il_u0301 <- ifelse(data_set$sex_m == 1,
                              ifelse(data_set$ipss_score>=0 & data_set$ipss_score<=7, 1,
                              ifelse(data_set$ipss_score>=8 & data_set$ipss_score<=19, 0,
                              ifelse(data_set$ipss_score>=20 & data_set$ipss_score<=35, 0, NA))))
  data_set$il_u0302 <- ifelse(data_set$sex_m == 1,
                              ifelse(data_set$ipss_score>=0 & data_set$ipss_score<=7, 0,
                              ifelse(data_set$ipss_score>=8 & data_set$ipss_score<=19, 1,
                              ifelse(data_set$ipss_score>=20 & data_set$ipss_score<=35, 0, NA))))
  data_set$il_u0303 <- ifelse(data_set$sex_m == 1,
                              ifelse(data_set$ipss_score>=0 & data_set$ipss_score<=7, 0,
                              ifelse(data_set$ipss_score>=8 & data_set$ipss_score<=19, 0,
                              ifelse(data_set$ipss_score>=20 & data_set$ipss_score<=35, 1, NA))))
  
  #### 배뇨증상 관련 삶의 질_경증, 중등도, 중증
  data_set$il_u0400 <- ifelse(data_set$sex_m == 1 & data_set$bpa_10z1 %in% c(1:7), data_set$bpa_10z1-1, NA)
  
  #### 이환영역(대사증후군)
  
  #### 대사증후군 인지율
  data_set$il_p0100 <- ifelse(data_set$sya_01z1 == 1, 1,
                       ifelse(data_set$sya_01z1 == 2, 0, NA))
  
  #### 이환영역(에이즈)
  
  #### 에이즈 인지율
  data_set$il_q0100 <- ifelse(data_set$aia_01z1 == 1, 1,
                       ifelse(data_set$aia_01z1 == 2, 0, NA))
  
  #### 에이즈에 대한 지식 정답_분모정의
  data_set$il_q03 <- ifelse(data_set$aia_01z1 == 1 & data_set$aia_03z1 %in% c(1,2) & data_set$aia_04z1 %in% c(1,2) &
                            data_set$aia_05z1 %in% c(1,2) & data_set$aia_06z1 %in% c(1,2), 1, NA)
  
  #### 에이즈 인식_콘돔예방
  data_set$il_q0301 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_03z1 %in% c(1:3), data_set$aia_03z1 == 1, NA))
  
  #### 에이즈 인식_건강정상
  data_set$il_q0302 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_04z1 %in% c(1:3), data_set$aia_04z1 == 1, NA))
  
  #### 에이즈 인식_모기감염
  data_set$il_q0303 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_05z1 %in% c(1:3), data_set$aia_05z1 == 2, NA))
  
  #### 에이즈 인식_식사감염
  data_set$il_q0304 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_06z1 %in% c(1:3), data_set$aia_06z1 == 2, NA))
  
  #### 에이즈에 대한 지식 정답률
  data_set$il_q0400 <- ifelse(data_set$il_q03 == 1,
                              ifelse(data_set$il_q0301 + data_set$il_q0302 + data_set$il_q0303 + data_set$il_q0304 == 4, 1,
                              ifelse(data_set$il_q0301 + data_set$il_q0302 + data_set$il_q0303 + data_set$il_q0304>=0 &
                                     data_set$il_q0301 + data_set$il_q0302 + data_set$il_q0303 + data_set$il_q0304<=3, 0, NA)))
  
  #### 에이즈에 대한 긍정적 태도_분모정의
  data_set$il_q04 <- ifelse(data_set$aia_01z1 == 1 & data_set$aia_07z1 %in% c(1,2) & data_set$aia_08z1 %in% c(1,2) &
                            data_set$aia_09z1 %in% c(1,2) & data_set$aia_10z1 %in% c(1,2), 1, NA)
  
  #### 에이즈 태도_감염인가족
  data_set$il_q0701 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_07z1 %in% c(1:3), data_set$aia_07z1 == 1, NA))
  
  #### 에이즈 태도_감염인가게
  data_set$il_q0702 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_08z1 %in% c(1:3), data_set$aia_08z1 == 1, NA))
  
  #### 에이즈 태도_감염인학교
  data_set$il_q0703 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_09z1 %in% c(1:3), data_set$aia_09z1 == 1, NA))
  
  #### 에이즈 태도_감염인비밀
  data_set$il_q0704 <- ifelse(data_set$aia_01z1 == 1,
                              ifelse(data_set$aia_10z1 %in% c(1:3), data_set$aia_10z1 == 2, NA))
  
  #### 에이즈에 대한 긍정적 태도율
  data_set$il_q0800 <- ifelse(data_set$il_q04 == 1,
                              ifelse(data_set$il_q0701 + data_set$il_q0702 + data_set$il_q0703 + data_set$il_q0704 == 4, 1,
                              ifelse(data_set$il_q0701 + data_set$il_q0702 + data_set$il_q0703 + data_set$il_q0704>=0 &
                                     data_set$il_q0701 + data_set$il_q0702 + data_set$il_q0703 + data_set$il_q0704<=3, 0, NA)))
  
  #### 의료이용영역
  
  #### 필요의료서비스 미치료 이유
  data_set$sr_a0201 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 1, 1,
                       ifelse(data_set$dra_01z1 %in% c(2), 0, NA)))
  data_set$sr_a0202 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 2, 2,
                       ifelse(data_set$sra_01z1 %in% c(2), 0, NA)))
  data_set$sr_a0203 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 3, 3,
                       ifelse(data_set$sra_01z1 %in% c(2), 0, NA)))
  data_set$sr_a0204 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 4, 4,
                       ifelse(data_set$sra_01z1 %in% c(2), 0, NA)))
  data_set$sr_a0205 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 5, 5,
                       ifelse(data_set$sra_01z1 %in% c(2), 0, NA)))
  data_set$sr_a0206 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 6, 6,
                       ifelse(data_set$sra_01z1 %in% c(2), 0, NA)))
  data_set$sr_a0207 <- ifelse(data_set$sra_01z1 %in% c(1) & data_set$sra_02z1 %in% c(1:7),
                              ifelse(data_set$sra_02z1 == 7, 7,
                       ifelse(data_set$sra_01z1 %in% c(2), 0, NA)))
  
  #### 사고 및 중독(낙상) 영역
  
  #### 연간 사고중독 경험률
  data_set$ac_a0100 <- ifelse(data_set$ira_01z1 == 1, 1,
                       ifelse(data_set$ira_01z1 == 2, 0, NA))
  
  #### 연간 사고중독 건수율
  data_set$ac_a0200 <- ifelse(data_set$ira_01z1 == 1 & data_set$ira_02z1 %in% c(1:5), data_set$ira_02z1,
                       ifelse(data_set$ira_01z1 == 2, 0, NA))
  
  #### 분모정의
  data_set$age_o65 <- ifelse(data_set$age>=65 & data_set$age<=110, 1, NA)
  
  #### 연간 낙상 경험률
  data_set$ac_b0100 <- ifelse(data_set$irb_01z1 == 1, 1,
                       ifelse(data_set$irb_01z1 == 2, 0, NA))
  
  #### 연간 낙상 건수율
  data_set$ac_b0200 <- ifelse(data_set$irb_01z1 == 1 & irb_02z1 %in% c(1:5), data_set$irb_02z1,
                       ifelse(data_set$irb_01z1 == 2, 0, NA))
  
  #### 낙상에 대한 두려움
  data_set$ac_b0500 <- ifelse(data_set$irb_03z1 %in% c(2,3), 1,
                       ifelse(data_set$irb_03z1 == 1, 0, NA))
  
  #### 장소별 낙상 분포
  data_set$ac_b0301 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 1, 1, NA))
  data_set$ac_b0302 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 2, 2, NA))
  data_set$ac_b0303 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 3, 3, NA))
  data_set$ac_b0304 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 4, 4, NA))
  data_set$ac_b0305 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 5, 5, NA))
  data_set$ac_b0306 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 6, 6, NA))
  data_set$ac_b0307 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 7, 7, NA))
  data_set$ac_b0308 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 8, 8, NA))
  data_set$ac_b0309 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 9, 9, NA))
  data_set$ac_b0310 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 10, 10, NA))
  data_set$ac_b0311 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b03 %in% c(1:11),
                              ifelse(data_set$ac_b03 == 11, 11, NA))
  
  #### 원인별 낙상 분포
  data_set$ac_b0401 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 1, 1, NA))
  data_set$ac_b0402 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 2, 2, NA))
  data_set$ac_b0403 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 3, 3, NA))
  data_set$ac_b0404 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 4, 4, NA))
  data_set$ac_b0405 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 5, 5, NA))
  data_set$ac_b0406 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 6, 6, NA))
  data_set$ac_b0407 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 7, 7, NA))
  data_set$ac_b0408 <- ifelse(data_set$irb_01z1 == 1 & data_set$ac_b04 %in% c(1:8),
                              ifelse(data_set$ac_b04 == 8, 8, NA))
  
  #### 활동제한영역
  
  #### 주관적 건강수준 인지율
  data_set$ql_a0100 <- ifelse(data_set$qoa_01z1 %in% c(1,2), 1,
                       ifelse(data_set$qoa_01z1 %in% c(3:5), 0, NA))
  
  #### 행복감지수
  data_set$ql_c1400 <- ifelse(data_set$qoc_07z1 %in% c(1:10), data_set$qoc_07z1, NA)
  
  #### 침상와별 경험률
  data_set$ql_b0300 <- ifelse(data_set$qoa_02z1 == 1, 1,
                       ifelse(data_set$qoa_02z1 == 2, 0, NA))
  
  #### 침상와별일수
  data_set$ql_b0400 <- ifelse(data_set$qoa_03z1>=1 & data_set$qoa_03z1<=31, data_set$qoa_03z1, NA)
  
  #### 결근결석 경험률
  data_set$ql_b0500 <- ifelse(data_set$qoa_04z1 %in% c(1,2) & data_set$soa_06z1 %in% c(1:11),
                              ifelse(data_set$qoa_04z1 == 1, 1,
                              ifelse(data_set$qoa_04z1 == 2, 0,
                       ifelse(data_set$qoa_04z1 %in% c(1,2) & data_set$soa_06z1 %in% c(12,13), .,
                       ifelse(data_set$qoa_04z1 == 3, ., NA)))))
  
  #### 결근결석 일수
  data_set$ql_b0600 <- ifelse(data_set$qoa_05z1>=1 & data_set$qoa_05z1<=31, data_set$qoa_05z1, NA)
  
  #### 보건기관이용영역
  
  #### 보건기관 이용률 
  data_set$ct_a0100 <- ifelse(data_set$hma_01z1 == 1, 0,
                       ifelse(data_set$hma_01z1 %in% c(2:4), 1, NA))
  
  #### 이용한 보건기관 종류
  data_set$ct_a0201 <- ifelse(data_set$hma_01z1 %in% c(1:4),
                              ifelse(data_set$hma_01z1 == 1, 1, NA))
  data_set$ct_a0202 <- ifelse(data_set$hma_01z1 %in% c(1:4),
                              ifelse(data_set$hma_01z1 == 2, 2, NA))
  data_set$ct_a0203 <- ifelse(data_set$hma_01z1 %in% c(1:4),
                              ifelse(ata_set$hma_01z1 == 3, 3, NA))
  data_set$ct_a0204 <- ifelse(data_set$hma_01z1 %in% c(1:4),
                              ifelse(data_set$hma_01z1 == 4, 4, NA))
    
  #### 참여한 보건기관 서비스 종류
  data_set$ct_a0301 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04a1 == 1, 1,
                              ifelse(data_set$hma_04a1 == 2, 0, NA)))
  data_set$ct_a0302 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04b1 == 1, 1,
                              ifelse(data_set$hma_04b1 == 2, 0, NA)))
  data_set$ct_a0303 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04c1 == 1, 1,
                              ifelse(data_set$hma_04c1 == 2, 0, NA)))
  data_set$ct_a0304 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04d1 == 1, 1,
                              ifelse(data_set$hma_04d1 == 2, 0, NA)))
  data_set$ct_a0305 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04e1 == 1, 1,
                              ifelse(data_set$hma_04e1 == 2, 0, NA)))
  data_set$ct_a0306 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04f1 == 1, 1,
                              ifelse(data_set$hma_04f1 == 2, 0, NA)))
  data_set$ct_a0307 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04g1 == 1, 1,
                              ifelse(data_set$hma_04g1 == 2, 0, NA)))
  data_set$ct_a0308 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04h1 == 1, 1,
                              ifelse(data_set$hma_04h1 == 2, 0, NA)))
  data_set$ct_a0309 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04i1 == 1, 1,
                              ifelse(data_set$hma_04i1 == 2, 0, NA)))
  data_set$ct_a0310 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04j1 == 1, 1,
                              ifelse(data_set$hma_04j1 == 2, 0, NA)))
  data_set$ct_a0311 <- ifelse(data_set$ct_a0100 == 1,
                              ifelse(data_set$hma_04k1 == 1, 1,
                              ifelse(data_set$hma_04k1 == 2, 0, NA)))
  
  #### 보건기관 서비스 만족도
  data_set$ct_a0400 <- ifelse(data_set$hma_05z1 %in% c(1,2), 1,
                       ifelse(data_set$hma_05z1 %in% c(3:5), 0, NA))
  
  #### 보건기관을 이용하는 이유
  data_set$ct_a0501 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 1, 1, NA))
  data_set$ct_a0502 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 2, 2, NA))
  data_set$ct_a0503 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 3, 3, NA))
  data_set$ct_a0504 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 4, 4, NA))
  data_set$ct_a0505 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 5, 5, NA))
  data_set$ct_a0506 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 6, 6, NA))
  data_set$ct_a0507 <- ifelse(data_set$hma_02z3 %in% c(1:7),
                              ifelse(data_set$hma_02z3 == 7, 7, NA))
  
  #### 보건기관을 이용하지 않는 이유_분모정의
  data_set$ct_b0100 <- ifelse(data_set$hma_01z1 == 1, 1,
                       ifelse(data_set$hma_01z1 %in% c(2:4), 0, NA))
  
  #### 보건기관을 이용하지 않는 이유_분자정의
  data_set$ct_b0201 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 1, 1, NA))
  data_set$ct_b0202 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 2, 2, NA))
  data_set$ct_b0203 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 3, 3, NA))
  data_set$ct_b0204 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 4, 4, NA))
  data_set$ct_b0205 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 5, 5, NA))
  data_set$ct_b0206 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 6, 6, NA))
  data_set$ct_b0207 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 7, 7, NA))
  data_set$ct_b0208 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 8, 8, NA))
  data_set$ct_b0209 <- ifelse(data_set$hmb_01z2 %in% c(1:9),
                              ifelse(data_set$hmb_01Z1 == 9, 9, NA))
  
  #### 보건기관 질병 홍보 및 교육 경험률_고혈압
  data_set$ct_c0101 <- ifelse(data_set$hmc_01a1 == 1, 1,
                       ifelse(data_set$hmc_01a1 == 2, 0, NA))
  
  #### 보건기관 질병 홍보 및 교육 경험률_당뇨병
  data_set$ct_c0102 <- ifelse(data_set$hmc_01b1 == 1, 1,
                       ifelse(data_set$hmc_01b1 == 2, 0, NA))
  
  #### 보건기관 질병 홍보 및 교육 경험률_관절염
  data_set$ct_c0103 <- ifelse(data_set$hmc_01c1 == 1, 1,
                       ifelse(data_set$hmc_01c1 == 2, 0, NA))
  
  #### 보건기관 질병 홍보 및 교육 경험률_아토피천식
  data_set$ct_c0104 <- ifelse(data_set$hmc_01d1 == 1, 1,
                       ifelse(data_set$hmc_01d1 == 2, 0, NA))
  
  #### 보건기관 질병 홍보 및 교육 경험률_치매
  data_set$ct_c0105 <- ifelse(data_set$hmc_01e1 == 1, 1,
                       ifelse(data_set$hmc_01e1 == 2, 0, NA))
  
  #### 사회 물리적 환경 영역
  
  #### 지역의 사회 물리적 환경에 대한 긍정적 태도율
  data_set$en_a0101 <- ifelse(data_set$ena_01a1 == 1, 1,
                       ifelse(data_set$ena_01a1 == 2, 0, NA))
  data_set$en_a0102 <- ifelse(data_set$ena_01b1 == 1, 1,
                       ifelse(data_set$ena_01b1 == 2, 0, NA))
  data_set$en_a0103 <- ifelse(data_set$ena_01c1 == 1, 1,
                       ifelse(data_set$ena_01c1 == 2, 0, NA))
  data_set$en_a0104 <- ifelse(data_set$ena_01d1 == 1, 1,
                       ifelse(data_set$ena_01d1 == 2, 0, NA))
  data_set$en_a0105 <- ifelse(data_set$ena_01e1 == 1, 1,
                       ifelse(data_set$ena_01e1 == 2, 0, NA))
  data_set$en_a0106 <- ifelse(data_set$ena_01f1 == 1, 1,
                       ifelse(data_set$ena_01f1 == 2, 0, NA))
  data_set$en_a0107 <- ifelse(data_set$ena_01g1 == 1, 1,
                       ifelse(data_set$ena_01g1 == 2, 0, NA))
  
  #### 65세 이상 정의
  data_set$age_o65 <- ifelse(data_set$age>=19 & data_set$age<=64, 0,
                      ifelse(data_set$age>=65 & data_set$age<=110, 1, NA))
  
  #### 사회적 연결망(친척)
  data_set$en_b0101 <- ifelse(data_set$enb_01z1 %in% c(1:6),
                              ifelse(data_set$enb_01z1 == 1, 1, NA))
  data_set$en_b0102 <- ifelse(data_set$enb_01z1 %in% c(1:6),
                              ifelse(data_set$enb_01z1 == 2, 2, NA))
  data_set$en_b0103 <- ifelse(data_set$enb_01z1 %in% c(1:6),
                              ifelse(data_set$enb_01z1 == 3, 3, NA))
  data_set$en_b0104 <- ifelse(data_set$enb_01z1 %in% c(1:6),
                              ifelse(data_set$enb_01z1 == 4, 4, NA))
  data_set$en_b0105 <- ifelse(data_set$enb_01z1 %in% c(1:6),
                              ifelse(data_set$enb_01z1 == 5, 5, NA))
  data_set$en_b0106 <- ifelse(data_set$enb_01z1 %in% c(1:6),
                              ifelse(data_set$enb_01z1 == 6, 6, NA))
  
  #### 사회적 연결망(이웃)
  data_set$en_b0201 <- ifelse(data_set$enb_02z1 %in% c(1:6),
                              ifelse(data_set$enb_02z1 == 1, 1, NA))
  data_set$en_b0202 <- ifelse(data_set$enb_02z1 %in% c(1:6),
                              ifelse(data_set$enb_02z1 == 2, 2, NA))
  data_set$en_b0203 <- ifelse(data_set$enb_02z1 %in% c(1:6),
                              ifelse(data_set$enb_02z1 == 3, 3, NA))
  data_set$en_b0204 <- ifelse(data_set$enb_02z1 %in% c(1:6),
                              ifelse(data_set$enb_02z1 == 4, 4, NA))
  data_set$en_b0205 <- ifelse(data_set$enb_02z1 %in% c(1:6),
                              ifelse(data_set$enb_02z1 == 5, 5, NA))
  data_set$en_b0206 <- ifelse(data_set$enb_02z1 %in% c(1:6),
                              ifelse(data_set$enb_02z1 == 6, 6, NA))
  
  #### 사회적 연결망(친구)
  data_set$en_b0301 <- ifelse(data_set$enb_03z1 %in% c(1:6),
                              ifelse(data_set$enb_03z1 == 1, 1, NA))
  data_set$en_b0302 <- ifelse(data_set$enb_03z1 %in% c(1:6),
                              ifelse(data_set$enb_03z1 == 2, 2, NA))
  data_set$en_b0303 <- ifelse(data_set$enb_03z1 %in% c(1:6),
                              ifelse(data_set$enb_03z1 == 3, 3, NA))
  data_set$en_b0304 <- ifelse(data_set$enb_03z1 %in% c(1:6),
                              ifelse(data_set$enb_03z1 == 4, 4, NA))
  data_set$en_b0305 <- ifelse(data_set$enb_03z1 %in% c(1:6),
                              ifelse(data_set$enb_03z1 == 5, 5, NA))
  data_set$en_b0306 <- ifelse(data_set$enb_03z1 %in% c(1:6),
                              ifelse(data_set$enb_03z1 == 6, 6, NA))
  
  #### 사회적 참여율
  data_set$en_b0400 <- ifelse(data_set$enb_04z1 == 1, 1,
                       ifelse(data_set$enb_04z1 == 2, 0, NA))
  data_set$en_b0500 <- ifelse(data_set$enb_05z1 == 1, 1,
                       ifelse(data_set$enb_05z1 == 2, 0, NA))
  data_set$en_b0600 <- ifelse(data_set$enb_06z1 == 1, 1,
                       ifelse(data_set$enb_06z1 == 2, 0, NA))
  data_set$en_b0700 <- ifelse(data_set$enb_07z1 == 1, 1,
                       ifelse(data_set$enb_07z1 == 2, 0, NA))
