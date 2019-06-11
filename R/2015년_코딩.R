
  #### 흡연영역

  #### 현재비흡연자의 가정실내 간접흡연 노출률
  #### 분모정의
  data_set$sm_e0100 <- ifelse(data_set$sma_01z2 == 2, 1,
                       ifelse(data_set$sma_01z2 == 1,
                              ifelse(data_set$sma_03z2 %in% c(1,2), 0,
                              ifelse(data_set$sma_03z2 == 3, 1, NA))))
  #### 분자정의
  data_set$sm_c0700 <- ifelse(data_set$smc_08z1 == 1,
                              ifelse(data_set$smc_09z1 == 1, 1,
                              ifelse(data_set$smc_09z1 == 2, 0,
                       ifelse(data_set$smc_08z1 == 2, 0, NA))))
  
  #### 현재비흡연자의 직장실내 간접흡연 노출률
  #### 분모정의
  data_set$sm_e0200 <- ifelse(data_set$smc_10z1 %in% c(1,2),
                              ifelse(data_set$sma_01z2 == 2, 1,
                              ifelse(data_Set$sma_01z2 == 1,
                                     ifelse(data_set$sma_03z2 %in% c(1,2), 0,
                                     ifelse(data_set$sma_03z2 == 3, 1, NA)))))
  #### 분자정의
  data_set$sm_c0800 <- ifelse(data_set$smc_10z1 == 1, 1,
                       ifelse(data_Set$smc_10z1 == 2, 0,
                       ifelse(data_set$smc_10z1 == 3, ., NA)))
  
  #### 현재 전자담배 사용 경험률
  #### 분모정의
  data_set$sm_a1000 <- ifelse(data_set$sma_08z1 == 1,
                              ifelse(data_set$sma_09z1 == 1, 1,
                              ifelse(data_set$sma_09z1 == 2, 0,
                       ifelse(data_set$sma_08z1 == 2, 0, NA))))
  
  #### 과거흡연자의 금연기간_1년 미만
  #### 분모정의
  data_set$sm_d2300 <- ifelse(data_set$sma_01z2 == 1,
                              ifelse(data_set$sma_03z2 == 3, 1,
                              ifelse(data_set$sma_03z2 %in% c(1,2), 0,
                       ifelse(data_set$sma_01z2 == 2, 0, NA))))
  #### 분자정의
  data_set$sm_d2601 <- ifelse(data_set$smb_09z1 == 1, 1,
                       ifelse(data_set$smb_09z1 %in% c(2:6), 0, NA))
  
  #### 과거흡연자의 금연기간_1년~5년 미만
  data_set$sm_d2602 <- ifelse(data_set$smb_09z2 == 2, 1,
                       ifelse(data_set$smb_09z2 %in% c(1,3,4,5,6), 0, NA))
  
  #### 과거흡연자의 금연기간상_5년~10년 미만
  data_set$sm_d2603 <- ifelse(data_set$smb_09z2 == 3, 1,
                       ifelse(data_set$smb_09z2 %in% c(1,2,4,5,6), 0, NA))
  
  #### 과거흡연자의 금연기간_10년~15년 미만
  data_set$sm_d2604 <- ifelse(data_set$smb_09z2 == 4, 1,
                       ifelse(data_set$smb_09z2 %in% c(1,2,3,5,6), 0, NA))
  
  #### 과거흡연자의 금연기간_15년~20년 미만
  data_set$sm_d2605 <- ifelse(data_set$smb_09z2 == 5, 1,
                       ifelse(data_set$smb_09z2 %in% c(1,2,3,4,6), 0, NA))
  
  #### 과거흡연자의 금연기간_20년 이상
  data_set$sm_d2606 <- ifelse(data_set$smb_09z2 == 6, 1,
                       ifelse(data_set$smb_09z2 %in% c(1:5), 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_자신의 의지
  #### 분모정의
  data_set$sm_d1600 <- ifelse(data_set$smd_02z2 %in% c(1,2), 1,
                       ifelse(data_set$smd_02z2 %in% c(3), 0, NA))
  #### 분자정의
  data_set$sm_d2201 <- ifelse(data_set$smd_05a3 == 1, 1,
                       ifelse(data_set$smd_05a3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_니코틴 대체용품
  data_set$sm_d2202 <- ifelse(data_set$smd_05b3 == 1, 1,
                       ifelse(data_set$smd_05b3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_금연초
  data_set$sm_d2203 <- ifelse(data_set$smd_05c3 == 1, 1,
                       ifelse(data_set$smd_05c3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_보건소 금연클리닉
  data_set$sm_d2204 <- ifelse(data_set$smd_05d3 == 1, 1,
                       ifelse(data_set$smd_05d3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_금연상담센터
  data_set$sm_d2205 <- ifelse(data_set$smd_05e3 == 1, 1,
                       ifelse(data_set$smd_05e3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_의사 처방
  data_set$sm_d2206 <- ifelse(data_set$smd_05f3 == 1, 1,
                       ifelse(data_set$smd_05f3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_금연침
  data_set$sm_d2207 <- ifelse(data_set$smd_05g3 == 1, 1,
                       ifelse(data_set$smd_05g3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자의 금연방법_기타
  data_set$sm_d2208 <- ifelse(data_set$smd_05h3 == 1, 1,
                       ifelse(data_set$smd_05h3 == 2, 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_건강이 나빠져서
  #### 분모정의
  data_set$sm_d1700 <- ifelse(data_set$smd_01z2 %in% c(1:3) | data_set$smd_02z2 %in% c(1,2), 1,
                       ifelse(data_set$smd_01z2 == 4 & data_set$smd_02z2 == 3, 0, NA))
  #### 분자정의
  data_set$sm_d0701 <- ifelse(data_set$smd_03z2 == 1, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_향후건강
  data_set$sm_d0702 <- ifelse(data_set$smd_03z2 == 2, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_가족건강
  data_set$sm_d0703 <- ifelse(data_set$smd_03z2 == 3, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_주위피해
  data_set$sm_d0704 <- ifelse(data_set$smd_03z2 == 4, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_주위권유
  data_set$sm_d0705 <- ifelse(data_set$smd_03z2 == 5, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_담뱃값부담
  data_set$sm_d0706 <- ifelse(data_set$smd_03z2 == 6, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_공익광고,문구보고
  data_set$sm_d0707 <- ifelse(data_set$smd_03z2 == 7, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_흡연장소가없어
  data_set$sm_d0708 <- ifelse(data_set$smd_03z2 == 8, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재흡연자 중 금연시도자 또는 금연계획자의 금연이유_기타
  data_set$sm_d0709 <- ifelse(data_set$smd_03z2 == 9, 1,
                       ifelse(data_set$smd_03z2 %in% c(1:9), 0, NA))
  
  #### 현재비흡연자의 공공장소 간접흡연 노출률
  data_set$sm_c0500 <- ifelse(data_set$smc_07z1 == 1, 1,
                       ifelse(data_set$smc_07z1 == 2, 0, NA))
  
  #### 현재흡연자의 의료인 금연권고 경험률
  data_set$sm_d1700 <- ifelse(data_set$smd_14z1 == 1, 1,
                       ifelse(data_set$smd_14z1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 경험률
  #### 분모정의
  data_set$sm_a1200 <- ifelse(data_set$sma_01z2 == 1,
                              ifelse(data_set$sma_03z2 %in% c(1,2), 1,
                              ifelse(data_Set$sma_03z2 == 3,
                                     ifelse(data_set$smb_09z1 == 1, 1,
                                     ifelse(data_set$smb_09z1 %in% c(2:6), 0,
                       ifelse(data_set$sma_01z2 == 2, 0, NA))))))
  #### 분자정의
  data_set$sm_a1100 <- ifelse(data_set$sma_10z1 == 1, 1,
                       ifelse(data_set$sma_10z1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 유형_담배를 끊었다
  #### 분모정의
  data_set$sm_a1400 <- ifelse(data_set$sm_a1200 == 1,
                              ifelse(data_set$sma_10z1 == 1, 1,
                              ifelse(data_set$sma_10z1 == 2, 0, NA)))
  #### 분자정의
  data_set$sm_a1301 <- ifelse(data_set$sma_11a1 == 1, 1,
                       ifelse(data_set$sma_11a1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 유형_담배를 더 적게 피운다
  data_set$sm_a1302 <- ifelse(data_set$sma_11b1 == 1, 1,
                       ifelse(data_set$sma_11b1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 유형_가격이 더 저렴한 담배를 피운다
  data_set$sm_a1303 <- ifelse(data_set$sma_11c1 == 1, 1,
                       ifelse(data_set$sma_11c1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 유형_전자담배를 사용한다
  data_set$sm_a1304 <- ifelse(data_set$sma_11d1 == 1, 1,
                       ifelse(data_set$sma_11d1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 유형_다른 담배 제품을 사용한다
  data_set$sm_a1305 <- ifelse(data_set$sma_11e1 == 1, 1,
                       ifelse(data_set$sma_11e1 == 2, 0, NA))
  
  #### 담뱃값 인상 영향 유형_기타
  data_set$sm_a1306 <- ifelse(data_set$sma_11f1 == 1, 1,
                       ifelse(data_set$sma_11f1 == 2, 0, NA))
  
  #### 금연정책 강화 필요 영역_금연구역 확대
  data_set$sm_d2801 <- ifelse(data_set$smd_17z1 == 1, 1,
                       ifelse(data_set$smd_17z1 %in% c(1:5), 0, NA))
  
  #### 금연정책 강화 필요 영역_금연구역 내 흡연 단속
  data_set$sm_d2802 <- ifelse(data_set$smd_17z1 == 2, 1,
                       ifelse(data_set$smd_17z1 %in% c(1:5), 0, NA))
  
  #### 금연정책 강화 필요 영역_금연클리닉 확대 운영
  data_set$sm_d2803 <- ifelse(data_set$smd_17z1 == 3, 1,
                       ifelse(data_set$smd_17z1 %in% c(1:5), 0, NA))
  
  #### 금연정책 강화 필요 영역_금연 교육 및 홍보 강화
  data_set$sm_d2804 <- ifelse(data_set$smd_17z1 == 4, 1,
                       ifelse(data_set$smd_17z1 %in% c(1:5), 0, NA))
  
  #### 금연정책 강화 필요 영역_더 강화할 정책이 없다
  data_set$sm_d2805 <- ifelse(data_set$smd_17z1 == 5, 1,
                       ifelse(data_set$smd_17z1 %in% c(1:5), 0, NA))
  
  #### 금연구역 법률 인지율
  data_set$sm_d2900 <- ifelse(data_set$smd_15z1 == 1 & data_set$smd_16z1 == 1, 1,
                       ifelse(data_set$smd_15z1 == 1 & data_set$smd_16z1 == 2, 0,
                       ifelse(data_set$smd_15z1 == 2 & data_set$smd_16z1 == 1, 0,
                       ifelse(data_set$smd_15z1 == 2 & data_set$smd_16z1 == 2, 0, NA))))
  
  #### 평생 다른 담배 제품 사용 경험률
  data_set$sm_a1500 <- ifelse(data_set$sma_12z1 == 1, 1,
                       ifelse(data_set$sma_12z1 == 2, 0, NA))
  
  #### 현재 다른 담배 제품 사용 경험률
  data_set$sm_a1600 <- ifelse(data_set$sma_13z1 == 1, 1,
                       ifelse(data_set$sma_13z1 == 2, 0, NA))
  
  #### 지역선택문항
  
  #### 현재 흡연자의 금연권고 경험률
  data_set$sm_d1800 <- ifelse(data_set$smd_10z1 %in% c(3,4), 1,
                       ifelse(data_set$smd_10z1 %in% c(1,2), 0, NA))
  
  #### 금연클리닉·금연상담전화 인지율
  data_set$sm_d1900 <- ifelse(data_set$smd_11z1 == 1, 1,
                       ifelse(data_set$smd_11z1 == 2, 0, NA))

  #### 현재 흡연자의 금연클리닉·금연상담전화 이용률
  #### 분모정의
  data_set$sm_d2000 <- ifelse(data_set$sm_a0100 == 1,
                              ifelse(data_set$sm_d1900 == 1, 1,
                              ifelse(data_set$sm_d1900 == 0, 0,
                       ifelse(data_set$sm_a0100 == 0, 0, NA))))
  #### 분자정의
  data_set$sm_d2100 <- ifelse(data_set$smd_12z1 == 1, 1,
                       ifelse(data_set$smd_12z1 == 2, 0, NA))
  
  #### 흡연시작동기_주위사람이 권해서
  data_set$sm_a0601 <- ifelse(data_set$sma_04z1 == 1, 1,
                       ifelse(data_set$sma_04z1 %in% c(1:6), 0, NA))
  
  #### 흡연시작동기_호기심
  data_set$sm_a0602 <- ifelse(data_set$sma_04z1 == 2, 1,
                       ifelse(data_set$sma_04z1 %in% c(1:6), 0, NA))
  
  #### 흡연시작동기_스트레스
  data_set$sm_a0603 <- ifelse(data_set$sma_04z1 == 3, 1,
                       ifelse(data_set$sma_04z1 %in% c(1:6), 0, NA))
  
  #### 흡연시작동기_대인관계
  data_set$sm_a0604 <- ifelse(data_set$sma_04z1 == 4, 1,
                       ifelse(data_set$sma_04z1 %in% c(1:6), 0, NA))
  
  #### 흡연시작동기_매스컴
  data_set$sm_a0605 <- ifelse(data_set$sma_04z1 == 5, 1,
                       ifelse(data_set$sma_04z1 %in% c(1:6), 0, NA))
  
  #### 흡연시작동기_기타
  data_set$sm_a0606 <- ifelse(data_set$sma_04z1 == 6, 1,
                       ifelse(data_set$sma_04z1 %in% c(1:6), 0, NA))
  
  #### 효과적인 금연프로그램_공익광고
  data_set$sm_d2501 <- ifelse(data_set$smd_13z1 == 1, 1,
                       ifelse(data_set$smd_13z1 %in% c(1:4), 0, NA))
  
  #### 효과적인 금연프로그램_금연교육
  data_set$sm_d2502 <- ifelse(data_set$smd_13z1 == 2, 1,
                       ifelse(data_set$smd_13z1 %in% c(1:4), 0, NA))
  
  #### 효과적인 금연프로그램_금연클리닉
  data_set$sm_d2503 <- ifelse(data_set$smd_13z1 == 3, 1,
                       ifelse(data_set$smd_13z1 %in% c(1:4), 0, NA))
  
  #### 효과적인 금연프로그램_금연상담전화
  data_set$sm_d2504 <- ifelse(data_set$smd_13z1 == 4, 1,
                       ifelse(data_set$smd_13z1 %in% c(1:4), 0, NA))
  
  #### 매일 흡연시작 연령
  data_set$sm_b0300 <- ifelse(data_set$sma_01z2 == 1,
                              ifelse((data_set$smb_10z1>=0 & data_set$smb_10z1<= 110) &
                                    (data_set$smb_10z1>=0 & data_set$smb_10z1<=data_set$age), data_set$smb_10z1, NA))
  
  #### 연간 금연시도 횟수
  data_set$sm_d3000 <- ifelse(data_set$smd_18z1>0 & data_set$smd_18z1<=76, data_set$smd_18z1, NA)
  
  #### 담배 판매점 담배 광고 노출 경험률
  data_set$sm_e0500 <- ifelse(data_set$sme_01z1 == 1, 1,
                       ifelse(data_set$sme_01z1 == 2, 0, NA))
  
  #### 전자담배 사용 이유_담배보다 덜 해로울 것 같아서
  data_set$sm_a1701 <- ifelse(data_set$sma_14z1 == 1, 1,
                       ifelse(data_set$sma_14z1 %in% c(1:6), 0, NA))
  
  #### 전자담배 사용 이유_금연하는데 도움이 될 것 같아서
  data_set$sm_a1702 <- ifelse(data_set$sma_14z1 == 1, 2,
                       ifelse(data_set$sma_14z1 %in% c(1:6), 0, NA))
  
  #### 전자담배 사용 이유_담배보다 가격이 저렴해서
  data_set$sm_a1703 <- ifelse(data_set$sma_14z1 == 1, 3,
                       ifelse(data_set$sma_14z1 %in% c(1:6), 0, NA))
  
  #### 전자담배 사용 이유_담배연기가 나지 않아서
  data_set$sm_a1704 <- ifelse(data_set$sma_14z1 == 1, 4,
                       ifelse(data_set$sma_14z1 %in% c(1:6), 0, NA))
  
  #### 전자담배 사용 이유_향이나 맛이 다양해서
  data_set$sm_a1705 <- ifelse(data_set$sma_14z1 == 1, 5,
                       ifelse(data_set$sma_14z1 %in% c(1:6), 0, NA))
  
  #### 전자담배 사용 이유_기타
  data_set$sm_a1706 <- ifelse(data_set$sma_14z1 == 1, 6,
                       ifelse(data_set$sma_14z1 %in% c(1:6), 0, NA))
  
  #### 전자담배 사용 기간_1년 미만
  data_set$sm_a1801 <- ifelse(data_set$sma_15z1 == 1, 1,
                       ifelse(data_set$sma_15z1 %in% c(1:4), 0, NA))
  
  #### 전자담배 사용 기간_1년 이상 2년 미만
  data_set$sm_a1802 <- ifelse(data_set$sma_15z1 == 2, 1,
                       ifelse(data_set$sma_15z1 %in% c(1:4), 0, NA))
  
  #### 전자담배 사용 기간_2년 이상 3년 미만
  data_set$sm_a1803 <- ifelse(data_set$sma_15z1 == 3, 1,
                       ifelse(data_set$sma_15z1 %in% c(1:4), 0, NA))
  
  #### 전자담배 사용 기간_3년 이상
  data_set$sm_a1804 <- ifelse(data_set$sma_15z1 == 4, 1,
                       ifelse(data_set$sma_15z1 %in% c(1:4), 0, NA))
  
  #### 음주영역
  
  #### 연간 음주폐해 경험률
  data_set$dr_f0100 <- ifelse(data_set$drf_01z1 == 1, 1,
                       ifelse(data_set$drf_01z1 == 2, 0, NA))
  
  #### 연간 절주 또는 금주시도율
  data_set$dr_g0100 <- ifelse(data_set$drb_02z1 == 1,
                              ifelse(data_set$drg_01z1 == 1, 1,
                              ifelse(data_set$drg_01z1 == 2, 0, NA)))
  
  #### 지역선택문항
  
  ####평생 음주예방 교육 경험률
  data_set$dr_e0200 <- ifelse(data_set$dre_02z1 == 1, 1,
                       iflese(data_set$dre_02z1 == 2, 0, NA))
  
  data_set$dr_e0100 <- ifelse(data_set$dre_01z1 == 1, 1,
                       ifelse(data_set$dre_02z1 == 2, 0, NA))
  
  #### 안전의식영역
  
  #### 동승차량 뒷좌석 안전벨트 착용률
  #### 분모정의
  data_set$sf_a0500 <- ifelse(data_set$sfa_12z1 %in% c(2:5), 1,
                       ifelse(data_set$sfa_12z1 == 1, 0, NA))
  #### 분자정의
  data_set$sf_a0600 <- ifelse(data_set$sfa_12z1 %in% c(2:4), 0,
                       ifelse(data_set$sfa_12z1 == 5, 1, NA))
  
  #### 버스 안전벨트 착용률
  #### 분모정의
  data_set$sf_a0700 <- ifelse(data_set$sfa_13z1 %in% c(2:5), 1, 
                       ifelse(data_set$sfa_13z1 == 1, 0, NA))
  #### 분자정의
  data_set$sf_a0800 <- ifelse(data_set$sfa_13z1 %in% c(2:4), 0,
                       ifelse(data_set$sfa_13z1 == 5, 1, NA))
  
  #### 연간 자전거 음주운전 경험률
  #### 분모정의
  data_set$sf_c0300 <- ifelse(data_set$sfa_07z1 == 1, 1,
                       ifelse(data_set$sfa_07z1 == 2, 0, NA))
  #### 분자정의
  data_set$sf_b1000 <- ifelse(data_set$sfb_01z1 == 1, 1,
                       ifelse(data_set$sfb_01z1 %in% c(7,9), ., 0))
  
  #### 연간 스쿨존 과속운전 경험률
  #### 분모정의
  data_set$sf_e0400 <- ifelse(data_set$sfa_01z1 == 1 & data_set$sfa_10z2 %in% c(1,2), 1,
                       ifelse(data_set$sfa_01z1 == 2 | data_set$sfa_10z2 %in% c(3,4), 0, NA))
  #### 분자정의
  data_set$sf_e0500 <- ifelse(data_set$sfa_10z2 == 1, 1,
                       ifelse(data_set$sfa_10z2 %in% c(2:4), 0, NA))

  #### 신체활동영역
  
  #### 지역선택문항
  
  ### 지역사회 내 운동프로그램 참여율
  data_set$ph_c0200 <- ifelse(data_set$phc_02z1 == 1, 1,
                       ifelse(data_set$phc_02z1 == 2, 0, NA))
  
  #### 월간 규칙적 운동 실천율
  data_set$ph_a1400 <- ifelse(data_set$pha_19z1 == 1, 1,
                       ifelse(data_set$pha_19z1 == 2, 0, NA))
  
  #### 운동장소_공원
  data_set$ph_c0301 <- ifelse(data_set$phc_03z1 == 1, 1,
                       ifelse(data_set$phc_03z1 %in% c(1:6), 0, NA))
  
  #### 운동장소_체육관
  data_set$ph_c0302 <- ifelse(data_set$phc_03z1 == 2, 1,
                       ifelse(data_set$phc_03z1 %in% c(1:6), 0, NA))
  
  #### 운동장소_체육센터
  data_set$ph_c0303 <- ifelse(data_set$phc_03z1 == 3, 1,
                       ifelse(data_set$phc_03z1 %in% c(1:6), 0, NA))
  
  #### 운동장소_학교운동장
  data_set$ph_c0304 <- ifelse(data_set$phc_03z1 == 4, 1,
                       ifelse(data_set$phc_03z1 %in% c(1:6), 0, NA))
  
  #### 운동장소_가정실내
  data_set$ph_c0305 <- ifelse(data_set$phc_03z1 == 5, 1,
                       ifelse(data_set$phc_03z1 %in% c(1:6), 0, NA))
  
  #### 운동장소_기타
  data_set$ph_c0306 <- ifelse(data_set$phc_03z1 == 6, 1,
                       ifelse(data_set$phc_03z1 %in% c(1:6), 0, NA))
  
  #### 희망 운동종목_수영
  data_set$ph_a1501 <- ifelse(data_set$pha_17z1 == 1, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_등산
  data_set$ph_a1502 <- ifelse(data_set$pha_17z1 == 2, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_요가
  data_set$ph_a1503 <- ifelse(data_set$pha_17z1 == 3, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_걷기
  data_set$ph_a1504 <- ifelse(data_set$pha_17z1 == 4, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_헬스
  data_set$ph_a1505 <- ifelse(data_set$pha_17z1 == 5, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_골프
  data_set$ph_a1506 <- ifelse(data_set$pha_17z1 == 6, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_댄스스포츠
  data_set$ph_a1507 <- ifelse(data_set$pha_17z1 == 7, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))

  #### 희망 운동종목_테니스
  data_set$ph_a1508 <- ifelse(data_set$pha_17z1 == 8, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_에어로빅
  data_set$ph_a1509 <- ifelse(data_set$pha_17z1 == 9, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 희망 운동종목_기타
  data_set$ph_a1510 <- ifelse(data_set$pha_17z1 == 10, 1,
                       ifelse(data_set$pha_17z1 %in% c(1:10), 0, NA))
  
  #### 연간 체육활동 참여율
  data_set$ph_a1600 <- ifelse(data_set$pha_14z1 == 1, 1,
                       ifelse(data_set$pha_14z1 == 2, 0, NA))
  
  #### 체육활동 미참여 이유_시간이 없어서
  #### 분모정의
  data_set$ph_a1800 <- ifelse(data_set$pha_14z1 == 2, 1,
                       ifelse(data_set$pha_14z1 == 1, 0, NA))
  #### 분자정의
  data_set$ph_a1701 <- ifelse(data_set$pha_15z1 == 1, 1,
                       ifelse(data_set$pha_15z1 %in% c(1:6), 0, NA))
  
  #### 체육활동 미참여 이유_경제적인 이유로
  data_set$ph_a1702 <- ifelse(data_set$pha_15z1 == 1, 1,
                       ifelse(data_set$pha_15z1 %in% c(1:6), 0, NA))
  
  #### 체육활동 미참여 이유_체육활동을 할 수 있는 장소가 없어서
  data_set$ph_a1703 <- ifelse(data_set$pha_15z1 == 2, 1,
                       ifelse(data_set$pha_15z1 %in% c(1:6), 0, NA))
  
  #### 체육활동 미참여 이유_건강상의 이유로
  data_set$ph_a1704 <- ifelse(data_set$pha_15z1 == 3, 1,
                       ifelse(data_set$pha_15z1 %in% c(1:6), 0, NA))
  
  #### 체육활동 미참여 이유_같이 체육활동을 할 수 있는 사람이 없어서
  data_set$ph_a1705 <- ifelse(data_set$pha_15z1 == 4, 1,
                       ifelse(data_set$pha_15z1 %in% c(1:6), 0, NA))
  
  #### 체육활동 미참여 이유_기타
  data_set$ph_a1706 <- ifelse(data_set$pha_15z1 == 5, 1,
                       ifelse(data_set$pha_15z1 %in% c(1:6), 0, NA))
  
  #### 식생활영역
  
  #### 영양표시 인지율
  data_set$nu_c0200 <- ifelse(data_set$nuc_02z1 == 1, 1,
                       ifelse(data_set$nuc_02z1 == 2, 0, NA))
  
  #### 영양표시 독해율
  data_set$nu_c0300 <- ifelse(data_set$nu_c0200 == 1,
                              ifelse(data_set$nuc_01z2 == 1, 1,
                              ifelse(data_set$nuc_01z2 == 2, 0,
                       ifelse(data_set$nu_c0200 == 0, 0, NA))))
  
  #### 영향표시 활용률
  data_set$nu_c0400 <- ifelse(data_set$nu_c0200 == 1 & data_set$nu_c0300 == 1,
                              ifelse(data_set$nuc_03z1 == 1, 1,
                              ifelse(data_set$nuc_03z1 == 2, 0, NA)))
  
  #### 국 또는 찌개 섭취율
  data_set$nu_d0800 <- ifelse(data_set$nub_05z1 %in% c(1,2), 1,
                       ifelse(data_set$nub_05z1 %in% c(3:5), 0, NA))
  
  #### 저염식 실천 노력 경험률
  data_set$nu_b0700 <- ifelse(data_set$nub_04z1 %in% c(4,5), 1,
                       ifelse(data_set$nub_04z1 %in% c(1:3), 0, NA))
  
  #### 연간 외식 빈도_일2회이상
  data_set$nu_a0401 <- ifelse(data_set$nua_08z1 == 1, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_일1회
  data_set$nu_a0402 <- ifelse(data_set$nua_08z1 == 2, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_주5-6회
  data_set$nu_a0403 <- ifelse(data_set$nua_08z1 == 3, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_주3-4회
  data_set$nu_a0404 <- ifelse(data_set$nua_08z1 == 4, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_주1-2회
  data_set$nu_a0405 <- ifelse(data_set$nua_08z1 == 5, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_월1-3회
  data_set$nu_a0406 <- ifelse(data_set$nua_08z1 == 6, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_거의안함
  data_set$nu_a0407 <- ifelse(data_set$nua_08z1 == 7, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 연간 외식 빈도_전혀안함
  data_set$nu_a0408 <- ifelse(data_set$nua_08z1 == 8, 1,
                       ifelse(data_set$nua_08z1 %in% c(1:8), 0, NA))
  
  #### 구강건강영역
  
  #### 아침식사 후 칫솔질 실천율
  #### 분모정의
  data_set$or_d0500 <- ifelse(data_set$ord_01b2 %in% c(1,2), 1, 0)
  #### 분자정의
  data_set$or_d0600 <- ifelse(data_set$ord_01b2 == 1, 1,
                       ifelse(data_set$ord_01b2 == 2, 0, NA))
  
  #### 저녁식사 후 칫솔질 실천율
  #### 분모정의
  data_set$or_d0700 <- ifelse(data_set$ord_01f2 %in% c(1,2), 1, 0)
  #### 분자정의
  data_set$or_d0800 <- ifelse(data_set$ord_01f2 == 1, 1,
                       ifelse(data_set$ord_01f2 == 2, 0, NA))
  
  #### 잠자기 전 칫솔질 실천율
  #### 분모정의
  data_set$or_d0900 <- ifelse(data_set$ord_01h1 %in% c(1,2), 1, 0)
  #### 분자정의
  data_set$or_d1000 <- ifelse(data_set$ord_01h1 == 1, 1,
                       ifelse(data_set$ord_01h1 == 2, 0, NA))
  
  #### 주관적 틀니 필요 인지율(50세 이상)
  #### 분모정의
  data_set$or_c0300 <- ifelse(data_set$age_o50 == 1 & data_set$orc_01z1 == 4, 1, NA)
  #### 분자정의
  data_set$or_c0400 <- ifelse(data_set$orc_02z1 == 1, 1,
                       ifelse(data_set$orc_02z1 == 2, 0, NA))
  
  #### 연간 치과병의원 이용률
  data_set$or_e0700 <- ifelse(data_set$ore_08z1 == 1, 1,
                       ifelse(data_set$ore_08z1 == 2, 0, NA))
  
  #### 치과 진료 할목_구강검사
  data_set$or_e0801 <- ifelse(data_set$ore_09a1 == 1, 1,
                       ifelse(data_set$ore_09a1 == 2, 0, NA))
  
  #### 치과 진료 할목_잇몸병치료
  data_set$or_e0802 <- ifelse(data_set$ore_09b1 == 1, 1,
                       ifelse(data_set$ore_09b1 == 2, 0, NA))
  
  #### 치과 진료 할목_단순 충치치료
  data_set$or_e0803 <- ifelse(data_set$ore_09c1 == 1, 1,
                       ifelse(data_set$ore_09c1 == 2, 0, NA))
  
  #### 치과 진료 할목_치아신경치료
  data_set$or_e0804 <- ifelse(data_set$ore_09d1 == 1, 1,
                       ifelse(data_set$ore_09d1 == 2, 0, NA))
  
  #### 치과 진료 할목_예방치료
  data_set$or_e0805 <- ifelse(data_set$ore_09e1 == 1, 1,
                       ifelse(data_set$ore_09e1 == 2, 0, NA))
  
  #### 치과 진료 할목_발치 홋은 구강 내 수술
  data_set$or_e0806 <- ifelse(data_set$ore_09f1 == 1, 1,
                       ifelse(data_set$ore_09f1 == 2, 0, NA))
  
  #### 치과 진료 할목_다쳐서 빠지거나 부러진 치아의 치료
  data_set$or_e0807 <- ifelse(data_set$ore_09g1 == 1, 1,
                       ifelse(data_set$ore_09g1 == 2, 0, NA))
  
  #### 치과 진료 할목_보철물 제작 혹은 수리
  data_set$or_e0808 <- ifelse(data_set$ore_09h1 == 1, 1,
                       ifelse(data_set$ore_09h1 == 2, 0, NA))
  
  #### 치과 진료 할목_기타
  data_set$or_e0809 <- ifelse(data_set$ore_09i1 == 1, 1,
                       ifelse(data_set$ore_09i1 == 2, 0, NA))
  
  #### 정신건강영역
  
  #### 지역선택문항
  
  #### 우울감 원인_경제적인 문제
  data_set$mt_b0301 <- ifelse(data_set$mtb_03z1 == 1, 1,
                       ifelse(data_set$mtb_03z1 %in% c(1:6), 0, NA))
  
  #### 우울감 원인_질병문제
  data_set$mt_b0302 <- ifelse(data_set$mtb_03z1 == 2, 1,
                       ifelse(data_set$mtb_03z1 %in% c(1:6), 0, NA))
  
  #### 우울감 원인_가족문제
  data_set$mt_b0303 <- ifelse(data_set$mtb_03z1 == 3, 1,
                       ifelse(data_set$mtb_03z1 %in% c(1:6), 0, NA))
  
  #### 우울감 원인_진로문제
  data_set$mt_b0304 <- ifelse(data_set$mtb_03z1 == 4, 1,
                       ifelse(data_set$mtb_03z1 %in% c(1:6), 0, NA))
  
  #### 우울감 원인_대인관계
  data_set$mt_b0305 <- ifelse(data_set$mtb_03z1 == 5, 1,
                       ifelse(data_set$mtb_03z1 %in% c(1:6), 0, NA))
  
  #### 우울감 원인_기타
  data_set$mt_b0306 <- ifelse(data_set$mtb_03z1 == 6, 1,
                       ifelse(data_set$mtb_03z1 %in% c(1:6), 0, NA))
  
  #### 스트레스 관리방법_운동
  data_set$mt_a0301 <- ifelse(data_set$mta_09a1 == 1, 1,
                       ifelse(data_set$mta_09a1 == 2, 0, NA))
  
  #### 스트레스 관리방법_수면
  data_set$mt_a0302 <- ifelse(data_set$mta_09b1 == 1, 1,
                       ifelse(data_set$mta_09b1 == 2, 0, NA))
  
  #### 스트레스 관리방법_음주
  data_set$mt_a0303 <- ifelse(data_set$mta_09c1 == 1, 1,
                       ifelse(data_set$mta_09c1 == 2, 0, NA))
  
  #### 스트레스 관리방법_흡연
  data_set$mt_a0304 <- ifelse(data_set$mta_09d1 == 1, 1,
                       ifelse(data_set$mta_09d1 == 2, 0, NA))
  
  #### 스트레스 관리방법_친교활동
  data_set$mt_a0305 <- ifelse(data_set$mta_09e1 == 1, 1,
                       ifelse(data_set$mta_09e1 == 2, 0, NA))
  
  #### 스트레스 관리방법_취미활동
  data_set$mt_a0306 <- ifelse(data_set$mta_09f1 == 1, 1,
                       ifelse(data_set$mta_09f1 == 2, 0, NA))
  
  #### 스트레스 관리방법_종교활동
  data_set$mt_a0307 <- ifelse(data_set$mta_09g1 == 1, 1,
                       ifelse(data_set$mta_09g1 == 2, 0, NA))
  
  #### 스트레스 관리방법_기타
  data_set$mt_a0308 <- ifelse(data_set$mta_09h1 == 1, 1,
                       ifelse(data_set$mta_09h1 == 2, 0, NA))
  
  #### 스트레스 원인_경제
  data_set$mt_a0401 <- ifelse(data_set$mta_08z1 == 1, 1,
                       ifelse(data_set$mta_08z1 %in% c(1:6), 0, NA))
  
  #### 스트레스 원인_질병
  data_set$mt_a0402 <- ifelse(data_set$mta_08z1 == 2, 1,
                       ifelse(data_set$mta_08z1 %in% c(1:6), 0, NA))
  
  #### 스트레스 원인_가족
  data_set$mt_a0403 <- ifelse(data_set$mta_08z1 == 3, 1,
                       ifelse(data_set$mta_08z1 %in% c(1:6), 0, NA))
  
  #### 스트레스 원인_진로
  data_set$mt_a0404 <- ifelse(data_set$mta_08z1 == 4, 1,
                       ifelse(data_set$mta_08z1 %in% c(1:6), 0, NA))
  
  #### 스트레스 원인_대인관계
  data_set$mt_a0405 <- ifelse(data_set$mta_08z1 == 5, 1,
                       ifelse(data_set$mta_08z1 %in% c(1:6), 0, NA))
  
  #### 스트레스 원인_기타
  data_set$mt_a0406 <- ifelse(data_set$mta_08z1 == 6, 1,
                       ifelse(data_set$mta_08z1 %in% c(1:6), 0, NA))
  
  #### 월간 스트레스 증상 경험률_저스트레스군
  data_set$tmp_mta_03z1 <- ifelse(data_set$mta_03z1 %in% c(1:5), data_set$mta_03z1, NA )
  data_set$tmp_mta_04z1 <- ifelse(data_set$mta_04z1 %in% c(1:5), data_set$mta_04z1, NA )
  data_set$tmp_mta_05z1 <- ifelse(data_set$mta_05z1 %in% c(1:5), data_set$mta_05z1, NA )
  data_set$tmp_mta_06z1 <- ifelse(data_set$mta_06z1 %in% c(1:5), data_set$mta_06z1, NA )
  data_set$tmp_mta_07z1 <- ifelse(data_set$mta_07z1 %in% c(1:5), data_set$mta_07z1, NA )
  data_set$tmp_stress <- (data_set$mta_03z1 + data_set$mta_04z1 + data_set$mta_05z1 + data_set$mta_06z1 + data_set$mta_07z1)/5
  data_set$mt_a0501 <- ifelse(data_set$tmp_stress<1.8, 1,
                       ifelse(data_set$tmp_stress>=1.8, 0, NA))
  
  #### 월간 스트레스 증상 경험률_중등도 스트레스군
  data_set$mt_a0502 <- ifelse(data_set$tmp_stress<1.8, 0,
                       ifelse(data_set$tmp_stress>=1.8 & data_set$tmp_stress<2.8, 1,
                       ifelse(data_set$tmp_stress>=2.8, 0, NA)))
  
  #### 월간 스트레스 증상 경험률_고스트레스군
  data_set$mt_a0503 <- ifelse(data_set$tmp_stress>= 2.8, 1,
                       ifelse(data_set$tmp_stress<2.8, 0, NA))
  
  #### 정신건강증진센터 인지율
  data_set$mt_h0100 <- ifelse(data_set$mth_01z1 == 1, 1,
                       ifelse(data_set$mth_01z1 == 2, 0, NA))
  
  #### 자살생각행동 원인_경제적 어려움 때문에
  data_set$mt_d0501 <- ifelse(data_set$mtd_05z1 == 1, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_이성 문제 때문에
  data_set$mt_d0502 <- ifelse(data_set$mtd_05z1 == 2, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_질병 또는 장애 때문에
  data_set$mt_d0503 <- ifelse(data_set$mtd_05z1 == 3, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_직장 문제 때문에
  data_set$mt_d0504 <- ifelse(data_set$mtd_05z1 == 4, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_외로움, 고독 때문에
  data_set$mt_d0505 <- ifelse(data_set$mtd_05z1 == 5, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_가정불화로 인해
  data_set$mt_d0506 <- ifelse(data_set$mtd_05z1 == 6, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_진로 문제로 인해
  data_set$mt_d0507 <- ifelse(data_set$mtd_05z1 == 7, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 자살생각행동 원인_기타
  data_set$mt_d0508 <- ifelse(data_set$mtd_05z1 == 8, 1,
                       ifelse(data_set$mtd_05z1 %in% c(1:8), 0, NA))
  
  #### 고민 상담자 여부
  data_set$mt_e0300 <- ifelse(data_set$mte_02z1 == 1, 1,
                       ifelse(data_set$mte_02z1 == 2, 0, NA))
  
  #### 이환영역(당뇨병)
  
  #### 당뇨병 약물치료 방법_인슐린
  data_set$il_b3001 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_06z1 == 1,
                                     ifelse(data_set$dia_13a1 == 1, 1,
                                     ifelse(data_set$dia_13a1 == 2, 0,
                              ifelse(data_set$dia_06z1 == 2, 0, NA)))))
  
  #### 당뇨병 약물치료 방법_당뇨병약
  data_set$il_b3002 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_06z1 == 1,
                                     ifelse(data_set$dia_13b1 == 1, 1,
                                     ifelse(data_set$dia_13b1 == 2, 0,
                              ifelse(data_set$dia_06z1 == 2, 0, NA)))))
  
  #### 당화혈색소 인지율
  data_set$il_b4300 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_21z2 == 1, 1,
                              ifelse(data_set$dia_21z2 == 2, 0, NA)))
  
  #### 당화혈색소 검사 횟수
  #### 분모정의
  data_set$il_b4500 <- ifelse(data_set$il_b0300 == 1,
                              ifelse(data_set$il_b4300 == 1, 1,
                              ifelse(data_set$il_b4300 == 0, 0, NA)))
  #### 분자정의
  data_set$il_b4401 <- ifelse(data_set$dia_22z1 == 1, 1,
                       ifelse(data_set %in% c(1:5,7,9), 0, NA))
  data_set$il_b4402 <- ifelse(data_set$dia_22z1 == 2, 1,
                       ifelse(data_set %in% c(1:5,7,9), 0, NA))
  data_set$il_b4403 <- ifelse(data_set$dia_22z1 == 3, 1,
                       ifelse(data_set %in% c(1:5,7,9), 0, NA))
  data_set$il_b4404 <- ifelse(data_set$dia_22z1 == 4, 1,
                       ifelse(data_set %in% c(1:5,7,9), 0, NA))
  data_set$il_b4405 <- ifelse(data_set$dia_22z1 == 5, 1,
                       ifelse(data_set %in% c(1:5,7,9), 0, NA))
  
  #### 이환영역(이상지질혈증(고지혈증 포함))
  
  #### 이상지질혈증의 약물치료율
  data_set$il_r0900 <- ifelse(data_set$il_r0100 == 1,
                              ifelse(data_set$dla_02z1 == 1,
                                     ifelse(data_set$dla_03a1 == 1 & data_set$dla_03b1>=0 & data_set$dla_03b1<=19, 0,
                                     ifelse(data_set$dla_03a1 == 1 & data_set$dla_03b1>=20 & data_set$dla_03b1<=31, 1,
                                     ifelse(data_set$dla_03a1 == 2, 0, NA)))),
                              ifelse(data_set$dla_02z1 == 2, 0, NA), NA)
  
  #### 이환영역(고혈압)
  
  #### 고혈압 미치료 이유_혈압 정상
  data_set$il_2901 <- ifelse(data_set$il_a0200 == 1,
                             ifelse(data_set$hya_28z1 == 1, 1,
                             ifelse(data_set$hya_28z1 %in% c(1:6), 0, NA)), NA)
  
  #### 고혈압 미치료 이유_증상 가벼움
  data_set$il_2902 <- ifelse(data_set$il_a0200 == 1,
                             ifelse(data_set$hya_28z1 == 2, 1,
                             ifelse(data_set$hya_28z1 %in% c(1:6), 0, NA)), NA)
  
  #### 고혈압 미치료 이유_경제적 이유
  data_set$il_2903 <- ifelse(data_set$il_a0200 == 1,
                             ifelse(data_set$hya_28z1 == 3, 1,
                             ifelse(data_set$hya_28z1 %in% c(1:6), 0, NA)), NA)
  
  #### 고혈압 미치료 이유_필요성 못느낌
  data_set$il_2904 <- ifelse(data_set$il_a0200 == 1,
                             ifelse(data_set$hya_28z1 == 4, 1,
                             ifelse(data_set$hya_28z1 %in% c(1:6), 0, NA)), NA)
  
  #### 고혈압 미치료 이유_병의원 멀어서
  data_set$il_2905 <- ifelse(data_set$il_a0200 == 1,
                             ifelse(data_set$hya_28z1 == 5, 1,
                             ifelse(data_set$hya_28z1 %in% c(1:6), 0, NA)), NA)
  
  #### 고혈압 미치료 이유_기타
  data_set$il_2906 <- ifelse(data_set$il_a0200 == 1,
                             ifelse(data_set$hya_28z1 == 6, 1,
                             ifelse(data_set$hya_28z1 %in% c(1:6), 0, NA)), NA)
  
  #### 고혈압 관리교육 장소 선호도_병의원
  data_set$il_a3001 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_26z1 == 1, 1,
                              ifelse(data_set$hya_26z1 %in% c(1:5), 0, NA)), NA)
  
  #### 고혈압 관리교육 장소 선호도_보건기관
  data_set$il_a3002 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_26z1 == 2, 1,
                              ifelse(data_set$hya_26z1 %in% c(1:5), 0, NA)), NA)
  
  #### 고혈압 관리교육 장소 선호도_공동이용시설
  data_set$il_a3003 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_26z1 == 3, 1,
                              ifelse(data_set$hya_26z1 %in% c(1:5), 0, NA)), NA)
  
  #### 고혈압 관리교육 장소 선호도_학교직장
  data_set$il_a3004 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_26z1 == 4, 1,
                              ifelse(data_set$hya_26z1 %in% c(1:5), 0, NA)), NA)
  
  #### 고혈압 관리교육 장소 선호도_기타
  data_set$il_a3005 <- ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$hya_26z1 == 5, 1,
                              ifelse(data_set$hya_26z1 %in% c(1:5), 0, NA)), NA)
  
  #### 이환영역(당뇨병)
  
  #### 당뇨병 미치료 이유_혈압 정상
  data_set$il_b4101 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_30z1 == 1, 1,
                              ifelse(data_set$dia_30z1 %in% c(1:6), 0, NA)), NA)
  
  #### 당뇨병 미치료 이유_증상 가벼움
  data_set$il_b4102 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_30z1 == 2, 1,
                              ifelse(data_set$dia_30z1 %in% c(1:6), 0, NA)), NA)
  
  #### 당뇨병 미치료 이유_경제적 이유
  data_set$il_b4103 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_30z1 == 3, 1,
                              ifelse(data_set$dia_30z1 %in% c(1:6), 0, NA)), NA)
  
  #### 당뇨병 미치료 이유_필요성 못느낌
  data_set$il_b4104 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_30z1 == 4, 1,
                              ifelse(data_set$dia_30z1 %in% c(1:6), 0, NA)), NA)
  
  #### 당뇨병 미치료 이유_병의원 멀어서
  data_set$il_b4105 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_30z1 == 5, 1,
                              ifelse(data_set$dia_30z1 %in% c(1:6), 0, NA)), NA)
  
  #### 당뇨병 미치료 이유_기타
  data_set$il_b4106 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_30z1 == 6, 1,
                              ifelse(data_set$dia_30z1 %in% c(1:6), 0, NA)), NA)
  
  #### 이환영역(뇌졸증)
  
  #### 뇌졸증 증상 대처 방법_병원으로 데려간다
  data_set$il_d0801 <- ifelse(data_set$cva_18z1 == 1, 1,
                       ifelse(data_set$cva_18z1 %in% c(1:5), 0, NA))
  
  #### 뇌졸증 증상 대처 방법_한방병원으로 데려간다
  data_set$il_d0802 <- ifelse(data_set$cva_18z1 == 2, 1,
                       ifelse(data_set$cva_18z1 %in% c(1:5), 0, NA))
  
  #### 뇌졸증 증상 대처 방법_119에 전화한다
  data_set$il_d0803 <- ifelse(data_set$cva_18z1 == 3, 1,
                       ifelse(data_set$cva_18z1 %in% c(1:5), 0, NA))
  
  #### 뇌졸증 증상 대처 방법_가족에게 연락한다
  data_set$il_d0804 <- ifelse(data_set$cva_18z1 == 4, 1,
                       ifelse(data_set$cva_18z1 %in% c(1:5), 0, NA))
  
  #### 뇌졸증 증상 대처 방법_기타
  data_set$il_d0805 <- ifelse(data_set$cva_18z1 == 5, 1,
                       ifelse(data_set$cva_18z1 %in% c(1:5), 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_통증은 없으면서 갑자기 한쪽 팔, 다리에 힘이 빠진다
  data_set$il_d0901 <- ifelse(data_set$cva_11z1 == 1, 1,
                       ifelse(data_set$cva_11z1 == 2, 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_말이 어눌해지거나 다른 사람의 말을 이해하지 못한다
  data_set$il_d0902 <- ifelse(data_set$cva_12z1 == 1, 1,
                       ifelse(data_set$cva_12z1 == 2, 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_좌측 가슴에 통증이 생긴다
  data_set$il_d0903 <- ifelse(data_set$cva_13z1 == 1, 1,
                       ifelse(data_set$cva_13z1 == 2, 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_시야의 오른쪽 반 혹은 왼쪽 반이 보이지 않는다
  data_set$il_d0904 <- ifelse(data_set$cva_14z1 == 1, 1,
                       ifelse(data_set$cva_14z1 == 2, 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_뒷목이 뻐근하다
  data_set$il_d0905 <- ifelse(data_set$cva_15z1 == 1, 1,
                       ifelse(data_set$cva_15z1 == 2, 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_몸의 중심을 잡기 힘들고 어지럽다
  data_set$il_d0906 <- ifelse(data_set$cva_16z1 == 1, 1,
                       ifelse(data_set$cva_16z1 == 2, 0, NA))
  
  #### 뇌졸증 조기 증상 인지율_심한 두통이 생긴다
  data_set$il_d0907 <- ifelse(data_set$cva_17z1 == 1, 1,
                       ifelse(data_set$cva_17z1 == 2, 0, NA))
  
  #### 이환영역(심근경색)
  
  #### 심근경색증 조기 증상 인지율_턱, 목 또는 등쪽에 통증이나 불편함이 있다
  data_set$il_e0401 <- ifelse(data_set$mya_10z1 == 1, 1,
                       ifelse(data_set$mya_10z1 == 2, 0, NA))
  
  #### 심근경색증 조기 증상 인지율_힘이 없고 어지럽다
  data_set$il_e0402 <- ifelse(data_set$mya_11z1 == 1, 1,
                       ifelse(data_set$mya_11z1 == 2, 0, NA))
  
  #### 심근경색증 조기 증상 인지율_가슴에 통증이나 불편감이 있다
  data_set$il_e0403 <- ifelse(data_set$mya_12z1 == 1, 1,
                       ifelse(data_set$mya_12z1 == 2, 0, NA))
  
  #### 심근경색증 조기 증상 인지율_한쪽 또는 양쪽 눈이 흐리게 보인다
  data_set$il_e0404 <- ifelse(data_set$mya_13z1 == 1, 1,
                       ifelse(data_set$mya_13z1 == 2, 0, NA))
  
  #### 심근경색증 조기 증상 인지율_팔 또는 어깨에 통증이나 불편감이 있다
  data_set$il_e0405 <- ifelse(data_set$mya_14z1 == 1, 1,
                       ifelse(data_set$mya_14z1 == 2, 0, NA))
  
  #### 심근경색증 조기 증상 인지율_숨이 찬다
  data_set$il_e0406 <- ifelse(data_set$mya_15z1 == 1, 1,
                       ifelse(data_set$mya_15z1 == 2, 0, NA))

  #### 이환영역(대사증후군)
    
  #### 대사증후군 매체경험률_TV
  data_set$il_p0201 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02a1 == 1, 1,
                              ifelse(data_set$sya_02a1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_신문
  data_set$il_p0202 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02b1 == 1, 1,
                              ifelse(data_set$sya_02b1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_현수막
  data_set$il_p0203 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02c1 == 1, 1,
                              ifelse(data_set$sya_02c1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_소식지
  data_set$il_p0204 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02d1 == 1, 1,
                              ifelse(data_set$sya_02d1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_건강정보지
  data_set$il_p0205 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02e1 == 1, 1,
                              ifelse(data_set$sya_02e1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_인터넷
  data_set$il_p0206 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02f1 == 1, 1,
                              ifelse(data_set$sya_02f1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_전문가강의
  data_set$il_p0207 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02g1 == 1, 1,
                              ifelse(data_set$sya_02g1 == 2, 0, NA)), NA)
  
  #### 대사증후군 매체경험률_친인척
  data_set$il_p0208 <- ifelse(data_set$il_p0100 == 1,
                              ifelse(data_set$sya_02h1 == 1, 1,
                              ifelse(data_set$sya_02h1 == 2, 0, NA)), NA)
  
  #### 이환영역(치매)
  
  #### 평생 치매 예방관리프로그램 참여 경험률
  data_set$il_bf0100 <- ifelse(data_set$dta_01z1 == 1, 1,
                        ifelse(data_set$dta_01z1 == 2, 0, NA))
  
  #### 평생 치매 선별검사 수검율(60세 이상)
  #### 분모정의
  data_set$age_o60 <- ifelse(data_set$age>=60 & data_set$age<=110, 1, NA)
  #### 분자정의
  data_set$il_bf0400 <- ifelse(data_set$age_o60 == 1,
                               ifelse(data_set$dta_03z1 == 1, 1,
                               ifelse(data_set$dta_03z1 == 2, 0, NA)), NA)
  
  #### 치매 예방관리 프로그램 참여 희망률
  #### 분모변수
  data_set$il_bf0500 <- ifelse(data_set$dta_01z1 == 1, 0,
                        ifelse(data_set$dta_01z1 == 2, 1, NA))
  #### 분자변수
  data_set$il_bf0600 <- ifelse(data_set$dta_02z1 == 1, 0,
                        ifelse(data_set$dta_02z1 == 2, 1, NA))
  
  #### 의료이용영역
  
  #### 지역선택문항
  
  #### 외래이용률
  data_set$sr_b0200 <- ifelse(data_set$sra_07z1 == 1, 1,
                       ifelse(data_set$dra_07z1 == 2, 0, NA))
  
  #### 사고 및 중독(낙상) 영역
  
  #### 연간 낙상 치료 경험률
  data_set$ac_b0600 <- ifelse(data_set$ac_b0100 == 1,
                              ifelse(data_set$irb_04z1 == 1, 1,
                              ifelse(data_set$irb_04z1 == 2, 1, NA)), NA)
  
  #### 활동제한영역
  
  #### 지역선택문항
  
  #### 장애로 인한 활동제한 경험률
  data_set$ql_b0700 <- ifelse(data_set$qob_15z1 == 1, 1,
                       ifelse(data_set$qob_15z1 == 2, 0, NA))
  
  #### 손씻기 영역
  
  #### 지역선택문항
  
  #### 평균 손 씻기 횟수_3회 이하
  data_set$hw_a0702 <- ifelse(data_set$hwa_07z1 == 2, 1,
                       ifelse(data_set$hwa_07z1 %in% c(1:5), 0, NA))
  
  #### 평균 손 씻기 횟수_4-6회
  data_set$hw_a0703 <- ifelse(data_set$hwa_07z1 == 3, 1,
                       ifelse(data_set$hwa_07z1 %in% c(1:5), 0, NA))
  
  #### 평균 손 씻기 횟수_7-10회
  data_set$hw_a0704 <- ifelse(data_set$hwa_07z1 == 4, 1,
                       ifelse(data_set$hwa_07z1 %in% c(1:5), 0, NA))
  
  #### 평균 손 씻기 횟수_11회 이상
  data_set$hw_a0705 <- ifelse(data_set$hwa_07z1 == 5, 1,
                       ifelse(data_set$hwa_07z1 %in% c(1:5), 0, NA))
  
  #### 여성건강
  
  #### 현재 가임기 여성비율
  #### 분모정의
  data_set$sex_f55 <- ifelse(data_set$sex == 2 & data_set$age>=19 & data_set$age<=55, 1, NA)
  #### 분자정의
  data_set$fe_a0800 <- ifelse(data_set$fea_01z2 == 2, 1,
                       ifelse(data_set$fea_01z2 %in% c(1,3,4,5), 0, NA))
  
  #### 지역선택문항
  
  #### 출산 경험률
  data_set$fe_a0300 <- ifelse(data_set$fea_03z1 == 1, 1,
                      ifelse(data_set$fea_03z1 == 2, 0, NA))
  
  #### 모유수유 경험률
  data_set$fe_a0700 <- ifelse(data_set$fea_07z1 == 1, 1,
                       ifelse(data_set$fea_07z1 == 2, 0, NA))
  
  #### 첫 출산 연령
  data_set$fe_a0900 <- ifelse(data_set$fea_03z1 == 1,
                              ifelse((data_set$fea_10z1>=0 & data_set$fea_10z1<=110) &
                                     (data_set$fea_10z1>=0 & data_set$fea_10z1<=data_set$age), data_set$fea_10z1, NA))
  
  #### 가구조사
  
  #### 식품 안정성 확보율
  data_set$nu_e0200 <- ifelse(data_set$nue_01z1 %in% c(1,2), 1,
                       ifelse(data_set$nue_01z1 %in% c(3,4), 0, NA))
  
  #### 식품 안정성 확보율_충분한양, 다양한종류
  data_set$nu_e1100 <- ifelse(data_set$nue_01z1 == 1, 1,
                       ifelse(data_set$nue_01z1 %in% c(1:4), 0, NA))
  
  #### 식품 안정성 확보율_충분한양
  data_set$nu_e1200 <- ifelse(data_set$nue_01z1 == 2, 1,
                       ifelse(data_set$nue_01z1 %in% c(1:4), 0, NA))
  
  #### 식품 안정성 확보율_가끔부족
  data_set$nu_e1300 <- ifelse(data_set$nue_01z1 == 3, 1,
                       ifelse(data_set$nue_01z1 %in% c(1:4), 0, NA))
  
  #### 식품 안정성 확보율_자주부족
  data_set$nu_e1400 <- ifelse(data_set$nue_01z1 == 4, 1,
                       ifelse(data_set$nue_01z1 %in% c(1:4), 0, NA))
  
  
  
  