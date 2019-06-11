
  #### 흡연영역

  #### 흡연자의 1개월 내 금연계획률
  data_set$sm_d0500 <- ifelse(data_set$smd_01z2 == 1, 1,
                       ifelse(data_set$smd_01z2 %in% c(2:4), 0, NA))
  
  #### 흡연자의 금연시도율
  data_set$sm_d0600 <- ifelse(data_set$smd_02z2 == 1, 1,
                       ifelse(data_set$smd_02z2 %in% c(2,3), 0, NA))
  
  #### 흡연자의 금연시도율(최근1년 + 과거)
  data_set$sm_d0601 <- ifelse(data_set$smd_02z2 %in% c(1,2), 1,
                       ifelse(data_set$smd_02z2 %in% c(3), 0, NA))
  
  #### 금연시도 또는 금연계획자(분모변수)
  data_set$sm_d0602 <- ifelse(data_set$smd_01z2 %in% c(1:3) | data_set$smd_02z2 %in% c(1,2), 1,
                       ifelse(data_set$smd_01z2 == 4 & data_set$smd_02z2 == 3, 0, NA))
  
  #### 흡연자의 금연방법_자신의 의지
  data_set$sm_d0801 <- ifelse(data_set$smd_05a2 == 1, 1,
                       ifelse(data_set$smd_05a2 == 2, 0, NA))
  
  #### 흡연자의 금연방법_금연보조제
  data_set$sm_d0802 <- ifelse(data_set$smd_05b2 == 1, 1,
                       ifelse(data_set$smd_05b2 == 2, 0, NA))
  
  #### 흡연자의 금연방법_의사처방
  data_set$sm_d0803 <- ifelse(data_set$smd_05c2 == 1, 1,
                       ifelse(data_set$smd_05c2 == 2, 0, NA))
  
  #### 흡연자의 금연방법_금연클리닉
  data_set$sm_d0804 <- ifelse(data_set$smd_05d2 == 1, 1,
                       ifelse(data_set$smd_05d2 == 2, 0, NA))
  
  #### 흡연자의 금연이유_건강이 나빠져서
  data_set$sm_d0701 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 1, 1, NA))
  
  #### 흡연자의 금연이유_향후건강
  data_set$sm_d0702 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 2, 1, NA))
  
  #### 흡연자의 금연이유_가족건강
  data_set@sm_d0703 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 3, 1, NA))
  
  #### 흡연자의 금연이유_주위피해
  data_set$sm_d0704 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 4, 1, NA))
  
  #### 흡연자의 금연이유_주위권유
  data_set$sm_d0705 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 5, 1, NA))
  
  #### 흡연자의 금연이유_담뱃값부담
  data_set$sm_d0706 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 6, 1, NA))
  
  #### 흡연자의 금연이유_공익광고, 문구보고
  data_set$sm_d0707 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 7, 1, NA))
  
  #### 흡연자의 금연이유_흡연장소가없어
  data_set$sm_d0708 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 8, 1, NA))
  
  #### 흡연자의 금연이유_기타
  data_set$sm_d0709 <- ifelse(data_set$smd_03z2 %in% c(1:9), 0,
                       ifelse(data_set$smd_03z2 == 9, 1, NA))
  
  #### 음주영역
  
  #### Audit
  data_set$dr_b0100 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_01z2 %in% c(1,2), 1,
                              ifelse(data_set$drb_01z2 %in% c(3:5), dats_set$drb_01z2-1, NA)))
  data_set$dr_b0100 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0200 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_03z1 %in% c(1:5), drb_03z1-1, NA))
  data_set$dr_b0200 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0300 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_04z1 %in% c(1:5), drb_04z1-1, NA))
  data_set$dr_b0400 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_05z1 %in% c(1:5), drb_05z1-1, NA))
  data_set$tmp_drk <- ifelse(data_set$gender == "남", data_set$dr_b0300,
                      ifelse(data_set$gender == "여", data_set$dr_b0400, NA))
  data_set$tmp_drk <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0500 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_06z1 %in% c(1:5), drb_06z1-1, NA))
  data_set$dr_b0500 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0500 <- ifelse(data_set$drb_03z1 == 1 & (data_set$drb_04z1 == 1 | data_set$drb_05z1 == 1) &
                              data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0600 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_07z1 %in% c(1:5), drb_07z1-1, NA))
  data_set$dr_b0600 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0600 <- ifelse(data_set$drb_03z1 == 1 & (data_set$drb_04z1 == 1 | data_set$drb_05z1 == 1) &
                              data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0700 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_08z1 %in% c(1:5), drb_08z1-1, NA))
  data_set$dr_b0700 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0700 <- ifelse(data_set$drb_03z1 == 1 & (data_set$drb_04z1 == 1 | data_set$drb_05z1 == 1) &
                              data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0800 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_09z1 %in% c(1:5), drb_09z1-1, NA))
  data_set$dr_b0800 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0800 <- ifelse(data_set$drb_03z1 == 1 & (data_set$drb_04z1 == 1 | data_set$drb_05z1 == 1) &
                              data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0900 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_10z1 %in% c(1:5), drb_10z1-1, NA))
  data_set$dr_b0900 <- ifelse(data_set$drb_02z1 == 2 & data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b0900 <- ifelse(data_set$drb_03z1 == 1 & (data_set$drb_04z1 == 1 | data_set$drb_05z1 == 1) &
                              data_set$drb_11z1 %in% c(1:3) & data_set$drb_12z1 %in% c(1:3), 0, NA)
  data_set$dr_b1000 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_11z1 == 1, 0,
                              ifelse(data_set$drb_11z1 == 2, 2,
                              ifelse(data_set$drb_11z1 == 3, 4, NA))))
  data_set$dr_b1100 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$drb_12z1 == 1, 0,
                              ifelse(data_set$drb_12z1 == 2, 2,
                              ifelse(data_set$drb_12z1 == 3, 4, NA))))
  
  data_set$dr_b1200 <- data_set$dr_b0100 + data_set$dr_b0200 + data_set$tmp_drk + data_set$dr_b0500 + data_set$dr_b0600 + data_set$dr_b0700 + data_set$dr_b0800 + data_set$dr_b0900 + data_set$dr_b1000 + data_set$dr_b1100
  
  #### 정상음주율
  data_set$dr_b1300 <- ifelse(data_set$dr_b1200 == ., .,
                       ifelse(data_set$dr_b1200 %in% c(0:7), 1,
                       ifelse(data_set$dr_b1200>7, 0, NA)))
  
  #### 문제음주율
  data_set$dr_b1400 <- ifelse(data_set$dr_b1200 == ., .,
                       ifelse(data_set$dr_b1200 %in% c(0:7), 0,
                       ifelse(data_set$dr_b1200 %in% c(8:15), 1,
                       ifelse(data_set$dr_b1200>=16, 0, NA))))
  
  #### 알코올의존율I
  data_set$dr_b1500 <- ifelse(data_set$dr_b1200 == ., .,
                       ifelse(data_set$dr_b1200 %in% c(0:15), 0,
                       ifelse(data_set$dr_b1200 %in% c(16:19), 1,
                       ifelse(data_set$dr_b1200>=20, 0, NA))))
  
  #### 알코올의존율II
  data_set$dr_b1600 <- ifelse(data_set$dr_b1200 == ., .,
                       ifelse(data_set$dr_b1200 %in% c(0:19), 0,
                       ifelse(data_set$dr_b1200>=20, 1, NA)))
  
  #### 신체활동영역
  
  #### 지역사회 내 운동시설 접근성
  data_set$ph_c0100 <- ifelse(data_set$phc_01z1 %in% c(1,2), 1,
                       ifelse(data_set$phc_01z1 %in% c(3,4), 0, NA))
  
  #### 지역사회 내 운동프로그램 참여율
  data_set$ph_c0200 <- ifelse(data_set$phc_02z1 == 1, 1,
                       ifelse(data_set$phc_02z1 == 2, 0, NA))
  
  #### 영양영역
  
  #### 식생활지침 인지율
  data_set$nu_f0100 <- ifelse(data_set$nuf_01z1 == 1, 1,
                       ifelse(data_set$muf_01z1 == 2, 0, NA))
  
  #### 비만영역
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_운동
  data_set$ob_b0201 <- ifelse(data_set$obb_02a1 == 1, 1,
                       ifelse(data_set$obb_02a1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_단식
  data_set$ob_b0202 <- ifelse(data_set$obb_02b1 == 1, 1,
                       ifelse(data_set$obb_02b1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_식단조절
  data_set$ob_b0203 <- ifelse(data_set$obb_02c1 == 1, 1,
                       ifelse(data_set$obb_02c1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_임의약복용
  data_set$ob_b0204 <- ifelse(data_set$obb_02d1 == 1, 1,
                       ifelse(data_set$obb_02d1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_의사처방복용
  data_set$ob_b0205 <- ifelse(data_set$obb_02e1 == 1, 1,
                       ifelse(data_set$obb_02e1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_한약복용
  data_set$ob_b0206 <- ifelse(data_set$obb_02f1 == 1, 1,
                       ifelse(data_set$obb_02f1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_건강식품
  data_set$ob_b0207 <- ifelse(data_set$obb_02g1 == 1, 1,
                       ifelse(data_set$obb_02g1 == 2, 0, NA))
  
  #### 최근 1년(365일) 동안 체중조절 시도방법_원푸드
  data_set$ob_b0208 <- ifelse(data_set$obb_02h1 == 1, 1,
                       ifelse(data_set$obb_02h1 == 2, 0, NA))
  
  #### 자가보고 과체중(OECD요구)
  data_set$ob_a0204 <- ifelse(data_set$ob_a0101>=0 & data_set$ob_a0101<10, .,
                       ifelse(data_set$ob_a0101>=10 & data_set$ob_a0101<25, 0,
                       ifelse(data_set$ob_a0101>=25 & data_set$ob_a0101<30, 1,
                       ifelse(data_set$ob_a0101>=30 & data_set$ob_a0101<50, 0, .))))
  
  #### 자가보고 비만(OECD요구)
  data_set$ob_a0205 <- ifelse(data_set$ob_a0101>=0 & data_set$ob_a0101<10, .,
                       ifelse(data_set$ob_a0101>=10 & data_set$ob_a0101<30, 0,
                       ifelse(data_set$ob_a0101>=30 & data_set$ob_a0101<50, 1, .)))
  
  #### 건강검진영역
  
  #### 건강검진 수진율
  data_set$sc_b0100 <- ifelse(data_set$scb_01z1 == 1, 1,
                       ifelse(data_set$scb_01z1 == 2, 0, NA))
  
  #### 건강검진 사후 관리 상담률
  data_set$sc_b0200 <- ifelse(data_set$scb_04z1 == 1, 1,
                       ifelse(data_set$scb_04z1 == 2, 0, NA))
  
  #### 건강검진 미수진 이유_분모정의
  data_set$sc_b0300 <- ifelse(data_set$scb_01z1 == 1, 0,
                       ifelse(data_set$scb_01z1 == 2, 1, NA))
  
  #### 건강검진 미수진 이유_비싸서
  data_set$sc_b0401 <- ifelse(data_set$scb_05z1 %in% c(1:5), 0,
                       ifelse(data_set$scb_05z1 == 1, 1, NA))
  
  #### 건강검진 미수진 이유_시간이없어서
  data_set$sc_b0402 <- ifelse(data_set$scb_05z1 %in% c(1:5), 0,
                       ifelse(data_set$scb_05z1 == 2, 1, NA))
  
  #### 건강검진 미수진 이유_위치를 몰라서
  data_set$sc_b0403 <- ifelse(data_set$scb_05z1 %in% c(1:5), 0,
                       ifelse(data_set$scb_05z1 == 3, 1, NA))
  
  #### 건강검진 미수진 이유_필요성을 느끼지 못해서
  data_set$sc_b0404 <- ifelse(data_set$scb_05z1 %in% c(1:5), 0,
                       ifelse(data_set$scb_05z1 == 4, 1, NA))
  
  #### 건강검진 미수진 이유_기타
  data_set$sc_b0405 <- ifelse(data_set$scb_05z1 %in% c(1:5), 0,
                       ifelse(data_set$scb_05z1 == 5, 1, NA))
  
  #### 암 검진율
  data_set$sc_c0100 <- ifelse(data_set$scc_01z1 == 1, 1,
                       ifelse(data_set$scc_01z1 == 2, 0, NA))
  
  #### 암 검진율_위함(남여)
  data_set$sc_c0301 <- ifelse(data_set$gender == "남",
                              ifelse(data_set$scc_15a1 == 1, 1,
                              ifelse(data_set$scc_15a1 == 2, 0,
                       ifelse(data_set$gender == "여",
                              ifelse(data_set$scc_16a1 == 1, 1,
                              ifelse(data_set$scc_16a1 == 2, 0, NA))))))
  
  #### 암 검진율_간암(남여)
  data_set$sc_c0302 <- ifelse(data_set$gender == "남",
                              ifelse(data_set$scc_15b1 == 1, 1,
                              ifelse(data_set$scc_15b1 == 2, 0,
                       ifelse(data_set$gender == "여",
                              ifelse(data_set$scc_16b1 == 1, 1,
                              ifelse(data_set$scc_16b1 == 2, 0, NA))))))
  
  #### 암 검진율_대장암(남여)
  data_set$sc_c0303 <- ifelse(data_set$gender == "남",
                              ifelse(data_set$scc_15c1 == 1, 1,
                              ifelse(data_set$scc_15c1 == 2, 0,
                       ifelse(data_set$gender == "여",
                              ifelse(data_set$scc_16c1 == 1, 1,
                              ifelse(data_set$scc_16c1 == 2, 0, NA))))))
  
  #### 암 검진율_갑상선암(남여)
  data_set$sc_c0304 <- ifelse(data_set$gender == "남",
                              ifelse(data_set$scc_15d1 == 1, 1,
                              ifelse(data_set$scc_15d1 == 2, 0,
                       ifelse(data_set$gender == "여",
                              ifelse(data_set$scc_16d1 == 1, 1,
                              ifelse(data_set$scc_16d1 == 2, 0, NA))))))
  
  #### 암 검진율_그외암(남여)
  data_set$sc_c0305 <- ifelse(data_set$gender == "남",
                              ifelse(data_set$scc_15f1 == 1, 1,
                              ifelse(data_set$scc_15f1 == 2, 0,
                       ifelse(data_set$gender == "여",
                              ifelse(data_set$scc_16f1 == 1, 1,
                              ifelse(data_set$scc_16f1 == 2, 0, NA))))))
  
  #### 암 검진율_전립선암(남)
  #### 분모정의
  data_set$sc_c0306 <- ifelse(data_set$sc_c0100 == 1 & data_set$gender == "남", 1, NA)
  #### 분자정의
  data_set$sc_c0307 <- ifelse(data_set$scc_15e1 == 1, 1,
                       ifelse(data_set$scc_15e1 == 2, 0, NA))
  
  #### 암 검진율_유방암(여)
  #### 분모정의
  data_set$sc_c0308 <- ifelse(data_set$sc_c0100 == 1 & data_set$gender == "여", 1, NA)
  #### 분자정의
  data_set$sc_c0309 <- ifelse(data_set$scc_16e1 == 1, 1,
                       ifelse(data_set$scc_16e1 == 2, 0, NA))
  
  #### 암 검진율_자궁경부암(여)
  data_set$sc_c0310 <- ifelse(data_set$scc_16f1 == 1, 1,
                       ifelse(data_set$scc_16f1 == 2, 0, NA))
  
  #### 암검진 사후 관리 상담률
  data_set$sc_c0400 <- ifelse(data_set$scc_12z1 == 1, 1,
                       ifelse(data_set$scc_12z1 == 2, 0, NA))
  
  #### 암검진 미수진이유_분모정의
  data_set$sc_c0200 <- ifelse(data_set$scc_01z1 == 1, 0,
                       ifelse(data_set$scC_01z1 == 2, 1, NA))
  
  #### 암검진 미수진이유_비싸서
  data_set$sc_c0501 <- ifelse(data_set$scc_13z1 == 1, 1,
                       ifelse(data_set$scc_13z1 %in% c(2:5), 0, NA))
  
  #### 암검진 미수진이유_시간이없어
  data_set$sc_c0502 <- ifelse(data_set$scc_13z1 == 2, 1,
                       ifelse(data_set$scc_13z1 %in% c(1,3,4,5), 0, NA))
  
  #### 암검진 미수진이유_위치를몰라
  data_set$sc_c0503 <- ifelse(data_set$scc_13z1 == 3, 1,
                       ifelse(data_set$scc_13z1 %in% c(1,2,4,5), 0, NA))
  
  #### 암검진 미수진이유_필요성이없어
  data_set$sc_c0504 <- ifelse(data_set$scc_13z1 == 4, 1,
                       ifelse(data_set$scc_13z1 %in% c(1,2,3,5), 0, NA))
  
  #### 암검진 미수진이유_기타
  data_set$sc_c0505 <- ifelse(data_set$scc_13z1 == 5, 1,
                       ifelse(data_set$scc_13z1 %in% c(1:4), 0, NA))
  
  #### 이환영역(고혈압)

  #### 연간 평균 혈압 측정획수
  data_set$il_a0100 <- ifelse(data_set$hya_12z1 %in% c(1:5),
                              ifelse(data_set$hya_12z1 == 1 & data_set$hya_13a1>=1 & data_set$hya_13a1<=776, data_set$hya_13a1*365,
                              ifelse(data_set$hya_12z1 == 2 & data_set$hya_13b1>=1 & data_set$hya_13b1<=776, data_set$hya_13b1*52,
                              ifelse(data_set$hya_12z1 == 3 & data_set$hya_13c1>=1 & data_set$hya_13c1<=776, data_set$hya_13c1*12,
                              ifelse(data_set$hya_12z1 == 4 & data_set$hya_13d1>=1 & data_set$hya_13d1<=776, data_set$hya_13d1,
                              ifelse(data_set$hya_12z1 == 5, 0, NA))))))
  #### 이상치 제거
  data_set$il_a0100 <- ifelse(data_set$il_a0200 == 0,
                              ifelse(data_set$il_a0100>365, .,
                       ifelse(data_set$il_a0200 == 1,
                              ifelse(data_set$il_a0100>730, ., NA))))
  
  #### 고혈압 의료기관 방문 횟수
  data_set$il_a2000 <- ifelse(data_set$hya_20z1 %in% c(0:776), data_set$hya_20z1, NA)
  
  #### 이환영역(당뇨병)
  
  #### 당뇨병 의료기관 방문 횟수
  data_set$il_b2000 <- ifelse(data_set$dia_20z1 %in% c(0:776), data_set$dia_20z1, NA)
  
  #### 당화혈색소 인지율
  data_set$il_b2100 <- ifelse(data_set$dia_21z1 == 1, 1,
                       ifelse(data_set$dia_21z1 == 2, 0, NA))
  
  #### 당화혈색소 검사 횟수
  #### 분모정의
  data_set$il_b2200 <- ifelse(data_set$il_b0300 == 1,
                              ifelse(data_set$il_b2100 == 1, 1,
                              ifelse(data_set$il_b2100 == 0, 0, NA)))
  #### 분자정의
  data_set$il_b2300 <- ifelse(data_set$dia_22z1 %in% c(1:5),
                              ifelse(data_set$dia_22z1 == 1, 1, 0))
  data_set$il_b2400 <- ifelse(data_set$dia_22z1 %in% c(1:5),
                              ifelse(data_set$dia_22z1 == 2, 1, 0))
  data_set$il_b2500 <- ifelse(data_set$dia_22z1 %in% c(1:5),
                              ifelse(data_set$dia_22z1 == 3, 1, 0))
  data_set$il_b2600 <- ifelse(data_set$dia_22z1 %in% c(1:5),
                              ifelse(data_set$dia_22z1 == 4, 1, 0))
  data_set$il_b2700 <- ifelse(data_set$dia_22z1 %in% c(1:5),
                              ifelse(data_set$dia_22z1 == 5, 1, 0))
  
  #### 이환영역(이상지질혈증(고지혈증 포함))
  
  #### 이상지질혈증 현재치료율
  data_set$il_r0500 <- ifelse(data_set$il_r0100 == 1,
                              ifelse(data_set$dla_02z1 == 1, 1,
                              ifelse(data_set$dla_02z1 == 2, 0, NA)))
  
  #### 이상지질혈증의 치료방법_약복용_30세이상
  #### 분모정의
  data_set$il_r0600 <- ifelse(data_set$age_o30 == 1,
                              ifelse(data_set$il_r0500 == 1, 1,
                              ifelse(data_set$il_r0500 == 0, 0, NA)))
  #### 분자정의
  data_set$il_r0701 <- ifelse(data_set$il_r0200 == 1,
                              ifelse(data_set$dla_03a1 == 1, 1,
                              ifelse(data_set$dla_03a1 == 2, 0, NA)))
  
  #### 이상지질혈증의 치료방법_비약물요법_30세이상
  data_set$il_r0702 <- ifelse(data_set$il_r0200 == 1,
                              ifelse(data_set$dla_03c1 == 1, 1,
                              ifelse(data_set$dla_03c1 == 2, 0, NA)))
  
  #### 이상지질혈증의 약물치료율_30세이상
  data_set$il_r0800 <- ifelse(data_set$il_r0200 == 1,
                              ifelse(data_set$dla_02z1 == 1,
                                     ifelse(data_set$dla_03a1 == 1 & (data_set$dla_03b1>=0 & data_set$dla_03b1<=19), 0,
                                     ifelse(data_set$dla_03a1 == 1 & data_set$dla_03b1>=20 & data_set$dla_03b1<=31, 1,
                                     ifelse(data_set$dla_03a1 == 2, 0,
                              ifelse(data_set$dla_02z1 == 2, 0, NA))))))
                                     
  #### 이환영역(뇌졸중)
  
  #### 뇌졸중 현재치료율
  data_set$il_d0500 <- ifelse(data_set$il_d0100 == 1,
                              ifelse(data_set$cva_06z1 == 1, 1,
                              ifelse(data_set$cva_06z1 == 2, 0, NA)))
  
  #### 뇌졸중 후유증 이환율
  data_set$il_d0400 <- ifelse(data_set$il_d0100 == 1,
                              ifelse(data_set$cva_09z1 == 1, 1,
                              ifelse(data_set$cva_09z1 %in% c(2,3), 0, NA)))
  
  #### 뇌졸중 후유증_50세 이상
  data_set$il_d0600 <- ifelse(data_set$age_o50 == 1,
                              ifelse(data_set$il_d0400 == 1, 1,
                              ifelse(data_set$il_d0400 == 0, 0, NA)))
  
  #### 뇌종줄 후유증 증상별 분포_팔다리마비
  data_set$il_d0701 <- ifelse(data_set$il_d0400 == 1,
                              ifelse(data_set$cva_10a1 == 1, 1,
                              ifelse(data_set$cva_10a1 == 2, 0, NA)))
  
  #### 뇌종줄 후유증 증상별 분포_안면마비
  data_set$il_d0702 <- ifelse(data_set$il_d0400 == 1,
                              ifelse(data_set$cva_10b1 == 1, 1,
                              ifelse(data_set$cva_10b1 == 2, 0, NA)))
  
  #### 뇌종줄 후유증 증상별 분포_발음장애
  data_set$il_d0703 <- ifelse(data_set$il_d0400 == 1,
                              ifelse(data_set$cva_10c1 == 1, 1,
                              ifelse(data_set$cva_10c1 == 2, 0, NA)))
  
  #### 뇌종줄 후유증 증상별 분포_삼킴장애
  data_set$il_d0704 <- ifelse(data_set$il_d0400 == 1,
                              ifelse(data_set$cva_10d1 == 1, 1,
                              ifelse(data_set$cva_10d1 == 2, 0, NA)))
  
  #### 뇌종줄 후유증 증상별 분포_안보임
  data_set$il_d0705 <- ifelse(data_set$il_d0400 == 1,
                              ifelse(data_set$cva_10e1 == 1, 1,
                              ifelse(data_set$cva_10e1 == 2, 0, NA)))
  
  #### 이환영역(심근경색)
  
  #### 심근경색 현재치료율
  data_set$il_e0300 <- ifelse(data_set$il_e0100 == 1,
                              ifelse(data_set$mya_06z1 == 1, 1,
                              ifelse(data_set$mya_06z1 == 2, 0, NA)))
  
  #### 이환영역(협심증)
  
  #### 협심증 현재치료율
  data_set$il_f0300 <- ifelse(data_set$il_f0100 == 1,
                              ifelse(data_set$ana_06z1 == 1, 1,
                              ifelse(data_set$ana_06z1 == 2, 0, NA)))
  
  #### 이환영역(골다공증)
  
  #### 골다공증의 현재치료율
  data_set$il_h0300 <- ifelse(data_set$il_h0100 == 1,
                              ifelse(data_set$osa_06z1 == 1, 1,
                              ifelse(data_set$oas_06z1 == 2, 0, NA)))
  
  #### 골다공증 부위별 골절진단율_엉덩이
  data_set$il_h0401 <- ifelse(data_set$il_h0100 == 1,
                              ifelse(data_set$osa_09a1 == 1, 1,
                              ifelse(data_set$osa_09a1 == 2, 0, NA)))
  
  #### 골다공증 부위별 골절진단율_척추
  data_set$il_h0402 <- ifelse(data_set$il_h0100 == 1,
                              ifelse(data_set$osa_09b1 == 1, 1,
                              ifelse(data_set$osa_09b1 == 2, 0, NA)))
  
  #### 골다공증 부위별 골절진단율_손목뼈
  data_set$il_h0403 <- ifelse(data_set$il_h0100 == 1,
                              ifelse(data_set$osa_09c1 == 1, 1,
                              ifelse(data_set$osa_09c1 == 2, 0, NA)))
  
  #### 이환영역(폐결핵)
  
  #### 폐결핵의 현재치료율
  data_set$il_i0200 <- ifelse(data_set$il_i0100 == 1,
                              ifelse(data_set$tua_04z1 == 1, 1,
                              ifelse(data_Set$tua_04z1 == 2, 0, NA)))
  
  #### 이환영역(천식)
  
  #### 천식의 현재치료율
  data_set$il_j0400 <- ifelse(data_set$il_j0300 == 1,
                              ifelse(data_set$asa_10z1 == 1, 1,
                              ifelse(data_set$asa_10z1 == 2, 0, NA)))
  
  #### 천식 악화율
  data_set$il_j0500 <- ifelse(data_set$il_j0300 == 1,
                              ifelse(data_set$asa_14z1 == 1, 1,
                              ifelse(data_set$asa_14z1 == 2, 0, NA)))
  
  #### 천식 악화로 인한 의료기관 이용률
  data_set$il_j0600 <- ifelse(data_set$il_j0300 == 1,
                              ifelse(data_set$asa_15z1 %in% c(1,2), 1,
                              ifelse(data_set$asa_15z1 == 3, 0, NA)))
  
  #### 이환영역(알레르기비염)
  
  #### 알레르기비염의 현재치료율
  data_set$il_k0200 <- ifelse(data_set$il_k0100 == 1,
                              ifelse(data_set$rha_06z1 == 1, 1,
                              ifelse(data_set$rha_06z1 == 2, 0, NA)))
  
  #### 이환영역(아토피피부염)
  
  #### 아토피피부염의 현재치료율
  data_set$il_l0200 <- ifelse(data_set$il_l0100 == 1,
                              ifelse(data_set$dea_06z1 == 1, 1,
                              ifelse(data_set$dea_06z1 == 2, 0, NA)))
  
  #### 이환영역(백내장)
  
  #### 백내장의 현재치료율
  data_set$il_m0300 <- ifelse(data_set$il_m0100 == 1,
                              ifelse(data_set$caa_06z1 == 1, 1,
                              ifelse(data_set$caa_06z1 == 2, 0, NA)))
  
  #### 이환영역(B형 간염)
  
  #### B형 간염의 현재치료율
  data_set$il_n0200 <- ifelse(data_set$il_n0100 == 1,
                              ifelse(data_set$hba_06z1 == 1, 1,
                              ifelse(data_set$hba_06z1 == 2, 0, NA)))
  
  #### 이환영역(C형 간염)
  
  #### C형 간염 평생 의사진단 경험률
  data_set$il_v0100 <- ifelse(data_set$hcv_01z1 == 1, 1,
                       ifelse(data_set$hcv_01z1 == 2, 0, NA))
  
  #### c형 간염의 현재치료율
  data_set$il_v0200 <- ifelse(data_set$il_v0100 == 1,
                              ifelse(data_set$hcv_02z1 == 1, 1,
                              ifelse(data_set$hcv_02z1 == 2, 0, NA)))
  
  #### 이환영역(빈혈)
  
  #### 빈혈 평생 의사진단 경험률
  data_set$il_w0100 <- ifelse(data_set$ama_09z1 == 1, 1,
                       ifelse(data_set$ama_09z1 == 2, 0, NA))
  
  #### 빈혈의 현재치료율
  data_set$il_w0200 <- ifelse(data_set$il_w0100 == 1,
                              ifelse(data_set$ama_10z1 == 1, 1,
                              ifelse(data_set$ama_10z1 == 2, 0, NA)))
  
  #### 이환영역(중이염)
  
  #### 중이염 평생 의사진단 경험률
  data_set$il_x0100 <- ifelse(data_set$oma_09z1 == 1, 1,
                       ifelse(data_set$oma_09z1 == 2, 0, NA))
  
  #### 중이염의 현재치료율
  data_set$il_x0200 <- ifelse(data_set$il_x0100 == 1,
                              ifelse(data_set$oma_10z1 == 1, 1,
                              ifelse(data_set$oma_10z1 == 2, 0, NA)))
  
  #### 이환영역(위십이지장궤양)
  
  #### 위십이지장궤양 평생 의사진단 경험률
  data_set$il_y0100 <- ifelse(data_set$uca_09z1 == 1, 1,
                       ifelse(data_set$uca_09z1 == 2, 0, NA))
  
  #### 위십이지장궤양의 현재치료율
  data_set$il_y0200 <- ifelse(data_set$il_y0100 == 1,
                              ifelse(data_set$uca_10z1 == 1, 1,
                              ifelse(data_set$uca_10z1 == 2, 0, NA)))
  
  #### 이환영역(치질)
  
  #### 치질 평생 의사진단 경험률
  data_set$il_z0100 <- ifelse(data_set$hda_09z1 == 1, 1,
                       ifelse(data_set$hda_09z1 == 2, 0, NA))
  
  #### 치질의 현재치료율
  data_set$il_z0200 <- ifelse(data_set$il_z0100 == 1,
                              ifelse(data_set$hda_10z1 == 1, 1,
                              ifelse(data_set$hda_10z1 == 2, 0, NA)))
  
  #### 이환영역(암)
  
  #### 암 평생 의사진단 경험률
  data_set$il_ba0100 <- ifelse(data_set$cra_10z1 == 1, 1,
                        ifelse(data_set$cra_10z1 == 2, 0, NA))
  
  #### 암의 현재치료율
  data_set$il_ba0200 <- ifelse(data_set$il_ba0100 == 1,
                               ifelse(data_set$cra_11z1 == 1, 1,
                               ifelse(data_set$cra_11z1 == 2, 0, NA)))
  
  #### 이환영역(갑상선장애)
  
  ####갑상선장애 평생 의사진단 경험률
  data_set$il_bb0100 <- ifelse(data_set$tha_09z1 == 1, 1,
                        ifelse(data_set$tha_09z1 == 2, 0, NA))
  
  #### 갑상선장애의 현재치료율
  data_set$il_bb0200 <- ifelse(data_set$il_bb0100 == 1,
                               ifelse(data_set$tha_10z1 == 1, 1,
                               ifelse(data_set$tha_10z1 == 2, 0, NA)))
  
  #### 이환영역(우울증)
  
  #### 우울증의 현재치료율
  data_set$il_o0200 <- ifelse(data_set$il_o0100 == 1,
                              ifelse(data_set$dpa_04z1 == 1, 1,
                              ifelse(data_set$dpa_04z1 == 2, 0, NA)))
  
  #### 이환영역(과민성방광)
  
  #### 과민성방광 점수(경증, 중등도, 중증)
  data_set$oabss <- ifelse(data_set$obs_01z1 %in% c(1:3) & data_set$obs_02z1 %in% c(1:4) &
                           data_set$obs_03z1 %in% c(1:4) & data_set$obs_04z1 %in% c(1:6),
                           data_set$obs_01z1 + data_set$obs_02z1 + data_set$obs_03z1 + data_set$obs_04z1 - 4, NA)
  data_set$il_bc0101 <- ifelse(data_set$oabss %in% c(0:5), 1,
                        ifelse(data_set$oabss %in% c(6:15), 0, NA))
  data_set$il_bc0102 <- ifelse(data_set$oabss %in% c(0:5), 0,
                        ifelse(data_set$oabss %in% c(6:11), 1,
                        ifelse(data_set$oabss %in% c(12:15), 0, NA)))
  data_set$il_bc0103 <- ifelse(data_set$oabss %in% c(0:11), 0,
                        ifelse(data_set$oabss %in% c(12:15), 1, NA))
  
  #### 과민성방광 평생 의사진단 경험률
  data_set$il_bc0200 <- ifelse(data_set$obs_05z1 == 1, 1,
                        ifelse(data_set$obs_05z1 == 2, 0, NA))
  
  #### 과민성방광의 현재치료율
  data_set$il_bc0300 <- ifelse(data_set$il_bc0200 == 1,
                               ifelse(data_set$obs_06z1 == 1, 1,
                               ifelse(data_set$obs_06z1 == 2, 0, NA)))
  
  #### 이환영역(요실금)
  
  #### 요실금의 현재치료율
  data_set$il_t0200 <- ifelse(data_set$il_t0100 == 1,
                              ifelse(data_set$ura_06z1 == 1, 1,
                              ifelse(data_set$ura_06z1 == 2, 0, NA)))
  
  #### 심정지 영역
  
  #### 심폐소생술 인지율
  data_set$cp_a0100 <- ifelse(data_set$cpr_01z1 == 1, 1,
                       ifelse(data_set$cpr_01z1 == 2, 0, NA))
  
  #### 심폐소생술 교육경험률
  data_set$cp_a0200 <- ifelse(data_set$cpr_02z1 == 1, 1,
                       ifelse(data_set$cpr_02z1 %in% c(2,3), 0, NA))
  
  #### 심폐소생술 마네킹 실습경험률
  data_set$cp_a0300 <- ifelse(data_set$cpr_03z1 == 1, 1,
                       ifelse(data_set$cpr_03z1 == 2, 0, NA))
  
  #### 심폐소생술 시행능력률
  data_set$cp_a0400 <- ifelse(data_set$cpr_04z1 %in% c(1,2), 1,
                       ifelse(data_set$cpr_04z1 == 3, 0, NA))
  