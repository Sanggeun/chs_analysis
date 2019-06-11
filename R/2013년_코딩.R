
  #### 흡연영역

  #### 금연클리닉·금연상담전화 인지율
  data_set$sm_d1900 <- ifelse(data_set$smd_11z1 == 1, 1,
                       ifelse(data_set$smd_11z1 == 2, 0, NA))
  
  #### 현재 흡연자의 금연클리닉·금연상담전화 이용률
  #### 분모정의
  data_set$sm_d2000 <- ifelse(data_set$sm_a0100 == 1,
                              ifelse(data_set$sm_d1900 == 1, 1,
                              ifelse(data_set$sm_a0100 == 1 | data_set$sm_d1900 == 0, 0,
                       ifelse(data_set$sm_a0100 == 0, 0, NA))))
  #### 분자정의
  data_set$sm_d2100 <- ifelse(data_set$smd_12z1 == 1, 1,
                       ifelse(data_set$smd_12z1 == 2, 0, NA))
  
  #### 음주영역
  
  #### 음주시작동기_주위사람이권해서
  data_set$dr_a0601 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 1, 1, 0))
  
  #### 음주시작동기_호기심
  data_set$dr_a0602 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 2, 1, 0))
  
  #### 음주시작동기_친목도모
  data_set$dr_a0603 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 3, 1, 0))
  
  #### 음주시작동기_축하할일이있어서
  data_set$dr_a0604 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 4, 1, 0))
  
  #### 음주시작동기_스트레스풀려고
  data_set$dr_a0605 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 5, 1, 0))
  
  #### 음주시작동기_광고등에멋있게보여서
  data_set$dr_a0606 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 6, 1, 0))
  
  #### 음주시작동기_기타
  data_set$dr_a0607 <- ifelse(data_set$dr_a0100 == 1,
                              ifelse(data_set$dra_03z1 == 7, 1, 0))
  
  #### 연간 음주자의 금주권고 경험률
  data_set$dr_d0100 <- ifelse(data_set$dr_a0300 == 1,
                              ifelse(data_set$drb_12z2 == 1, 1,
                              ifelse(data_set$drb_12z2 == 2, 0, NA)))
  
  #### 영양영역
  
  #### 1일 1회 이상 과일섭취율
  data_set$nu_g0100 <- ifelse(data_set$nug_01z2 %in% c(3:5), 1,
                       ifelse(data_set$nug_01z2 %in% c(1,2), 0, NA))
  
  #### 1일 1회 이상 생채소섭취율
  data_set$nu_g0300 <- ifelse(data_set$nug_03z2 %in% c(3:5), 1,
                       ifelse(data_set$nug_03z2 %in% c(1,2), 0, NA))
  
  #### 정신건강영역
  
  #### 자살생각률
  data_set$mt_d0100 <- ifelse(data_set$mtd_01z1 == 1, 1,
                       ifelse(data_set$mtd_01z1 == 2, 0, NA))
  
  #### 자살 생각으로 인한 정신상담률
  data_set$mt_d0200 <- ifelse(data_set$mtd_02z1 == 1, 1,
                       ifelse(data_set$mtd_02z1 == 2, 0, NA))
  
  #### 자살시도율
  data_set$mt_d0300 <- ifelse(data_set$mtd_03z1 == 1, 1,
                       ifelse(data_set$mtd_03z1 == 2, 0, NA))
  
  #### 자살 시도로 인한 정신상담률
   data_set$mt_d0400 <- ifelse(data_set$mtd_04z1 == 1, 1,
                        ifelse(data_set$mtd_04z1 == 2, 0, NA))
  
  #### 인터넷, 게임(오락), 스마트폰 일상생활 지장 경험 빈도
  data_set$mt_f0101 <- ifelse(data_set$mtg_01z1 %in% c(1:5),
                              ifelse(data_set$mtg_01z1 == 1, 1, NA))
  data_set$mt_f0102 <- ifelse(data_set$mtg_01z1 %in% c(1:5),
                              ifelse(data_set$mtg_01z1 == 2, 2, NA))
  data_set$mt_f0103 <- ifelse(data_set$mtg_01z1 %in% c(1:5),
                              ifelse(data_set$mtg_01z1 == 3, 3, NA))
  data_set$mt_f0104 <- ifelse(data_set$mtg_01z1 %in% c(1:5),
                              ifelse(data_set$mtg_01z1 == 4, 4, NA))
  data_set$mt_f0105 <- ifelse(data_set$mtg_01z1 %in% c(1:5),
                              ifelse(data_set$mtg_01z1 == 5, 5, NA))
  
  #### 인터넷, 게임(오락), 스마트폰의 지나친 사용에 대한 지적 경험률
  data_set$mt_f0200 <- ifelse(data_set$mtg_02z1 %in% c(3,4), 1,
                       ifelse(data_set$mtg_02z1 %in% c(1,2), 0, NA))
  
  #### 심뇌혈관질환
  
  #### 심뇌혈관질환 인지율
  data_set$il_bd0100 <- ifelse(data_set$cvd_01z1 == 1, 1,
                        ifelse(data_set$cvs_01z1 == 2, 0, NA))
  
  #### 심뇌혈관질환 매체경험률_인터넷
  data_set$il_bd0201 <- ifelse(data_set$cvd_02a1 == 1, 1,
                        ifelse(data_set$cvd_02a1 == 2, 0, NA))
  
  #### 심뇌혈관질환 매체경험률_TV,라디오,옥외광고,지하철 등
  data_set$il_bd0202 <- ifelse(data_set$cvd_02b1 == 1, 1,
                        ifelse(data_set$cvd_02b1 == 2, 0, NA))
  
  #### 심뇌혈관질환 매체경험률_병의원
  data_set$il_bd0203 <- ifelse(data_set$cvd_02c1 == 1, 1,
                        ifelse(data_set$cvd_02c1 == 2, 0, NA))
  
  #### 심뇌혈관질환 매체경험률_병의원
  data_set$il_bd0204 <- ifelse(data_set$cvd_02d1 == 1, 1,
                        ifelse(data_set$cvd_02d1 == 2, 0, NA))
  
  #### 심뇌혈관질환 예방수칙 인지율
  data_set$il_bd0300 <- ifelse(data_set$cvd_03z1 == 1, 1,
                        ifelse(data_set$cvd_03z1 == 2, 0, NA))
  
  #### 심뇌혈관질환 예방수칙 매체 경험률_인터넷
  data_set$il_bd0401 <- ifelse(data_set$cvd_04a1 == 1, 1,
                        ifelse(data_set$cvd_04a1 == 2, 0, NA))
  
  #### 심뇌혈관질환 예방수칙 매체 경험률_TV,라디오,옥외광고,지하철 등
  data_set$il_bd0402 <- ifelse(data_set$cvd_04b1 == 1, 1,
                        ifelse(data_set$cvd_04b1 == 2, 0, NA))
  
  #### 심뇌혈관질환 예방수칙 매체 경험률_병의원
  data_set$il_bd0403 <- ifelse(data_set$cvd_04c1 == 1, 1,
                        ifelse(data_set$cvd_04c1 == 2, 0, NA))
  
  #### 심뇌혈관질환 예방수칙 매체 경험률_보건기관
  data_set$il_bd0404 <- ifelse(data_set$cvd_04d1 == 1, 1,
                        ifelse(data_set$cvd_04d1 == 2, 0, NA))
  
  #### 손씻기 영역
  
  #### 손 씻기 감염병 예방 인지율
  data_set$hw_a0100 <- ifelse(data_set$hwa_01z1 %in% c(1,2), 1,
                       ifelse(data_set$hwa_01z1 %in% c(3,4), 0, NA))
  
  #### 손 씻기 실천율(식사 전)
  data_set$hw_a0200 <- ifelse(data_set$hwa_02z1 %in% c(1,2), 1,
                       ifelse(data_set$hwa_02z1 %in% c(3,4), 0, NA))
  
  #### 손 씻기 실천율(화장실 다녀온 후)
  data_set$hw_a0300 <- ifelse(data_set$hwa_03z1 %in% c(1,2), 1,
                       ifelse(data_set$hwa_03z1 %in% c(3,4), 0, NA))
  
  #### 손 씻기 실천율(외출 후)
  data_set$hw_a0400 <- ifelse(data_set$hwa_04z1 %in% c(1,2), 1,
                       ifelse(data_set$hwa_04z1 %in% c(3,4), 0, NA))
  
  #### 비누, 손 세정제 사용률
  data_set$hw_a0500 <- ifelse(data_set$hwa_05z1 %in% c(1,2), 1,
                       ifelse(data_set$hwa_05z1 %in% c(3,4), 0, NA))
  
  #### 손 씻기 교육 및 홍보 경험률
  data_set$hw_a0600 <- ifelse(data_set$hwa_06z1 == 1, 1,
                       ifelse(data_set$hwa_06z1 == 2, 0, NA))
