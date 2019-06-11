
  #### 흡연영역

  #### 지역선택문항

  #### 전자담배 인지율
  data_set$sm_a2000 <- ifelse(data_set$sma_17z1 == 1, 1,
                       ifelse(data_set$sma_17z1 == 2, 0, NA))
  
  #### 주관적 전자담배 정의_일반 담배와 달리 건강에 덜 해로울 것이다
  data_set$sm_a2101 <- ifelse(data_set$sma_18z1 == 1, 1,
                       ifelse(data_set$sma_18z1 %in% c(1:3), 0, NA))
  
  #### 주관적 전자담배 정의_일반 담배와 비슷한 정도로 건강에 해로울 것이다
  data_set$sm_a2102 <- ifelse(data_set$sma_18z1 == 2, 1,
                       ifelse(data_set$sma_18z1 %in% c(1:3), 0, NA))
  
  #### 주관적 전자담배 정의_일반 담배보다 더 심하게 건강에 해로울 것이다
  data_set$sm_a2103 <- ifelse(data_set$sma_18z1 == 3, 1,
                       ifelse(data_set$sma_18z1 %in% c(1:3), 0, NA))
  
  #### 현재흡연자의 니코틴 의존율_낮은 의존도
  data_set$tmp_sma_28z1 <- ifelse(data_set$sma_28z1 %in% c(1:4),
                                  ifelse(data_set$sma_28z1 == 1, 0,
                                  ifelse(data_set$sma_28z1 == 2, 1,
                                  ifelse(data_set$sma_28z1 == 3, 2,
                                  ifelse(data_set$sma_28z1 == 4, 3, NA)))))
  data_set$tmp_sma_29z1 <- ifelse(data_set$sma_29z1 %in% c(1:4),
                                  ifelse(data_set$sma_29z1 == 1, 0,
                                  ifelse(data_set$sma_29z1 == 2, 1,
                                  ifelse(data_set$sma_29z1 == 3, 2,
                                  ifelse(data_set$sma_29z1 == 4, 3, NA)))))
  data_set$tmp_sma_30z1 <- ifelse(data_set$sma_31z1 %in% c(1,2),
                                  ifelse(data_set$sma_30z1 == 1, 0,
                                  ifelse(data_set$sma_30z1 == 2, 1, NA)))
  data_set$tmp_sma_31z1 <- ifelse(data_set$sma_31z1 %in% c(1,2),
                                  ifelse(data_set$sma_31z1 == 1, 0,
                                  ifelse(data_set$sma_31z1 == 2, 1, NA)))
  data_set$tmp_sma_32z1 <- ifelse(data_set$sma_32z1 %in% c(1,2),
                                  ifelse(data_set$sma_32z1 == 1, 0,
                                  ifelse(data_set$sma_32z1 == 2, 1, NA)))
  data_set$tmp_sma_33z1 <- ifelse(data_set$sma_33z1 %in% c(1,2),
                                  ifelse(data_set$sma_33z1 == 1, 0,
                                  ifelse(data_set$sma_33z1 == 2, 1, NA)))
  data_set$tmp_sm_a2400 <- data_set$tmp_sma_28z1 + data_set$tmp_sma_29z1 + data_set$tmp_sma_30z1 +
                           data_set$tmp_sma_31z1 + data_set$tmp_sma_32z1 + data_set$tmp_sma_33z1 
  data_set$sm_a2401 <- ifelse(data_set$tmp_sm_a2400>=0 & data_set$tmp_sm_a2400<=2, 1,
                       ifelse(data_set$tmp_sm_a2400>=3 & data_set$tmp_sm_a2400<=10, 0, NA))
  
  #### 현재흡연자의 니코틴 의존율_중간 의존도
  data_set$sm_a2402 <- ifelse(data_set$tmp_sm_a2400>=0 & data_set$tmp_sm_a2400>=2, 0,
                       ifelse(data_set$tmp_sm_a2400>=3 & data_set$tmp_sm_a2400>=5, 1,
                       ifelse(data_set$tmp_sm_a2400>=6 & data_set$tmp_sm_a2400>=10, 0, NA)))
  
  #### 현재흡연자의 니코틴 의존율_심한 의존도
  data_set$sm_a2403 <- ifelse(data_set$tmp_sm_a2400>=0 & data_set$tmp_sm_a2400<=5, 0,
                       ifelse(data_set$tmp_sm_a2400>=6 & data_set$tmp_sm_a2400<=10, 1, NA))
  
  #### 신체활동영역
  
  #### 지역선택문항
  
  #### 체육동호회 가입률
  data_set$ph_a1900 <- ifelse(data_set$pha_20z1 == 1, 1,
                       ifelse(data_set$pha_20z1 == 2, 0, NA))
  
  #### 정신건강영역
  
  #### 우울증상유병률
  data_set$tmp_mtb_07a1 <- ifelse(data_set$mtb_07a1 %in% c(1:4),
                                  ifelse(data_set$mtb_07a1 == 1, 0,
                                  ifelse(data_set$mtb_07a1 == 2, 1,
                                  ifelse(data_set$mtb_07a1 == 3, 2,
                                  ifelse(data_set$mtb_07a1 == 4, 3, NA)))))
  data_set$tmp_mtb_07b1 <- ifelse(data_set$mtb_07b1 %in% c(1:4),
                                  ifelse(data_set$mtb_07b1 == 1, 0,
                                  ifelse(data_set$mtb_07b1 == 2, 1,
                                  ifelse(data_set$mtb_07b1 == 3, 2,
                                  ifelse(data_set$mtb_07b1 == 4, 3, NA)))))
  data_set$tmp_mtb_07c1 <- ifelse(data_set$mtb_07c1 %in% c(1:4),
                                  ifelse(data_set$mtb_07c1 == 1, 0,
                                  ifelse(data_set$mtb_07c1 == 2, 1,
                                  ifelse(data_set$mtb_07c1 == 3, 2,
                                  ifelse(data_set$mtb_07c1 == 4, 3, NA)))))
  data_set$tmp_mtb_07d1 <- ifelse(data_set$mtb_07d1 %in% c(1:4),
                                  ifelse(data_set$mtb_07d1 == 1, 0,
                                  ifelse(data_set$mtb_07d1 == 2, 1,
                                  ifelse(data_set$mtb_07d1 == 3, 2,
                                  ifelse(data_set$mtb_07d1 == 4, 3, NA)))))
  data_set$tmp_mtb_07e1 <- ifelse(data_set$mtb_07e1 %in% c(1:4),
                                  ifelse(data_set$mtb_07e1 == 1, 0,
                                  ifelse(data_set$mtb_07e1 == 2, 1,
                                  ifelse(data_set$mtb_07e1 == 3, 2,
                                  ifelse(data_set$mtb_07e1 == 4, 3, NA)))))
  data_set$tmp_mtb_07f1 <- ifelse(data_set$mtb_07f1 %in% c(1:4),
                                  ifelse(data_set$mtb_07f1 == 1, 0,
                                  ifelse(data_set$mtb_07f1 == 2, 1,
                                  ifelse(data_set$mtb_07f1 == 3, 2,
                                  ifelse(data_set$mtb_07f1 == 4, 3, NA)))))
  data_set$tmp_mtb_07g1 <- ifelse(data_set$mtb_07g1 %in% c(1:4),
                                  ifelse(data_set$mtb_07g1 == 1, 0,
                                  ifelse(data_set$mtb_07g1 == 2, 1,
                                  ifelse(data_set$mtb_07g1 == 3, 2,
                                  ifelse(data_set$mtb_07g1 == 4, 3, NA)))))
  data_set$tmp_mtb_07h1 <- ifelse(data_set$mtb_07h1 %in% c(1:4),
                                  ifelse(data_set$mtb_07h1 == 1, 0,
                                  ifelse(data_set$mtb_07h1 == 2, 1,
                                  ifelse(data_set$mtb_07h1 == 3, 2,
                                  ifelse(data_set$mtb_07h1 == 4, 3, NA)))))
  data_set$tmp_mtb_07i1 <- ifelse(data_set$mtb_07i1 %in% c(1:4),
                                  ifelse(data_set$mtb_07i1 == 1, 0,
                                  ifelse(data_set$mtb_07i1 == 2, 1,
                                  ifelse(data_set$mtb_07i1 == 3, 2,
                                  ifelse(data_set$mtb_07i1 == 4, 3, NA)))))
  data_set$tmp_mt_b0500 <- data_set$tmp_mtb_07a1 + data_set$tmp_mtb_07b1 + data_set$tmp_mtb_07c1 + data_set$tmp_mtb_07d1 + 
                           data_set$tmp_mtb_07e1 + data_set$tmp_mtb_07f1 + data_set$tmp_mtb_07g1 + data_set$tmp_mtb_07h1 + data_set$tmp_mtb_07i1
  data_set$mt_b0500 <- ifelse(data_set$tmp_mt_b0500>=0 & data_set$tmp_mt_b0500<=9, 0,
                       ifelse(data_set$tmp_mt_b0500>=10 & data_set$tmp_mt_b0500<=27, 1, NA))
  
  #### 우울증상 경험자의 정신문제 상담경험률
  data_set$mt_b0600 <- ifelse(data_set$mte_01z1 == 1, 1,
                       ifelse(data_set$mte_01z1 == 2, 0, NA))
  
  #### 예방접종 및 검진 영역
  
  #### 연간 인플루엔자 예방접종 횟수
  data_set$sc_a0300 <- ifelse(data_set$sca_01z1 == 1 & data_set$sca_05z1 %in% c(1:3), data_set$sca_05z1,
                       ifelse(data_set$sca_01z1 == 2, 0, NA))
  
  #### 계절별 인플루엔자 예방접종 경험률
  data_set$sc_a0401 <- ifelse(data_set$sca_01z1 == 1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a02 %in% c(3:5), 1, 0))
  data_set$sc_a0402 <- ifelse(data_set$sca_01z1 == 1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a02 %in% c(6:8), 1, 0))
  data_set$sc_a0403 <- ifelse(data_set$sca_01z1 == 1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a02 %in% c(9:11), 1, 0))
  data_set$sc_a0404 <- ifelse(data_set$sca_01z1 == 1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a02 %in% c(12,1,2), 1, 0))
  
  #### 연간 인플루엔자 예방접종 장소
  data_set$sc_a0501 <- ifelse(data_set$sca_01z1 ==1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a03 == 1, 1,
                             ifele(data_set$sc_a03 %in% c(2,3), 0, NA)))
  data_set$sc_a0502 <- ifelse(data_set$sca_01z1 ==1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a03 == 2, 1,
                             ifele(data_set$sc_a03 %in% c(1,3), 0, NA)))
  data_set$sc_a0503 <- ifelse(data_set$sca_01z1 ==1 & (data_set$sc_a01 == 2016 & data_set$sc_a02 %in% c(8:12)) |
                             (data_set$sc_a01 == 2017 & data_set$sc_a02 %in% c(1:11)),
                             ifelse(data_set$sc_a03 == 3, 1,
                             ifele(data_set$sc_a03 %in% c(1,2), 0, NA)))
  
  #### 이환영역(뇌졸중(중풍))
  
  #### 뇌졸중(중풍) 조기 증상 인지율
  data_set$il_d1000 <- ifelse(data_set$cva_11z2 == 1 & data_set$cva_12z1 == 1 & data_set$cva_14z2 == 1 & data_set$cva_16z2 == 1 & data_set$cva_17z1 == 1, 1,
                       ifelse(data_set$cva_11z2 == 2 | data_set$cva_12z1 == 2 | data_set$cva_14z2 == 2 | data_set$cva_16z2 == 2 | data_set$cva_17z1 == 2, 0, NA))
  
  #### 이환영역(심근경색증)
  
  #### 심근경색증 조기 증상 인지율
  data_set$il_e0600 <- ifelse(data_set$mya_10z2 == 1 & data_set$mya_11z2 == 1 & data_set$mya_12z2 == 1 & data_set$mya_14z1 == 1 & data_set$mya_15z1 == 1, 1,
                       ifelse(data_set$mya_10z2 == 2 | data_set$mya_11z2 == 2 | data_set$mya_12z2 == 2 | data_set$mya_14z1 == 2 | data_set$mya_15z1 == 2, 0, NA))
  
  #### (지역)이환영역(당뇨병)
  
  #### 당뇨병 안질환 합병증 진단 경험률
  data_set$il_b4900 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_39z1 == 1, 1,
                              ifelse(data_set$dia_39z1 == 2, 0, NA)))
  
  #### (지역)이환영역
  
  #### 민물고기 회 섭취 분율
  data_set$il_bh0100 <- ifelse(data_set$csa_01z1 == 1, 1,
                        ifelse(data_set$csa_01z1 == 2, 0, NA))
  
  #### 간흡충 치료약 복용률
  #### 분모정의
  data_set$il_bh0300 <- ifelse(data_set$csa_02z1 == 1,
                               ifelse(data_set$csa_03z1 == 1, 1,
                               ifelse(data_set$csa_03z1 == 2, 0, NA)))
  #### 분자정의
  data_set$il_bh0400 <- ifelse(data_set$csa_02z1 == 1,
                               ifelse(data_set$csa_03z1 == 1,
                                      ifelse(data_set$csa_04z1 == 1, 1,
                                      ifelse(data_set$csa_04z1 == 2, 0, NA))))
  
  #### 개인위생 영역
  
  #### 지역선택문항
  
  #### 손 씻기 시간_1-5초
  data_set$hw_a0901 <- ifelse(data_set$hwa_08z1 == 1, 1,
                       ifelse(data_set$hwa_08z1 %in% c(1:5), 0, NA))
  
  #### 손 씻기 시간_6-10초
  data_set$hw_a0902 <- ifelse(data_set$hwa_08z1 == 2, 1,
                       ifelse(data_set$hwa_08z1 %in% c(1:5), 0, NA))
  
  #### 손 씻기 시간_11-15초
  data_set$hw_a0903 <- ifelse(data_set$hwa_08z1 == 3, 1,
                       ifelse(data_set$hwa_08z1 %in% c(1:5), 0, NA))
  
  #### 손 씻기 시간_16-20초
  data_set$hw_a0904 <- ifelse(data_set$hwa_08z1 == 4, 1,
                       ifelse(data_set$hwa_08z1 %in% c(1:5), 0, NA))
  
  #### 손 씻기 시간_21초 이상
  data_set$hw_a0905 <- ifelse(data_set$hwa_08z1 == 5, 1,
                       ifelse(data_set$hwa_08z1 %in% c(1:5), 0, NA))
  
  #### 교육 및 경제활동
  
  #### 지역선택문항
  
  #### 주당 평균 근로시간
  #### 분모정의
  data_set$ej_a0100 <- ifelse(data_set$soa_01z1 == 1, 1,
                       ifelse(data_set$soa_01z1 == 2, 0, NA))
  #### 분자정의
  data_set$ej_a0400 <- ifelse(data_set$soa_14z2>0 & data_set$doa_14z2>=168, data_set$soa_14z2, NA)
  
  #### 근로시간대_주간
  data_set$ej_a0501 <- ifelse(data_set$soa_15z2 == 1, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_저녁
  data_set$ej_a0502 <- ifelse(data_set$soa_15z2 == 2, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_밤
  data_set$ej_a0503 <- ifelse(data_set$soa_15z2 == 3, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_주야간 규칙적 교대 근무
  data_set$ej_a0504 <- ifelse(data_set$soa_15z2 == 4, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_24시간 교대 근무
  data_set$ej_a0505 <- ifelse(data_set$soa_15z2 == 5, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_분할 근무
  data_set$ej_a0506 <- ifelse(data_set$soa_15z2 == 6, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_불규칙 교대 근무
  data_set$ej_a0507 <- ifelse(data_set$soa_15z2 == 7, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 근로시간대_기타
  data_set$ej_a0508 <- ifelse(data_set$soa_15z2 == 8, 1,
                       ifelse(data_set$soa_15z2 %in% c(1:8), 0, NA))
  
  #### 가구조사
  
  #### 지역선택문항
  
  #### 치매환자 조력 가구율
  data_set$fa_a0700 <- ifelse(data_set$fma_22z1 == 1, 1,
                       ifelse(data_set$fma_22z1 == 2, 0, NA))
  
  #### 치매환자 간호시 애로사항_치매치료비
  #### 분모정의
  data_set$fa_a0600 <- ifelse(data_set$fma_26z1 == 1, 1,
                       ifelse(data_set$fma_26z1 == 2, 0, NA))
  #### 분자정의
  data_set$fa_a0801 <- ifelse(data_set$fma_23z1 == 1, 1,
                       ifelse(data_set$fma_23z1 %in% c(1:5), 0, NA))
  
  #### 치매환자 간호시 애로사항_신체적
  data_set$fa_a0802 <- ifelse(data_set$fma_23z1 == 2, 1,
                       ifelse(data_set$fma_23z1 %in% c(1:5), 0, NA))
  
  #### 치매환자 간호시 애로사항_심리적
  data_set$fa_a0803 <- ifelse(data_set$fma_23z1 == 3, 1,
                       ifelse(data_set$fma_23z1 %in% c(1:5), 0, NA))
  
  #### 치매환자 간호시 애로사항_시간없음
  data_set$fa_a0804 <- ifelse(data_set$fma_23z1 == 4, 1,
                       ifelse(data_set$fma_23z1 %in% c(1:5), 0, NA))
  
  #### 치매환자 간호시 애로사항_가족간 불화
  data_set$fa_a0805 <- ifelse(data_set$fma_23z1 == 5, 1,
                       ifelse(data_set$fma_23z1 %in% c(1:5), 0, NA))