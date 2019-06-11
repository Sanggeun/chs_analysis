
  #### 음주

  #### 지역선택문항

  #### 연간 비자발적 음주 경험률
  data_set$de_a0800 <- ifelse(data_set$dra_01z1 == 1,
                              ifelse(data_set$drb_02z1 == 1,
                                     ifelse(data_set$dra_05z1 == 1, 0,
                                     ifelse(data_set$dra_05z1 %in% c(2:5), 1,
                              ifelse(data_set$drb_02z1 == 2, 0,
                       ifelse(data_set$dra_01z1 ==2, 0, NA))))))
  
  #### 식생활영역
  
  #### 지역선택문항
  
  #### 국물섭취정도
  data_set$nu_b1000 <- ifelse(data_set$nub_06z1 %in% c(1,2), 1,
                       ifelse(data_set$nub_06z1 %in% c(3,4), 0, NA))
  
  #### 저염식 실천을 위한 물리적 환경 노출률
  data_set$nu_b0900 <- ifelse(data_set$nub_07z1 == 1, 1,
                       ifelse(data_set$nub_07z1 == 2, 0, NA))
  
  #### 식사시간_0-10분
  data_set$nu_a0501 <- ifelse(data_set$nua_09z1 == 1, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  
  #### 식사시간_11-20분
  data_set$nu_a0502 <- ifelse(data_set$nua_09z1 == 2, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  상
  #### 식사시간_21-30분
  data_set$nu_a0503 <- ifelse(data_set$nua_09z1 == 3, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  
  #### 식사시간_31-40분
  data_set$nu_a0504 <- ifelse(data_set$nua_09z1 == 4, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  
  #### 식사시간_41-50분
  data_set$nu_a0505 <- ifelse(data_set$nua_09z1 == 5, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  
  #### 식사시간_51-60분
  data_set$nu_a0506 <- ifelse(data_set$nua_09z1 == 6, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  
  #### 식사시간_1시간 이사
  data_set$nu_a0507 <- ifelse(data_set$nua_09z1 == 7, 1,
                       ifelse(data_set$nua_09z1 %in% c(1:7), 0, NA))
  
  #### 비만 및 체중조절
  
  #### 지역선택문항
  
  #### 연간 체중변화 경험률
  data_set$ob_b0300 <- ifelse(data_set$obb_03z1 == 1, 1,
                       ifelse(data_set$obb_03z1 %in% c(2,3), 0, NA))
  
  #### 연간 체중 증가 정도_3-6kg 미만 증가
  #### 분모정의
  data_set$ob_b0303 <- ifelse(data_set$obb_03z1 == 3, 1,
                       ifelse(data_set$obB_03z1 %in% c(1,2), 0, NA))
  #### 분자정의
  data_set$ob_b0401 <- ifelse(data_set$obb_04z1 == 1, 1,
                       ifelse(data_set$obb_04z1 %in% c(1:3), 0, NA))
  
  #### 연간 체중 증가 정도_6-10kg 미만 증가
  data_set$ob_b0401 <- ifelse(data_set$obb_04z1 == 1, 1,
                       ifelse(data_set$obb_04z1 %in% c(1:3), 0, NA))
  
  #### 연간 체중 증가 정도_10kg 이상 증가
  data_set$ob_b0401 <- ifelse(data_set$obb_04z1 == 1, 1,
                       ifelse(data_set$obb_04z1 %in% c(1:3), 0, NA))
  
  #### 정신건강영역
  
  #### 지역선택문항
  
  #### 우울증 치료 가능 인지율
  data_set$mt_b0400 <- ifelse(data_set$mtb_06z1 %in% c(1,2), 1,
                       ifelse(data_set$mtb_06z1 %in% c(3,4), 0, NA))
  
  #### 도박으로 인한 일상생활 지장 경험률
  data_set$mt_i0100 <- ifelse(data_set$mti_02z1 == 1, 1,
                       ifelse(data_set$mti_02z1 == 2, 0, NA))
  
  #### 도박으로 인한 거짓말 경험률
  data_set$mt_i0200 <- ifelse(data_set$mti_02z1 == 1, 1,
                       ifelse(data_set$mti_02z1 == 2, 0, NA))
  
  #### 도박에 돈을 베팅하고 싶은 욕구 경험률
  data_set$mt_i0300 <- ifelse(data_set$mti_03z1 == 1, 1,
                       ifelse(data_set$mti_03z1 == 2, 0, NA))
  
  #### (지역)이환영역(당뇨병)
  
  #### 당뇨병 합병증검사 미수진 이유_검사비용이 비싸서
  data_set$il_b4601 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_32z1 == 1, 1,
                              ifelse(data_set$dia_32z1 %in% c(1:6), 0, NA)))
  
  #### 당뇨병 합병증검사 미수진 이유_시간이 없어서
  data_set$il_b4602 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_32z1 == 2, 1,
                              ifelse(data_set$dia_32z1 %in% c(1:6), 0, NA)))
  
  #### 당뇨병 합병증검사 미수진 이유_합병증 검사를 몰라서
  data_set$il_b4603 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_32z1 == 3, 1,
                              ifelse(data_set$dia_32z1 %in% c(1:6), 0, NA)))
  
  #### 당뇨병 합병증검사 미수진 이유_필요성을 못느껴서
  data_set$il_b4604 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_32z1 == 4, 1,
                              ifelse(data_set$dia_32z1 %in% c(1:6), 0, NA)))
  
  #### 당뇨병 합병증검사 미수진 이유_검사결과가 두려워서
  data_set$il_b4605 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_32z1 == 5, 1,
                              ifelse(data_set$dia_32z1 %in% c(1:6), 0, NA)))
  
  #### 당뇨병 합병증검사 미수진 이유_기타
  data_set$il_b4606 <- ifelse(data_set$il_b0200 == 1,
                              ifelse(data_set$dia_32z1 == 6, 1,
                              ifelse(data_set$dia_32z1 %in% c(1:6), 0, NA)))
  
  #### (지역)이환영역(심근경색)
  
  #### 심근경색증 조기 증상 인지율_숨이 찬다
  data_set$il_e0406 <- ifelse(data_set$mya_15z1 == 1, 1,
                       ifelse(data_set$mya_15z1 == 2, 0, NA))
  
  #### 심근경색증 증상 대처방법_병원으로 데려간다
  data_set$il_0501 <- ifelse(data_set$mya_22z1 == 1, 1,
                      ifelse(data_set$mya_22z1 %in% c(1:5), 0, NA))
  
  #### 심근경색증 증상 대처방법_한방병원으로 데려간다
  data_set$il_0502 <- ifelse(data_set$mya_22z1 == 2, 1,
                      ifelse(data_set$mya_22z1 %in% c(1:5), 0, NA))
  
  #### 심근경색증 증상 대처방법_119에 전화한다
  data_set$il_0503 <- ifelse(data_set$mya_22z1 == 3, 1,
                      ifelse(data_set$mya_22z1 %in% c(1:5), 0, NA))
  
  #### 심근경색증 증상 대처방법_가족에게 연락한다
  data_set$il_0504 <- ifelse(data_set$mya_22z1 == 4, 1,
                      ifelse(data_set$mya_22z1 %in% c(1:5), 0, NA))
  
  #### 심근경색증 증상 대처방법_기타
  data_set$il_0505 <- ifelse(data_set$mya_22z1 == 5, 1,
                      ifelse(data_set$mya_22z1 %in% c(1:5), 0, NA))
  
  #### (지역)이환영역
  
  #### 건강지식 습득방법_의료인
  data_set$qu_a0301 <- ifelse(data_set$qua_03z1 == 1, 1,
                       ifelse(data_set$qua_03z1 %in% c(1:5), 0, NA))
  
  #### 건강지식 습득방법_대중매체
  data_set$qu_a0302 <- ifelse(data_set$qua_03z1 == 2, 1,
                       ifelse(data_set$qua_03z1 %in% c(1:5), 0, NA))
  
  #### 건강지식 습득방법_인터넷
  data_set$qu_a0303 <- ifelse(data_set$qua_03z1 == 3, 1,
                       ifelse(data_set$qua_03z1 %in% c(1:5), 0, NA))
  
  #### 건강지식 습득방법_소식지
  data_set$qu_a0304 <- ifelse(data_set$qua_03z1 == 4, 1,
                       ifelse(data_set$qua_03z1 %in% c(1:5), 0, NA))
  
  #### 건강지식 습득방법_기타
  data_set$qu_a0305 <- ifelse(data_set$qua_03z1 == 5, 1,
                       ifelse(data_set$qua_03z1 %in% c(1:5), 0, NA))
  
  #### 활동제한영역
  
  #### 지역선택문항
  
  #### 양호한 주관적 생활습관 인지율
  data_set$ql_d0100 <- ifelse(data_set$qod_05z1 %in% c(1,2), 1,
                       ifelse(data_set$qod_05z1 %in% c(3:5), 0, NA))
  
  #### 손씻기 영역
  
  #### 지역선택문항
  
  #### 기침예절 실천율
  data_set$hw_b0100 <- ifelse(data_set$hwb_01z1 %in% c(1,2), 1,
                       ifelse(data_set$hwb_01z1 %in% c(3,4), 0, NA))