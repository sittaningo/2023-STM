# 社会人口学的属性の操作化----

# 年齢
d <- d |> dplyr::mutate(age = q1_2_5)

# 女性ダミー
d <- d |> dplyr::mutate(female = q1_1 - 1, 
                        female = factor(female,
                                        levels = c(0, 1),
                                        labels = c("男性", "女性")
                                                )
                        )

# 大卒ダミー
d <- d |> dplyr::mutate(univ = dplyr::case_when(edssm < 10 ~ 0, 
                                                edssm == 10 | edssm == 11 ~ 1, 
                                                TRUE ~ NA_real_), 
                        univ = factor(univ,
                                      levels = c(0, 1), 
                                      labels = c("非大卒", "大卒")
                                              )
                        )

# 職業経歴の操作化----

# 移動経験のあるものに限定
d <- d |> filter(dansu > 1)

# 経歴：従業開始年齢
Py <- d |> 
  mutate(age1 = q8_h_1,
         age2 = q9_2_c_7,
         age3 = q9_3_c_7,
         age4 = q9_4_c_7,
         age5 = q9_5_c_7,
         age6 = q9_6_c_7,
         age7 = q9_7_c_7,
         age8 = q9_8_c_7,
         age9 = q9_9_c_7,
         age10 = q9_10_c_7,
         age11 = q9_11_c_7,
         age12 = q9_12_c_7,
         age13 = q9_13_c_7,
         age14 = q9_14_c_7,
         age15 = q9_15_c_7,
         age16 = q9_16_c_7,
         age17 = q9_17_c_7,
         age18 = q9_18_c_7,
         age22 = q9_22_c_7) |> 
  dplyr::select(id, age1:age22) |> 
  tidyr::pivot_longer(age1:age22, 
                      names_to = "dansu", 
                      names_prefix = "age", 
                      values_to = "agespan") |> 
  dplyr::mutate(agespan = dplyr::if_else(agespan > 80, NA_real_, agespan))

# 経歴：従業終了年齢
Py <- d |> 
  dplyr::mutate(endage1 = q8_h_2, 
                endage2 = q9_2_c_8, 
                endage3 = q9_3_c_8, 
                endage4 = q9_4_c_8, 
                endage5 = q9_5_c_8, 
                endage6 = q9_6_c_8, 
                endage7 = q9_7_c_8, 
                endage8 = q9_8_c_8, 
                endage9 = q9_9_c_8, 
                endage10 = q9_10_c_8, 
                endage11 = q9_11_c_8, 
                endage12 = q9_12_c_8, 
                endage13 = q9_13_c_8, 
                endage14 = q9_14_c_8, 
                endage15 = q9_15_c_8, 
                endage16 = q9_16_c_8, 
                endage17 = q9_17_c_8, 
                endage18 = q9_18_c_8, 
                endage22 = q9_22_c_8) |> 
  dplyr::select(id, endage1:endage22) |> 
  tidyr::pivot_longer(endage1:endage22, 
                      names_to = "dansu", 
                      names_prefix = "endage", 
                      values_to = "endage") |> 
  dplyr::mutate(endage = dplyr::if_else(endage > 80, NA_real_, endage)) |> 
  dplyr::left_join(Py, ., by = c("id", "dansu"))

# 経歴：無業ダミー
Py <- d |> 
  dplyr::mutate(unemp1 = q8_h_2, 
                unemp2 = q9_2_b, 
                unemp3 = q9_3_b, 
                unemp4 = q9_4_b, 
                unemp5 = q9_5_b, 
                unemp6 = q9_6_b, 
                unemp7 = q9_7_b, 
                unemp8 = q9_8_b, 
                unemp9 = q9_9_b, 
                unemp10 = q9_10_b, 
                unemp11 = q9_11_b, 
                unemp12 = q9_12_b, 
                unemp13 = q9_13_b, 
                unemp14 = q9_14_b, 
                unemp15 = q9_15_b, 
                unemp16 = q9_16_b, 
                unemp17 = q9_17_b, 
                unemp18 = q9_18_b, 
                unemp22 = q9_22_b) |> 
  dplyr::select(id, unemp1:unemp22) |> 
  tidyr::pivot_longer(unemp1:unemp22, 
                      names_to = "dansu", 
                      names_prefix = "unemp", 
                      values_to = "unemp") |> 
  dplyr::mutate(unemp = dplyr::if_else(unemp == 2, 1, 0)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()

# 経歴：従業先の変化
Py <- d |> 
  dplyr::mutate(empchange1 = NA_real_, 
                empchange2 = q9_2_a, 
                empchange3 = q9_3_a, 
                empchange4 = q9_4_a, 
                empchange5 = q9_5_a, 
                empchange6 = q9_6_a, 
                empchange7 = q9_7_a, 
                empchange8 = q9_8_a, 
                empchange9 = q9_9_a, 
                empchange10 = q9_10_a, 
                empchange11 = q9_11_a, 
                empchange12 = q9_12_a, 
                empchange13 = q9_13_a, 
                empchange14 = q9_14_a, 
                empchange15 = q9_15_a, 
                empchange16 = q9_16_a, 
                empchange17 = q9_17_a, 
                empchange18 = q9_18_a, 
                empchange22 = q9_22_a) |> 
  dplyr::select(id, empchange1:empchange22) |> 
  tidyr::pivot_longer(empchange1:empchange22, 
                      names_to = "dansu", 
                      names_prefix = "empchange", 
                      values_to = "empchange") |> 
  dplyr::mutate(empchange = dplyr::if_else(empchange > 2, NA_real_, empchange)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()

# 経歴：職業小分類
Py <- d |> 
  dplyr::mutate(occ1 = q8_f, 
                occ2 = q9_2_c_5, 
                occ3 = q9_3_c_5, 
                occ4 = q9_4_c_5, 
                occ5 = q9_5_c_5, 
                occ6 = q9_6_c_5, 
                occ7 = q9_7_c_5, 
                occ8 = q9_8_c_5, 
                occ9 = q9_9_c_5, 
                occ10 = q9_10_c_5, 
                occ11 = q9_11_c_5, 
                occ12 = q9_12_c_5, 
                occ13 = q9_13_c_5, 
                occ14 = q9_14_c_5, 
                occ15 = q9_15_c_5, 
                occ16 = q9_16_c_5, 
                occ17 = q9_17_c_5, 
                occ18 = q9_18_c_5, 
                occ22 = q9_22_c_5) |> 
  dplyr::select(id, occ1:occ22) |> 
  tidyr::pivot_longer(occ1:occ22, 
                      names_to = "dansu", 
                      names_prefix = "occ", 
                      values_to = "occ") |> 
  dplyr::mutate(occ = dplyr::if_else(occ > 8888, NA_real_, occ)) |> 
  dplyr::left_join(Py, ., by = c("id", "dansu"))

# 経歴：従業上の地位
Py <- d |> 
  dplyr::mutate(empform1 = q8_a, 
                empform2 = q9_2_c_4, 
                empform3 = q9_3_c_4, 
                empform4 = q9_4_c_4, 
                empform5 = q9_5_c_4, 
                empform6 = q9_6_c_4, 
                empform7 = q9_7_c_4, 
                empform8 = q9_8_c_4, 
                empform9 = q9_9_c_4, 
                empform10 = q9_10_c_4, 
                empform11 = q9_11_c_4, 
                empform12 = q9_12_c_4, 
                empform13 = q9_13_c_4, 
                empform14 = q9_14_c_4, 
                empform15 = q9_15_c_4, 
                empform16 = q9_16_c_4, 
                empform17 = q9_17_c_4, 
                empform18 = q9_18_c_4, 
                empform22 = q9_22_c_4) |> 
  dplyr::select(id, empform1:empform22) |> 
  tidyr::pivot_longer(empform1:empform22, 
                      names_to = "dansu", 
                      names_prefix = "empform", 
                      values_to = "empform") |> 
  dplyr::mutate(empform = dplyr::if_else(empform > 9, NA_real_, empform)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))() |> 
  dplyr::mutate(empform = dplyr::recode(empform, 
                                        `1` = "自営業", 
                                        `2` = "正規雇用", 
                                        `3` = "非正規雇用", 
                                        `4` = "非正規雇用", 
                                        `5` = "非正規雇用", 
                                        `6` = "非正規雇用", 
                                        `7` = "自営業", 
                                        `8` = "自営業", 
                                        `11` = "無業", 
                                        `888` = "無業", 
                                        .default = NA_character_))

# 経歴：企業規模
Py <- d |> 
  dplyr::mutate(firm1 = q8_c, 
                firm2 = q9_2_c_3, 
                firm3 = q9_3_c_3, 
                firm4 = q9_4_c_3, 
                firm5 = q9_5_c_3, 
                firm6 = q9_6_c_3, 
                firm7 = q9_7_c_3, 
                firm8 = q9_8_c_3, 
                firm9 = q9_9_c_3, 
                firm10 = q9_10_c_3, 
                firm11 = q9_11_c_3, 
                firm12 = q9_12_c_3, 
                firm13 = q9_13_c_3, 
                firm14 = q9_14_c_3, 
                firm15 = q9_15_c_3, 
                firm16 = q9_16_c_3, 
                firm17 = q9_17_c_3, 
                firm18 = q9_18_c_3, 
                firm22 = q9_22_c_3) |> 
  dplyr::select(id, firm1:firm22) |> 
  tidyr::pivot_longer(firm1:firm22, 
                      names_to = "dansu", 
                      names_prefix = "firm", 
                      values_to = "firm") |> 
  dplyr::mutate(firm = dplyr::if_else(firm > 10, NA_real_, firm)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))() |> 
  dplyr::mutate(firm = dplyr::case_when(firm == 1 ~ "1-29人", 
                                        firm == 2 ~ "1-29人",
                                        firm == 3 ~ "1-29人",
                                        firm == 4 ~ "1-29人",
                                        firm == 5 ~ "30-99人",
                                        firm == 6 ~ "100-299人",
                                        firm == 7 ~ "300-999人",
                                        firm == 8 ~ "300-999人",
                                        firm == 9 ~ "1000人以上",
                                        firm == 10 ~ "官公庁"))

# 経歴：就業理由（プリコード）
Py <- d |> 
  dplyr::mutate(reason1 = NA_real_, 
                reason2 = q9_2_b_1, 
                reason3 = q9_3_b_1, 
                reason4 = q9_4_b_1, 
                reason5 = q9_5_b_1, 
                reason6 = q9_6_b_1, 
                reason7 = q9_7_b_1, 
                reason8 = q9_8_b_1, 
                reason9 = q9_9_b_1, 
                reason10 = q9_10_b_1, 
                reason11 = q9_11_b_1, 
                reason12 = q9_12_b_1, 
                reason13 = q9_13_b_1, 
                reason14 = q9_14_b_1, 
                reason15 = q9_15_b_1, 
                reason16 = q9_16_b_1, 
                reason17 = q9_17_b_1, 
                reason18 = q9_18_b_1, 
                reason22 = q9_22_b_1) |> 
  dplyr::select(id, reason1:reason22) |> 
  tidyr::pivot_longer(reason1:reason22, 
                      names_to = "dansu", 
                      names_prefix = "reason", 
                      values_to = "reason") |> 
  dplyr::mutate(reason = dplyr::if_else(reason > 888, NA_real_, reason)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()

# py：就業理由（アフターコード）
Py <- d |> 
  dplyr::mutate(otherreason1 = NA_real_, 
                otherreason2 = q9_2_b_1_9, 
                otherreason3 = q9_3_b_1_9, 
                otherreason4 = q9_4_b_1_9, 
                otherreason5 = q9_5_b_1_9, 
                otherreason6 = q9_6_b_1_9, 
                otherreason7 = q9_7_b_1_9, 
                otherreason8 = q9_8_b_1_9, 
                otherreason9 = q9_9_b_1_9, 
                otherreason10 = q9_10_b_1_9, 
                otherreason11 = q9_11_b_1_9, 
                otherreason12 = q9_12_b_1_9, 
                otherreason13 = q9_13_b_1_9, 
                otherreason14 = q9_14_b_1_9, 
                otherreason15 = q9_15_b_1_9, 
                otherreason16 = q9_16_b_1_9, 
                otherreason17 = q9_17_b_1_9, 
                otherreason18 = q9_18_b_1_9, 
                otherreason22 = q9_22_b_1_9) |> 
  dplyr::select(id, otherreason1:otherreason22) |> 
  tidyr::pivot_longer(otherreason1:otherreason22, 
                      names_to = "dansu", 
                      names_prefix = "otherreason", 
                      values_to = "otherreason") |> 
  dplyr::mutate(otherreason = dplyr::if_else(otherreason > 88, NA_real_, otherreason)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()

# 就業理由の操作化

Py <- Py |> 
  dplyr::mutate(mobreason = dplyr::case_when(reason == 1 ~ "引退",
                                             reason == 2 ~ "非自発",
                                             reason == 3 ~ "積極的自発", 
                                             reason == 4 ~ "家族", 
                                             reason == 5 ~ "積極的自発", 
                                             reason == 6 ~ "消極的自発", 
                                             reason == 7 ~ "健康", 
                                             reason == 8 ~ NA_character_, 
                                             otherreason == 1 ~ "引退", 
                                             otherreason == 2 ~ "非自発", 
                                             otherreason == 3 ~ "積極的自発", 
                                             otherreason == 4 ~ "家族", 
                                             otherreason == 5 ~ "積極的自発", 
                                             otherreason == 6 ~ "消極的自発", 
                                             otherreason == 7 ~ "積極的自発", 
                                             otherreason == 8 ~ NA_character_, 
                                             otherreason == 9 ~ "消極的自発", 
                                             otherreason == 10 ~ "積極的自発", 
                                             otherreason == 11 ~ "積極的自発", 
                                             otherreason == 12 ~ "積極的自発", 
                                             otherreason == 13 ~ "健康", 
                                             otherreason == 14 ~ NA_character_, 
                                             otherreason == 15 ~ NA_character_, 
                                             otherreason == 21 ~ NA_character_), 
                mobreason = dplyr::recode(mobreason,
                                          "積極的自発" = "自発", 
                                          "消極的自発" = "自発",
                                          "非自発" = "非自発・家族・健康",
                                          "家族" = "非自発・家族・健康",
                                          "健康" = "非自発・家族・健康",
                                          "引退" = "定年"))

# その他の処理----

# dansuをnumeric型に
Py <- Py |> 
  dplyr::mutate(dansu = as.numeric(dansu))

# 性別と学歴を結合
Py <- d |> 
  dplyr::select(id, female, univ) |> 
  (\(.) dplyr::left_join(Py, ., by = "id"))()

# 移動時年齢を操作化
Py <- Py |> 
  dplyr::group_by(id) |>
  dplyr::mutate(after60mobility_d = if_else(agespan >= 60, 1, 0),
                mobilityage = case_when(
                  (agespan >= 20 & agespan < 30) ~ "20-29歳",
                  (agespan >= 30 & agespan < 40) ~ "30-39歳",
                  (agespan >= 40 & agespan < 50) ~ "40-49歳",
                  (agespan >= 50 & agespan < 60) ~ "50-59歳",
                  agespan >= 60 ~ "60歳以上",
                  TRUE ~ NA_character_)) |> 
  dplyr::ungroup()

