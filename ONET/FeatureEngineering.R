# スキル指標の構築----

# スキル指標の項目を抽出
onet_komugi <- onet |> 
  dplyr::select(IPD_04_03_01_001, IPD_04_03_01_002, IPD_04_03_01_003, IPD_04_03_01_004, IPD_04_03_01_009, IPD_04_04_01_015, IPD_04_03_01_011, IPD_04_03_01_012, IPD_04_03_01_013, IPD_04_03_01_021, IPD_04_03_01_026, IPD_04_03_01_022, IPD_04_03_01_023, IPD_04_03_01_015, IPD_04_03_01_020, IPD_04_03_01_037, IPD_04_03_01_039, IPD_04_04_01_001, IPD_04_04_01_011, IPD_04_04_01_010, IPD_04_04_01_012, IPD_04_04_01_014, IPD_04_04_01_016, IPD_04_04_01_017, IPD_04_04_01_018, IPD_04_10_011, IPD_04_10_020, IPD_04_10_029, IPD_04_10_034, IPD_04_10_036, ssm_occnum)

#すべてをdouble型にする
onet_komugi <- onet_komugi |> 
  dplyr::mutate(dplyr::across(.cols = everything(), as.numeric))

# 重複する職業を平均する処理
onet_komugi <- onet_komugi |> 
  dplyr::group_by(ssm_occnum) |> 
  dplyr::summarise(dplyr::across(
    c(IPD_04_03_01_001, IPD_04_03_01_002, IPD_04_03_01_003, IPD_04_03_01_004, IPD_04_03_01_009, IPD_04_04_01_015, IPD_04_03_01_011, IPD_04_03_01_012, IPD_04_03_01_013, IPD_04_03_01_021, IPD_04_03_01_026, IPD_04_03_01_022, IPD_04_03_01_023, IPD_04_03_01_015, IPD_04_03_01_020, IPD_04_03_01_037, IPD_04_03_01_039, IPD_04_04_01_001, IPD_04_04_01_011, IPD_04_04_01_010, IPD_04_04_01_012, IPD_04_04_01_014, IPD_04_04_01_016, IPD_04_04_01_017, IPD_04_04_01_018, IPD_04_10_011, IPD_04_10_020, IPD_04_10_029, IPD_04_10_034, IPD_04_10_036), 
    mean, na.rm = TRUE) , 
    .groups = "keep") |> 
  dplyr::ungroup() |> 
  tidyr::drop_na()

# SSMとマッチング----

Py <- onet_komugi |> 
  dplyr::rename(occ = ssm_occnum) |> 
  (\(.) dplyr::left_join(Py, ., by = "occ"))()


# komugiをlonger + 標準化----

Py <- Py |> 
  tidyr::pivot_longer(IPD_04_03_01_011:IPD_04_10_036, 
               names_to = "IPD", 
               values_to = "score") |> 
  jtools::gscale(vars = "score", n.sd = 1) |> 
  tidyr::pivot_wider(names_from = "IPD", 
              values_from = "score", 
              values_fn = mean)

# 指標を合算
Py <- Py |> 
  dplyr::mutate(anal = (IPD_04_03_01_011 + IPD_04_03_01_012 + IPD_04_03_01_013 + IPD_04_03_01_021)/4,
                engineer = (IPD_04_03_01_022 + IPD_04_03_01_023 + IPD_04_04_01_010 + IPD_04_04_01_012 + IPD_04_04_01_014 + IPD_04_04_01_016 + IPD_04_04_01_017 + IPD_04_04_01_018)/8,
                nurture = (IPD_04_03_01_015 + IPD_04_03_01_020 + IPD_04_10_029)/3
         )


# ラグを取る

Py <- Py |> 
  dplyr::group_by(id) |> 
  dplyr::mutate(preempform = lag(empform),
                preocc =lag(occ),
                preanal = lag(anal),
                prenurture = lag(nurture), 
                preengineer = lag(engineer)
  ) |> 
  dplyr::ungroup()