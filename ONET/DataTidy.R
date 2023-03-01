# IPDと項目名の対応表の作成----

# 項目名の抽出
item <- onet |> 
  dplyr::filter(`1` <= 18) |> 
  dplyr::mutate(`1` = as.character(`1`)) |> 
  tidyr::pivot_longer(`1`:`...372`, # (1)
                      names_to = "var", 
                      names_prefix = "V",
                      values_to = "item") |> 
  dplyr::mutate(row = row_number()) |> 
  dplyr::filter(row > 2 & row <= 372) |> # 2〜(1)まで
  dplyr::select(item) |> 
  unlist(use.names = FALSE) #FIXME


# IPDの抽出
IPD <- onet |> 
  dplyr::filter(`1` <= 18) |> 
  dplyr::mutate(`1` = as.character(`1`)) |> 
  tidyr::pivot_longer(`1`:`...372`, # (1)
                      names_to = "var", 
                      names_prefix = "V",
                      values_to = "item") |> 
  dplyr::mutate(row = row_number()) |> 
  dplyr::filter(row > 374 & row <= 744) |> #(1)の値+2〜(1)の値*2まで
  dplyr::select(item) |> 
  base::unlist(use.names = FALSE)

onet_item <- tibble(item, IPD) # 項目とIPDの対応表

# 変数名の付与
onet <- onet |> 
  dplyr::filter(`1` >= 18) |> 
  dplyr::select(-`1`, -`...2`) |> 
  setNames(IPD) |> 
  dplyr::mutate(row = row_number()) # 列名ありのO-NETデータ

# 不要になったvalを削除
rm(IPD, item)

# ONET職業とSSM職業のマッチング----

onet <- onet_ssm_occ |> 
  select(-onet_occlabel) |> 
  mutate(IPD_01_01_001 = as.character(onet_occnum)) |>
  select(-onet_occnum) |> 
  inner_join(onet, ., by = "IPD_01_01_001")
