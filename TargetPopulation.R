# agespanが欠損＝dansuの観察がないサンプルを除外
# employer changeの移動に限定
Py <- Py |> 
  dplyr::filter(!is.na(agespan)) |> 
  dplyr::filter(empchange == 2 & empform != "自営業" & preempform != "自営業")
