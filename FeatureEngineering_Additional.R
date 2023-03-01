

# 指標を再標準化＋Zスコア化
Py <- Py |> 
  tidyr::pivot_longer(c(anal, preanal), 
                      names_to = "item", 
                      values_to = "score") |> 
  jtools::gscale(vars = "score", n.sd = 1) |> 
  dplyr::mutate(score = score * 10 + 50) |> 
  tidyr::pivot_wider(names_from = "item", 
                     values_from = "score") |> 
  tidyr::pivot_longer(c(engineer, preengineer), 
                      names_to = "item", 
                      values_to = "score") |> 
  jtools::gscale(vars = "score", n.sd = 1) |> 
  dplyr::mutate(score = score * 10 + 50) |> 
  tidyr::pivot_wider(names_from = "item", 
                     values_from = "score") |> 
  tidyr::pivot_longer(c(nurture, prenurture), 
                      names_to = "item", 
                      values_to = "score") |> 
  jtools::gscale(vars = "score", n.sd = 1) |> 
  dplyr::mutate(score = score * 10 + 50) |> 
  tidyr::pivot_wider(names_from = "item", 
                     values_from = "score")

# 前職と現職のスキルスコアの差

PyAnalysis <- Py |> 
  tidyr::drop_na(anal, engineer, nurture, preanal, preengineer, prenurture) |> 
  dplyr::mutate(delta_anal = anal - preanal, 
                delta_engineer = engineer - preengineer, 
                delta_nurture = nurture - prenurture) |> 
  dplyr::select(id, dansu, empchange, after60mobility_d, mobilityage, female, univ, preempform, mobreason, agespan, delta_anal, delta_engineer, delta_nurture) |> 
  tidyr::drop_na()
