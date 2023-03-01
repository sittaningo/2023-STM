PyAnalysis |> 
  dplyr::select(delta_nurture, delta_engineer, delta_anal, female, univ, preempform, mobreason, after60mobility_d) |> 
  tbl_summary(by = after60mobility_d,
              label = list(female ~ "女性ダミー",
                           univ ~ "大卒ダミー",
                           delta_nurture ~ "ケア", 
                           delta_engineer ~ "科学技術", 
                           delta_anal ~ "分析", 
                           preempform ~ "前職の雇用形態", 
                           mobreason ~ "離職理由"),
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{p}"), 
              digits = list(all_continuous() ~ 2, 
                            all_categorical() ~ 2)
  ) |> 
  as_flex_table() |> 
  flextable::save_as_docx(path = "out/DescriptiveStatistics_Older.docx")

