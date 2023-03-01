#年齢層別----

# 出力用のデータに整形
Mobvis_Age <- PyAnalysis |> 
  tidyr::pivot_longer(c(delta_anal, delta_nurture, delta_engineer), 
               names_to = "skillname", 
               values_to = "skillscore") |> 
  dplyr::group_by(mobilityage, skillname, empchange) |> 
  dplyr::summarise(skillscore_mean = mean(skillscore, na.rm = TRUE),
                   skillscore_sd = sd(skillscore, na.rm = TRUE), 
                   .groups = "keep") |> 
  dplyr::mutate(mobilityage = as.factor(mobilityage),
                mobilityage = forcats::fct_rev(mobilityage)) |> 
  dplyr::ungroup()

# 出力
Mobvis_Age |> 
  dplyr::mutate(skillname = dplyr::recode(skillname, 
                                          delta_anal = "分析", 
                                          delta_engineer = "科学技術", 
                                          delta_nurture = "ケア"),
                skillname = forcats::fct_relevel(skillname, c("ケア", "科学技術", "分析"))
  ) |> 
  ggpubr::ggdotchart(x = "mobilityage", y = "skillscore_mean",
                     add = "segments",
                     sorting = "none",
                     rotate = TRUE, 
                     add.params = list(color = "lightgray", 
                                       size = 2),
                     dot.size = 4, 
                     label = round(Mobvis_Age$skillscore_mean, 2),
                     font.label = list(color = "black", 
                                       size = 12, 
                                       hjust = 1.25, 
                                       vjust = 0.5),
                     label.rectangle = TRUE
  ) + 
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  ggpubr::theme_pubr(base_size = 16, base_family = "SourceHanSans-Regular") + 
  ggplot2::facet_wrap(~ skillname) + 
  ggplot2::labs(x = "移動時年齢", 
       y = "スキルスコア") + 
  ggplot2::scale_y_continuous(limits = c(-5, 1)) + 
  ggplot2::ggsave("out/SkillTrasnsfer_Age.png", width = 12, height = 7.62)

# カテゴリ別----

Mobvis_Older <- PyAnalysis |> 
  dplyr::filter(after60mobility_d == 1)


#ジェンダー
Mobvis_Gender <- Mobvis_Older |> 
  tidyr::pivot_longer(c(delta_anal, delta_nurture, delta_engineer), 
                      names_to = "skillname", 
                      values_to = "skillscore") |> 
  dplyr::group_by(female, skillname, empchange) |> 
  dplyr::summarise(skillscore_mean = mean(skillscore, na.rm = T),
                   skillscore_sd = sd(skillscore, na.rm = T), 
                   .groups = "keep") |> 
  dplyr::rename(category = female) |>
  ungroup()

#学歴
Mobvis_Univ <- Mobvis_Older |> 
  tidyr::pivot_longer(c(delta_anal, delta_nurture, delta_engineer), 
                      names_to = "skillname", 
                      values_to = "skillscore") |> 
  dplyr::group_by(univ, skillname, empchange) |> 
  dplyr::summarise(skillscore_mean = mean(skillscore, na.rm = T),
                   skillscore_sd = sd(skillscore, na.rm = T), 
                   .groups = "keep") |> 
  dplyr::rename(category = univ) |>
  dplyr::ungroup()

#雇用形態
Mobvis_PreEmp <- Mobvis_Older |> 
  tidyr::pivot_longer(c(delta_anal, delta_nurture, delta_engineer), 
                      names_to = "skillname", 
                      values_to = "skillscore") |> 
  dplyr::group_by(preempform, skillname) |> 
  dplyr::summarise(skillscore_mean = mean(skillscore, na.rm = T),
                   skillscore_sd = sd(skillscore, na.rm = T), 
                   .groups = "keep") |> 
  dplyr::rename(category = preempform) |>
  dplyr::ungroup()

#離職理由
Mobvis_PreReason <- Mobvis_Older |> 
  tidyr::pivot_longer(c(delta_anal, delta_nurture, delta_engineer), 
                      names_to = "skillname", 
                      values_to = "skillscore") |> 
  dplyr::group_by(mobreason, skillname) |> 
  dplyr::summarise(skillscore_mean = mean(skillscore, na.rm = T),
                   skillscore_sd = sd(skillscore, na.rm = T), 
                   .groups = "keep") |> 
  dplyr::rename(category = mobreason) |>
  dplyr::ungroup()


# 各カテゴリの統合
Mobvis <- Mobvis_Gender |> 
  dplyr::bind_rows(list(Mobvis_Univ, Mobvis_PreEmp, Mobvis_PreReason)) |>
  dplyr::mutate(
    category = dplyr::recode(category,
                             "男性" = 1L,
                             "女性" = 2L,
                             "大卒" = 3L,
                             "非大卒" = 4L,
                             "正規雇用" = 5L,
                             "非正規雇用" = 6L,
                             "自発" = 7L,
                             "非自発・家族・健康" = 8L,
                             "定年" = 9L,
                             .default = NA_integer_),
    cat = factor(category,
                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                 labels = c("男性", "女性", "大卒", "非大卒", "正規雇用", "非正規雇用", "自発", "非自発・家族・健康", "定年")
    ))

# カテゴリに対応する枠組みを設定

Mobvis <- Mobvis |> 
  dplyr::mutate(class = dplyr::case_when(cat == "男性" ~ "ジェンダー", 
                                         cat == "女性" ~ "ジェンダー", 
                                         cat == "非大卒" ~ "学歴", 
                                         cat == "大卒" ~ "学歴", 
                                         cat == "正規雇用" ~ "雇用形態", 
                                         cat == "非正規雇用" ~ "雇用形態", 
                                         cat == "自発" ~ "離職理由", 
                                         cat == "非自発・家族・健康" ~ "離職理由", 
                                         cat == "定年" ~ "離職理由"
  ))

# 各カテゴリの可視化
Mobvis |> 
  dplyr::rename("カテゴリ" = cat) |> 
  dplyr::mutate(skillname = dplyr::recode(skillname, 
                                          delta_anal = "分析", 
                                          delta_engineer = "科学技術", 
                                          delta_nurture = "ケア"), 
                skillname = forcats::fct_relevel(skillname, c("ケア", "科学技術", "分析")),
                カテゴリ = fct_rev(カテゴリ)) |> 
  ggpubr::ggdotchart(x = "カテゴリ", y = "skillscore_mean",
                     add = "segments",
                     sorting = "none",
                     rotate = TRUE, 
                     add.params = list(color = "lightgray", size = 2),
                     dot.size = 4, 
                     label = round(Mobvis$skillscore_mean, 2),
                     font.label = list(color = "black", 
                                       size = 12,
                                       hjust = 1.25, 
                                       vjust = 0.5),
                     label.rectangle = TRUE) + 
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  ggpubr::theme_pubr(base_size = 16, base_family = "SourceHanSans-Regular") + 
  ggplot2::facet_wrap(~ skillname) + 
  ggplot2::labs(x = " ", 
       y = "スキルスコア") + 
  ggplot2::scale_y_continuous(limits = c(-6, 2)) + 
  ggplot2::ggsave("out/SkillTrasnsfer_Older.png", width = 12, height = 7.62)
