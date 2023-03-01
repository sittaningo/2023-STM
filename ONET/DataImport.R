# ONETデータの読み込み
onet <- readr::read_csv("Data/onet.csv") 

# 余分な行の削除
onet <- onet |> 
  dplyr::filter(`1` >= 16)

# ONET職業とSSM職業の対応表の読み込み
onet_ssm_occ <- readr::read_csv("/Volumes/HD-ADU3/01_Data/ONET/raw/onet_ssm_occ.csv")
