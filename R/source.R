root_path <- "March-Madness-2021/"

s_path <- paste(root_path, "R/0_scrape.R", sep = "")
source(s_path)

lc_path <- paste(root_path, "R/1_load_clean.R", sep = "")
source(lc_path)

m_path <- paste(root_path, "R/2_manipulate.R", sep = "")
source(m_path)

pp_path <- paste(root_path, "R/3_prepare_predict.R", sep = "")
source(pp_path)

pt_path <- paste(root_path, "R/4_prepare_train.R", sep = "")
source(pt_path)


t_path <- paste(root_path, "R/5_train.R", sep = "")
source(t_path)

b_path <- paste(root_path, "R/6_brackets.R", sep = "")
source(b_path)
