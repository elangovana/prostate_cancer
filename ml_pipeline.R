ml_pipeline <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                         test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir){ 
  source("./translate_data.R")
  print("cleaning core table for train")
  train_ct <- clean_ct_data (train_ct)
  print("cleaning core table labels for train")
  train_ct <- clean_labels (train_ct)
  
  print("cleaning core table for test")
  test_ct <- clean_ct_data(test_ct)
  
  
  test_lv <- clean_labvalue_data (test_lv)
  train_lv <- clean_labvalue_data (train_lv)
  
  
  source("./alg_random_forest.R")
  predict_timetolive(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                    test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  
  predict_death(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                     test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
}