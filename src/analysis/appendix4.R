# ------------------------------------------------
# Comparison between grouped lasso results. AP 4
# ------------------------------------------------
# File for Appendix 4 creation, 
# Requirements: Run data_analysis_outline.R before. 

# Non-zero coefficients for grouped lasso grouping large and small categories (original grouped version)
gp_lasso_group <- gp_lasso$b[gp_lasso$b !=0 ]
gp_lasso_poi_group <- gp_lasso_poisson$b[gp_lasso_poisson$b !=0 ]

# Non-zero coefficients for grouped lasso grouping small categories 
gp_lasso_group2 <- gp_lasso_v2$b[gp_lasso_v2$b !=0 ]
gp_lasso_poi_group2 <- gp_lasso_poisson_v2$b[gp_lasso_poisson_v2$b !=0 ]

# Non-zero coefficients for grouped lasso grouping by time
gp_lasso_time <- gp_lasso_time$b[ gp_lasso_time$b !=0 ]
gp_lasso_poi_time <- gp_lasso_poisson_timen$b[gp_lasso_poisson_time$b !=0 ]

rm(summary_findings_grouped)
summary_findings_grouped= data.frame()

for (i in names(gp_lasso_time)){

  if (i %in% names(gp_lasso_time)){
    summary_findings_grouped[i, 'gp_lasso_time'] = 1
  }
  if (i %in% names(gp_lasso_poi_time)){
    summary_findings_grouped[i, 'gp_lasso_poi_time'] = 1
    }
  if (i %in% names(gp_lasso_group2)){
    summary_findings_grouped[i, 'gp_lasso_group2'] = 1
    }
  if (i %in% names(gp_lasso_poi_group2)){
    summary_findings_grouped[i, 'gp_lasso_poi_group2'] = 1
    }
  if (i %in% names(gp_lasso_group)){
    summary_findings_grouped[i, 'gp_lasso_group'] = 1
    }
  if (i %in% names(gp_lasso_poi_group)){
    summary_findings_grouped[i, 'gp_lasso_poi_group'] = 1
  }
  }

for (i in names(gp_lasso_poi_time)){

  if (i %in% names(gp_lasso_time)){
    summary_findings_grouped[i, 'gp_lasso_time'] = 1
  }
  if (i %in% names(gp_lasso_poi_time)){
    summary_findings_grouped[i, 'gp_lasso_poi_time'] = 1
  }
  if (i %in% names(gp_lasso_group2)){
    summary_findings_grouped[i, 'gp_lasso_group2'] = 1
  }
  if (i %in% names(gp_lasso_poi_group2)){
    summary_findings_grouped[i, 'gp_lasso_poi_group2'] = 1
  }
  if (i %in% names(gp_lasso_group)){
    summary_findings_grouped[i, 'gp_lasso_group'] = 1
  }
  if (i %in% names(gp_lasso_poi_group)){
    summary_findings_grouped[i, 'gp_lasso_poi_group'] = 1
  }
}

for (i in names(gp_lasso_group2)){

  if (i %in% names(gp_lasso_time)){
    summary_findings_grouped[i, 'gp_lasso_time'] = 1
  }
  if (i %in% names(gp_lasso_poi_time)){
    summary_findings_grouped[i, 'gp_lasso_poi_time'] = 1
  }
  if (i %in% names(gp_lasso_group2)){
    summary_findings_grouped[i, 'gp_lasso_group2'] = 1
  }
  if (i %in% names(gp_lasso_poi_group2)){
    summary_findings_grouped[i, 'gp_lasso_poi_group2'] = 1
  }
  if (i %in% names(gp_lasso_group)){
    summary_findings_grouped[i, 'gp_lasso_group'] = 1
  }
  if (i %in% names(gp_lasso_poi_group)){
    summary_findings_grouped[i, 'gp_lasso_poi_group'] = 1
  }
}

for (i in names(gp_lasso_poi_group2)){

  if (i %in% names(gp_lasso_time)){
    summary_findings_grouped[i, 'gp_lasso_time'] = 1
  }
  if (i %in% names(gp_lasso_poi_time)){
    summary_findings_grouped[i, 'gp_lasso_poi_time'] = 1
  }
  if (i %in% names(gp_lasso_group2)){
    summary_findings_grouped[i, 'gp_lasso_group2'] = 1
  }
  if (i %in% names(gp_lasso_poi_group2)){
    summary_findings_grouped[i, 'gp_lasso_poi_group2'] = 1
  }
  if (i %in% names(gp_lasso_group)){
    summary_findings_grouped[i, 'gp_lasso_group'] = 1
  }
  if (i %in% names(gp_lasso_poi_group)){
    summary_findings_grouped[i, 'gp_lasso_poi_group'] = 1
  }
}

for (i in names(gp_lasso_group)){
  
  if (i %in% names(gp_lasso_time)){
    summary_findings_grouped[i, 'gp_lasso_time'] = 1
  }
  if (i %in% names(gp_lasso_poi_time)){
    summary_findings_grouped[i, 'gp_lasso_poi_time'] = 1
  }
  if (i %in% names(gp_lasso_group2)){
    summary_findings_grouped[i, 'gp_lasso_group2'] = 1
  }
  if (i %in% names(gp_lasso_poi_group2)){
    summary_findings_grouped[i, 'gp_lasso_poi_group2'] = 1
  }
  if (i %in% names(gp_lasso_group)){
    summary_findings_grouped[i, 'gp_lasso_group'] = 1
  }
  if (i %in% names(gp_lasso_poi_group)){
    summary_findings_grouped[i, 'gp_lasso_poi_group'] = 1
  }
}

for (i in names(gp_lasso_poi_group)){
  
  if (i %in% names(gp_lasso_time)){
    summary_findings_grouped[i, 'gp_lasso_time'] = 1
  }
  if (i %in% names(gp_lasso_poi_time)){
    summary_findings_grouped[i, 'gp_lasso_poi_time'] = 1
  }
  if (i %in% names(gp_lasso_group2)){
    summary_findings_grouped[i, 'gp_lasso_group2'] = 1
  }
  if (i %in% names(gp_lasso_poi_group2)){
    summary_findings_grouped[i, 'gp_lasso_poi_group2'] = 1
  }
  if (i %in% names(gp_lasso_group)){
    summary_findings_grouped[i, 'gp_lasso_group'] = 1
  }
  if (i %in% names(gp_lasso_poi_group)){
    summary_findings_grouped[i, 'gp_lasso_poi_group'] = 1
  }
}

summary_findings_grouped[is.na(summary_findings_grouped)] <- 0
summary_findings_grouped <- transform(summary_findings_grouped, sum=rowSums(summary_findings_grouped))

