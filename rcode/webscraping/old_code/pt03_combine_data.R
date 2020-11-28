cl_df <- readRDS(paste0(data_path,"cl_df.rds"))
mm_df <- readRDS(paste0(data_path,"mm_df.rds"))

# combining data
text_df <- rbind(cl_df, mm_df)

write_csv(text_df, paste0(data_path, "final_df.csv"))
