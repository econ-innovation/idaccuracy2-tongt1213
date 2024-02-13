library(readr)
library(stringr)
library(dplyr)

scientist_pub <- read_csv("D://scientist_pub.csv")

files <- list.files(path = "/Users/tongt1213/github/idaccuracy2-tongt1213/Aminer", 
                    pattern = "\\.csv$", full.names = TRUE)

# 定义一个函数来处理每个文件并计算准确率和召回率
process_file <- function(file_path) {
  aminer_data <- read_csv(file_path)
  
  # 提取 uniqueID
  unique_ID <- str_extract(file_path, "0_[0-9]+")
  
  # 筛选scientist_pub中uniqueID对应的数据
  scientist_pub_filtered <- filter(scientist_pub, uniqueID == unique_ID)
  
  # 转换DOI、标题、期刊为大写
  scientist_pub_filtered$doi <- toupper(scientist_pub_filtered$doi)
  scientist_pub_filtered$title <- toupper(scientist_pub_filtered$title)
  scientist_pub_filtered$journal <- toupper(scientist_pub_filtered$journal)
  
  aminer_data$doi <- toupper(aminer_data$doi)
  aminer_data$标题 <- toupper(aminer_data$标题)
  aminer_data$期刊 <- toupper(aminer_data$期刊)
  
  # 筛选匹配的论文数据
  matched_papers <- inner_join(aminer_data, scientist_pub_filtered, 
                               by = c("doi" = "doi", "标题" = "title", "期刊" = "journal", "年份" = 'pub_year'))
  
  # 计算精准度和查全率
  precision <- nrow(matched_papers) / nrow(aminer_data)
  recall <- nrow(matched_papers) / nrow(scientist_pub_filtered)
  
  return(data.frame(file_name = basename(file_path), unique_ID, precision, recall))
}

# 应用函数到每个文件
results <- lapply(files, process_file)

# 合并结果
final_results <- bind_rows(results)

# 保存结果到文件
write_csv(final_results, "/Users/chenjian/github/idaccuracy2-chenjiancqu/accuracy_recall_results.csv")

# 计算整体准确率和召回率
overall_precision <- mean(final_results$precision)
overall_recall <- mean(final_results$recall)

# 打印整体准确率和召回率
print(paste("Overall Precision: ", overall_precision))
print(paste("Overall Recall: ", overall_recall))