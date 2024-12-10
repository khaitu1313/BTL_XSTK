# Gọi thư viện
#Thư viện ggplot2 hỗ trợ vẽ biểu đồ
library(ggplot2)
#Dùng thư viện reshape2 để làm mềm main_data
library(reshape2)
#Dùng thư viện caret để train data
library(caret)

#đọc dữ liệu
GPU_data <- read.csv("D:/Downloaded/All_GPUs.csv")
head(GPU_data)
#chọn biến
main_data<-GPU_data[c("Memory_Bandwidth","Memory_Speed","L2_Cache","Memory_Bus","Shader","Dedicated","Manufacturer")]
head(main_data)
#lọc giá trị
#đổi giá trị rỗng thành NA
main_data[main_data == ""] <- NA
main_data[] <- lapply(main_data, function(x) gsub("^\\n- $", NA, x))
main_data[main_data == "NA"] <- NA

#thống kê data khuyết

na_summary <- data.frame(
  Column = names(main_data),
  NA_Count = sapply(main_data, function(x) sum(is.na(x))),
  NA_Percentage = sapply(main_data, function(x) mean(is.na(x)) * 100)
)

main_data<-na.omit(main_data)
#vẽ đồ thị thống kê

# Tạo biểu đồ cột cho tỷ lệ dữ liệu khuyết ở các biến
ggplot(na_summary, aes(x = Column, y = NA_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +  #làm đẹp đồ thị
  geom_text(aes(label = paste0(round(NA_Percentage, 1), "%")), 
            hjust = -0.2, size = 3) +  
  labs(title = "Proportion of missing data in variables",
       x = "variables",
       y = "Proportion of missing data (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) + 
  coord_flip()

columns_to_clean <- c("Memory_Bandwidth","Memory_Bus","Memory_Speed")
#Tạo hàm thực hiện xoá đơn vị ở các biến 
remove_units <- function(column) {
  # Sử dụng gsub để xóa tất cả các ký tự không phải số (kể cả đơn vị)
  cleaned_column <- gsub("[^0-9.]", "", column)
  # Chuyển đổi kết quả về kiểu numeric
  cleaned_column <- as.numeric(cleaned_column)
  
  return(cleaned_column)
}

# Áp dụng hàm cho các biến đã chọn
main_data[columns_to_clean] <- lapply(main_data[columns_to_clean], remove_units)
#xử lý ký hiệu của L2 cache
reformat_cache <- function(cache_values) {
  # Remove 'KB' and extract numeric value and multiplier
  extract_values <- function(x) {
    # Remove any whitespace and 'KB'
    x <- gsub("\\s*KB", "", x)
    # Check for multiplier (x2, x3, x4)
    multiplier_match <- regexpr("\\(x[2-4]\\)", x)
    if (multiplier_match != -1) {
      # Extract multiplier
      multiplier <- as.numeric(substr(x, multiplier_match + 2, multiplier_match + 2))
      # Extract base numeric value
      base_value <- as.numeric(substr(x, 1, multiplier_match - 1))
      return(base_value * multiplier)
    } else {
      # If no multiplier, just return the numeric value
      return(as.numeric(x))
    }
  }
  
  # Apply the extraction to the entire vector
  sapply(cache_values, extract_values)
}

main_data["L2_Cache"] <- lapply(main_data["L2_Cache"], reformat_cache)

#Kết thúc Data preprocessing


#Bắt đầu phần Descriptive Statistic - Graphs and Charts:
#Điều chỉnh biến Memory_Bandwidth để biến này tuân theo phân phối chuẩn
main_data$Memory_Bandwidth <- log(main_data$Memory_Bandwidth + 1)

#Chuyển đổi tất cả các biến còn lại sang đồng dạng X = ln(X + 1) giống như biến Y(Memory_Bandwidth)
main_data$Memory_Speed <- log(main_data$Memory_Speed + 1)
main_data$L2_Cache <- log(main_data$L2_Cache + 1)
main_data$Memory_Bus <- log(main_data$Memory_Bus + 1)

#Chuyển đổi trở về data gốc
main_data$Memory_Bandwidth <- exp(main_data$Memory_Bandwidth) - 1;
main_data$Memory_Speed <- exp(main_data$Memory_Speed) - 1;
main_data$L2_Cache <- exp(main_data$L2_Cache) - 1;
main_data$Memory_Bus <- exp(main_data$Memory_Bus) - 1;

#Vẽ histogram cho biến Memory_Bandwidth
ggplot(main_data, aes(x = Memory_Bandwidth)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Memory Bandwidth",
       x = "Memory Bandwidth (GB/s)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Vẽ đồ thị histogram cho các biến khác
ggplot(main_data, aes(x = Memory_Speed)) +
  geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Memory Speed",
       x = "Memory Speed (MHz)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#L2 Cache
ggplot(main_data, aes(x = L2_Cache)) +
  geom_histogram(fill = "lightpink", color = "black", alpha = 0.7) +
  labs(title = "Histogram of L2 Cache Size",
       x = "L2 Cache (KB)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Memory Bus
ggplot(main_data, aes(x = Memory_Bus)) +
  geom_histogram(fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Memory Bus(after using logarit)",
       x = "Memory Bus (Bit)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Vẽ các đồ thị phân tán của các biến theo biến Memory_Bandwidth(tất cả biến tham gia đã được chuyển đổi ln(X + 1))
#Memory Speed - Memory Bandwidth
ggplot(main_data, aes(x = Memory_Speed, y = Memory_Bandwidth)) +
  geom_point(color = "green", alpha = 0.7) +
  labs(title = "Scatter Plot of Memory Speed and Memory Bandwidth",
       x = "Memory Speed (MHz)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#L2 Cache - Memory Bandwidth
ggplot(main_data, aes(x = L2_Cache, y = Memory_Bandwidth)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(title = "Scatter Plot of L2 Cache Size and Memory Bandwidth",
       x = "L2 Cache (KB)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Memory Bus - Memory Bandwidth
ggplot(main_data, aes(x = Memory_Bus, y = Memory_Bandwidth)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatter Plot of Memory Bus and Memory Bandwidth",
       x = "Memory Bus (Bit)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Vẽ biểu đồ Correlation cho các số liệu phân tích trong main data
data <- main_data[c("Memory_Bandwidth","Memory_Speed","L2_Cache","Memory_Bus")]
cor_matrix <- cor(data)
cor_data <- melt(cor_matrix)

#Vẽ biểu đồ Correlation diagram
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limit = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Diagram",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#Vẽ biểu đồ hộps
#Boxplot Memory Speed
ggplot(main_data, aes(x = Memory_Speed, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Boxplot of Memory Bandwidth by Memory Speed",
       x = "Memory Speed(MHz)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot L2 Cache
ggplot(main_data, aes(x = L2_Cache, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Boxplot of Memory Bandwidth by L2 Cache",
       x = "L2 Cache(KB)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot Memory Bus
ggplot(main_data, aes(x = Memory_Bus, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Boxplot of Memory Bandwidth by Memory Bus",
       x = "Memory Bus(Bit)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Kết thúc phần Descriptive Statistic - Graphs and Charts:



set.seed(31112024)  # Đặt seed để tái lập kết quả
train_index <- createDataPartition(main_data$Memory_Bandwidth, p = 0.8, list = FALSE)

# Tập huấn luyện
train_data <- main_data[train_index, ]

# Tập kiểm tra
test_data <- main_data[-train_index, ]

# Kiểm tra kích thước của các tập
dim(train_data)
dim(test_data)

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(Memory_Bandwidth ~ Memory_Speed + L2_Cache + Memory_Bus, data = train_data)

# Xem tóm tắt của mô hình
summary(model)


# Kiểm tra các phần dư
par(mfrow = c(2, 2))  # Chia bố cục đồ thị thành 
plot(model)








