# Cài đặt và load thư viện MASS, có chứa bộ dữ liệu Boston cần nghiên cứu
# install.packages("MASS")
library("MASS")
# Tổng quan về bộ dữ liệu Boston: 
?Boston
str(Boston)
# Lệnh pair để vẽ biểu đồ phân tán giữa các biến
Boston_data = Boston
pairs(Boston_data)
pairs(~medv+lstat+age+rm, data= Boston_data)
#Ma trận tương quan giữa các biến medv, lstat, age, rm
cor(Boston_data[,c("medv", "lstat","age","rm")])
# Thực hiện bộ chia dữ liệu thành 2 phần:2/3 quan sát để xây dựng mô hình,
# 1/3 so quan sát để kiểm định sử dụng library caTools
library("caTools")
Boston_data["ID"] = c(1:506)
set.seed(123)
split = sample.split(Boston_data$ID, SplitRatio = 2/3)

mauXayDung = subset(Boston_data, split==TRUE)
mauKiemDinh = subset(Boston_data, split==FALSE)

# Xây dựng mô hình: Sử dụng tất cả các biến trong quá trình dự báo
moHinh = lm(medv~ ., data = mauXayDung)
moHinh
summary(moHinh)
# Ý nghĩa cảu bảng số summary:
#  Cột Estimate: Hệ số beta của mô hình hồi quy

# Cột Std. Error: Đọ lệch chuẩn của ước lượng beta tương ứng

# Cột t-value = Estimate/Std.Error: Là giá trị t trong kiểm định giả thiết
# với H0: beta = 0, H1: beta <> 0, nếu giá trị t có trị tuyệt đối
# lớn hơn giá trị tới hạn, giả thiết H0 bị bác bỏ, thể hiện hệ số beta có ý nghĩa thống kê

# Cột Pr(>|t|): Giá trị p-value trong kiểm định giả thiết H0: beta = 0, H1: beta <>0 

# Dự báo giá trị của medv trong bộ mẫu xây dựng, sử dụng hàm predict
duBao_xayDung = predict(moHinh, mauXayDung)

# Tổng phần dư bình phương (RSS) của bộ mẫu xây dựng
RSS_xayDung = sum((duBao_xayDung - mauXayDung$medv)^2)

# Tổng phần dư bình phương (RSS) của mô hình cơ sở
TSS_xayDung = sum((mean(mauXayDung$medv) - mauXayDung$medv)^2)

# Giá trị R-square(R2) của mô hình trên mẫu xây dựng
R2_xayDung = 1 - (RSS_xayDung/TSS_xayDung)

# Thực hiện tương tự để tính R2 cho mẫu kiểm định
duBao_kiemDinh = predict(moHinh, mauKiemDinh)
RSS_kiemDinh = sum((duBao_kiemDinh - mauKiemDinh$medv)^2)
TSS_kiemDinh = sum((mean(mauXayDung$medv) - mauKiemDinh$medv)^2)
R2_kiemDinh = 1 - (RSS_kiemDinh/TSS_kiemDinh) 

