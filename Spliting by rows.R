# Create a data frame (replace this with your actual data frame)
data <- data.frame(col1 = 1:10, col2 = 11:20)

# Calculate the number of rows for training
num_rows_train <- floor(0.8 * nrow(data))

# Split the data into train and test
train_data <- data[1:num_rows_train, ]
test_data <- data[(num_rows_train + 1):nrow(data), ]

print("Train Data:")
print(train_data)

print("Test Data:")
print(test_data)
