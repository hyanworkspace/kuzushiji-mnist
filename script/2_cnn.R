# reshape data unit to 3D to use 2D conv
train_cnn <- array_reshape(train_images, dim = c(dim(train_images), 1))
test_cnn <- array_reshape(test_images, dim = c(dim(test_images), 1))

cnn <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                input_shape = c(28, 28, 1)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = 'softmax')

# Compile model
cnn %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train model
cnn %>% fit(
  train_cnn, train_labels, 
  batch_size = 128, 
  epochs = 12,
  validation_data = list(test_cnn, test_labels)
)

analyse_result(test_images, test_labels, cnn)
# 0.9032 