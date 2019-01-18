# model <- keras_model_sequential()
# model %>%
#   layer_flatten(input_shape = c(28, 28)) %>%
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dense(units = 10, activation = 'softmax')
# # accuracy = 0.897 

model <- keras_model_sequential()
model %>% 
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
# 0.8846

# vgg 16
train_vgg <- array_reshape(train_images, dim = c(60000, 28, 28, 1))
test_vgg <- array_reshape(test_images, dim = c(10000, 28, 28, 1))
# create the base pre-trained model
input_tensor <- layer_input(shape = c(28, 28, 1))
base_model <- application_vgg16(input_tensor = input_tensor, 
                                weights = NULL, 
                                include_top = FALSE)

# add our custom layers
predictions <- base_model$output %>% 
  layer_dense(units = 10, activation = 'softmax')

# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)



model <- application_vgg16(input_tensor = input_tensor, 
                           weights = NULL, 
                           include_top = FALSE)

model %>% compile(
  optimizer = optimizer_adam(), 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

model %>% fit(train_images, train_labels, epochs = 10)
analyse_result(test_images, test_labels, model)