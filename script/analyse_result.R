analyse_result <- function(test_images, test_labels, predictions) {
  score <- model %>% evaluate(test_images, test_labels)
  
  cat('Test loss:', score$loss, "\n")
  cat('Test accuracy:', score$acc, "\n")
  
  predictions <- model %>% predict(test_images)
  
  par(mfcol = c(5,5))
  par(mar = c(0, 0, 1.5, 0), xaxs = 'i', yaxs = 'i')
  for (i in 1:25) { 
    img <- test_images[i, , ]
    img <- t(apply(img, 2, rev)) 
    # subtract 1 as labels go from 0 to 9
    predicted_label <- which.max(predictions[i, ]) - 1
    true_label <- which.max(test_labels[i, ]) - 1
    if (predicted_label == true_label) {
      color <- '#008800' 
    } else {
      color <- '#bb0000'
    }
    image(1:28, 1:28, img, col = gray((0:255)/255), 
          xaxt = 'n', yaxt = 'n',
          main = paste0(class_names[predicted_label + 1], " (",
                        class_names[true_label + 1], ")"),
          col.main = color)
  }
}