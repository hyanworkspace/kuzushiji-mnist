# Load the K-MNIST dataset into R 
# https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706
library(tidyverse)
library(keras)
dir_data <- './data/'

# download.file('http://codh.rois.ac.jp/kmnist/dataset/kmnist/train-images-idx3-ubyte.gz',
#               str_c(dir_data, 'train-images-idx3-ubyte.gz'))
# download.file('http://codh.rois.ac.jp/kmnist/dataset/kmnist/train-labels-idx1-ubyte.gz',
#               str_c(dir_data, 'train-labels-idx1-ubyte.gz'))
# download.file('http://codh.rois.ac.jp/kmnist/dataset/kmnist/t10k-images-idx3-ubyte.gz',
#               str_c(dir_data, 't10k-images-idx3-ubyte.gz'))
# download.file('http://codh.rois.ac.jp/kmnist/dataset/kmnist/t10k-labels-idx1-ubyte.gz',
#               str_c(dir_data, 't10k-labels-idx1-ubyte.gz'))
# 
# files_to_decomp <- list.files(dir_data) %>%
#   .[str_detect(., pattern = 'ubyte.gz')]
# 
# for (i in files_to_decomp) {
#   gunzip(str_c(dir_data, i))
# }

# helper function for visualization
show_digit <- function(image_data) {
  image <- as.data.frame(image_data)
  colnames(image) <- seq_len(ncol(image))
  image$y <- seq_len(nrow(image))
  image <- gather(image, "x", "value", -y)
  image$x <- as.integer(image$x)
  ggplot(image, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")
}

# load image files
load_image_file <- function(filename) {
  ret <- list()
  f <- file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol <- readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x <- readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  matrix(x, ncol = nrow * ncol, byrow = T)
}

# load label files
load_label_file <- function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# load images
train_images <- load_image_file(str_c(dir_data, 'train-images-idx3-ubyte'))
train_images <- array_reshape(train_images, dim = c(60000, 28, 28))
test_images <- load_image_file(str_c(dir_data, 't10k-images-idx3-ubyte'))
test_images <- array_reshape(test_images, dim = c(10000, 28, 28))

# load labels
train_labels <- load_label_file(str_c(dir_data, 'train-labels-idx1-ubyte'))
test_labels  <- load_label_file(str_c(dir_data, 't10k-labels-idx1-ubyte'))
train_labels <- to_categorical(train_labels, 10)
test_labels <- to_categorical(test_labels, 10)

class_names <- read.csv('./data/kmnist_classmap.csv', 
                        header = T, sep = ",", stringsAsFactors = F)$index

# view some images
show_digit(train_images[10001, ,])

# preprocessing
train_images <- train_images / 255
test_images <- test_images / 255

# ------ visualization -------
# pdf('./images/all_class.pdf')
# par(mfcol=c(5,5))
# par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
# for (i in 1:25) {
#   img <- train_images[i, , ]
#   img <- t(apply(img, 2, rev)) 
#   image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n')
# }
# dev.off()