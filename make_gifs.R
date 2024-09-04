############################ Cria um gif
library(imagefx) ### carregar a imagem
library(beepr)
library(magick) ### manipular a imagem
# Specify the directory containing your images
dir_out <- "D:/Users/JV/Desktop/Pbio Counting/paper_rois/dtm"

# List file names and read in
imgs <- list.files(dir_out, full.names = F)

# Split each string at the underscore and extract the first element (the numeral before the underscore)
split_list <- strsplit(imgs, "_")
first_elements <- sapply(split_list, function(x) as.numeric(x[1]))

# Sort the list based on the first elements
sorted_list <- imgs[order(first_elements)]

ordered_imgs <- paste(dir_out,"/", imgs, sep = "")

# Read the images in the ordered list
img_list <- lapply(ordered_imgs, image_read)

# Join the images together
img_joined <- image_join(img_list)

# Animate at x frames per second (you can adjust this as needed)
img_animated <- image_animate(img_joined, fps = 5)

# Save to disk
image_write(image = img_animated, path = "./novogifffff.gif")
beep(1)
        