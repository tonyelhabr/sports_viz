
# # setup ----
# library(tidyverse)
# token <- xengagement::get_twitter_token()
# tweets <- rtweet::get_timeline(user = 'utdarena', n = 3200)
# 
# tweets %>% 
#   filter(!is_retweet) %>% 
#   filter(!is_quote) %>% 
#   filter(!is.na(media_url)) %>% 
#   arrange(desc(favorite_count)) %>% 
#   mutate(url = sprintf('https://twitter.com/%s/status/%s', screen_name, status_id), text_trim = str_sub(text, 1, 40)) %>% 
#   select(created_at, favorite_count, retweet_count, url, text_trim)

# # stylex ----
# # https://github.com/jonthegeek/stylex/blob/main/tests/testthat/test-utils.R
# library(torch)
# library(stylex)
# content_data <- 1:5
# generated_data <- content_data
# generated_data[[1]] <- 2L
# content_layer <- torch::torch_tensor(
#   data = content_data,
#   dtype = torch::torch_float()
# )
# generated_layer <- torch::torch_tensor(
#   data = generated_data,
#   dtype = torch::torch_float()
# )
# content_loss <- stylex:::.calculate_content_loss(content_layer, generated_layer)
# as.numeric(content_loss)
# mean((content_data - generated_data)^2)

# blog ----
# https://blog.curso-r.com/posts/2021-02-22-neural-style-transfer/#ref-gatys2015neural
library(torch)
library(torchvision)
library(zeallot)
device <- torch_device(if(cuda_is_available()) "cuda" else "cpu")
cpu <- torch_device("cpu")
cpu

# VGG19 model
vgg <- model_vgg19(pretrained = TRUE)$features$to(device = device)

# congelando os pesos
vgg$parameters %>% purrr::walk(function(param) param$requires_grad_(FALSE))

content_loss <- function(content_layer, generated_layer) {
  nnf_mse_loss(content_layer, generated_layer)
}

gram_matrix <- function(tensor) {
  c(b,c,h,w) %<-% tensor$size()
  tensor <- tensor$view(c(c, h*w))
  torch_matmul(tensor, tensor$t())/(h*w)
}

style_loss <- function(style_layer, generated_layer) {
  style_gram <- gram_matrix(style_layer)
  generated_gram <- gram_matrix(generated_layer)
  nnf_mse_loss(style_gram, generated_gram)
}

features <- nn_module(
  initialize = function(convnet) {
    # poderia ser qualquer convnet pré-treinada. Iremos usar a VGG19
    self$convnet <- convnet
  },
  forward = function(img, layers_out = NULL) {
    layers <- seq_along(self$convnet) # 1 a 37
    if(is.null(layers_out)) layers_out <- layers
    conv_outs <- purrr::accumulate(layers, ~self$convnet[[.y]](.x), .init = img) # lista de 37 tensores
    conv_outs[layers_out] # lista apenas com as layers selecionadas
  }
)

to_r <- function(x) as.numeric(x$to(device = cpu))

plot_image <- function(tensor) {
  im <- tensor$to(device = "cpu")[1,..]$
    permute(c(2, 3, 1))$
    to(device = "cpu")$
    clamp(0,1) %>% # make it [0,1]
    as.array()
  par(mar = c(0,0,0,0))
  plot(as.raster(im))
}

load_image <- function(path, geometry = "250x200") {
  img <- path %>%
    magick_loader() %>%
    magick::image_resize(geometry) %>%
    transform_to_tensor() %>%
    torch_unsqueeze(1)
  
  img$to(device = device)
}

# INPUT: content and style images
# content <- load_image("https://blog.curso-r.com/images/posts/conteudo/2021-02-22-neural-style-transfer/cristoredentor3.jpg", "400x400")
content <- load_image('https://pbs.twimg.com/media/EzqxtIRWQAID4sg?format=jpg&name=large', '400x400')
# plot_image(content)
# https://twitter.com/utdarena/status/1385615226893123585
style <- load_image("https://blog.curso-r.com/images/posts/conteudo/2021-02-22-neural-style-transfer/vangogh_starry_night.jpg", "350x500")

# style and content feature setup
content_layer <- 14
style_layers <- c(2, 7, 12, 21, 29)
lambdas <- 1e5/(c(1,10,10,10,10)^2)
content_weight <- 2
style_weight <- 4e-1

# FEATURES: extraídas da VGG19
vgg_features <- features(vgg)
content_features <- vgg_features(content, content_layer)
style_features <- vgg_features(style, style_layers)

# OUTPUT: generated image
generated <- torch_clone(content)$requires_grad_(TRUE)
optim <- optim_adam(generated, lr = 0.02)
lr_scheduler <- lr_step(optim, 100, 0.9)

# loop de otimização
for(step in seq_len(6000)) {
  optim$zero_grad()
  # atualiza as features da imagem que está sendo gerada
  generated_features <- vgg_features(generated, c(content_layer, style_layers))
  
  # losses
  LC <- content_loss(content_features[[1]], generated_features[[1]])
  LS <- 0
  for(i in seq_along(lambdas)) 
    LS <- LS + lambdas[i]*style_loss(style_features[[i]], generated_features[-1][[i]])  
  
  loss <- content_weight * LC + style_weight * LS
  
  loss$backward()
  optim$step()
  lr_scheduler$step()
  
  # feedback
  if(step %% 100 == 0) {
    cat(glue::glue("LC = {to_r(LC)} - LS = {to_r(LS)} - Loss = {to_r(loss)}\n\n"))
    plot_image(generated)
  }
}

plot_image(generated)
