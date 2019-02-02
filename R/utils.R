##-- Caixas ----
box_blog <- function(cor, height = NULL, width = NULL, ...){
  
  if(is.null(height)){
    height <- '200px'
  }
  
  if(is.null(width)){
    div(class = "box_blog", 
        style = paste('border:1px solid', cor, '; 
                 background-color: ', cor, ';
                 height: ', height, ';
                 color: #FFFFFF'),
        ...)
    
  } else{
    div(class = "box_blog", 
        style = paste('border:1px solid', cor, '; 
                 background-color: ', cor, ';
                 height: ', height, ';
                 width: ', width, ';
                 color: #FFFFFF'),
        ...)
  }
}

tab_blog <- function(texto, numero, cor, icon, id, width = "100%"){
  HTML(paste0('<div class = "blog-block" style = "background-color:', cor, '; width:', width,';"> 
               <span class = "name">', texto, '</span>
               <h1><center>', numero, '<center></h1>
               <div class="img_block">
                  <div class="img_block_conteiner">
                     <img src="img/',icon,'">
                  </div>
               </div>
               </div>'))
}

app_title <- function(text, html_size, link){
  html_text <- sprintf("<a id='%s' href='#' class='action-button'><div><center><%s>%s</%s></center></div></a>", 
                       link, html_size, text, html_size)
  HTML(html_text)
}

app_img <- function(src, link, width){
  html_text <- sprintf("<a id='%s' href='#' class='action-button'><img class='img_responsive' src='%s' width='%s'/></a>",
                       link, src, width)
  HTML(html_text)
}

label_blog <- function(text, size, color = "white", center = T){
  if(center){
    text <- sprintf("<center><%s style = 'color:%s'><label class='control-label'>%s</label></%s></center>", size, color, text, size)
  } else{
    text <- sprintf("<%s style = 'color:%s'><label class='control-label'>%s</label></%s>", size, color, text, size)  
  }
  
  HTML(text)
}

