nm_listing <- function(data, title, side_title, side_category = "", cols_n) {
  
  t <- c(t = cols_n) 
  st <- c(st = cols_n)
  sc <- c(sc = cols_n)
  names(t) <- title
  names(st) <- side_title
  names(sc) <- side_category
  page_number <- c('<div class="pageNum"></div>' = cols_n)
  
  if(side_category != "") {
  kableExtra::kbl(data, align="l", escape = F, bootstrap_options = "basic") %>%
  kableExtra::kable_minimal(full_width = T, html_font = "Courier New") %>% 
  kableExtra::add_header_above(header = sc, escape = F, line = F, align = "l") %>% 
  kableExtra::add_header_above(header = page_number, escape = F, line = F, align = "r") %>% 
  kableExtra::add_header_above(header = t, escape = F, line = F) %>% 
  kableExtra::add_header_above(header = st, escape = F, line = F, align = "l") 
    
  }
  
  else{
  kableExtra::kbl(data, align="l", escape = F, bootstrap_options = "basic") %>%
  kableExtra::kable_minimal(full_width = T, html_font = "Courier New") %>% 
  kableExtra::add_header_above(header = page_number, escape = F, line = F, align = "r") %>% 
  kableExtra::add_header_above(header = t, escape = F, line = F) %>% 
  kableExtra::add_header_above(header = st, escape = F, line = F, align = "l") 
  }
}
