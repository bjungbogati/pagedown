pkg_resource = function(...) {
  system.file('resources', ..., package = 'pagedown', mustWork = TRUE)
}

lua_filters = function(...) {
  c(rbind("--lua-filter", pkg_resource('lua', c(...))))
}

list_css = function() {
  css = list.files(pkg_resource('css'), '[.]css$', full.names = TRUE)
  setNames(css, gsub('.css$', '', basename(css)))
}

check_css = function(css) {
  valid = names(list_css())
  if (length(invalid <- setdiff(css, valid)) == 0) return()
  invalid = invalid[1]
  maybe = sort(agrep(invalid, valid, value = TRUE))[1]
  hint = if (is.na(maybe)) '' else paste0('; did you mean "', maybe, '"?')
  stop(
    '"', invalid, '" is not a valid built-in CSS filename', if (hint != "") hint else ".",
    " Use `pagedown:::list_css()` to view all built-in CSS filenames.", call. = FALSE
  )
}

merge_list = function(x, y) {
  x[names(y)] = y
  x
}

to_json = function(x, ..., auto_unbox = TRUE, null = 'null') {
  jsonlite::toJSON(x, ..., auto_unbox = auto_unbox, null = null)
}

`%n%` = knitr:::`%n%`

run_servr = function() {
  # see https://github.com/rstudio/httpuv/issues/250
  later::with_loop(later::global_loop(), httpuv::service(NA))
}



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

