extractor = function(tmp){
  summary(tmp) -> x
  data.frame(pred = names(x$b[,1]), 
             estimate = x$b[,1], se = x$se, 
             tval = x$zval, pval = x$pval, row.names = 1:length(x$b[,1])) %>% 
    filter(!startsWith(pred, "disorder"))
  
}