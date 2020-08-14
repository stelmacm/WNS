filter.description<-function(dat){
  
  rD <- rsDriver(port = 4445L, browser = 'firefox')
  remDr <- rD$client
  
  # set timeout
  remDr$setTimeout(type = 'page load', milliseconds = 20000)
  
  # navigate to geocaching.com
  # going directly to the user sign in page
  remDr$navigate(url = "https://www.geocaching.com/account/signin?returnUrl=%2fplay")
  
  # send username
  username <- remDr$findElement(using = "id", value = "UsernameOrEmail")
  username$sendKeysToElement(list("a_forsythe"))
  
  # send password and click Enter
  passwd <- remDr$findElement(using = "id", value = "Password")
  passwd$sendKeysToElement(list("v3X@s45aqePN"))
  
  SignOn <- remDr$findElement(using = "id", "SignIn")
  SignOn$clickElement()
  
  #####
  keywords <- c("flashlight","cave","mine","bat","ore","mineral")
  full_set<-NULL
  
  for (i in as.character(dat$url)) {
    # navigate to page i
    remDr$navigate(url = i)
    
    # wait for page to finish loading. Is this necessary?
    Sys.sleep(0.5)
    
    page_source<-remDr$getPageSource()
    
    description<-xml2::read_html(page_source[[1]]) %>% html_nodes(".UserSuppliedContent") %>%
      html_text()
    
    column_matches<-NULL
    for (j in keywords) {
      if(length(grep(paste(" ",j,sep=""),description,ignore.case = TRUE,)>0)){
        match<-grep(paste(" ",j,sep=""),description,ignore.case = TRUE)
      } else {
        match <- 0
      }
      column_matches<-cbind(column_matches,match)
    }
    full_set<-rbind(full_set,cbind(column_matches,i))
    colnames(full_set) <- c(keywords,"url")
  }
  
  #
  long_full_set <- full_set %>% as.data.frame(full_set) %>%
    pivot_longer(cols = -url,names_to = "keyword") %>% mutate(value=as.integer(levels(value))[value]) %>% mutate(presence = ifelse(value > 0 ,1,0))
  
  wide_full_set<-long_full_set %>% 
    pivot_wider(names_from = keyword,values_from = presence,id_cols = url,values_fn = list(presence = min)) %>% 
    # mutate_all(as.integer()) %>%
    as.data.frame()
  
  png("figures/keyword_combinations.png",res = 300,height = 800,width = 1000,units = "px")
  UpSetR::upset(wide_full_set,nintersects = NA,nsets = length(keywords),order.by = "freq")
  dev.off()

##### Filtering
filtered_set<-subset(wide_full_set,  wide_full_set$flashlight > 0 & wide_full_set$cave > 0 | 
                       wide_full_set$ore > 0 & wide_full_set$mine > 0 | 
                       wide_full_set$mineral > 0 & wide_full_set$mine > 0) 

filtered_dat<-na.omit(dat[match(filtered_set$url,dat$url),])

write.csv(filtered_dat,"data/gc-list-filtered.csv")
}