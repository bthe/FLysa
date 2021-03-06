library(FLCore)
library(tidyverse)
library(FLAssess)
library(FLXSA)
library(FLa4a)
library(FLSAM)

dat <- 
  paste0("http://data.hafro.is/assmt/2017/haddock/",
         c('catage.csv','catch_weights.csv',
           'maturity.csv',
           'smb.csv','smh.csv','wstock.csv')) %>% 
  set_names(.,c('catch.n','catch.wt',
                'mat',
                'smb.csv','smh.csv',
                'wstock.csv') %>% 
              gsub('.csv','',.)) %>% 
  map(read_csv) %>% 
  bind_rows(.id='slot') %>% 
  mutate(units = '1000') %>% 
  select_all(tolower) %>% 
  gather(age,data,-c(year,slot,units)) %>% 
  mutate(age = gsub('\\+','',age) %>% as.numeric()) %>% 
  group_by(year,slot,units,age) %>% 
  summarise(data=sum(data, na.rm = TRUE)) %>% 
  bind_rows(read_csv('http://data.hafro.is/assmt/2017/haddock/landings.csv') %>% 
              select_all(tolower) %>% 
              select(year,data=total) %>% 
              mutate(slot = 'landings',
                     units = 'ton'))
  


had <- 
  FLStock(name = 'Haddock in 5a',

          catch.n = as.FLQuant(dat %>% 
                                 filter(slot == 'catch.n',age<11) %>% 
                                 ungroup() %>% 
                                 select(year,age,data) %>% 
                                 mutate(data = ifelse(data == 0, 0.0001,data)),
                               quant = 'age',
                               unit = '1e6'),
          catch.wt = as.FLQuant(dat %>% 
                                  filter(slot == 'catch.wt',year < 2017,age<11) %>% 
                                  ungroup() %>% 
                                  select(year,age,data)%>% 
                                  mutate(data = ifelse(data == 0, 0.0001,data)),
                                quant = 'age',
                                unit = 'g'),
          mat = as.FLQuant(dat %>% 
                             filter(slot == 'mat',year < 2017,age<11) %>% 
                             ungroup() %>% 
                             select(year,age,data)%>% 
                             mutate(data = ifelse(data == 0, 0.0001,data)),
                           quant = 'age',
                           unit = 'm'),
          m = FLQuant(matrix(c(rep(0.2,11)),#0.3,0.4)), 
                             nrow=11,#13, 
                             ncol=2016-1978, 
                             dimnames = list(age = 0:10,year=1979:2016)),
                      quant = 'age'),
          m.spwn = FLQuant(matrix(0,#0.3,0.4)), 
                             nrow=11,#13, 
                             ncol=2016-1978, 
                             dimnames = list(age = 0:10,year=1979:2016)),
                           quant = 'age',
                           unit = 'm'),
          harvest.spwn = FLQuant(matrix(0,#0.3,0.4)), 
                                        nrow=11,#13, 
                                        ncol=2016-1978, 
                                        dimnames = list(age = 0:10,year=1979:2016)),
                                 quant = 'age',
                                 unit = 'f'),
          stock.wt = as.FLQuant(dat %>% 
                                  filter(slot == 'wstock',year < 2017,age<11) %>% 
                                  ungroup() %>% 
                                  select(year,age,data),
                                quant = 'age',
                                unit = 'g'),
          discards.n = FLQuant(matrix(0,#0.3,0.4)), 
                                    nrow=11,#13, 
                                    ncol=2016-1978, 
                                    dimnames = list(age = 0:10,year=1979:2016)),
                               quant = 'age',
                               unit = 'g'),
          discards = FLQuant(matrix(0,#0.3,0.4)), 
                                    nrow=1,#13, 
                                    ncol=2016-1978, 
                                    dimnames = list(age = 0,year=1979:2016)),
                             quant = 'age',
                             unit = 'g'),
          discards.wt = as.FLQuant(dat %>% 
                                     filter(slot == 'catch.wt',year < 2017,age<11) %>% 
                                     ungroup() %>% 
                                     select(year,age,data),
                                   quant = 'age',
                                   unit = 'g'))
landings(had) <- computeCatch(had)

smb <- FLIndex(type = 'number',
               index = as.FLQuant(dat %>% 
                                    filter(slot == 'smb',year < 2017,age<11) %>% 
                                    ungroup() %>% 
                                    select(year,age,data)%>% 
                                    mutate(data = ifelse(data == 0, 0.0001,data))),
               catch.n = as.FLQuant(dat %>% 
                                      filter(slot == 'smb',year < 2017,age<11) %>% 
                                      ungroup() %>% 
                                      select(year,age,data)%>% 
                                      mutate(data = ifelse(data == 0, 0.0001,data))),
               effort = as.FLQuant(data_frame(year=1985:2016,data=1),quant = 'age'))

smh <- FLIndex(type = 'number',
               index = as.FLQuant(dat %>% 
                                    filter(slot == 'smh',year < 2017,age<11) %>% 
                                    ungroup() %>% 
                                    select(year,age,data)%>% 
                                    mutate(data = ifelse(data == 0, 0.0001,data))),
               catch.n = as.FLQuant(dat %>% 
                                      filter(slot == 'smh',year < 2017,age<11) %>% 
                                      ungroup() %>% 
                                      select(year,age,data)%>% 
                                      mutate(data = ifelse(data == 0, 0.0001,data))),
               effort = as.FLQuant(data_frame(year=1996:2016,data=1),quant = 'age'))


range(smb)[["startf"]] <- 0.66
range(smb)[["endf"]] <- 0.75
range(smh)[["startf"]] <- 0.66
range(smh)[["endf"]] <- 0.75


harvest(had)[ac(range(had)["max"]), ]     <- 1
harvest(had)[, ac(range(had)["maxyear"])] <- 1
had.vpa <- VPA(had, fit.plusgroup = F)
had.new <- had + had.vpa
landings(had.new) <- computeCatch(had.new)

fit.smb <- sca(had,FLIndices(smb))
fit.smh <- sca(had,FLIndices(smh))
fit.smx <- sca(had,FLIndices(smb,smh))

plot(FLStocks(smb=had+fit.smb,smh=had+fit.smh, smx = had + fit.smx,vpa=had.new))


## SAM
ctrl <- FLSAM.control(had,FLIndices(smb))
sam <- FLSAM(had,FLIndices(smh),ctrl)

plot(FLStocks(smb=had+fit.smb,smh=had+fit.smh, smx = had + fit.smx,vpa=had.new,sam=had+sam))
