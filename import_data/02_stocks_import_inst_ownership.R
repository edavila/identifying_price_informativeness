
library(here); library(tidyverse); library(lubridate); library(RPostgres); library(fredr)
path <- here::here(); print(path); setwd(path)

wrds_user <- read_file("input/wrds_user.txt")
wrds_pass <- read_file("input/wrds_pass.txt")

wrds <- dbConnect(Postgres(),
                  host     = "wrds-pgdata.wharton.upenn.edu",
                  port     = 9737,
                  user     = wrds_user,
                  password = wrds_pass,
                  dbname   = "wrds",
                  sslmode  = "require")

n_pull <- -1 # n =-1 retrieves all records

## CRSP ----

res <- dbSendQuery(wrds, "select a.permno, a.date, a.ret, a.vol, a.shrout, a.prc, a.cfacpr, a.cfacshr
                      from crsp.msf as a
                      left join crsp.msenames as b
                      on a.permno=b.permno
                      and b.namedt<=a.date
                      and a.date<=b.nameendt
                      where a.date between '03/01/1980' and '12/31/2017'
                      and b.shrcd between 10 and 11"
)

crsp <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select distinct permno, ncusip
                    from crsp.msenames
                    where ncusip != '' ")

crsp2 <- dbFetch(res, n = n_pull); dbClearResult(res)

## Ownership data ----

res = dbSendQuery(wrds, "select rdate, fdate, mgrno, mgrname
                      from tfn.s34type1")

fst_vint <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select fdate, mgrno, cusip, shares
                      from s34type3")

s34type3 <- dbFetch(res, n = n_pull); dbClearResult(res)

save(crsp, fst_vint, s34type3, crsp2, file = "output/raw_inst_ownership.RData")
