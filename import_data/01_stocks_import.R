
library(here); library(tidyverse); library(lubridate); library(RPostgres); library(fredr)
path <- here::here(); print(path); setwd(path)

fredr_key <- read_file("input/fred_key.txt"); fredr_set_key(fredr_key)
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

## compustat annual ----
# CSHO, CSHFD, CSHPRI, EPSFI, EPSFX, EPSPI, EPSPX,
res <- dbSendQuery(wrds, " select GVKEY, CUSIP, DATADATE, FYR, FYEAR, SICH, NAICSH,
                            AT, LT, SEQ, CEQ, PSTKL, PSTKRV, PSTK, TXDITC, TXDB, ITCB,
                            REVT, COGS, XINT, XSGA, IB, TXDI, DVC, ACT, CHE, LCT,
                            DLC, TXP, DP, PPEGT, INVT,
                            CONM, TIC, DLTT, RE, EBIT, EBITDA, NI, OPEPS, CAPX, CDVC, DV, DVT, OANCF
                        from COMP.FUNDA
                        where INDFMT='INDL' and DATAFMT='STD' and CONSOL='C' and POPSRC='D' 
                        and DATADATE between '01/01/1950' and '12/31/2019' ")

compustat_funda <- dbFetch(res, n = n_pull); dbClearResult(res)

## compustat quarterly ----
#CSHOQ, CSHFDQ, CSHPRQ, EPSFIQ, EPSFXQ, EPSPIQ, EPSPXQ,
res <- dbSendQuery(wrds," select GVKEY, CUSIP, DATADATE, FYR, FYEARQ,
                            ATQ, LTQ, SEQQ, CEQQ, PSTKRQ, PSTKQ, TXDITCQ, TXDBQ,
                            REVTQ, COGSQ, XINTQ, XSGAQ, IBQ, TXDIQ, ACTQ, CHEQ, LCTQ,
                            DLCQ, TXPQ, DPQ, PPEGTQ, INVTQ, EPSPXQ, RDQ, 
                            CONM, TIC, CSHPRQ, DLTTQ, NIQ, OIADPQ, OIBDPQ, OPEPSQ, REQ, CAPXY, CDVCY, DVY, OANCFY
                        from COMPM.FUNDQ
                        where INDFMT='INDL' and DATAFMT='STD' and CONSOL='C' and POPSRC='D'
                        and DATADATE between '01/01/1950' and '12/31/2019' ")

compustat_fundq <- dbFetch(res, n = n_pull); dbClearResult(res)

## compustat/CRSP link table (CCM) ----

res <- dbSendQuery(wrds," select GVKEY, Lpermno as permno, LINKDT, LINKENDDT, LINKTYPE, LINKPRIM
                    from crsp.ccmxpf_lnkhist") # where substr(linktype,1,1)='L' and (linkprim ='C' or linkprim='P') ") 

compustat_ccmlink <- dbFetch(res, n = n_pull); dbClearResult(res)

## CRSP: MSF, MSE, MSEDELIST, MSI ----

res <- dbSendQuery(wrds, "select a.date, a.permno, a.permco, a.cfacpr, a.cfacshr, 
                   a.shrout, a.prc, a.ret, a.retx, a.vol, a.cusip, a.askhi, a.bidlo, 
                   b.shrcd, b.exchcd, b.comnam, b.hexcd, b.siccd, b.naics, b.primexch
                   from crsp.msf as a left join crsp.msenames as b on a.permno=b.permno 
                   and b.namedt<=a.date
                   and a.date<=b.nameendt
                   where a.date between '01/01/1950' and '12/31/2019'
                   and b.shrcd between 10 and 11")

#and b.exchcd between 1 and 3
crsp_msf <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select permno, dlstdt, dlret, dlstcd, dlretx, dlprc
                   from crsp.msedelist 
                   where dlret is not null
                   and dlstdt between '01/01/1950' and '12/31/2019' ")

crsp_msedelist <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select date, vwretd, vwretx, ewretx, ewretd, sprtrn
                   from crsp.msi")

crsp_msi <- dbFetch(res, n = n_pull); dbClearResult(res)

## SP500 ----

res <- dbSendQuery(wrds, "select * from crsp.dsp500list")

sp500 <- dbFetch(res, n = n_pull); dbClearResult(res)

## IBES identifiers and CRSP/CCM names

res <- dbSendQuery(wrds, "select ticker, cusip, cname, sdates, oftic
                          from ibes.idsum
                          where usfirm = 1
                          order by ticker, cusip, sdates")

ibes_idsum <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select ticker, permno, ncusip as cusip, comnam, namedt, nameenddt
                          from crsp.stocknames
                          order by permno, ncusip, namedt")

crsp_names <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select gvkey, iid, ibtic, cusip
                          from comp.security")

compustat_names <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds,"select gvkey, liid as iid, lpermno as permno
                         from crsp.ccmxpf_linktable
                         where linktype = 'LC' or linktype = 'LU'
                         and linkprim = 'C' or linkprim = 'P'")

ccm_names <- dbFetch(res, n = n_pull) ; dbClearResult(res)

## IBES data (detail EPS US file) (FPI=1 one year, FPI = 6 one quarter)
# ibes.det_epsus (adjusted); ibes.detu_epsus (unadjusted)
res <- dbSendQuery(wrds, "select anndats, anndats_act, fpedats, fpi, analys, cusip, ticker, oftic, cname, actual, value
                          from ibes.det_epsus
                          where usfirm = 1
                          and (FPI = '1' or FPI = '2' or FPI = '6' or FPI = '7')
                          order by ticker, cusip")

ibes_eps <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select ticker, cusip, adj, cname, oftic, spdates
                          from ibes.adj
                          where usfirm = 1
                          order by ticker, cusip")

ibes_adj <- dbFetch(res, n = n_pull); dbClearResult(res)

## FRED ----

deflator <- fredr(series_id = "PCEPI")  %>% 
  transmute(date = date, deflator = value/100) %>% 
  mutate(year = year(date), month = month(date)) %>% 
  arrange(date)
  
gs10     <- fredr(series_id = "GS10")   %>% transmute(date = date, gs10     = value)
gs1      <- fredr(series_id = "GS1")    %>% transmute(date = date, gs1      = value)
unrate   <- fredr(series_id = "UNRATE") %>% transmute(date = date, unrate   = value)
cons     <- fredr(series_id = "PCE")    %>% transmute(date = date, cons     = value)
income   <- fredr(series_id = "PI")     %>% transmute(date = date, income   = value)
  
df_fred <- gs1 %>% 
  left_join(gs10,   by = "date") %>% 
  left_join(unrate, by = "date") %>%
  left_join(cons,   by = "date") %>% 
  left_join(income, by = "date") %>% 
  mutate(year  = year(date), 
         month = month(date), 
         datefrac = year + month / 12, 
         term_spread = gs10 - gs1) %>%
  select(date, year, month, datefrac, everything())

## Merging crsp_dsi, crsp_msf and delist ----

crsp_msedelist <- crsp_msedelist %>% 
  mutate(year  = year(dlstdt), 
         month = month(dlstdt)) %>% 
  arrange(permno, year, month)

crsp_m <- crsp_msf %>% 
  left_join(crsp_msi, by = "date") %>% 
  mutate(year  = year(date),   
         month = month(date)) %>% 
  full_join(crsp_msedelist, by = c("permno", "year", "month")) %>%
  arrange(permno, year, month) %>% 
  select(permno, year, month, everything())

## Saving ----

save(compustat_funda, compustat_fundq, compustat_ccmlink, file = "output/raw_compustat.RData")
save(crsp_m,                                              file = "output/raw_crsp.RData")
save(ibes_eps, ibes_adj,                                  file = "output/raw_ibes.RData")
save(ibes_idsum, crsp_names, compustat_names, ccm_names,  file = "output/raw_ibes_link.RData")
save(sp500,                                               file = "output/raw_sp500.RData")
save(deflator, df_fred,                                   file = "output/raw_fred.RData")
