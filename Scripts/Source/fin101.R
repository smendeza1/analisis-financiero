#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# fin101.txt                                                            #
#                                                                       #
# Version   : 1.0                                                       #
# Date      : 9/1/2010                                                  #
# Modified  : 4/17/2013                                                  #
# Author    : Yuxing Yan,  yuxing.yan@hofstra.edu                       #
#                                                                       #
# Objective : use R as a financial calculator                           #
#                                                                       #
# How to use this file?                                                 #
#                                                                       #
#  Step 1: download/install R @ http://www.r-project.org                #
#                                                                       #
#  Step 2: launch R by clicking R icon on your desktop                  #
#                                                                       #
#  Step 3: issue the following command                                  #
#       >source("http://people.hofstra.edu/yuxing_yan/fin101.txt")      #
#                                                                       #
#                                                                       #
# Now you are ready to use this internet-connected financial calculator #
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#                                                                       #
#    To view all funcitons                                              #
#          > fin101()        # method 1                                 #
#          > ls()            # method 2                                 #
#                                                                       #
#    To see more detial, just type a function name                      #
#          > pv_f                                                       #
#                                                                       #
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

options(warn=-1)


#--------------------------------------------------------#
#--- List all functions ---------------------------------#
#--------------------------------------------------------#
fin101<-function(){
  print("*----------------------------------------------------------------------------*")
  print("*- basic functions  - Fin Statements -----        -- Ratios------------------*")
  print("*----------------------------------------------------------------------------*")
  print(" pv_f               balance_sheet_annual        current_ratio                 ")
  print(" pv_perpetuity      balance_sheet_quarterly     quick_ratio                   ")  
  print(" pv_perpetuity_due  income_statement_annual     debt_assests_ratio            ")  
  print(" pv_annuity         cash_flow_quarterly         debt_equity_ratio             ")  
  print(" pv_excel           cash_flow_annual            long_term_debt_assests_ratio  ")  
  print(" pv_growing_annuity income_statement_quarterly                                ")  
  print(" fv_f               get_fin_statement                                         ")  
  print(" fv_annuity         view_f                                                    ")  
  print(" fv_excel           save_balance_sheet_annual                                 ")  
  print(" PMT                save_balance_sheet_quarterly                              ")  
  print(" NPER               save_income_statement_annual                              ")  
  print(" n_period           save_income_statement_quarterly                           ")  
  print(" IRR_f              save_fin_statement                                        ")  
  print(" npv_f                                                                        ")  
  print(" npv2_f             -- others--------------------                             ")    
  print(" Rc_f               exchange_rate                                             ")  
  print(" effective_rate     black_scholes_call                                        ")  
  print(" r_continuous       bs_call                                                   ")  
  print(" Rm_f               bs_f                                                      ") 
  print(" duration                                                                     ")  
  print(" modified_duration  high_low_52w                                              ") 
  print(" Beta               fiftytwo_week_high_low                                    ")  
  print(" beta2              yahoo_daily_price                                         ")  
  print(" bond_price         DuPont                                                    ")    
  print("*---------------------------------------------------------------------------*")  
  print(" How to use pv_f? Just type                                                   ")
  print(" >pv_f                                                                        ")
  print("*----------------------------------------------------------------------------*")
}






effective_rate<-function(APR,rateA2rateB){
  "
  Objective: from one APR (Annual percentage rate) to another effective rate and APR
  rateA2rateB: could be 's2a', 's2q', 's2c', etc. 
  a for annual
  s for semi-annual
  q for quarterly
  m for monthly
  d for daily
  c for continuously
  e.g., 
  > effective_rate(0.1,'s2a')
  Two rates
  effective annual rate    0.1025
  APR                      0.1025
  
  > effective_rate(0.1,'s2q')
  Two rates
  effective quarterly rate  0.02469508
  APR                      0.09878031
  
  formula 1: rateB=(1+rateA/n1)^(n2/n1)-1
  formula 2: rateC=n1*ln(1+rateA/n1)
  "
  return(effective_rate_(APR,rateA2rateB))
}





#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
effective_rate_<-function(APR,rateA2rateB){
  r=rateA2rateB
  if(nchar(r)!=3) stop("Wrong input for the 2nd variable")
  
  if(substr(r,1,1)=='a')        {n_in=1
  } else if(substr(r,1,1)=='s') {n_in=2
  } else if(substr(r,1,1)=='q') {n_in=4
  } else if(substr(r,1,1)=='m') {n_in=12
  } else if(substr(r,1,1)=='d') {n_in=365 
  } else if(substr(r,1,1)=='c') {n_in=1 
  }else{
    return(-99)
  }
  
  if(substr(r,3,3)=='a')        {n_out=1;  label='annual'
  } else if(substr(r,3,3)=='s') {n_out=2;  label='semi-annual'
  } else if(substr(r,3,3)=='q') {n_out=4;  label='quarterly'
  } else if(substr(r,3,3)=='m') {n_out=12; label='monthly'
  } else if(substr(r,3,3)=='d') {n_out=365;label='daily'
  } else if(substr(r,3,3)=='c') {n_out=1;  label='continuously'
  }else{
    print('check your 2nd input. It should be s2a, s2q, etc')
    return(-99)
  }
  
  x=matrix(1,2)
  n=n_in/n_out
  
  
  if(substr(r,1,1)=='c') {
    x[1,1]<-exp(APR)^(1/n_out)-1
  } else{
    
    x[1,1]<-(1+APR/n_in)^n-1
  }
  
  if(substr(r,3,3)=='c') {
    x[1,1]<-n_in*log(1+APR/n_in)
  }
  
  x[2,1]<-x[1,1]*n_out
  rownames(x)=c(paste('effective',label,'rate'), 'APR')
  colnames(x)<-"Two rates"
  
  if(substr(r,3,3)=='c') {
    rownames(x)=c(paste('effective',label,'rate'), '[same as above]')
    colnames(x)<-"One rate"
    
  }
  return(x)
}





#----------------------------------------------------------#
#----Mimic FV() function in Excel -------------------------#
#----------------------------------------------------------#
fv_excel<-function(rate,nper,pmt=0,pv=0,type=0){
  "
  Objective: estimate future value (mimic Excel function)
  rate : period rate
  nper : number of periods
  pmt  : payment per period
  pv   : present value
  type : type=0, payments occur at the end of each period
  type=1, payments occur at the begining of each perod 
  default value is type=0
  "
  fv<-fv_f(pv,rate,nper) +fv_annuity(nper,pmt,rate)
  if(type==1) fv=fv*(1+rate)
  return(fv)
}

#----------------------------------------------------------#
#----Mimic PV() function in Excel -------------------------#
#----------------------------------------------------------#
pv_excel<-function(rate,nper,pmt=0,fv=0,type=0){
  "
  Objective: estimate present value (mimic Excel function)
  rate : period rate
  nper : number of periods
  pmt  : payment per period
  fv   : future value
  type : type=0, payments occur at the end of each period
  type=1, payments occur at the begining of each perod 
  default value is type=0
  "
  pv<-pv_f(fv,rate,nper) +pv_annuity(nper,pmt,rate)
  if(type==1) pv=pv*(1+rate)
  return(pv)
}



#--------------------------------------------------------#
#---Dupont ----------------------------------------------#
#--------------------------------------------------------#
DuPont<-function(ticker){
  "
  > DuPont('IBM')
  
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"IS","A") 
  x2<-x[grep('Net Income',rownames(x)),]
  net_income<-x2[nchar(rownames(x2))<12]
  y<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A") 
  total_equity<-y[grep('Total Equity',rownames(y)),]
  total_assets<-y[grep('Total Assets',rownames(y)),]
  
  cat("Dupon \n       ROE=(net income/sales)*(sales/total assets)*(total assets/equity)\n")
  tt<-round(net_income/total_equity,digits=4)
  kk<-matrix(0,4,4)
  
  total_revenue<-x[grep('Total Revenue',rownames(x)),]
  ratio1<-round(net_income/total_revenue,digits=4)
  
  ratio2<-round(total_revenue/total_assets,digits=4)
  ratio3<-round(total_assets/total_equity,digits=4)
  
  for(i in 1:4){
    kk[i,1]<-tt[i]
  }
  rownames(kk)<-c(colnames(x)[1], colnames(x)[2],colnames(x)[3],colnames(x)[4])
  colnames(kk)<-c("ROE =",' Net Profit Margin *','Asset Turnover *','Equity Multiplier')
  kk[,2]<-ratio1
  kk[,3]<-ratio2
  kk[,4]<-ratio3
  return(kk)
}




#----------------------------------------------------------#
#----Get debt/asset ratio ---------------------------------#
#----------------------------------------------------------#
debt_assets_ratio<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-debt_assets_ratio('DELL')
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A")
  x2<-x[grep('Total Liabilities',rownames(x)),]
  debt<-x2[nchar(rownames(x2))<20]
  assets<-x[grep('Total Assets',rownames(x)),]
  return(round(debt/assets,digits=3))
}



#----------------------------------------------------------#
#----Debt/Equity ratio ------------------------------------#
#----------------------------------------------------------#
debt_equity_ratio<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-debt_equity_ratio('DELL')
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A")
  x2<-x[grep('Total Liabilities',rownames(x)),]
  D<-x2[nchar(rownames(x2))<20]
  E<-x[grep('Total Equity',rownames(x)),]
  return(round(D/E,digits=4))
}


#----------------------------------------------------------#
#----Get long_term_debt/asset ratio -----------------------#
#----------------------------------------------------------#
long_term_debt_assets_ratio<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-debt_assets_ratio('DELL')
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A")
  debt<-x[grep('Total Long Term Debt',rownames(x)),]
  assets<-x[grep('Total Assets',rownames(x)),]
  return(round(debt/assets,digits=3))
}



#----------------------------------------------------------#
#----View related rows by phrase --------------------------#
#----------------------------------------------------------#
view_f<-function(data_set,phrase){
  " Objective: view data items when wheir rownames contain a given phrase
  e.g., 
  > x<-balance_sheet_annual('ibm')
  > view_f(x,'Total')    # phrase is not case sensitive
  # i.e., total, Total, TOTAL are the same
  (part of the output is given below)
  2010-12-31 2009-12-31 2008-12-31 2007-12-31
  Total Receivables, Net                   28225.00   26793.00    27555.0   28789.00
  Total Inventory                           2450.00    2493.00     2700.0    2664.00
  Other Current Assets, Total               1564.00    1730.00     1542.0    1687.00
  Total Current Assets                     48116.00   48936.00    49003.0   53177.00
  Property/Plant/Equipment, Total - Gross  40289.00   39595.00    38444.0   38585.00
  Accumulated Depreciation, Total         -26193.00  -25431.00   -24140.0  -23503.00
  "
  x<-data_set
  return(x[grep(toupper(phrase),toupper(rownames(x))),])
}


#----------------------------------------------------------#
#----Get annual balance sheets-----------------------------#
#----------------------------------------------------------#
balance_sheet_annual<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-balance_sheet_annual('DELL')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A"))
}

#----------------------------------------------------------#
#----Get annual cash flow statement -----------------------#
#----------------------------------------------------------#
cash_flow_annual<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-cash_flow_annual('DELL')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"CF","A"))
}

#----------------------------------------------------------#
#----Get quarterly cash flow statement --------------------#
#----------------------------------------------------------#
cash_flow_quarterly<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-cash_flow_quarterly('DELL')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"CF","Q"))
}

#----------------------------------------------------------#
#----Get annual Income Statements -------------------------#
#----------------------------------------------------------#
income_statement_annual<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-income_statement_anual('IBM')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"IS","A"))
}

#----------------------------------------------------------#
#----Get annual Cash Flow Statements ----------------------#
#----------------------------------------------------------#
cashflow_statement_annual<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-cashflow_statement_annual('IBM')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"CF","A"))
}



#----------------------------------------------------------#
#----Get Quarterly balance sheets--------------------------#
#----------------------------------------------------------#
balance_sheet_quarterly<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-balance_sheet_quarterly('DELL')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","Q"))
}


#----------------------------------------------------------#
#----Get Guarterly Income Statements ----------------------#
#----------------------------------------------------------#
income_statement_quarterly<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-income_statement_quarterly('IBM')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"IS","Q"))
}

#----------------------------------------------------------#
#----Get quarterly cash flow statements -------------------#
#----------------------------------------------------------#
cashflow_statement_quarterly<-function(ticker){
  "
  ticker  : stock/company symbol
  e.g., 
  > x<-cashflow_statement_quarterly('IBM')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),"CF","Q"))
}


#--------------------------------------------------------#
#--- Save annual balance sheets to a given csv file -----#
#--------------------------------------------------------#
save_balance_sheet_annual<-function(ticker,outfile){
  "
  ticker  : stock symbol
  outfile : output file name
  e.g., 
  > save_balance_sheet_annual('DELL','dell_is.csv')
  note: relative path, i.e., save dell_is.csv under 
  the current working directory
  > save_balance_sheet_annual('DELL','c:/test_R/dell_is.csv')
  Note: absolute path method
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A")
  rownames(x)<-gsub(",",";",rownames(x)) 
  
  n<-nchar(outfile)
  n2<-nchar(gsub(":","",outfile))
  if(n==n2){
    path<-getwd()
    z<-paste(path,'/',outfile,sep="")
    z<-gsub(" ","",z)
  }else{
    z<-outfile
  }
  z<-paste("Your saved file is ==>",z,sep="")
  print(z)
  write.csv(x,file=outfile,quote=F)
}


#--------------------------------------------------------#
#--- Save quarterly balance sheets to a given csv file --#
#--------------------------------------------------------#
save_balance_sheet_quarterly<-function(ticker,outfile){
  "
  ticker  : stock symbol
  outfile : output file name
  e.g., 
  > save_balance_sheet_quarterly('DELL','dell_bs.csv')
  note: relative path, i.e., save dell_is.csv under 
  the current working directory
  > save_balance_sheet_quarterly('DELL','c:/test_R/dell_bs.csv')
  Note: absolute path method
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","Q")
  rownames(x)<-gsub(",",";",rownames(x)) 
  n<-nchar(outfile)
  n2<-nchar(gsub(":","",outfile))
  if(n==n2){
    path<-getwd()
    z<-paste(path,'/',outfile,sep="")
    z<-gsub(" ","",z)
  }else{
    z<-outfile
  }
  z<-paste("Your saved file is ==>",z,sep="")
  print(z)
  write.csv(x,file=outfile,quote=F)
}


#--------------------------------------------------------#
#--- Save annual income statement to a given csv file ---#
#--------------------------------------------------------#
save_income_statement_annual<-function(ticker,outfile){
  "
  ticker  : stock symbol
  outfile : output file name
  e.g., 
  > save_income_statement_annual('DELL','dell_is.csv')
  note: relative path, i.e., save dell_is.csv under 
  the current working directory
  > save_income_statement_annual('DELL','c:/test_R/dell_is.csv')
  Note: absolute path method
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"IS","A")
  rownames(x)<-gsub(",",";",rownames(x)) 
  
  n<-nchar(outfile)
  n2<-nchar(gsub(":","",outfile))
  if(n==n2){
    path<-getwd()
    z<-paste(path,'/',outfile,sep="")
    z<-gsub(" ","",z)
  }else{
    z<-outfile
  }
  z<-paste("Your saved file is ==>",z,sep="")
  print(z)
  write.csv(x,file=outfile,quote=F)
}


#----------------------------------------------------------#
#----Future Value (fv_f) ----------------------------------#
#----------------------------------------------------------#
fv_f<-function(pv,r,n){
  "
  Objective: estimate future value
  pv : present value
  r  : period rate
  n  : number of periods
  e.g., 
  > fv_f(100,0.1,1)
  [1] 110
  > fv_f(pv=100,r=0.1,n=1)
  > fv_f(r=0.1,pv=100,n=1)
  > fv_f(n=1,r=0.1,pv=100)
  "
  pv*(1+r)^n
}


#----------------------------------------------------------#
#----Present Value (pv_f) ---------------------------------#
#----------------------------------------------------------#
pv_f<-function(fv,r,n) {
  "
  Objective: estimate present value
  fv : future value
  r  : discount rate
  n  : number of periods
  e.g.,
  > pv_f(100,0.1,1)
  [1] 90.90909
  Note: the followng three give the same result 
  > pv_f(fv=100,r=0.1,n=1)
  > pv_f(r=0.1,fv=100,n=1)
  > pv_f(n=1,r=0.1,fv=100)
  "
  fv*(1+r)^(-n)
}


#----------------------------------------------------------#
#--IRR estimation -----------------------------------------#
#----------------------------------------------------------#
IRR_f<-function(cash_flows){
  " Example: 
  > x<-c(550,-500,-500,-500,1000)
  > IRR_f(x)
  [1] 0.072 0.337
  "
  tiny=1e-5
  r<-seq(-0.90,1.0,by=tiny)
  n<-length(r)
  n_cash_flow<-length(cash_flows)
  epsilon<-mean(abs(cash_flows))*0.001
  
  irr<--99
  j<-1
  
  sign_old<-sign(npv_f(cash_flows,r[1]))
  
  for(i in 2:n){
    sign_new<- sign(npv_f(cash_flows,r[i]))
    
    if(sign_new*sign_old<0){
      irr[j]<-r[i]
      j<-j+1
      sign_old<-sign_new
    }
  }
  return(irr)
}


#--------------------------------------------------------#
#--- Estimate NPV ---------------------------------------#
#--------------------------------------------------------#
npv_f<-function(cash_flows,r){
  "  Note 1  : the first cash-flow occurs TODAY
  Note 2  : cash inflows (outflows) are positive (negative)
  cash_flow : n*1 vector
  r: discount rate
  > x<-c(-100,50,50,50)
  > npv_f(x,0.1)
  [1] 24.3426
  "    
  total<-cash_flows[1]
  for(i in 2:length(cash_flows))
    total<-total+cash_flows[i]/(1+r)^(i-1)
  return(total)
}


#--------------------------------------------------------#
#---- Estimate FV for a set of fixed payments -----------#
#--------------------------------------------------------#
fv2_f<-function(rate,nper,pmt,pv,type=0){
  "
  Objective: estimate future value
  pv : present value
  r  : period rate
  n  : number of periods
  e.g., 
  "
  return(length(pmt))
  
  sum<-0
  for(i in 1:nper){
    sum<-sum+fv_f(rate,nper-i,pv[i])
  }
  sum<-sum+fv_f(rate,nper,pv)
  if(type==1) sum<-sum*(1+rate)
  return(sum)
}



#--------------------------------------------------------#
#--- Bond price -----------------------------------------#
#--------------------------------------------------------#
bond_price<-function(c,r,n,face_value){
  "
  Objective: estimte price of a bond
  c:       coupon payment
  r      : discount rate 
  n      : number of period
  face_value: principal 
  e.g., 
  > bond_price(10,0.12,20,100)
  [1] 85.06111
  > bond_price(10,0.10,20,100)
  [1] 100
  "
  price<-c/r*(1-1/(1+r)^n) + face_value/(1+r)^n
  return(price)
}


#--------------------------------------------------------#
#--- Present value of a perpetuity         --------------#
#--------------------------------------------------------#
pv_perpetuity<-function(c,r){
  " 
  Objective: estimate present value of a perpetuity
  c: period payment
  r: discount rate
  e.g., 
  > pv_perpetuity(100,0.08)
  [1] 1250
  >  pv_perpetuity(r=0.1,c=100)
  [1] 1000
  "
  return(c/r)
}


#--------------------------------------------------------#
#--- Present value of a growing perpetuity --------------#
#--------------------------------------------------------#
pv_grow_perpetuity<-function(c,r,q){
  " 
  Objective: estimate present value of a growthing perpetuity
  r: discount rate
  q: growth rate of perpetuity
  c: period payment
  e.g., (p106)
  > perpetuity_f(30000,0.08,0.04) 
  [1] 750000
  "
  return(c/(r-q))
}

#--------------------------------------------------------#
#--- Present value of a perpetuity due     --------------#
#--------------------------------------------------------#
pv_perpetuity_due<-function(c,r){
  " 
  Objective: estimate present value of a perpetuity due
  i.e., 1st cash flow occurs today
  c: period payment
  r: discount rate
  > pv_perpetuity_due(30000,0.08)
  [1] 405000
  "
  return(c/r*(1+r))
}


#--------------------------------------------------------#
#--- Estimate Payment of an annuity ---------------------#
#--------------------------------------------------------#
PMT<-function(n,r,pv,fv){
  "
  Objective: estimate period payment
  n    : number of periods
  r    : discount rate
  pv   : present value 
  fv   : period payment
  e.g., (p111)
  > PMT(10,0.08,100000,0)
  [1] 14902.95
  "
  return((pv-fv/(1+r)^n)*r/(1-1/(1+r)^n))
}


#--------------------------------------------------------#
#--- Estimte the number of periods for an annuity -------#
#--------------------------------------------------------#
NPER<-function(r,pv,pmt,fv){
  "
  Objective: esimate the number of periods
  r  : discount rate
  fv : future value
  pv : present value
  pmt: payment per period
  e.g.,   (p118)
  > NPER(0.0725,10050,5000,60000)
  [1] 6.999346
  "
  log((fv*r+pmt)/(pv*r+pmt))/log(1+r)
}



#--------------------------------------------------------#
#--- Estimate the number of periods for an annuity ------#
#--------------------------------------------------------#
n_period<-function(r,pv,pmt,fv){
  "
  Objective: esimate the number of period
  r  : discount rate
  fv : future value
  pv : present value
  pmt: payment per period
  e.g.,   (p118)
  > n_period(0.0725,10050,5000,60000)
  [1] 6.999346
  "
  log((fv*r+pmt)/(pv*r+pmt))/log(1+r)
}




#--------------------------------------------------------#
#--- Present value of an annuity ------------------------#
#--------------------------------------------------------#
pv_annuity<-function(n,c,r) {
  "
  Objective: estimate present value of an annuity 
  m_period_from_today: deault is zero
  n   : number of payments    
  c   : payment amount
  r   : discount
  e.g. (p102-103)
  > pv_annuity(29,1,0.08)
  [1] 11.15841
  "
  return(c/r*(1-1/(1+r)^n))
}


#--------------------------------------------------------#
#--- Present value of an annuity ------------------------#
#--------------------------------------------------------#
fv_annuity<-function(n,c,r) {
  "
  Objective: estimate future value of an annuity 
  n   : number of payments    
  c   : payment amount
  r   : discount
  "
  return(c/r*((1+r)^n-1))
}


#--------------------------------------------------------#
#--- Present value of an annuity ------------------------#
#--------------------------------------------------------#
pv_annuity_n_period_from_today<-function(n_period_from_today=0,n,c,r) {
  "
  Objective: estimate present value of an annuity 
  n_period_from_today: 1st payment n_period from today
  default is zero
  n   : number of payments    
  c   : payment amount
  r   : discount
  e.g. (p102-103)
  > pv_annuity_n_period_from_today(0,29,1,0.08)
  [1] 11.15841
  "
  m<-n_period_from_today
  pv1<-c/r*(1-1/(1+r)^n)
  return(pv_f(fv1,r,m))
}


#--------------------------------------------------------#
#-- Present value of a growing annuity ------------------#
#--------------------------------------------------------#
pv_growing_annuity<-function(n,c,r,g) {
  "
  Objective: estimate present value of a growting annuity    
  c    : payment
  n    : number of payments
  r    : discount rate
  g    : growth rate  (g<r)
  e.g. (p107)
  >pv_growing_annuity(30,10000,0.1,0.05)
  [1] 150463.1
  "
  tt<-((1+g)/(1+r))^n
  return(c*1/(r-g)*(1-tt))
}



#--------------------------------------------------------#
#--- NVP (2) --------------------------------------------#
#--------------------------------------------------------#
npv2_f<-function(cash_flows,discount_rate){
  "
  Objective: estimate NPV for all cash-flows 
  assume 1) the first cash-flow occurs TODAY
  2) cash inflows (outflows) are positive (negative)
  cash_flows   : n*1 vector
  discount_rate: discount rate
  e.g., 
  > x<-c(-100,50,50,50)
  > npv_f(x,0.1)
  [1] 24.3426
  "    
  x<-cash_flows
  r<-discount_rate
  n<-length(x)
  total<-x[1]
  
  for(i in 2:n)
    total<-total+x[i]/(1+r)^(i-1)
  return(total)
}


#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
r_continuous<-function(r,m) return(m*log(1+r/m))

# note Rc and r_continuouse are equlvalent


#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
Rc_f<-function(r,m) {
  "
  Convert a given norminal rate to a continuous compounded rate 
  
  r : norminal rate
  m : number of times compounded each year
  e.g., Rc_f(0.1,2)= 0.09758033    (p77-78 )
  "
  return(m*log(1+r/m))
}

#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
Rm_f<-function(Rc,m) {
  "
  
  Get norminal rate for a given continuous compounded rate 
  
  Rc : continuous compounded rate
  m  : number of desired times compounded each year
  e.g., Rm_f(0.08,4)= 0.08080536  (p78)
  "
  return(m*(exp(Rc/m)-1))
}


#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
duration<-function(t,cash_flow,yield){
  "
  Objective: estimate duration of a coupon bond
  t        : is time (nx1 vector)
  cash_flow : is the future cash flow (nx1 vector)
  yield     : is coutinuous compounded bond yield 
  e.g., 
  t<-c(0.5,1,1.5,2 ,2.5,3)
  cash<-c(5,5,5,5,5,105)
  duration(t,cash,0.12) =2.65301
  "
  n<-length(t)
  B<-0     # B is the bond's present value
  for (i in 1:n)
    B<-B+cash_flow[i]*exp(-yield*t[i])
  
  D<-0     # D is the duration
  for (i in 1:n)
    D<-D+t[i]*cash_flow[i]*exp(-yield*t[i])/B
  return(D)
}


#----------------------------------------------------------#
#----------------------------------------------------------#
#----------------------------------------------------------#
modified_duration<-function(t,cash_flow,y,m){
  "
  Objective: estimate duration of a coupon bond
  t        : is time (nx1 vector)
  cash_flow : is the future cash flow (nx1 vector)
  yield     : is compounded bond yield m times per year
  m         : number of compounding per year 
  e.g., 
  t<-c(0.5,1,1.5,2 ,2.5,3)
  cash<-c(5,5,5,5,5,105)
  modified_duration(t,cash,0.123673,2) =2.498511
  "
  yield<-r_continuous(y,m)
  n<-length(t)
  B<-0     # B is the bond's present value
  for (i in 1:n)
    B<-B+cash_flow[i]*exp(-yield*t[i])
  
  D<-0     # D is the duration
  for (i in 1:n)
    D<-D+t[i]*cash_flow[i]*exp(-yield*t[i])/B
  return(D/(1+y/m))
}




#----------------------------------------------------------#
#---Beta: just stock ret and market -----------------------#
#----------------------------------------------------------#
Beta<-function(ticker,year){
  " e.g., 
  > Beta('IBM',2010)
  [1] 'Beta for  IBM in 2010 = 0.7718'
  "
  k<-'http://chart.yahoo.com/table.csv?s=$S'
  k2<-sub('$S',ticker,k,fixed=T)
  t<-read.csv(k2,header=T)
  n<-nrow(t)
  ret<-data.frame(as.Date(t[1:(n-1),1]), (t[1:(n-1),7]-t[2:n,7])/t[2:n,7])
  colnames(ret)<-c('date','ret'); rm(k,k2,t,n)
  t<-read.csv("http://chart.yahoo.com/table.csv?s=^GSPC",header=T)
  n<-nrow(t)
  mkt<-data.frame(as.Date(t[1:(n-1),1]), (t[1:(n-1),7]-t[2:n,7])/t[2:n,7])
  colnames(mkt)<-c('date','mkt')
  data<-merge(ret,mkt)
  d2<-subset(data,format(data[,1],"%Y")==year)
  beta<-round(coef(lm(d2$ret~d2$mkt))[2],digits=4)
  show(paste('Beta for ',ticker,'in',year,'=',beta))
}



#--------------------------------------------------------#
#---Now only valid for 50 stocks ------------------------#
#--------------------------------------------------------#
beta2<-function(ticker_in,year){
  "
  > beta('IBM',2010)
  beta 
  0.7719005 
  "
  con<-url("http://data4research.com/retD50.RData")
  load(con)
  close(con)
  x<-subset(retD50,retD50[,1]==ticker_in)
  z<-merge(x,sp500)
  z<-subset(z,format(z$date,"%Y")==year)
  beta<-z$mkt
  return(coef(lm(z$ret~beta))[2])
}


#--------------------------------------------------------#
#---- 52-week high and low for a given ticker -----------#
#--------------------------------------------------------#
fiftytwo_week_high_low<-function(ticker){
  "
  > fiftytwo_week_high_low('ibm')
  [1] '52-week:   low,     high, today price,from low, from high'
  138.03     185.21   176.85     82 %     18 %
  > fiftytwo_week_high_low('dell')
  [1] '52-week:   low,     high, today price,from low, from high'
  13.15       17.52   15.38     51 %     49 %
  "
  h<-paste("http://chart.yahoo.com/table.csv?s=",ticker,sep="")
  x<-read.csv(h,header=T)
  print("52-week:   low,   high,   today price,from low, from high")
  t<-range(x[1:252,5])
  from_high<-round((t[2]-x[1,5])/(t[2]-t[1])*100,0)
  cat('            ', t[1],'     ',t[2],' ',x[1,5],'   ',100-from_high,'%    ', from_high,'%\n')
}

#--------------------------------------------------------#
#---- high_low_52w  -------------------------------------#
#--------------------------------------------------------#
high_low_52w<-function(ticker){
  "
  > high_low_52w('ibm')
  [1] '52-week:   low,     high, today price,from low, from high'
  138.03     185.21   176.85     82 %     18 %
  > high_low_52w('dell')
  [1] '52-week:   low,     high, today price,from low, from high'
  13.15       17.52   15.38     51 %     49 %
  "
  h<-paste("http://chart.yahoo.com/table.csv?s=",ticker,sep="")
  x<-read.csv(h,header=T)
  print("52-week:   low,     high, today price,from low, from high")
  t<-range(x[1:252,5])
  from_high<-round((t[2]-x[1,5])/(t[2]-t[1])*100,0)
  cat('            ', t[1],'     ',t[2],' ',x[1,5],'   ',100-from_high,'%    ', from_high,'%\n')
}

#--------------------------------------------------------#
#---- get daily historical stock data  ------------------#
#--------------------------------------------------------#
yahoo_daily_price<-function(ticker){
  "  e.g., 
  >x<-yahoo_daily_price('IBM')
  > head(x)
  Date   Open   High    Low  Close  Volume Adj.Close
  1 2011-09-14 164.01 169.66 161.99 167.24 6980700    167.24
  2 2011-09-13 163.64 163.92 161.54 163.43 4723800    163.43
  3 2011-09-12 160.04 162.44 158.76 162.42 5247200    162.42
  4 2011-09-09 164.57 165.19 160.81 161.37 6743900    161.37
  5 2011-09-08 167.00 169.58 165.10 165.25 6027200    165.25
  6 2011-09-07 167.28 167.90 166.16 167.31 6796600    167.31
  "
  h<-paste("http://chart.yahoo.com/table.csv?s=",ticker,sep="")
  return(read.csv(h,header=T))
}


#--------------------------------------------------------#
#---Now only valid for 50 stocks ------------------------#
#--------------------------------------------------------#
stocks_available<-function(){
  con<-url("http://data4research.com/retD50.RData")
  load(con)
  close(con)
  print(unique(retD50[,1]))
}



#--------------------------------------------------------#
#--- Get foreign exchange rate --------------------------#
#--------------------------------------------------------#
exchange_rate<-function(pair){
  "
  e.g.
  > ex<-exchange_rate('USD/JPY')
  > head(ex)
  GMT
  USD/JPY
  2010-03-24 90.3526
  2010-03-25 91.5530
  2010-03-26 92.3578
  2010-03-27 92.5702
  2010-03-28 92.5700
  2010-03-29 92.5469
  "
  library("fImport")
  return(oandaSeries(pair))
}


#--------------------------------------------------------#
#--- Get prime rate -------------------------------------#
#--------------------------------------------------------#
download_prime<-function(){
  return(read.table("http://research.stlouisfed.org/fred2/series/DPRIME/downloaddata/DPRIME.txt",skip=11,header=T))
}



#--------------------------------------------------------#
#--- List several major currencies ----------------------#
#--------------------------------------------------------#
list_currency<-function(){
  print("USD : US Dollar")
  print("GBP : British Pound")
  print("DEM : German Mark")
  print("FRF : French Frac")
  print("EUR : Euro")
  print("ITL : Italian Lira")
  print("CNY : Chinese Yuan (Renminbi")
  print("RUB : Russian Rouble")
}



#--------------------------------------------------------#
#--Download EDM1 (European dollar 1-m deposite rate -----#
#--------------------------------------------------------#
download_EDM1<-function(){
  "
  Objective: download 1-month Euro dollar rate
  e.g., 
  x<-download_EDM1()     # no input needed
  "
  return(read.csv("http://www.federalreserve.gov/releases/h15/data/Business_day/H15_ED_M1.txt",skip=8,header=T))
}


#--------------------------------------------------------#
#-- Black-Scholes European call option model ------------#
#--------------------------------------------------------#
bs_call<-function(S,X,T,r,sigma){
  "
  Objective : calculate Black-Scholes call price
  S     : stock price today
  X     : exercise price
  r     : risk-free rate
  sigma : volatility of the stock
  e.g., bs_call(40,42,0.5,0.1,0.2)  
  [1] 2.277780
  "
  d1 = (log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*sqrt(T))
  d2 = d1-sigma*sqrt(T)
  call<-S*pnorm(d1)-X*exp(-r*T)*pnorm(d2)
  return(call)
}



#--------------------------------------------------------#
#-- Black-Scholes European call option model ------------#
#--------------------------------------------------------#
black_shcoles_call<-function(S,X,T,r,sigma){
  "
  Objective : calculate Black-Scholes call price
  S     : stock price today
  X     : exercise price
  r     : risk-free rate
  sigma : volatility of the stock
  e.g., black_scholes_call(40,42,0.5,0.1,0.2)  
  [1] 2.277780
  "
  d1 = (log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*sqrt(T))
  d2 = d1-sigma*sqrt(T)
  call<-S*pnorm(d1)-X*exp(-r*T)*pnorm(d2)
  return(call)
}


#--------------------------------------------------------#
#---Black-Scholes Eoropen call and put ------------------#
#--------------------------------------------------------#
bs_f<-function(flag,S,X,T,r,sigma){
  "
  Black-Sholes model
  usage: bs_(flag,S,X,T,r,sigma)
  flag  is 'C' or 'P'
  S    is the current stock price
  X    is the exercise price
  T    is the maturity (in years)
  r    is the risk free rate
  sigma  is the volatility (std)
  
  > bs_f('C',42,40,0.5,0.1,0.2) 
  [1] 4.759422
  > bs_f('p',42,40,0.5,0.1,0.2) 
  [1] 0.8085994    
  
  "
  d1 = (log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*sqrt(T))
  d2 = d1-sigma*sqrt(T)
  
  if (toupper(flag)=='C'){ 
    S*pnorm(d1)-X*exp(-r*T)*pnorm(d2)
  }else if(toupper(flag)=='P'){ 
    X*exp(-r*T)*pnorm(-d2)-S*pnorm(-d1)
  } else {
    stop("flag should be C, c, P or p")
  }
}



#--------------------------------------------------------#
#-- Save a financial statement to your PC as a csv file -#
#--------------------------------------------------------#
save_fin_statement<-function(ticker,type,freq,outfile){
  "
  ticker  : stock symbol, such as 'IBM'
  type    : 'IS' for Income statement
  'BS' for Balance Sheet
  'CF' for Cash flow
  freq    : 'A'  for annual
  : 'Q'  for quarterly
  outfile : output file name
  e.g., 
  save_fin_statement('DELL','IS','A','dell_is.csv')
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),type,freq)
  rownames(x)<-gsub(",",";",rownames(x)) 
  
  n<-nchar(outfile)
  n2<-nchar(gsub(":","",outfile))
  if(n==n2){
    path<-getwd()
    z<-paste(path,'/',outfile,sep="")
    z<-gsub(" ","",z)
  }else{
    z<-outfile
  }
  z<-paste("Your saved file is ==>",z,sep="")
  print(z)
  write.csv(x,file=outfile,quote=F)
}



#--------------------------------------------------------#
#--- Download latest several years' financial statements-#
#--------------------------------------------------------#
get_fin_statement<-function(ticker,type,freq){
  "
  ticker  : stock symbol, such as 'IBM'
  type    : 'IS' for Income statement
  'BS' for Balance Sheet
  'CF' for Cash flow
  freq    : 'A'  for annual
  : 'Q'  for quarterly
  e.g., 
  x<-get_fin_statement('DELL','IS','A')
  "
  return(viewFin2(getFin2(ticker,auto.assign=FALSE),type,freq))
}



#--------------------------------------------------------#
#---Estimate current ratios for several 4 years   -------#
#--------------------------------------------------------#
current_ratio<-function(ticker){
  "
  > current_ratio('IBM')
  Annual Balance Sheet for IBM
  2010-12-31 2009-12-31 2008-12-31 2007-12-31 
  1.186      1.359      1.155      1.200
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A") 
  current_a<-x[grep('Total Current Assets',rownames(x)),]
  current_l<-x[grep('Total Current Liabilities',rownames(x)),]
  return(round(current_a/current_l,digits=3))
}

#--------------------------------------------------------#
#---Estimate quick ratios for several years   -----------#
#--------------------------------------------------------#
quick_ratio<-function(ticker){
  "
  > quick_ratio('IBM')
  Annual Balance Sheet for IBM
  "
  x<-viewFin2(getFin2(ticker,auto.assign=FALSE),"BS","A") 
  current_asset<-x[grep('Total Current Assets',rownames(x)),]
  current_lia<-x[grep('Total Current Liabilities',rownames(x)),]
  inventory<-x[grep('Total Inventory',rownames(x)),]
  return(round((current_asset-inventory)/current_lia,digits=3))
}







#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#----- Users should not modify the following codes ------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
viewFin2<-function (x, type = c("BS", "IS", "CF"), period = c("A", "Q"), subset = NULL){
  if (!inherits(x, "financials")) 
    stop(paste(sQuote("x"), "must be of type", sQuote("financials")))
  type <- match.arg(toupper(type[1]), c("BS", "IS", "CF"))
  period <- match.arg(toupper(period[1]), c("A", "Q"))
  statements <- list(BS = "Balance Sheet", IS = "Income Statement", 
                     CF = "Cash Flow Statement", A = "Annual", Q = "Quarterly")
  if (is.null(subset)) {
    message(paste(statements[[period]], statements[[type]], 
                  "for", attr(x, "symbol")))
    return(x[[type]][[period]])
  }
  else {
    tmp.table <- as.matrix(as.xts(t(x[[type]][[period]]), 
                                  dateFormat = "Date")[subset])
    dn1 <- rownames(tmp.table)
    dn2 <- colnames(tmp.table)
    tmp.table <- t(tmp.table)[, NROW(tmp.table):1]
    if (is.null(dim(tmp.table))) {
      dim(tmp.table) <- c(NROW(tmp.table), 1)
      dimnames(tmp.table) <- list(dn2, dn1)
    }
    message(paste(statements[[period]], statements[[type]], 
                  "for", attr(x, "symbol")))
    return(tmp.table)
  }
}


getFin2<-function (Symbol, env = .GlobalEnv, src = "google", auto.assign = TRUE, ...){
  
  Symbol <- strsplit(Symbol, ";")[[1]]
  if (length(Symbol) > 1) 
    return(unlist(lapply(Symbol, getFin, env = env, src = src, 
                         auto.assign = auto.assign)))
  Symbol.name <- Symbol
  
  
  google.fin <- "http://finance.google.com/finance?fstype=ii&q="
  tmp <- tempfile()
  download.file(paste(google.fin, Symbol, sep = ""), quiet = TRUE, 
                destfile = tmp)
  
  Symbol <- readLines(tmp)
  thead <- grep("thead", Symbol)
  tbody <- grep("tbody", Symbol)
  c1 <- lapply(seq(1, 11, 2), function(x) Symbol[thead[x]:thead[x + 
                                                                  1]])
  c2 <- lapply(c1, gsub, pattern = "<.*?>", replacement = "")
  cnames <- lapply(c2, function(x) x[-which(x == "")][-1])
  d1 <- lapply(seq(1, 11, 2), function(x) {
    Symbol[tbody[x]:tbody[x + 1]]
  })
  d2 <- lapply(d1, gsub, pattern = "<.*?>", replacement = "", 
               perl = TRUE)
  d3 <- lapply(d2, function(x) x[-which(x == "")])
  fnames <- lapply(d3, function(x) {
    gsub("&amp;", "&", x[grep("[A-Za-z]", x)])
  })
  vals <- lapply(d3, function(x) {
    as.numeric(gsub(",", "", gsub("^-$", NA, x[-grep("[A-Za-z]", 
                                                     x)])))
  })
  make_col_names <- function(name) {
    substr(name, nchar(name) - 9, nchar(name))
  }
  fin <- lapply(1:6, function(x) {
    structure(matrix(vals[[x]], nr = length(fnames[[x]]), 
                     byrow = TRUE), .Dimnames = list(fnames[[x]], make_col_names(cnames[[x]])), 
              col_desc = cnames[[x]])
  })
  fin <- list(IS = list(Q = fin[[1]], A = fin[[2]]), BS = list(Q = fin[[3]], 
                                                               A = fin[[4]]), CF = list(Q = fin[[5]], A = fin[[6]]))
  if (auto.assign) {
    assign(paste(gsub(":", ".", Symbol.name), "f", sep = "."), 
           structure(fin, symbol = Symbol.name, class = "financials", 
                     src = "google", updated = Sys.time()), env)
    return(paste(gsub(":", ".", Symbol.name), "f", sep = "."))
  }
  else {
    return(structure(fin, symbol = Symbol.name, class = "financials", 
                     src = "google", updated = Sys.time()))
  }
}




#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#---- END, END, END--------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#
#--------------------------------------------------------#