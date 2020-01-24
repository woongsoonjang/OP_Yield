
# height calculation function by crown class
ht.cal.fun <- function(si, age, class) {
    dom.ht <- (1.88*si-7.178)*(((1-exp(-0.025*age))^(0.001*si+1.64)))
    switch(class,
        dom.ht,                    # dominant tree height
        -1.009+0.929*dom.ht,     # co-dominant tree height
        -1.433+0.848*dom.ht,     # intermediate tree height
        0.4*dom.ht               # suppressed tree height
    )
}

# dbh calculation function by crown class
dbh.cal.fun <- function(si, ht, spacing, class) {      # class: 1=dominant, 2=codom, 3=intermediate, 4=suppressed
    if (ht <= 4.5) {
    0
    } else {
    X1 <-  log(ht-4.5)*log(spacing)
    X2 <-  log(ht-4.5)*(ht-4.5)^0.25
    V  <-  exp((0.004059 + 0.2238 + 2*(log(ht-4.5))*0.009384 + 
            2*log(si)*(-0.09332) + 0.002822*((log(ht-4.5))^2) + 
            2*log(ht-4.5)*log(si)*(-0.0060635) + 0.04212*((log(si))^2))/2)
    switch(class,
            exp(0.509945+0.14520471*X1-0.048485*X2-2.105922/spacing)*((ht-4.5)^0.442444)*(spacing^-0.24418)*V,
            exp(0.790506+0.217807*X1-0.1023089*X2-2.105922/spacing)*((ht-4.5)^0.410839)*(spacing^-0.388764)*V,
            exp(0.9514389+0.232373*X1-0.107065*X2-2.105922/spacing)*((ht-4.5)^0.341894)*(spacing^-0.398099)*V,
            exp(0.180428+0.133295*X1-0.123396*X2-2.105922/spacing)*((ht-4.5)^0.593805)*(spacing^-0.0580662)*V
    )    
    }
    
}

# proprotion of stems by crown class
prop.stem.func <- function(Hd, tpa, class){ # dominant height (Hd), stem density (tpa)
    X <- Hd*tpa/1000-1
    switch(class,
            ifelse(X>1, 0.73131*X^(-0.242754-0.0039915*X),0.84),                    # 1= dominant
            1-ifelse(X>1, 0.73131*X^(-0.242754-0.0039915*X),0.84) -                 # 2= codominant (= 1-dom-int-supp)
                ifelse(X>1, 0.7279+0.02321*(X)-0.008762*(X)^1.2-0.7403*(X)^-0.1,  0) - ifelse(X>0.5,3.8721*((1-exp(-0.002807*(X)))^1.5023),0),
            ifelse(X>1, 0.7279+0.02321*(X)-0.008762*(X)^1.2-0.7403*(X)^-0.1,  0),   # 3= intermediate
            ifelse(X>0.5,3.8721*((1-exp(-0.002807*(X)))^1.5023),0)                  # 4= suppressed
    )
}

# mortality proportion by diameter (Oliver and Powers 1978; Larson 1975)
# side note: Ritchie and Zhang (2018) have a different form.
mort.prop.func <- function(dbh){0.01*(0.276+0.003*(dbh-22)^2)}

# function for cubit foot volume calculation
cf.vol.cal<-function(ineq.cf, ht, dbh, tpa){
    cf.vol.parms <- switch(ineq.cf,
		"Oliver & Powers 0_1"= list(c0 = 0, c1 = 1, c2 = 1, c3 = 1, c4 = 0.024843, c5 = 0.001765, c6 = 0, c7 = 0),
		"Wensel & Olson 0_1" = list(c0 = 0.002525, c1 = 1.8515, c2 = 1.0004, c3 = 1.00647, c4 = 0, c5 = 0, c6 = 0, c7 = 0),
		"MacLean&Berger 4_1" = list(c0 = 0, c1 = 1, c2 = 1, c3 = 1, c4 = 0, c5 = 0, c6 = 0.40206, c7 = -0.89991) )          
    with (cf.vol.parms, ifelse(dbh>0.1,tpa*((c0*(dbh^c1)*(ht^c2)*(c3^dbh))+(c4+c5*dbh*dbh*ht)+(0.005454154*dbh*dbh*ht)*(c6+c7/ht)),0) )
}

# function for board foot volume calculation
bf.vol.cal<-function(ineq.bf, ht, dbh, tpa, cf.merch.lim){
    bf.vol.parms <- switch(ineq.bf,
		"Wensel & Olson 8_1" = list(b0 = 0.0001453, b1 = 2.471, b2 = 1.1371, b3 = 0.99058, b4 = 0, b5 = 0, b6 = 0, b7 = 0 , b8= 0),
		"Wensel & Olson 6_1" = list(b0 = 0.0001805, b1 = 2.3988, b2 = 1.137, b3 = 0.99187, b4 = 0, b5 = 0, b6 = 0, b7 = 0 , b8= 0),
		"Wensel & Olson 4_1" = list(b0 = 0.0002054, b1 = 2.3562, b2 = 1.1372, b3 = 0.9926, b4 = 0, b5 = 0, b6 = 0, b7 = 0 , b8= 0), 
        "MacLean&Berger v_1" = list(b0 = 0, b1 = 1, b2 = 1, b3 = 1, b4 = 0, b5 = 0, b6 = 3.2294, b7 = -585.5 , b8= -21.7575) )          
    with (bf.vol.parms, ifelse(dbh > cf.merch.lim,
                        tpa*((10*b0*(dbh^b1)*(ht^b2)*(b3^dbh))+(b4+b5*dbh*dbh*ht) +
                        ifelse(dbh > 12, (dbh*dbh*ht*0.005454154)*(b6+b7/(dbh*ht)+b8/dbh),0)), 0) )
}



# create calculation table
# this is a working table for the output tables/figures



OP.cal.table<-function(                  
                        plant.den ,#= 303,                # TPA
                        si ,#= 105,                       # site index (acceptable value: 40-125 at age 50)
                        ini.plant.surv ,#= 0.85,         # acceptable value: 0.005 - 0.01 (0.5 - 1 %)
                        max.sdi ,#= 400,
                        pct.age ,#= 7,                   # 
                        pct.tgt ,#= 180,                  # 
                        bg.mort ,#= 0,                    # acceptable value: 0-1%
                        cf.merch.lim ,#= 11.5,            # CF Merchantability Limit (inches)
                        ineq.cf ,#= "Oliver & Powers 0_1",# equation index for cf vol calculation
                        ineq.bf ,#= "Wensel & Olson 4_1", # equation inxex ofr bf vol calculation
                        reten.tgt ,#= 80,                 # targer rotation retention
                        reineke.term )#= -1.605)         # Reineke slope coef (allow users to select)

{
# calculate plantation spacing
plant.spacing = sqrt(43560/plant.den)

OP.calcs.table <- data.frame (Tgt.pct = rep(0,56), Age=5:60)
OP.calcs.table$Tgt.pct[which(OP.calcs.table$Age==pct.age)] <- pct.tgt

OP.calcs.table$Prim.mort <- NA      # fill with NAs for initiation 
OP.calcs.table$Prim.mort[1] <- plant.den-plant.den*((1-bg.mort/100)^4)*ini.plant.surv # initial planting survival at age 5
OP.calcs.table$Sec.mort <- NA       # fill with NAs for initiation
OP.calcs.table$Sec.mort[1] <- 0     # initial value

OP.calcs.table$Prim.tpa1 <- NA    # fill with NAs for initiation
OP.calcs.table$Prim.tpa1[1] <- plant.den - OP.calcs.table$Prim.mort[1]
OP.calcs.table$Prim.tpa2 <- NA    # fill with NAs for initiation
OP.calcs.table$Prim.tpa2[1] <- ifelse(OP.calcs.table$Tgt.pct[1] > 0, 
                                       ifelse(OP.calcs.table$Prim.tpa1[1] > OP.calcs.table$Tgt.pct[1],
                                                OP.calcs.table$Tgt.pct[1], OP.calcs.table$Prim.tpa1[1]), OP.calcs.table$Prim.tpa1[1])

OP.calcs.table$Ht.dom <- NA; OP.calcs.table$Ht.dom[1]   <- ht.cal.fun(si=si, age= OP.calcs.table$Age[1], class=1)
OP.calcs.table$Ht.codom <- NA; OP.calcs.table$Ht.codom[1] <- ht.cal.fun(si=si, age= OP.calcs.table$Age[1], class=2)
OP.calcs.table$Ht.int <- NA; OP.calcs.table$Ht.int[1]   <- ht.cal.fun(si=si, age= OP.calcs.table$Age[1], class=3)
OP.calcs.table$Ht.supp <- NA; OP.calcs.table$Ht.supp[1]  <- ht.cal.fun(si=si, age= OP.calcs.table$Age[1], class=4)

OP.calcs.table$Dbh.dom <- NA; OP.calcs.table$Dbh.dom[1]     <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.dom[1], spacing=plant.spacing, class=1)
OP.calcs.table$Dbh.codom <- NA; OP.calcs.table$Dbh.codom[1]    <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.codom[1], spacing=plant.spacing, class=2)
OP.calcs.table$Dbh.int <- NA; OP.calcs.table$Dbh.int[1]      <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.int[1], spacing=plant.spacing, class=3)
OP.calcs.table$Dbh.supp <- NA; OP.calcs.table$Dbh.supp[1]     <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.supp[1], spacing=plant.spacing, class=4)

OP.calcs.table$Pre.mort.prop.dom <- NA; OP.calcs.table$Pre.mort.prop.dom[1] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[1], tpa=OP.calcs.table$Prim.tpa1[1], class =1)
OP.calcs.table$Pre.mort.prop.codom <- NA; OP.calcs.table$Pre.mort.prop.codom[1] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[1], tpa=OP.calcs.table$Prim.tpa1[1], class =2)
OP.calcs.table$Pre.mort.prop.int <- NA; OP.calcs.table$Pre.mort.prop.int[1] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[1], tpa=OP.calcs.table$Prim.tpa1[1], class =3)
OP.calcs.table$Pre.mort.prop.supp <- NA; OP.calcs.table$Pre.mort.prop.supp[1] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[1], tpa=OP.calcs.table$Prim.tpa1[1], class =4)

OP.calcs.table$Int.cal.QMDg <- NA; 
OP.calcs.table$Int.cal.QMDg[1] <- with (OP.calcs.table, sqrt((Dbh.dom[1]*Dbh.dom[1]*Pre.mort.prop.dom[1] + Dbh.codom[1]*Dbh.codom[1]*Pre.mort.prop.codom[1] + 
                                        Dbh.int[1]*Dbh.int[1]*Pre.mort.prop.int[1] + Dbh.supp[1]*Dbh.supp[1]*Pre.mort.prop.supp[1]) / 
                                        (Pre.mort.prop.dom[1] + Pre.mort.prop.codom[1] + Pre.mort.prop.int[1]+Pre.mort.prop.supp[1])) )
OP.calcs.table$Int.cal.SDIg <- NA; OP.calcs.table$Int.cal.SDIg[1] <- OP.calcs.table$Prim.tpa1[1]*(OP.calcs.table$Int.cal.QMDg[1]/10)^-reineke.term

for (i in 2:56) {
    OP.calcs.table$Ht.dom[i]  <- ht.cal.fun(si=si, age= OP.calcs.table$Age[i], class=1)
    OP.calcs.table$Ht.codom[i] <- ht.cal.fun(si=si, age= OP.calcs.table$Age[i], class=2)
    OP.calcs.table$Ht.int[i]   <- ht.cal.fun(si=si, age= OP.calcs.table$Age[i], class=3)
    OP.calcs.table$Ht.supp[i]  <- ht.cal.fun(si=si, age= OP.calcs.table$Age[i], class=4)

    OP.calcs.table$Dbh.dom[i]     <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.dom[i], spacing=plant.spacing, class=1)
    OP.calcs.table$Dbh.codom[i]    <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.codom[i], spacing=plant.spacing, class=2)
    OP.calcs.table$Dbh.int[i]      <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.int[i], spacing=plant.spacing, class=3)
    OP.calcs.table$Dbh.supp[i]     <- dbh.cal.fun(si=si, ht=OP.calcs.table$Ht.supp[i], spacing=plant.spacing, class=4)

    OP.calcs.table$Prim.mort[i] <- OP.calcs.table$Prim.tpa2[i-1]*(bg.mort/100)
    OP.calcs.table$Prim.tpa1[i] <- OP.calcs.table$Prim.tpa2[i-1] - OP.calcs.table$Prim.mort[i]

    OP.calcs.table$Pre.mort.prop.dom[i] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[i], tpa=OP.calcs.table$Prim.tpa1[i], class =1)
    OP.calcs.table$Pre.mort.prop.codom[i] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[i], tpa=OP.calcs.table$Prim.tpa1[i], class =2)
    OP.calcs.table$Pre.mort.prop.int[i] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[i], tpa=OP.calcs.table$Prim.tpa1[i], class =3)
    OP.calcs.table$Pre.mort.prop.supp[i] <-prop.stem.func(Hd=OP.calcs.table$Ht.dom[i], tpa=OP.calcs.table$Prim.tpa1[i], class =4)

    OP.calcs.table$Int.cal.QMDg[i] <- with (OP.calcs.table, sqrt((Dbh.dom[i]*Dbh.dom[i]*Pre.mort.prop.dom[i] + Dbh.codom[i]*Dbh.codom[i]*Pre.mort.prop.codom[i] + 
                                            Dbh.int[i]*Dbh.int[i]*Pre.mort.prop.int[i] + Dbh.supp[i]*Dbh.supp[i]*Pre.mort.prop.supp[i]) / 
                                            (Pre.mort.prop.dom[i]+Pre.mort.prop.codom[i]+Pre.mort.prop.int[i]+Pre.mort.prop.supp[i])) )
    OP.calcs.table$Int.cal.SDIg[i]<- OP.calcs.table$Prim.tpa1[i]*(OP.calcs.table$Int.cal.QMDg[i]/10)^-reineke.term

    OP.calcs.table$Sec.mort[i] <- ifelse(OP.calcs.table$Int.cal.SDIg[i]>(max.sdi-1), OP.calcs.table$Prim.tpa2[i-1]-(max.sdi-5)*(OP.calcs.table$Int.cal.QMDg[i]/10)^reineke.term,   
                                        ifelse(OP.calcs.table$Int.cal.SDIg[i]>(max.sdi*0.6), OP.calcs.table$Prim.tpa2[i-1]*((OP.calcs.table$Int.cal.SDIg[i]-(max.sdi*0.6))/12000),  0))

    OP.calcs.table$Prim.tpa2[i] <- ifelse(OP.calcs.table$Age[i] > 15,  OP.calcs.table$Prim.tpa1[i] - OP.calcs.table$Sec.mort[i], 
                                        ifelse(OP.calcs.table$Tgt.pct[i] > 0, 
                                           ifelse(OP.calcs.table$Prim.tpa1[i] > OP.calcs.table$Tgt.pct[i],
                                                    OP.calcs.table$Tgt.pct[i], OP.calcs.table$Prim.tpa1[i]), OP.calcs.table$Prim.tpa1[i]) )
}

OP.calcs.table$Prelim.tpa.dom <- OP.calcs.table$Pre.mort.prop.dom * OP.calcs.table$Prim.tpa1
OP.calcs.table$Prelim.tpa.codom <- OP.calcs.table$Pre.mort.prop.codom * OP.calcs.table$Prim.tpa1
OP.calcs.table$Prelim.tpa.int <- OP.calcs.table$Pre.mort.prop.int * OP.calcs.table$Prim.tpa1
OP.calcs.table$Prelim.tpa.supp <- OP.calcs.table$Pre.mort.prop.supp * OP.calcs.table$Prim.tpa1

OP.calcs.table$Post.mort.prop.dom <- mort.prop.func(OP.calcs.table$Dbh.dom)
OP.calcs.table$Post.mort.prop.codom <- mort.prop.func(OP.calcs.table$Dbh.codom)
OP.calcs.table$Post.mort.prop.int <- mort.prop.func(OP.calcs.table$Dbh.int)
OP.calcs.table$Post.mort.prop.supp <- mort.prop.func(OP.calcs.table$Dbh.supp)

OP.calcs.table$SDI.mort.tpa.dom <- with (OP.calcs.table, ifelse((Prim.tpa1-Prim.tpa2)>0, 
                                            Pre.mort.prop.dom*(Prim.tpa1-Prim.tpa2)/
                                            (Pre.mort.prop.dom+Pre.mort.prop.codom+Pre.mort.prop.int+Pre.mort.prop.supp), 0))
OP.calcs.table$SDI.mort.tpa.codom <-  with (OP.calcs.table, ifelse((Prim.tpa1-Prim.tpa2)>0, 
                                            Pre.mort.prop.codom*(Prim.tpa1-Prim.tpa2)/
                                            (Pre.mort.prop.dom+Pre.mort.prop.codom+Pre.mort.prop.int+Pre.mort.prop.supp), 0))
OP.calcs.table$SDI.mort.tpa.int <-  with (OP.calcs.table, ifelse((Prim.tpa1-Prim.tpa2)>0, 
                                            Pre.mort.prop.int*(Prim.tpa1-Prim.tpa2)/
                                            (Pre.mort.prop.dom+Pre.mort.prop.codom+Pre.mort.prop.int+Pre.mort.prop.supp), 0))
OP.calcs.table$SDI.mort.tpa.supp <-  with (OP.calcs.table, ifelse((Prim.tpa1-Prim.tpa2)>0, 
                                            Pre.mort.prop.supp*(Prim.tpa1-Prim.tpa2)/
                                            (Pre.mort.prop.dom+Pre.mort.prop.codom+Pre.mort.prop.int+Pre.mort.prop.supp), 0))                                                                             

OP.calcs.table$Post.mort.tpa.dom <- with (OP.calcs.table, Prelim.tpa.dom-SDI.mort.tpa.dom)
OP.calcs.table$Post.mort.tpa.codom <- with (OP.calcs.table, Prelim.tpa.codom-SDI.mort.tpa.codom)
OP.calcs.table$Post.mort.tpa.int <- with (OP.calcs.table, Prelim.tpa.int-SDI.mort.tpa.int)
OP.calcs.table$Post.mort.tpa.supp <- with (OP.calcs.table, Prelim.tpa.supp-SDI.mort.tpa.supp)

OP.calcs.table$BA <- with (OP.calcs.table, (Dbh.dom*Dbh.dom*Post.mort.tpa.dom+Dbh.codom*Dbh.codom*Post.mort.tpa.codom+
                                            Dbh.int*Dbh.int*Post.mort.tpa.int+Dbh.supp*Dbh.supp*Post.mort.tpa.supp)*0.005454154 )
OP.calcs.table$QMDnet <- with (OP.calcs.table, sqrt((BA/(Post.mort.tpa.dom+Post.mort.tpa.codom+Post.mort.tpa.int+Post.mort.tpa.supp))/0.005454154) )
OP.calcs.table$SDI <- with (OP.calcs.table, (Post.mort.tpa.dom+Post.mort.tpa.codom+Post.mort.tpa.int+Post.mort.tpa.supp)*(QMDnet/10)^-reineke.term )

OP.calcs.table$CFV.dom <- cf.vol.cal(ineq.cf=ineq.cf, ht=OP.calcs.table$Ht.dom, dbh = OP.calcs.table$Dbh.dom, tpa= OP.calcs.table$Post.mort.tpa.dom)
OP.calcs.table$CFV.codom <- cf.vol.cal(ineq.cf=ineq.cf, ht=OP.calcs.table$Ht.codom, dbh = OP.calcs.table$Dbh.codom, tpa= OP.calcs.table$Post.mort.tpa.codom)
OP.calcs.table$CFV.int <- cf.vol.cal(ineq.cf=ineq.cf, ht=OP.calcs.table$Ht.int, dbh = OP.calcs.table$Dbh.int, tpa= OP.calcs.table$Post.mort.tpa.int)
OP.calcs.table$CFV.supp <- cf.vol.cal(ineq.cf=ineq.cf, ht=OP.calcs.table$Ht.supp, dbh = OP.calcs.table$Dbh.supp, tpa= OP.calcs.table$Post.mort.tpa.supp)

OP.calcs.table$BFV.dom <- bf.vol.cal(ineq.bf=ineq.bf, ht=OP.calcs.table$Ht.dom, dbh = OP.calcs.table$Dbh.dom, 
                                    tpa= OP.calcs.table$Post.mort.tpa.dom, cf.merch.lim=cf.merch.lim)
OP.calcs.table$BFV.codom <- bf.vol.cal(ineq.bf=ineq.bf, ht=OP.calcs.table$Ht.codom, dbh = OP.calcs.table$Dbh.codom, 
                                    tpa= OP.calcs.table$Post.mort.tpa.codom, cf.merch.lim=cf.merch.lim)
OP.calcs.table$BFV.int <- bf.vol.cal(ineq.bf=ineq.bf, ht=OP.calcs.table$Ht.int, dbh = OP.calcs.table$Dbh.int, 
                                    tpa= OP.calcs.table$Post.mort.tpa.int, cf.merch.lim=cf.merch.lim)
OP.calcs.table$BFV.supp <- bf.vol.cal(ineq.bf=ineq.bf, ht=OP.calcs.table$Ht.supp, dbh = OP.calcs.table$Dbh.supp, 
                                    tpa= OP.calcs.table$Post.mort.tpa.supp, cf.merch.lim=cf.merch.lim)

OP.calcs.table$CFVnet <- with (OP.calcs.table, CFV.dom + CFV.codom + CFV.int + CFV.supp)
OP.calcs.table$Cvmerch <- with (OP.calcs.table, ifelse(Dbh.dom > cf.merch.lim, CFV.dom, 0) + 
                                                ifelse(Dbh.codom > cf.merch.lim, CFV.codom, 0) + 
                                                ifelse(Dbh.int > cf.merch.lim,CFV.codom ,0) +
                                                ifelse(Dbh.supp > cf.merch.lim, CFV.supp ,0) )

OP.calcs.table$BFScrib <- with (OP.calcs.table, BFV.dom + BFV.codom + BFV.int + BFV.supp) 

OP.calcs.table$Cfvol.CAI<-NA; OP.calcs.table$Cfvol.CAI[1]<-0
OP.calcs.table$M.Cfvol.CAI<-NA; OP.calcs.table$M.Cfvol.CAI[1]<-0
OP.calcs.table$BFvol.CAI<-NA; OP.calcs.table$BFvol.CAI[1]<-0
OP.calcs.table$Cfvol.MAI<-NA; OP.calcs.table$Cfvol.MAI[1]<-0
OP.calcs.table$M.Cfvol.MAI<-NA; OP.calcs.table$M.Cfvol.MAI[1]<-0
OP.calcs.table$BFvol.MAI<-NA; OP.calcs.table$BFvol.MAI[1]<-0

for (i in 2:56) {
    OP.calcs.table$Cfvol.CAI[i] <- OP.calcs.table$CFVnet[i] - OP.calcs.table$CFVnet[i-1] 
    OP.calcs.table$M.Cfvol.CAI[i] <- OP.calcs.table$Cvmerch[i] - OP.calcs.table$Cvmerch[i-1] 
    OP.calcs.table$BFvol.CAI[i] <- OP.calcs.table$BFScrib[i] - OP.calcs.table$BFScrib[i-1] 
    OP.calcs.table$Cfvol.MAI[i] <- OP.calcs.table$CFVnet[i] / OP.calcs.table$Age[i]
    OP.calcs.table$M.Cfvol.MAI[i] <- OP.calcs.table$Cvmerch[i] / OP.calcs.table$Age[i]
    OP.calcs.table$BFvol.MAI[i] <- OP.calcs.table$BFScrib[i] / OP.calcs.table$Age[i]
}

OP.calcs.table$TPA <- with(OP.calcs.table, Post.mort.tpa.dom + Post.mort.tpa.codom + 
                            Post.mort.tpa.int + Post.mort.tpa.supp)

OP.calcs.table$CTTARG <- NA; 
OP.calcs.table$CT1 <- NA;
OP.calcs.table$CT2 <- NA;
OP.calcs.table$CT3 <- NA;
OP.calcs.table$CT4 <- NA;


for (i in 1:56) {
    OP.calcs.table$CTTARG[i] <- ifelse(i>20,OP.calcs.table$TPA[i]-reten.tgt , 0) 
    OP.calcs.table$CT4[i] <- with (OP.calcs.table, ifelse(i>20,   ifelse(CTTARG[i] > 0, 
                                ifelse(Post.mort.tpa.supp[i] > CTTARG[i], CTTARG[i], Post.mort.tpa.supp[i]),0) , 0) )
    OP.calcs.table$CT3[i] <- with (OP.calcs.table, ifelse(i>20,   ifelse(CTTARG[i] - CT4[i] > 0, 
                                ifelse(Post.mort.tpa.int[i] > (CTTARG[i] - CT4[i]), CTTARG[i] - CT4[i], Post.mort.tpa.int[i]),0) , 0) )
    OP.calcs.table$CT2[i] <- with (OP.calcs.table, ifelse(i>20,   ifelse(CTTARG[i] - CT4[i] - CT3[i] > 0, 
                                ifelse(Post.mort.tpa.codom[i] > (CTTARG[i] - CT4[i] - CT3[i]), CTTARG[i] - CT4[i] - CT3[i], Post.mort.tpa.codom[i]),0) , 0) )
    OP.calcs.table$CT1[i] <- with (OP.calcs.table, ifelse(i>20,   ifelse(CTTARG[i] - CT4[i] - CT3[i] - CT2[i] > 0, 
                                ifelse(Post.mort.tpa.dom[i] > (CTTARG[i] - CT4[i] - CT3[i] - CT2[i]), CTTARG[i] - CT4[i] - CT3[i] - CT2[i], Post.mort.tpa.dom[i]),0) , 0) )
}


OP.calcs.table$Crop.D.Vol <- with (OP.calcs.table, ifelse(Post.mort.tpa.dom>0, BFV.dom*(Post.mort.tpa.dom-CT1)/Post.mort.tpa.dom,0) )
OP.calcs.table$Crop.C.Vol <- with (OP.calcs.table, ifelse(Post.mort.tpa.codom>0, BFV.codom*(Post.mort.tpa.codom-CT2)/Post.mort.tpa.codom,0) )
OP.calcs.table$Crop.I.Vol <- with (OP.calcs.table, ifelse(Post.mort.tpa.int>0, BFV.int*(Post.mort.tpa.int-CT3)/Post.mort.tpa.int,0) )
OP.calcs.table$Crop.S.Vol <- with (OP.calcs.table, ifelse(Post.mort.tpa.supp>0, BFV.supp*(Post.mort.tpa.supp-CT4)/Post.mort.tpa.supp,0) )
OP.calcs.table$CT.Vol  <- with (OP.calcs.table, BFV.dom + BFV.codom + BFV.int + BFV.supp - Crop.D.Vol - Crop.C.Vol-Crop.I.Vol-Crop.S.Vol )

OP.calcs.table$CT.BFVOL.CAI <- NA; OP.calcs.table$CT.BFVOL.CAI[1] <- 0;

for (i in 2:56) {
    OP.calcs.table$CT.BFVOL.CAI[i] <- OP.calcs.table$CT.Vol[i] - OP.calcs.table$CT.Vol[i-1]
}

OP.calcs.table$Mean.ht <- with(OP.calcs.table, (Ht.dom*Post.mort.tpa.dom + Ht.codom*Post.mort.tpa.codom + 
                            Ht.int*Post.mort.tpa.int + Ht.supp*Post.mort.tpa.supp)/TPA )


OP.calcs.table$Mort.lbs.biom.dom <- with(OP.calcs.table, 
                                        SDI.mort.tpa.dom*(((Ht.dom*0.3048)*((Dbh.dom*1.10336+1.190382)*2.54/100)^2)*278.1143*2.20462+0.40039*2.20462))
OP.calcs.table$Mort.lbs.biom.codom <- with(OP.calcs.table, 
                                        SDI.mort.tpa.codom*(((Ht.codom*0.3048)*((Dbh.codom*1.10336+1.190382)*2.54/100)^2)*278.1143*2.20462+0.40039*2.20462))
OP.calcs.table$Mort.lbs.biom.int <- with(OP.calcs.table, 
                                        SDI.mort.tpa.int*(((Ht.int*0.3048)*((Dbh.int*1.10336+1.190382)*2.54/100)^2)*278.1143*2.20462+0.40039*2.20462))
OP.calcs.table$Mort.lbs.biom.supp <- with(OP.calcs.table, 
                                        SDI.mort.tpa.supp*(((Ht.supp*0.3048)*((Dbh.supp*1.10336+1.190382)*2.54/100)^2)*278.1143*2.20462+0.40039*2.20462))
OP.calcs.table$Dead.B <-  with(OP.calcs.table, Mort.lbs.biom.dom + Mort.lbs.biom.codom + Mort.lbs.biom.int + Mort.lbs.biom.supp)

OP.calcs.table

}

yield.summary.table <- function (OP.calcs.table) {
    OP.yield.sum <- data.frame (Age = seq(5, 60, by=5))
    OP.yield.sum$Total.CF.Vol <- round(OP.calcs.table$CFVnet[seq(1, 56, by=5)], 0)
    OP.yield.sum$Merch.CF.Vol <- round(OP.calcs.table$Cvmerch[seq(1, 56, by=5)], 0)
    OP.yield.sum$Total.BF.Vol <- round(OP.calcs.table$BFScrib[seq(1, 56, by=5)], 0)
    OP.yield.sum$Crop.BF.Vol <- round(with(OP.calcs.table, Crop.D.Vol + Crop.C.Vol + Crop.I.Vol+ Crop.S.Vol)[seq(1, 56, by=5)], 0)
    OP.yield.sum$Comm.Thin.BF.Vol <- round(OP.calcs.table$CT.Vol[seq(1, 56, by=5)], 0)
    OP.yield.sum$SDI <-  round(OP.calcs.table$SDI[seq(1, 56, by=5)], 0)
    OP.yield.sum$TPA <-  round(OP.calcs.table$Prim.tpa2[seq(1, 56, by=5)], 0)
    OP.yield.sum$QMD <- round(OP.calcs.table$QMDnet[seq(1, 56, by=5)], 1)
    OP.yield.sum$BA <-  round(with(OP.yield.sum, TPA*QMD*QMD*pi/576), 1)
    MAI.60 <- c("MAI 60", unlist(round(OP.yield.sum[12,2:6]/60),0),"","","","")
    CAI.60 <- c("CAI 60", round(OP.calcs.table$Cfvol.CAI[56],0), round(OP.calcs.table$M.Cfvol.CAI[56],0),
                round(OP.calcs.table$BFvol.CAI[56],0), 
                round(OP.yield.sum$Crop.BF.Vol[12]-(OP.calcs.table$BFScrib[55]-OP.calcs.table$CT.Vol[55]),0), 
                round(OP.yield.sum$Comm.Thin.BF.Vol[12]-OP.calcs.table$CT.Vol[55],0),
                "","","","")
    OP.yield.sum <- rbind(OP.yield.sum, MAI.60, CAI.60)
    OP.yield.sum

}

PCT.summary.table <- function (OP.calcs.table) {
    if(length(which(OP.calcs.table$Tgt.pct>0))==0) {
        PCT.Age = 0; Total.Removal = 0; Slash.biomass = 0;
    }
    else {
    PCT.Age <- OP.calcs.table$Age[which(OP.calcs.table$Tgt.pct>0)]
    Total.Removal <- round(OP.calcs.table$Prim.tpa1[which(OP.calcs.table$Age==PCT.Age)]
         - OP.calcs.table$Tgt.pct[which(OP.calcs.table$Age==PCT.Age)], 0)
    Slash.biomass <- round(OP.calcs.table$Dead.B[which(OP.calcs.table$Age==PCT.Age)]/2000, 2)
    }
    PCT.summary <- data.frame(PCT.Age = PCT.Age, Total.Removal = Total.Removal, Slash.biomass = Slash.biomass)
    PCT.summary
}

Growing.stock.table <- function  (OP.calcs.table, cf.merch.lim, Age) {
    Stock.table <- data.frame(Crown.class = 
                      c("Dominant", "Codominant", "Intermediate", "Suppressed", "Total", "MAI", "CAI", "CAI%") )
    index <- which(OP.calcs.table$Age==Age)
    Stock.table$CFVol.Merch <- NA
    Stock.table$CFVol.Merch[1] <- round(ifelse(OP.calcs.table$Dbh.dom[index] > cf.merch.lim, OP.calcs.table$CFV.dom[index], 0), 0)
    Stock.table$CFVol.Merch[2] <- round(ifelse(OP.calcs.table$Dbh.codom[index] > cf.merch.lim, OP.calcs.table$CFV.codom[index], 0), 0)
    Stock.table$CFVol.Merch[3] <- round(ifelse(OP.calcs.table$Dbh.int[index] > cf.merch.lim, OP.calcs.table$CFV.int[index], 0), 0)
    Stock.table$CFVol.Merch[4] <- round(ifelse(OP.calcs.table$Dbh.supp[index] > cf.merch.lim, OP.calcs.table$CFV.supp[index], 0), 0)
    Stock.table$CFVol.Merch[5] <- Stock.table$CFVol.Merch[1] + Stock.table$CFVol.Merch[2] + Stock.table$CFVol.Merch[3] + Stock.table$CFVol.Merch[4]
    Stock.table$CFVol.Merch[6] <- round(Stock.table$CFVol.Merch[5]/Age, 0)
    Stock.table$CFVol.Merch[7] <- round(OP.calcs.table$Cfvol.CAI[index], 0)
    Stock.table$CFVol.Merch[8] <- ifelse(Stock.table$CFVol.Merch[5] > 0, round(100*Stock.table$CFVol.Merch[7]/Stock.table$CFVol.Merch[5], 1),"-")

    Stock.table$BFVol <- NA
    Stock.table$BFVol[1] <- round(OP.calcs.table$BFV.dom[index], 0)
    Stock.table$BFVol[2] <- round(OP.calcs.table$BFV.codom[index], 0)
    Stock.table$BFVol[3] <- round(OP.calcs.table$BFV.int[index], 0)
    Stock.table$BFVol[4] <- round(OP.calcs.table$BFV.supp[index], 0)
    Stock.table$BFVol[5] <- Stock.table$BFVol[1] + Stock.table$BFVol[2] + Stock.table$BFVol[3] + Stock.table$BFVol[4]
    Stock.table$BFVol[6] <- round(Stock.table$BFVol[5]/Age, 0)
    Stock.table$BFVol[7] <- round(OP.calcs.table$BFvol.CAI[index], 0)
    Stock.table$BFVol[8] <- ifelse(Stock.table$BFVol[5] > 0, round(100*Stock.table$BFVol[7]/Stock.table$BFVol[5], 1),"-")

    Stock.table$CT.BFVol <- NA
    Stock.table$CT.BFVol[1] <- round(OP.calcs.table$BFV.dom[index] - OP.calcs.table$Crop.D.Vol[index], 0)
    Stock.table$CT.BFVol[2] <- round(OP.calcs.table$BFV.codom[index] - OP.calcs.table$Crop.C.Vol[index], 0)
    Stock.table$CT.BFVol[3] <- round(OP.calcs.table$BFV.int[index] - OP.calcs.table$Crop.I.Vol[index], 0)
    Stock.table$CT.BFVol[4] <- round(OP.calcs.table$BFV.supp[index] - OP.calcs.table$Crop.S.Vol[index], 0)
    Stock.table$CT.BFVol[5] <- Stock.table$CT.BFVol[1] + Stock.table$CT.BFVol[2] + Stock.table$CT.BFVol[3] + Stock.table$CT.BFVol[4]
    Stock.table$CT.BFVol[6] <- round(Stock.table$CT.BFVol[5]/Age, 0)
    Stock.table$CT.BFVol[7] <- round(OP.calcs.table$CT.BFVOL.CAI[index], 0)
    Stock.table$CT.BFVol[8] <- ifelse(Stock.table$CT.BFVol[5] > 0, round(100*Stock.table$CT.BFVol[7]/Stock.table$CT.BFVol[5], 1),"-") 
    
    Stock.table$CT.TPA <- NA; 
    Stock.table$CT.TPA[1] <-  round(OP.calcs.table$CT1[index], 0)
    Stock.table$CT.TPA[2] <-  round(OP.calcs.table$CT2[index], 0)
    Stock.table$CT.TPA[3] <-  round(OP.calcs.table$CT3[index], 0)
    Stock.table$CT.TPA[4] <-  round(OP.calcs.table$CT4[index], 0)
    Stock.table$CT.TPA[5] <- Stock.table$CT.TPA[1] + Stock.table$CT.TPA[2] + Stock.table$CT.TPA[3] + Stock.table$CT.TPA[4]
    Stock.table$CT.TPA[6] <- ""; Stock.table$CT.TPA[7] <- ""; Stock.table$CT.TPA[8] <- ""
    
    Stock.table$Diameter<- NA; 
    Stock.table$Diameter[1] <-  round(OP.calcs.table$Dbh.dom[index], 1)
    Stock.table$Diameter[2] <-  round(OP.calcs.table$Dbh.codom[index], 1)
    Stock.table$Diameter[3] <-  round(OP.calcs.table$Dbh.int[index], 1)
    Stock.table$Diameter[4] <-  round(OP.calcs.table$Dbh.supp[index], 1)
    Stock.table$Diameter[5] <-  round(OP.calcs.table$QMDnet[index], 1)
    Stock.table$Diameter[7] <-  ifelse(Age > 5, round(Stock.table$Diameter[5]-OP.calcs.table$QMDnet[index-1], 1), "-")
    Stock.table$Diameter[8] <-  ifelse(Age > 5, ifelse(Stock.table$Diameter[5] > 0, round(100*(Stock.table$Diameter[7]/Stock.table$Diameter[5]), 1), "-"), "-")
    Stock.table$Diameter[6] <- ""

    Stock.table$Height<- NA; 
    Stock.table$Height[1] <-  round(OP.calcs.table$Ht.dom[index], 1)
    Stock.table$Height[2] <-  round(OP.calcs.table$Ht.codom[index], 1)
    Stock.table$Height[3] <-  round(OP.calcs.table$Ht.int[index], 1)
    Stock.table$Height[4] <-  round(OP.calcs.table$Ht.supp[index], 1)

    Stock.table$TPA<- NA; 
    Stock.table$TPA[1] <-  round(OP.calcs.table$Post.mort.tpa.dom[index], 0)
    Stock.table$TPA[2] <-  round(OP.calcs.table$Post.mort.tpa.codom[index], 0)
    Stock.table$TPA[3] <-  round(OP.calcs.table$Post.mort.tpa.int[index], 0)
    Stock.table$TPA[4] <-  round(OP.calcs.table$Post.mort.tpa.supp[index], 0)
    Stock.table$TPA[5] <-  Stock.table$TPA[1] + Stock.table$TPA[2] + Stock.table$TPA[3] + Stock.table$TPA[4]
    Stock.table$TPA[7] <-  ifelse(Age > 5, round(Stock.table$TPA[5] - OP.calcs.table$Prim.tpa2[index-1], 1), "-")
    Stock.table$TPA[8] <-  ifelse(Age > 5, ifelse(Stock.table$TPA[5] > 0, round(100*(Stock.table$TPA[7]/Stock.table$TPA[5]), 1), "-"), "-")
   
    Stock.table$Height[5] <-  with(Stock.table, round((Height[1]*TPA[1]+Height[2]*TPA[2]+Height[3]*TPA[3]+Height[4]*TPA[4])/Stock.table$TPA[5], 1))
    Stock.table$Height[7] <-  ifelse(Age > 5, round(Stock.table$Height[5]-OP.calcs.table$Mean.ht[index-1], 1), "-")
    Stock.table$Height[8] <-  ifelse(Age > 5, ifelse(Stock.table$Height[5] > 0, round(100*(Stock.table$Height[7]/Stock.table$Height[5]), 1), "-"), "-")
    Stock.table$Height[6] <- ""     # because data type in the dataframe is changed, produced error
    Stock.table$TPA[6] <- ""

    Stock.table$BFV.CFV <-NA
    Stock.table$BFV.CFV[1] <- ifelse(Stock.table$CFVol.Merch[1] > 1, round(Stock.table$BFVol[1]/Stock.table$CFVol.Merch[1], 1), "-")
    Stock.table$BFV.CFV[2] <- ifelse(Stock.table$CFVol.Merch[2] > 1, round(Stock.table$BFVol[2]/Stock.table$CFVol.Merch[2], 1), "-")
    Stock.table$BFV.CFV[3] <- ifelse(Stock.table$CFVol.Merch[3] > 1, round(Stock.table$BFVol[3]/Stock.table$CFVol.Merch[3], 1), "-")
    Stock.table$BFV.CFV[4] <- ifelse(Stock.table$CFVol.Merch[4] > 1, round(Stock.table$BFVol[4]/Stock.table$CFVol.Merch[4], 1), "-")
    Stock.table$BFV.CFV[5] <- ifelse(Stock.table$CFVol.Merch[5] > 1, round(Stock.table$BFVol[5]/Stock.table$CFVol.Merch[5], 1), "-")
    Stock.table$BFV.CFV[6] <- ""; Stock.table$BFV.CFV[7] <- ""; Stock.table$BFV.CFV[8] <- ""

    Stock.table$BA <-NA
    Stock.table$BA[1] <- round(as.numeric(Stock.table$TPA[1])*as.numeric(Stock.table$Diameter[1])**2*pi/576, 1)
    Stock.table$BA[2] <- round(as.numeric(Stock.table$TPA[2])*as.numeric(Stock.table$Diameter[2])**2*pi/576, 1)
    Stock.table$BA[3] <- round(as.numeric(Stock.table$TPA[3])*as.numeric(Stock.table$Diameter[3])**2*pi/576, 1)
    Stock.table$BA[4] <- round(as.numeric(Stock.table$TPA[4])*as.numeric(Stock.table$Diameter[4])**2*pi/576, 1)
    Stock.table$BA[5] <- Stock.table$BA[1] + Stock.table$BA[2] + Stock.table$BA[3] + Stock.table$BA[4]
    Stock.table$BA[6] <- ""; Stock.table$BA[7] <- ""; Stock.table$BA[8] <- ""
    
    Stock.table    
}

Ann.vol.inc.view <- function(OP.calcs.table){
    par(mar=c(3.5,3.5,1.2,1), mgp=c(1.9, 0.6, 0))
    
    plot(OP.calcs.table$Age, OP.calcs.table$Cfvol.CAI, type="l", main="Net Cubit Foot Volume",
    		 xlab="Age",
             ylab=expression(paste("Volume (",~ft^3,~ac^{-1},")",sep="")), cex.axis=1.3,cex.lab=1.3, lwd=2,lty=2)
    lines(OP.calcs.table$Age, OP.calcs.table$Cfvol.MAI, type="l", lwd=2, col="black")
    legend("topleft", lty=1:2,legend=c("MAI","CAI"),bty="n", cex=1.3, lwd=2)
    abline(v=c(10, 20, 30, 40, 50, 60), lty=2)
}

Den.by.class.view <- function(OP.calcs.table){
    par(mar=c(3.5,3.5,1.2,1), mgp=c(1.9, 0.6, 0))
    
    plot(OP.calcs.table$Age, OP.calcs.table$Post.mort.tpa.dom, type="l", main="Density by Crown Class",
    		 xlab="Age", ylim= c(0,max(OP.calcs.table$Post.mort.tpa.dom)*1.1),
             ylab=expression(paste("Trees ",~ac^{-1},sep="")), cex.axis=1.3,cex.lab=1.3, lwd=2, col="green")
    lines(OP.calcs.table$Age, OP.calcs.table$Post.mort.tpa.codom, type="l", lwd=2, col="blue")
    lines(OP.calcs.table$Age, OP.calcs.table$Post.mort.tpa.int, type="l", lwd=2, col="orange")
    lines(OP.calcs.table$Age, OP.calcs.table$Post.mort.tpa.supp, type="l", lwd=2, col="red")
    
    legend("topright", legend=c("Dominant","Codominant", "Intermediate", "Suppressed"), bty="n", 
            cex=1.3, lwd=2, col=c("green","blue","orange","red"))
    }

Density.view <- function(OP.calcs.table, max.sdi, sdi.lines=250){
    par(mar=c(3.5,3.5,1.2,1), mgp=c(1.9, 0.6, 0))
    
    plot(OP.calcs.table$Age, OP.calcs.table$SDI, type="l", main="Density by Crown Class",
    		 xlab="Age", ylim= c(0,max(c(OP.calcs.table$Prim.tpa2, max.sdi))+100),
             ylab=expression(paste("Trees ",~ac^{-1},sep="")), cex.axis=1.3,cex.lab=1.3, lwd=2, lty=2)
    lines(OP.calcs.table$Age, OP.calcs.table$Prim.tpa2, type="l", lwd=2)
    abline(h=max.sdi, col="red", lwd=2, lty=2); text(59.5, max.sdi*1.03, paste("SDI ", max.sdi), col="red")
    abline(h=sdi.lines, col="orange", lwd=2, lty=2); text(59.5, sdi.lines+13, paste("SDI ", sdi.lines), col="orange")
    
    legend("bottomright", legend=c("SDI","TPA", "Max SDI", "SDI lines"), bty="n", 
            cex=1.3, lwd=2, lty = c(2, 1, 2, 2), col=c("black","black","red","orange"))
}

TPA.for.PCT.table <- function(OP.calcs.table, reineke.term){
    PCT.table <- data.frame(UMZ.SDI = seq(260,160, by=-10) )
    PCT.table$QMD.10 <- round(PCT.table$UMZ.SDI*(10/10)^reineke.term, 0)
    PCT.table$QMD.11 <- round(PCT.table$UMZ.SDI*(11/10)^reineke.term, 0)
    PCT.table$QMD.12 <- round(PCT.table$UMZ.SDI*(12/10)^reineke.term, 0)
    PCT.table$QMD.13 <- round(PCT.table$UMZ.SDI*(13/10)^reineke.term, 0)
    PCT.table$QMD.14 <- round(PCT.table$UMZ.SDI*(14/10)^reineke.term, 0)
    PCT.table$QMD.15 <- round(PCT.table$UMZ.SDI*(15/10)^reineke.term, 0)
    PCT.table$QMD.16 <- round(PCT.table$UMZ.SDI*(16/10)^reineke.term, 0)
    PCT.table$QMD.17 <- round(PCT.table$UMZ.SDI*(17/10)^reineke.term, 0)
    PCT.table$QMD.18 <- round(PCT.table$UMZ.SDI*(18/10)^reineke.term, 0)
    PCT.table
}

Input.text = ("
Init.Spacing Init.TPA Thin.Spacing.50.percts  Thin.TPA.50.percts Thin.Spacing.33.percts Thin.TPA.33.percts
10.0  436 14.1  218 12.2  290
10.5  395 14.8  198 12.9  263
11.0  360 15.6  180 13.5  240
11.5  329 16.3  165 14.1  220
12.0  303 17.0  151 14.7  202
12.5  279 17.7  139 15.3  186
13.0  258 18.4  129 15.9  172
13.5  239 19.1  120 16.5  159
14.0  222 19.8  111 17.1  148
14.5  207 20.5  104 17.8  138
15.0  194 21.2  97  18.4  129
15.5  181 21.9  91  19.0  121
16.0  170 22.6  85  19.6  113
16.5  160 23.3  80  20.2  107
17.0  151 24.0  75  20.8  100
17.5  142 24.7  71  21.4  95
18.0  134 25.5  67  22.0  90
")
Density.guide<-read.table(textConnection(Input.text),header=TRUE)
