####################

print("")
M<- meanscales(I, FALSE)
S<- meanscales(I, TRUE)

print("bla")
G<-data.frame(c(1:ncases)) # Grouping Variables
C<-data.frame(c(1:ncases)) # Differences

#Addition A
#Difference D
#Categories C
#Categrories Strings S

print("bli")
G$pho <- makebool(M$pho, 1, 2, 2.5, 4)
G$phi <- makebool(M$phi, 1, 2.5, 3, 4)
G$kat <- makebool(M$kat, 1, 2, 2.5, 4)

G$sex<-rawdata$q0001-1
G$flatemates <- makebool(I$flatmates, 0, 0, 1, 9)



G$relsingle <- makebool(rawdata$q0003, 0, 999, 1, 1)
G$relsinglesearching <- makebool(rawdata$q0003, 0, 999, 2, 2)
G$relrelationship <- makebool(rawdata$q0003, 0, 999, 3, 3)
G$relrelationshipopen <- makebool(rawdata$q0003, 0, 999, 4, 4)
G$relmaried <- makebool(rawdata$q0003, 0, 999, 5, 5)
G$reldivorced <- makebool(rawdata$q0003, 0, 999, 6, 6)

G$fromgermany <- makebool(rawdata$q0003, 0, 999, 1, 1)
G$fromswitzerland <- makebool(rawdata$q0003, 0, 999, 3, 3)


###########
#Calculate new variables (e.g. differences) -- C=combine or calculated
C$decisioninall<-S$decisioninteas+S$decisioninrid

C$selfinall<-S$selfinteas+S$selfinrid
C$otherinall<-S$actorinteas+S$actorinrid
C$teasinall<-S$teasinteas+S$teasinrid
C$ridinall<-S$ridinteas+S$ridinrid
C$funinall<-S$funinteas+S$funinrid
C$aversinall<-S$aversinteas+S$aversinrid

C$decdiff<-M$decisioninrid-M$decisioninteas
C$selfdiff<-M$selfinrid-M$selfinteas
C$otherdiff<-M$actorinrid-M$actorinteas
C$teasdiff<-M$teasinrid-M$teasinteas
C$riddiff<-M$ridinrid-M$ridinteas
C$fundiff<-M$funinrid-M$funinteas
C$aversdiff<-M$aversinrid-M$aversinteas


C$joygaininteas<-M$joyinteas-M$joyinnonsocial
C$sadgaininteas<-M$sadinteas-M$sadinnonsocial
C$angrygaininteas<-M$angryinteas-M$angryinnonsocial
C$disgustgaininteas<-M$disgustinteas-M$disgustinnonsocial
C$surprisegaininteas<-M$surpriseinteas-M$surpriseinnonsocial
C$shamegaininteas<-M$shameinteas-M$shameinnonsocial
C$feargaininteas<-M$fearinteas-M$fearinnonsocial


C$joygaininrid<-M$joyinrid-M$joyinnonsocial
C$sadgaininrid<-M$sadinrid-M$sadinnonsocial
C$angrygaininrid<-M$angryinrid-M$angryinnonsocial
C$disgustgaininrid<-M$disgustinrid-M$disgustinnonsocial
C$surprisegaininrid<-M$surpriseinrid-M$surpriseinnonsocial
C$shamegaininrid<-M$shameinrid-M$shameinnonsocial
C$feargaininrid<-M$fearinrid-M$fearinnonsocial

C$joydiff<-M$joyinrid-M$joyinteas
C$saddiff<-M$sadinrid-M$sadinteas
C$angrydiff<-M$angryinrid-M$angryinteas
C$disgustdiff<-M$disgustinrid-M$disgustinteas
C$surprisediff<-M$surpriseinrid-M$surpriseinteas
C$shamediff<-M$shameinrid-M$shameinteas
C$feardiff<-M$fearinrid-M$fearinteas

C$joygaindiff<-C$joygaininrid-C$joygaininteas
C$sadgaindiff<-C$sadgaininrid-C$sadgaininteas
C$angrygaindiff<-C$angrygaininrid-C$angrygaininteas
C$disgustgaindiff<-C$disgustgaininrid-C$disgustgaininteas
C$surprisegaindiff<-C$surprisegaininrid-C$surprisegaininteas
C$shamegaindiff<-C$shamegaininrid-C$shamegaininteas
C$feargaindiff<-C$feargaininrid-C$feargaininteas

C$temint<-S$temintfear+S$temintnervous+S$temintsad+S$temintguilt+S$temintjoy+S$temintpride+S$temintaffection+S$temintsurprise

C$temintneg<-S$temintfear+S$temintnervous+S$temintsad+S$temintguilt
C$temintpos<-S$temintjoy+S$temintpride+S$temintaffection

C$stci<-M$badmood-M$cheerfulness
C$epq<-M$N-M$E
C$phophikat<-M$pho-M$phi
C$both<-M$badmood-M$cheerfulness+(3*(M$N-M$E))
C$all<-M$badmood-M$cheerfulness+M$pho-M$phi+(3*(M$N-M$E))

I2<-I
for(i in 1:length(names(I2))){names(I2)[i]<-paste(names(I2)[i],"i", sep="")}
all<-c(C,M,I2)
