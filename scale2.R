####################

print("")
M<- meanscales(I, FALSE)
print("M(eans) of items created")
S<- meanscales(I, TRUE)
print("S(ums) of items created")
G<-data.frame(c(1:ncases)) # Grouping Variables
print("G(oups) created")


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
D<-data.frame(c(1:ncases)) # Differences
print("D(ifferences) of extents created")

#Calculate new variables (e.g. differences) -- C=combine or calculated
D$decdiff<-M$decisioninrid-M$decisioninteas
D$selfdiff<-M$selfinrid-M$selfinteas
D$otherdiff<-M$actorinrid-M$actorinteas
D$teasdiff<-M$teasinrid-M$teasinteas
D$riddiff<-M$ridinrid-M$ridinteas
D$fundiff<-M$funinrid-M$funinteas
D$aversdiff<-M$aversinrid-M$aversinteas


D$joygaininteas<-M$joyinteas-M$joyinnonsocial
D$sadgaininteas<-M$sadinteas-M$sadinnonsocial
D$angrygaininteas<-M$angryinteas-M$angryinnonsocial
D$disgustgaininteas<-M$disgustinteas-M$disgustinnonsocial
D$surprisegaininteas<-M$surpriseinteas-M$surpriseinnonsocial
D$shamegaininteas<-M$shameinteas-M$shameinnonsocial
D$feargaininteas<-M$fearinteas-M$fearinnonsocial


D$joygaininrid<-M$joyinrid-M$joyinnonsocial
D$sadgaininrid<-M$sadinrid-M$sadinnonsocial
D$angrygaininrid<-M$angryinrid-M$angryinnonsocial
D$disgustgaininrid<-M$disgustinrid-M$disgustinnonsocial
D$surprisegaininrid<-M$surpriseinrid-M$surpriseinnonsocial
D$shamegaininrid<-M$shameinrid-M$shameinnonsocial
D$feargaininrid<-M$fearinrid-M$fearinnonsocial

D$joydiff<-M$joyinrid-M$joyinteas
D$saddiff<-M$sadinrid-M$sadinteas
D$angrydiff<-M$angryinrid-M$angryinteas
D$disgustdiff<-M$disgustinrid-M$disgustinteas
D$surprisediff<-M$surpriseinrid-M$surpriseinteas
D$shamediff<-M$shameinrid-M$shameinteas
D$feardiff<-M$fearinrid-M$fearinteas

D$joygaindiff<-D$joygaininrid-D$joygaininteas
D$sadgaindiff<-D$sadgaininrid-D$sadgaininteas
D$angrygaindiff<-D$angrygaininrid-D$angrygaininteas
D$disgustgaindiff<-D$disgustgaininrid-D$disgustgaininteas
D$surprisegaindiff<-D$surprisegaininrid-D$surprisegaininteas
D$shamegaindiff<-D$shamegaininrid-D$shamegaininteas
D$feargaindiff<-D$feargaininrid-D$feargaininteas

D$stci<-M$badmood-M$cheerfulness
D$epq<-M$N-M$E
D$phophikat<-M$pho-M$phi
D$both<-M$badmood-M$cheerfulness+(3*(M$N-M$E))
D$all<-M$badmood-M$cheerfulness+M$pho-M$phi+(3*(M$N-M$E))

###################

A<-data.frame(c(1:ncases)) # Differences

A$decisioninall<-S$decisioninteas+S$decisioninrid

A$selfinall<-S$selfinteas+S$selfinrid
A$otherinall<-S$actorinteas+S$actorinrid
A$teasinall<-S$teasinteas+S$teasinrid
A$ridinall<-S$ridinteas+S$ridinrid
A$funinall<-S$funinteas+S$funinrid
A$aversinall<-S$aversinteas+S$aversinrid

A$temint<-S$temintfear+S$temintnervous+S$temintsad+S$temintguilt+S$temintjoy+S$temintpride+S$temintaffection+S$temintsurprise

A$temintneg<-S$temintfear+S$temintnervous+S$temintsad+S$temintguilt
A$temintpos<-S$temintjoy+S$temintpride+S$temintaffection

print("A(dditions) of extents created")


C<-data.frame(c(1:ncases)) # Differences
print("C(ategries) of extents created")

T<-data.frame(c(1:ncases)) # Differences
print("T(ext) categories of extents created")



I2<-I
for(i in 1:length(names(I2))){names(I2)[i]<-paste(names(I2)[i],"i", sep="")}
all<-c(C,M,I2)



#################


  dscales <-rbind(
 desctable("Gelotophobia", M$pho, I$pho), 
 desctable("Gelotophilia", M$phi, I$phi), 
 desctable("Katagelasticism", M$kat, I$kat),
 desctable("Psychoticism", M$P, I$P),
  desctable("Extraversion", M$E, I$E),
  desctable("Neuroticism", M$N, I$N),
  desctable("Social Desirability", M$L, I$L),
  desctable("Cheerfulness", M$cheerfulness, I$cheerfulness),
  desctable("Seriousness", M$seriousness, I$seriousness),
  desctable("Bad Mood", M$badmood, I$badmood))
  
  dplat <-rbind(
 desctable("Rating of Teasing Items", M$decisioninteas, I$decisioninteas), 
 desctable("Rating of Ridicule Items", M$decisioninrid, I$decisioninrid), 
 desctable("How it feels in teasing items", M$selfinteas, I$selfinteas), 
 desctable("How it feels in ridicule items", M$selfinrid, I$selfinrid), 
 desctable("Goal of actor in teasing items", M$actorinteas, I$actorinteas), 
 desctable("Goal of actor in ridicule items", M$actorinrid, I$actorinrid), 
  desctable("Teasing component in teasing items", M$teasinteas, I$teasinteas), 
 desctable("Teasing component in ridicule items", M$teasinrid, I$teasinrid), 
 desctable("Ridicule component in teasing items", M$ridinteas, I$ridinteas), 
 desctable("Ridicule component in ridicule items", M$ridinrid, I$ridinrid), 
 desctable("Funniness in teasing items", M$funinteas, I$funinteas), 
 desctable("Funniness in ridicule items", M$funinrid, I$funinrid), 
 desctable("Aversiveness in teasing items", M$aversinteas, I$aversinteas), 
 desctable("Aversiveness in ridicule items", M$aversinrid, I$aversinrid))
 
  
 drtsq <-rbind(
desctable("Mean rating of joy (inteas)", M$joyinteas, I$joyinteas), 
desctable("Mean rating of sadness (inteas)", M$sadinteas, I$sadinteas), 
desctable("Mean rating of being angry (inteas)", M$angryinteas, I$angryinteas), 
desctable("Mean rating of disgust (inteas)", M$disgustinteas, I$disgustinteas), 
desctable("Mean rating of surprise (inteas)", M$surpriseinteas, I$surpriseinteas), 
desctable("Mean rating of shame (inteas)", M$shameinteas, I$shameinteas), 
desctable("Mean rating of fear (inteas)", M$fearinteas, I$fearinteas), 
desctable("Mean rating of joy (inrid)", M$joyinrid, I$joyinrid), 
desctable("Mean rating of sadness (inrid)", M$sadinrid, I$sadinrid), 
desctable("Mean rating of being angry (inrid)", M$angryinrid, I$angryinrid), 
desctable("Mean rating of disgust (inrid)", M$disgustinrid, I$disgustinrid), 
desctable("Mean rating of surprise (inrid)", M$surpriseinrid, I$surpriseinrid), 
desctable("Mean rating of shame (inrid)", M$shameinrid, I$shameinrid), 
desctable("Mean rating of fear (inrid)", M$fearinrid, I$fearinrid), 
desctable("Mean rating of joy (inambig)", M$joyinambig, I$joyinambig), 
desctable("Mean rating of sadness (inambig)", M$sadinambig, I$sadinambig), 
desctable("Mean rating of being angry (inambig)", M$angryinambig, I$angryinambig), 
desctable("Mean rating of disgust (inambig)", M$disgustinambig, I$disgustinambig), 
desctable("Mean rating of surprise (inambig)", M$surpriseinambig, I$surpriseinambig), 
desctable("Mean rating of shame (inambig)", M$shameinambig, I$shameinambig), 
desctable("Mean rating of fear (inambig)", M$fearinambig, I$fearinambig), 
desctable("Mean rating of joy (innonsocial)", M$joyinnonsocial, I$joyinnonsocial), 
desctable("Mean rating of sadness (innonsocial)", M$sadinnonsocial, I$sadinnonsocial), 
desctable("Mean rating of being angry (innonsocial)", M$angryinnonsocial, I$angryinnonsocial), 
desctable("Mean rating of disgust (innonsocial)", M$disgustinnonsocial, I$disgustinnonsocial), 
desctable("Mean rating of surprise (innonsocial)", M$surpriseinnonsocial, I$surpriseinnonsocial), 
desctable("Mean rating of shame (innonsocial)", M$shameinnonsocial, I$shameinnonsocial), 
desctable("Mean rating of fear (innonsocial)", M$fearinnonsocial, I$fearinnonsocial) 
)

drtsqgain <- rbind(
desctable("Mean rating of joy (gaininteas)", D$joygaininteas, I$joyinteas), 
desctable("Mean rating of sadness (gaininteas)", D$sadgaininteas, I$sadinteas), 
desctable("Mean rating of being angry (gaininteas)", D$angrygaininteas, I$angryinteas), 
desctable("Mean rating of disgust (gaininteas)", D$disgustgaininteas, I$disgustinteas), 
desctable("Mean rating of surprise (gaininteas)", D$surprisegaininteas, I$surpriseinteas), 
desctable("Mean rating of shame (gaininteas)", D$shamegaininteas, I$shameinteas), 
desctable("Mean rating of fear (gaininteas)", D$feargaininteas, I$fearinteas), 
desctable("Mean rating of joy (gaininrid)", D$joygaininrid, I$joyinrid), 
desctable("Mean rating of sadness (gaininrid)", D$sadgaininrid, I$sadinrid), 
desctable("Mean rating of being angry (gaininrid)", D$angrygaininrid, I$angryinrid), 
desctable("Mean rating of disgust (gaininrid)", D$disgustgaininrid, I$disgustinrid), 
desctable("Mean rating of surprise (gaininrid)", D$surprisegaininrid, I$surpriseinrid), 
desctable("Mean rating of shame (gaininrid)", D$shamegaininrid, I$shameinrid), 
desctable("Mean rating of fear (gaininrid)", D$feargaininrid, I$fearinrid)
)

d8hfm <- rbind(
desctable("socially warm, boorish competent (cheerful fun)", M$hfv2, I$hfv2), 
desctable("rude, earthy mean-spirited humor", M$hfv1, I$hfv1), 
desctable("inept cold repressed humor (lack of humor)", M$hfv3, I$hfv3), 
desctable("reflective benign humor (cultured)", M$hfv4, I$hfv4), 
desctable("benevolent humor", M$hfn1, I$hfn1), 
desctable("morally grounded ridicule", M$hfn2, I$hfn2), 
desctable("propensity to laughter, lack of control ", M$hfv5, I$hfv5), 
desctable("telling jokes vs. spontaneous wit", M$hfv6, I$hfv6)
) 

dtemint <- rbind(
desctable("Angry (mean difference)", M$temintangry, I$temintangry),
desctable("Fear (mean difference)", M$temintfear, I$temintfear),
desctable("Nervous (mean difference)", M$temintnervous, I$temintnervous),
desctable("Sadness (mean difference)", M$temintsad, I$temintsad),
desctable("Guilt (mean difference)", M$temintguilt, I$temintguilt),
desctable("Joy (mean difference)", M$temintjoy, I$temintjoy),
desctable("Pride (mean difference)", M$temintpride, I$temintpride),
desctable("Affection (mean difference)", M$temintaffection, I$temintaffection),
desctable("Surprise (mean difference)", M$temintsurprise, I$temintsurprise)
)

dmisc <- rbind(
desctable("Sex (1=male, 2=female)", M$sex, I$sex, alpha=FALSE),
desctable("Age", M$age, I$age, alpha=FALSE),
desctable("Mobbing", M$mobbing, I$mobbing, alpha=FALSE),
desctable("Flatmates", M$flatmates, I$flatmates, alpha=FALSE),
desctable("Inhabitants", M$inhabitants, I$inhabitants, alpha=FALSE),
desctable("Education", M$education, I$education, alpha=FALSE)
)

