

# Decriptive Tables:
t1 <- xtable(dscales,caption="Descriptives of EPQ, STCI and PhoPhiKat", label="dscales")
t2 <- xtable(dplat,caption="Descriptives of PLAT-ratings", label="dplat")
t3 <- xtable(drtsq,caption="Descriptives of RTSq-ratings", label="drtsq")
t4 <- xtable(drtsqgain,caption="Descriptives of differences in ridicule and teasing ratings compared to ratings of non-social situations", label="drtsqgain")
t5 <-xtable(d8hfm,caption="Descriptives of the scales from the 8HFM", label="d8hfm")
t6 <-xtable(dtemint,caption="Descriptives of the differences to the actual value of all emotions (TEMINT)", label="dtemint")

#define chunks
df.phophikat<-data.frame(M$pho, M$phi, M$kat)
df.epq<-data.frame(M$P, M$E, M$N, M$L)
df.stci<-data.frame(M$cheerfulness, M$seriousness, M$badmood)
df.inventories<-data.frame(df.phophikat, df.epq, df.stci)

df.platcomp<-data.frame(M$teasinteas, M$ridinteas, M$teasinrid,M$ridinrid)
df.platfun<-data.frame(M$funinteas, M$aversinteas, M$funinrid,M$aversinrid)
df.platdec<-data.frame(M$decisioninteas, M$decisioninrid, A$decisioninall, D$decdiff)
df.platself<-data.frame(M$selfinteas, M$selfinrid, A$selfinall, D$selfdiff)
df.platactor<-data.frame(M$actorinteas, M$actorinrid, A$otherinall, D$otherdiff)
df.platdiff<-data.frame(D$teasdiff,D$riddiff,D$fundiff,D$aversdiff)
df.platall<-data.frame(A$teasinall,A$ridinall,A$funinall,A$aversinall)
df.plat<-data.frame(df.platdec, df.platself, df.platactor, df.platcomp, df.platfun, df.platall, df.platdiff)

df.rtsqteas1<-data.frame(M$joyinteas, M$sadinteas, M$angryinteas, M$disgustinteas)
df.rtsqteas2<-data.frame(M$surpriseinteas, M$shameinteas, M$fearinteas)
df.rtsqrid1<-data.frame(M$joyinrid, M$sadinrid, M$angryinrid, M$disgustinrid)
df.rtsqrid2<-data.frame(M$surpriseinrid, M$shameinrid, M$fearinrid)
df.rtsqambig1<-data.frame(M$joyinnonsocial, M$sadinnonsocial, M$angryinnonsocial, M$disgustinnonsocial)
df.rtsqambig2<-data.frame(M$surpriseinnonsocial, M$shameinnonsocial, M$fearinnonsocial)
df.rtsqnonsocial1<-data.frame(M$joyinambig, M$sadinambig, M$angryinambig, M$disgustinambig)
df.rtsqnonsocial2<-data.frame(M$surpriseinambig, M$shameinambig, M$fearinambig)
df.rtsq<-data.frame(df.rtsqteas1, df.rtsqteas2, df.rtsqrid1, df.rtsqrid2, df.rtsqambig1, df.rtsqambig2, df.rtsqnonsocial1, df.rtsqnonsocial2)

 df.rtsqgainrid1<-data.frame(D$joygaininteas, D$sadgaininteas, D$angrygaininteas, D$disgustgaininteas) 
 df.rtsqgainrid2<-data.frame(D$surprisegaininteas, D$shamegaininteas, D$feargaininteas) 
 df.rtsqgainteas1<-data.frame(D$surprisegaininrid, D$shamegaininrid, D$feargaininrid)
 df.rtsqgainteas2<-data.frame(D$joygaininrid, D$sadgaininrid, D$angrygaininrid, D$disgustgaininrid)
 df.rtsqgain<-data.frame(df.rtsqgainrid1, df.rtsqgainrid2, df.rtsqgainteas1, df.rtsqgainteas2)
 
df.8hfm<-data.frame(M$hfv2, M$hfv1, M$hfv3, M$hfv4, M$hfn1, M$hfn2, M$hfv5, M$hfv6)

df.temint<-data.frame(M$temintangry,M$temintfear,M$temintnervous,M$temintsad,M$temintguilt,M$temintjoy,M$temintpride,M$temintaffection,M$temintsurprise)

df.temintabs<-data.frame(M$temintangryabs,M$temintfearabs,M$temintnervousabs,M$temintsadabs,M$temintguiltabs,M$temintjoyabs,M$temintprideabs,M$temintaffectionabs,M$temintsurpriseabs)

df.misc<-data.frame(M$autism, M$autismabs, M$probability, M$emotionaffect, M$alcohol)
df.demo<-data.frame(M$age, M$sex, M$flatmates, M$inhabitants, M$education)

scalechunks<-list()
scalechunks[[1]]<-df.scales
scalechunks[[2]]<-df.plat 
scalechunks[[3]]<-df.rtsq
scalechunks[[4]]<-df.rtsqgain
scalechunks[[5]]<-df.8hfm
scalechunks[[6]]<-df.temint
scalechunks[[7]]<-df.temintabs
scalechunks[[8]]<-data.frame(df.misc, df.demo)

table<-list()
j<-0



for (j in 0:7)
{
	print(j)
	i<-j*27
	df.scales<-scalechunks[[j+1]]
	
	table[[1+i]] <- cortable(df.phophikat, df.scales)
	table[[1+i]] <- xtable(table[[1+i]],caption="Correlatrion table", label="cor")
	
	table[[2+i]]<-cortable(df.epq, df.scales)
	table[[2+i]] <- xtable(table[[2+i]],caption="Correlatrion table", label="cor")

	table[[3+i]]<-cortable(df.stci, df.scales)
	table[[3+i]] <- xtable(table[[3+i]],caption="Correlatrion table", label="cor")

	table[[4+i]]<-cortable(df.platcomp, df.scales)
	table[[4+i]] <- xtable(table[[4+i]],caption="Correlatrion table", label="cor")
	table[[5+i]]<-cortable(df.platfun, df.scales)
	table[[5+i]] <- xtable(table[[5+i]],caption="Correlatrion table", label="cor")
	table[[6+i]]<-cortable(df.platdec, df.scales)
	table[[6+i]]<-xtable(table[[6+i]],caption="Correlatrion table", label="cor")
	table[[7+i]]<-cortable(df.platself, df.scales)
	table[[7+i]]<-xtable(table[[7+i]],caption="Correlatrion table", label="cor")
	table[[8+i]]<-cortable(df.platactor, df.scales)
	table[[8+i]]<-xtable(table[[8+i]],caption="Correlatrion table", label="cor")


	table[[9+i]]<-cortable(df.rtsqteas1, df.scales)
	table[[9+i]] <- xtable(table[[9+i]],caption="Correlatrion table", label="cor")
	table[[10+i]]<-cortable(df.rtsqteas2, df.scales)
	table[[10+i]] <- xtable(table[[10+i]],caption="Correlatrion table", label="cor")

	table[[11+i]]<-cortable(df.rtsqrid1, df.scales)
	table[[11+i]] <- xtable(table[[11+i]],caption="Correlatrion table", label="cor")
	table[[12+i]]<-cortable(df.rtsqrid2, df.scales)
	table[[12+i]] <- xtable(table[[12+i]],caption="Correlatrion table", label="cor")

	table[[13+i]]<-cortable(df.rtsqambig1, df.scales)
	table[[13+i]] <- xtable(table[[13+i]],caption="Correlatrion table", label="cor")
	table[[14+i]]<-cortable(df.rtsqambig2, df.scales)
	table[[14+i]] <- xtable(table[[14+i]],caption="Correlatrion table", label="cor")

	table[[15+i]]<-cortable(df.rtsqnonsocial1, df.scales)
	table[[15+i]] <- xtable(table[[15+i]],caption="Correlatrion table", label="cor")
	table[[16+i]]<-cortable(df.rtsqnonsocial2, df.scales)
	table[[16+i]] <- xtable(table[[16+i]],caption="Correlatrion table", label="cor")


	table[[17+i]]<-cortable(df.rtsqgainteas1, df.scales)
	table[[17+i]] <- xtable(table[[17+i]],caption="Correlatrion table", label="cor")
	table[[18+i]]<-cortable(df.rtsqgainteas2, df.scales)
	table[[18+i]] <- xtable(table[[18+i]],caption="Correlatrion table", label="cor")

	table[[19+i]]<-cortable(df.rtsqgainrid1, df.scales)
	table[[19+i]] <- xtable(table[[19+i]],caption="Correlatrion table", label="cor")
	table[[20+i]]<-cortable(df.rtsqgainrid2, df.scales)
	table[[20+i]] <- xtable(table[[20+i]],caption="Correlatrion table", label="cor")

	table[[21+i]]<-cortable(df.8hfm[1:4], df.scales)
	table[[21+i]] <- xtable(tc16,caption="Correlatrion table", label="cor")
	table[[22+i]]<-cortable(df.8hfm[5:8], df.scales)
	table[[22+i]] <- xtable(table[[22+i]],caption="Correlatrion table", label="cor")

	table[[23+i]]<-cortable(df.temint[1:5], df.scales)
	table[[23+i]] <- xtable(table[[23+i]],caption="Correlatrion table", label="cor")
	table[[23+i]]<-cortable(df.temint[6:9], df.scales)
	table[[23+i]] <- xtable(table[[23+i]],caption="Correlatrion table", label="cor")

	
	
	table[[24+i]]<-cortable(df.temintabs[1:5], df.scales)
	table[[24+i]] <- xtable(table[[24+i]],caption="Correlatrion table", label="cor")
	table[[25+i]]<-cortable(df.temintabs[6:9], df.scales)
	table[[25+i]] <- xtable(table[[25+i]],caption="Correlatrion table", label="cor")

	table[[26+i]]<-cortable(df.misc, df.scales)
	table[[26+i]] <- xtable(table[[26+i]],caption="Correlatrion table", label="cor")

	table[[27+i]]<-cortable(df.demo, df.scales)
	table[[27+i]] <- xtable(table[[27+i]],caption="Correlatrion table", label="cor")
}
