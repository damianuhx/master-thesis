source("desktop/dafunctions.r")

rawdata <- read.csv(file="desktop/plat.csv",head=TRUE,sep=";")
ncases<-length(rawdata[,1])
I=data.frame(c(1:ncases))
print("rawdata read")

if (1==1)
{
I$age<-rawdata$q0002
I$sex<-rawdata$q0001
I$flatmates<-rawdata$q0005
I$inhabitants<-rawdata$q0006
I$education<-rawdata$q0007
I$livehere<-rawdata$q0009
I$emotionaffect<-rawdata$q0011
I$pastridicule<-rawdata$q0012_0005
I$alcohol<-rawdata$q0013

I$relsingle <- makebool(rawdata$q0003, 0, 999, 1, 1)
I$relsinglesearching <- makebool(rawdata$q0003, 0, 999, 2, 2)
I$relrelationship <- makebool(rawdata$q0003, 0, 999, 3, 3)
I$relrelationshipopen <- makebool(rawdata$q0003, 0, 999, 4, 4)
I$relmaried <- makebool(rawdata$q0003, 0, 999, 5, 5)
I$reldivorced <- makebool(rawdata$q0003, 0, 999, 6, 6)

I$fromgermany <- makebool(rawdata$q0003, 0, 999, 1, 1)
I$fromswitzerland <- makebool(rawdata$q0003, 0, 999, 3, 3)





print("demographics arranged")


I$mobbing<-scale(rawdata,
c("q0012_0001", "q0012_0002", "q0012_0003", "q0012_0004", "q0012_0005", "q0012_0006"),
c(),
0,0)	
print("mobbing arranged")

I$pho<-scale(rawdata,
c("q0157_0001", "q0157_0004", "q0157_0007", "q0157_0010", "q0157_0013", "q0157_0016", "q0157_0019", "q0157_0022", "q0157_0025", "q0157_0028", "q0157_0031", "q0157_0034", "q0157_0037", "q0157_0040", "q0157_0043"),
c(),
0,0)
print("Gelotophobia (pho) arranged")

I$phi<-scale(rawdata,
c("q0157_0002", "q0157_0005", "q0157_0008", "q0157_0011", "q0157_0014", "q0157_0017", "q0157_0020", "q0157_0023", "q0157_0026", "q0157_0029", "q0157_0032", "q0157_0035", "q0157_0038", "q0157_0041", "q0157_0044"),
c(),
0,0)
print("Gelotophilia (phi) arranged")

I$kat<-scale(rawdata,
c("q0157_0003", "q0157_0006", "q0157_0009", "q0157_0012", "q0157_0015", "q0157_0018", "q0157_0021", "q0157_0024", "q0157_0027", "q0157_0030", "q0157_0033", "q0157_0036", "q0157_0039", "q0157_0042", "q0157_0045"),
c(),
0,0)
print("katagelasticsim (kat) arranged")

I$cheerfulness<-scale(rawdata, 
c("q0170_0001", "q0170_0005", "q0170_0009", "q0170_0011", "q0170_0013", "q0170_0017", "q0171_0001", "q0171_0005", "q0171_0009", "q0171_0012", "q0171_0014", "q0171_0016", "q0171_0020", "q0172_0001", "q0172_0005", "q0172_0009", "q0172_0011", "q0172_0013", "q0172_0017", "q0172_0021" ), 
c(), 
0, 5)
print("cheerfulness arranged")

I$seriousness<-scale(rawdata, 
c("q0170_0002", "q0170_0004", "q0170_0010", "q0170_0014", "q0170_0016", "q0171_0002", "q0171_0004", "q0171_0007", "q0171_0011", "q0171_0013", "q0171_0018", "q0171_0021", "q0172_0002", "q0172_0004", "q0172_0008", "q0172_0010", "q0172_0014", "q0172_0016", "q0172_0019"), 
c("q0170_0007"), 
0, 5)
print("seriousness arranged")

I$badmood<-scale(rawdata, 
c("q0170_0003", "q0170_0006", "q0170_0008", "q0170_0012", "q0170_0015", "q0170_0018", "q0171_0003", "q0171_0006", "q0171_0008", "q0171_0010", "q0171_0015", "q0171_0017", "q0171_0019", "q0172_0003", "q0172_0006", "q0172_0007", "q0172_0012", "q0172_0015", "q0172_0018", "q0172_0020"), 
c(), 
0, 5)
print("badmood arranged")

I$P<-scale(rawdata, 
c("q0156_0003", "q0156_0006", "q0156_0007", "q0156_0010", "q0156_0012", "q0156_0022", "q0156_0031", "q0156_0037", "q0156_0039", "q0156_0044", "q0156_0050"), 
c("q0156_0016", "q0156_0026", "q0156_0047"), 
1, 1)
print("psychoticism (P) arranged")

I$E<-scale(rawdata, 
c("q0156_0015", "q0156_0025"), 
c("q0156_0002", "q0156_0005", "q0156_0008", "q0156_0011", "q0156_0024", "q0156_0027", "q0156_0030", "q0156_0036", "q0156_0046", "q0156_0049"), 
1, 1)
print("extraversion (E) arranged")

I$N<-scale(rawdata, 
c(), 
c("q0156_0001", "q0156_0009", "q0156_0013", "q0156_0017", "q0156_0019", "q0156_0020", "q0156_0021", "q0156_0032", "q0156_0034", "q0156_0038", "q0156_0041", "q0156_0042"), 
1, 1)
print("neuroticism (N) arranged")

I$L<-scale(rawdata, 
c("q0156_0004", "q0156_0018", "q0156_0023", "q0156_0028", "q0156_0029", "q0156_0033", "q0156_0035", "q0156_0040", "q0156_0045", "q0156_0048"), 
c("q0156_0014", "q0156_0043"), 
1, 1)
print("social desirability (L) arranged")



I$teasinteas<-scale(rawdata, 
c("q0028_0001", "q0040_0001", "q0064_0001", "q0076_0001", "q0080_0001", "q0092_0001", "q0100_0001", "q0116_0001", "q0128_0001", "q0132_0001", "q0136_0001", "q0140_0001", "q0148_0001"), 
c(), 
0, 0)
print("teasinteas arranged")

I$ridinteas<-scale(rawdata, 
c("q0028_0002", "q0040_0002", "q0064_0002", "q0076_0002", "q0080_0002", "q0092_0002", "q0100_0002", "q0116_0002", "q0128_0002", "q0132_0002", "q0136_0002", "q0140_0002", "q0148_0002"), 
c(), 
0, 0)
print("ridinteas arranged")

I$funinteas<-scale(rawdata, 
c("q0028_0003", "q0040_0003", "q0064_0003", "q0076_0003", "q0080_0003", "q0092_0003", "q0100_0003", "q0116_0003", "q0128_0003", "q0132_0003", "q0136_0003", "q0140_0003", "q0148_0003"), 
c(), 
0, 0)
print("funinteas arranged")

I$aversinteas<-scale(rawdata, 
c("q0028_0004", "q0040_0004", "q0064_0004", "q0076_0004", "q0080_0004", "q0092_0004", "q0100_0004", "q0116_0004", "q0128_0004", "q0132_0004", "q0136_0004", "q0140_0004", "q0148_0004"), 
c(), 
0, 0)
print("aversinteas arranged")

I$teasinrid<-scale(rawdata,
c("q0032_0001", "q0036_0001", "q0044_0001", "q0048_0001", "q0052_0001", "q0056_0001", "q0060_0001", "q0068_0001", "q0072_0001", "q0084_0001", "q0088_0001", "q0096_0001", "q0104_0001", "q0108_0001", "q0112_0001", "q0120_0001", "q0124_0001", "q0144_0001", "q0152_0001"),
c(),
0,0)
print("teasinrid arranged")

I$ridinrid<-scale(rawdata,
c("q0032_0002", "q0036_0002", "q0044_0002", "q0048_0002", "q0052_0002", "q0056_0002", "q0060_0002", "q0068_0002", "q0072_0002", "q0084_0002", "q0088_0002", "q0096_0002", "q0104_0002", "q0108_0002", "q0112_0002", "q0120_0002", "q0124_0002", "q0144_0002", "q0152_0002"),
c(),
0,0)
print("ridinrid arranged")

I$funinrid<-scale(rawdata,
c("q0032_0003", "q0036_0003", "q0044_0003", "q0048_0003", "q0052_0003", "q0056_0003", "q0060_0003", "q0068_0003", "q0072_0003", "q0084_0003", "q0088_0003", "q0096_0003", "q0104_0003", "q0108_0003", "q0112_0003", "q0120_0003", "q0124_0003", "q0144_0003", "q0152_0003"),
c(),
0,0)
print("funinrid arranged")

I$aversinrid<-scale(rawdata,
c("q0032_0004", "q0036_0004", "q0044_0004", "q0048_0004", "q0052_0004", "q0056_0004", "q0060_0004", "q0068_0004", "q0072_0004", "q0084_0004", "q0088_0004", "q0096_0004", "q0104_0004", "q0108_0004", "q0112_0004", "q0120_0004", "q0124_0004", "q0144_0004", "q0152_0004"),
c(),
0,0)
print("aversinrid arranged")

I$decisioninteas<-scale(rawdata, 
c("q0029", "q0041", "q0065", "q0077", "q0081", "q0093", "q0101", "q0117", "q0129", "q0133", "q0137", "q0141", "q0149"), 
c(), 
1, 1)
print("decisioninteas arranged")

I$selfinteas<-scale(rawdata, 
c("q0030", "q0042", "q0066", "q0078", "q0082", "q0094", "q0102", "q0118", "q0130", "q0134", "q0138", "q0142", "q0150"), 
c(), 
1, 1)
print("selfinteas arranged")

I$actorinteas<-scale(rawdata, 
c("q0030", "q0042", "q0066", "q0078", "q0082", "q0094", "q0102", "q0118", "q0130", "q0134", "q0138", "q0142", "q0150"), 
c(), 
1, 1)
print("actorinteas arranged")

I$decisioninrid<-scale(rawdata,
c("q0033", "q0037", "q0045", "q0049", "q0053", "q0057", "q0061", "q0069", "q0073", "q0085", "q0089", "q0097", "q0105", "q0109", "q0113", "q0121", "q0125", "q0145", "q0153"),
c(),
1,1)
print("decisioninrid arranged")

I$selfinrid<-scale(rawdata,
c("q0034", "q0038", "q0046", "q0050", "q0054", "q0058", "q0062", "q0070", "q0074", "q0086", "q0090", "q0098", "q0106", "q0110", "q0114", "q0122", "q0126", "q0146", "q0154"),
c(),
1,1)
print("selfinrid arranged")

I$actorinrid<-scale(rawdata,
c("q0035", "q0039", "q0047", "q0051", "q0055", "q0059", "q0063", "q0071", "q0075", "q0087", "q0091", "q0099", "q0107", "q0111", "q0115", "q0123", "q0127", "q0147", "q0155"),
c(),
1,1)
print("actorinrid arranged")


I$joyinteas<-scale(rawdata,
c("q0018_0001", "q0021_0001", "q0022_0001", "q0027_0001"),
c(),
0,0)
print("joyinteas arranged")

I$sadinteas<-scale(rawdata,
c("q0018_0002", "q0021_0002", "q0022_0002", "q0027_0002"),
c(),
0,0)
print("sadinteas arranged")

I$angryinteas<-scale(rawdata,
c("q0018_0003", "q0021_0003", "q0022_0003", "q0027_0003"),
c(),
0,0)
print("angryinteas arranged")

I$disgustinteas<-scale(rawdata,
c("q0018_0004", "q0021_0004", "q0022_0004", "q0027_0004"),
c(),
0,0)
print("disgustinteas arranged")

I$surpriseinteas<-scale(rawdata,
c("q0018_0005", "q0021_0005", "q0022_0005", "q0027_0005"),
c(),
0,0)
print("surpriseinteas arranged")

I$shameinteas<-scale(rawdata,
c("q0018_0006", "q0021_0006", "q0022_0006", "q0027_0006"),
c(),
0,0)
print("shameinteas arranged")

I$fearinteas<-scale(rawdata,
c("q0018_0007", "q0021_0007", "q0022_0007", "q0027_0007"),
c(),
0,0)
print("fearinteas arranged")



I$joyinrid<-scale(rawdata,
c("q0015_0001", "q0016_0001", "q0019_0001", "q0025_0001"),
c(),
0,0)
print("joyinrid arranged")

I$sadinrid<-scale(rawdata,
c("q0015_0002", "q0016_0002", "q0019_0002", "q0025_0002"),
c(),
0,0)
print("sadinrid arranged")

I$angryinrid<-scale(rawdata,
c("q0015_0003", "q0016_0003", "q0019_0003", "q0025_0003"),
c(),
0,0)
print("angryinrid arranged")

I$disgustinrid<-scale(rawdata,
c("q0015_0004", "q0016_0004", "q0019_0004", "q0025_0004"),
c(),
0,0)
print("disgustinrid arranged")

I$surpriseinrid<-scale(rawdata,
c("q0015_0005", "q0016_0005", "q0019_0005", "q0025_0005"),
c(),
0,0)
print("surpriseinrid arranged")

I$shameinrid<-scale(rawdata,
c("q0015_0006", "q0016_0006", "q0019_0006", "q0025_0006"),
c(),
0,0)
print("shameinrid arranged")

I$fearinrid<-scale(rawdata,
c("q0015_0007", "q0016_0007", "q0019_0007", "q0025_0007"),
c(),
0,0)
print("fearinrid arranged")


I$joyinnonsocial<-scale(rawdata,
c("q0014_0001", "q0017_0001", "q0020_0001", "q0023_0001", "q0026_0001"),
c(),
0,0)
print("joyinnonsocial arranged")

I$sadinnonsocial<-scale(rawdata,
c("q0014_0002", "q0017_0002", "q0020_0002", "q0023_0002", "q0026_0002"),
c(),
0,0)
print("sadinnonsocial arranged")

I$angryinnonsocial<-scale(rawdata,
c("q0014_0003", "q0017_0003", "q0020_0003", "q0023_0003", "q0026_0003"),
c(),
0,0)
print("angryinnonsocial arranged")

I$disgustinnonsocial<-scale(rawdata,
c("q0014_0004", "q0017_0004", "q0020_0004", "q0023_0004", "q0026_0004"),
c(),
0,0)
print("disgustinnonsocial arranged")

I$surpriseinnonsocial<-scale(rawdata,
c("q0014_0005", "q0017_0005", "q0020_0005", "q0023_0005", "q0026_0005"),
c(),
0,0)
print("surpriseinnonsocial arranged")

I$shameinnonsocial<-scale(rawdata,
c("q0014_0006", "q0017_0006", "q0020_0006", "q0023_0006", "q0026_0006"),
c(),
0,0)
print("shameinnonsocial arranged")

I$fearinnonsocial<-scale(rawdata,
c("q0014_0007", "q0017_0007", "q0020_0007", "q0023_0007", "q0026_0007"),
c(),
0,0)
print("fearinnonsocial arranged")


I$joyinambig<-scale(rawdata,
c("q0024_0001"),
c(),
0,0)
print("joyinambig arranged")

I$sadinambig<-scale(rawdata,
c("q0024_0002"),
c(),
0,0)
print("sadinambig arranged")

I$angryinambig<-scale(rawdata,
c("q0024_0003"),
c(),
0,0)
print("angryinambig arranged")

I$disgustinambig<-scale(rawdata,
c("q0024_0004"),
c(),
0,0)
print("disgustinambig arranged")

I$surpriseinambig<-scale(rawdata,
c("q0024_0005"),
c(),
0,0)
print("surpriseinambig arranged")

I$shameinambig<-scale(rawdata,
c("q0024_0006"),
c(),
0,0)
print("shameinambig arranged")

I$fearinambig<-scale(rawdata,
c("q0024_0007"),
c(),
0,0)
print("fearinambig arranged")


I$hfv2<-scale(rawdata,
c("q0174_0001", "q0174_0009", "q0175_0002", "q0175_0010", "q0176_0002", "q0176_0010"),
c(),
0,0)
print("hfv2 arranged")



I$hfv1<-scale(rawdata,
c("q0174_0002", "q0174_0010", "q0175_0003", "q0175_0011", "q0176_0003", "q0176_0011"),
c(),
0,0)
print("hfv1 arranged")

I$hfv3<-scale(rawdata,
c("q0174_0003", "q0174_0011", "q0175_0004", "q0175_0012", "q0176_0004", "q0176_0012"),
c(),
0,0)
print("hfv3 arranged")


I$hfv4<-scale(rawdata,
c("q0174_0004", "q0174_0012", "q0175_0005", "q0175_0013", "q0176_0005", "q0176_0013"),
c(),
0,0)
print("hfv4 arranged")


I$hfn1<-scale(rawdata,
c("q0174_0005", "q0174_0013", "q0175_0006", "q0175_0014", "q0176_0006", "q0176_0014"),
c(),
0,0)
print("hfn1 arranged")

I$hfn2<-scale(rawdata,
c("q0174_0006", "q0174_0014", "q0175_0007", "q0175_0015", "q0176_0007", "q0176_0015"),
c(),
0,0)
print("hfn2 arranged")

I$hfv5<-scale(rawdata,
c("q0174_0007", "q0174_0015", "q0175_0008", "q0175_0016", "q0176_0008", "q0176_0016"),
c(),
0,0)
print("hfv5 arranged")

I$hfv6<-scale(rawdata,
c("q0174_0008", "q0175_0001", "q0175_0009", "q0176_0001", "q0176_0009", "q0176_0017"),
c(),
0,0)
print("hfv6 arranged")



}

I$temintavers<-scale(rawdata,
c("none", "q0159_0001", "q0160_0001", "q0161_0001", "q0162_0001", "q0163_0001", "q0164_0001", "none", "none", "none", "q0168_0001", "q0169_0001"),
c(),
1,0)  
print("temintavers arranged")

I$temintangry<-scale(rawdata,
c("none", "q0159_0002", "q0160_0002", "q0161_0002", "q0162_0002", "q0163_0002", "q0164_0002", "none", "q0166_0001", "q0167_0001", "q0168_0002", "q0169_0002"),
c(),
1,0)  
print("temintangry arranged")


I$temintfear<-scale(rawdata,
c("q0158_0001", "q0159_0003", "q0160_0003", "q0161_0003", "none", "q0163_0003", "none", "q0165_0001", "q0166_0002", "q0167_0002", "q0168_0003", "q0169_0003"),
c(),
1,0)  
print("temintfear arranged")

I$temintnervous<-scale(rawdata,
c("q0158_0002", "q0159_0004", "q0160_0004", "q0161_0004", "none", "q0163_0004", "q0164_0003", "q0165_0002", "q0166_0003", "none", "q0168_0004", "q0169_0004"),
c(),
1,0)  
print("temintnervous arranged")

I$temintsad<-scale(rawdata,
c("q0158_0003", "none", "q0160_0005", "q0161_0005", "q0162_0003", "none", "none", "q0165_0003", "q0166_0004", "q0167_0003", "none", "q0169_0005"),
c(),
1,0)  
print("temintsad arranged")

I$temintguilt<-scale(rawdata,
c("q0158_0004", "none", "q0160_0006", "none", "q0162_0004", "none", "q0164_0004", "none", "none", "none", "none", "q0169_0006"),
c(),
1,0)  
print("temintguilt arranged")

I$temintjoy<-scale(rawdata,
c("q0158_0005", "q0159_0005", "q0160_0007", "q0161_0006", "q0162_0005", "q0163_0005", "q0164_0005", "none", "q0166_0005", "q0167_0004", "q0168_0005", "q0169_0007"),
c(),
1,0)  
print("temintjoy arranged")

I$temintpride<-scale(rawdata,
c("q0158_0006", "q0159_0006", "q0160_0008", "q0161_0007", "none", "none", "none", "none", "none", "q0167_0005", "q0168_0006", "q0169_0008"),
c(),
1,0)  
print("temintpride arranged")

I$temintaffection<-scale(rawdata,
c("q0158_0007", "q0159_0007", "q0160_0009", "none", "none", "q0163_0006", "q0164_0006", "q0165_0004", "q0166_0006", "q0167_0006", "q0168_0007", "q0169_0009"),
c(),
1,0)  
print("temintaffection arranged")

I$temintsurprise<-scale(rawdata,
c("q0158_0008", "none", "none", "q0161_0008", "q0162_0006", "none", "none", "q0165_0005", "none", "q0167_0007", "none", "q0169_0010"),
c(),
1,0)  
print("temintsurprise arranged") 

I$autism<-scale(rawdata,
c("q0173_0002", "q0173_0004", "q0173_0005", "q0173_0006", "q0173_0007", "q0173_0009", "q0173_0012", "q0173_0013", "q0173_0016", "q0173_0018", "q0173_0019", "q0173_0020", "q0173_0021", "q0173_0022", "q0173_0023", "q0173_0026", "q0173_0033", "q0173_0035", "q0173_0039", "q0173_0041", "q0173_0042", "q0173_0043", "q0173_0045", "q0173_0046"),
c("q0173_0001", "q0173_0003", "q0173_0008", "q0173_0010", "q0173_0011", "q0173_0014", "q0173_0015", "q0173_0017", "q0173_0024", "q0173_0025", "q0173_0027", "q0173_0028", "q0173_0029", "q0173_0030", "q0173_0031", "q0173_0032", "q0173_0034", "q0173_0036", "q0173_0037", "q0173_0038", "q0173_0040", "q0173_0044", "q0173_0047", "q0173_0048", "q0173_0049", "q0173_0050"),
1,3)
print("autism arranged")


I$probability<-scale(rawdata,
c("q0177","q0178","q0179","q0180"),
c(),
0,0)
print("probability arranged")

#########################
I$temintaversabs<-I$temintavers
I$temintangryabs<-I$temintangry
I$temintfearabs<-I$temintfear
I$temintnervousabs<-I$temintnervous
I$temintsadabs<-I$temintsad
I$temintguiltabs<-I$temintguilt
I$temintjoyabs<-I$temintjoy
I$temintprideabs<-I$temintpride
I$temintaffectionabs<-I$temintaffection
I$temintsurpriseabs<-I$temintsurprise
I$autismabs<-I$autism
print("original values of variables that will be recoded have been saved with the extension 'abs' like absolute value")

I$probability[,1] <- recode(I$probability[,1], c(1,2,3,4,5), c(0,0,0,1,0))
I$probability[,2:3] <- recode(I$probability[,2:3], c(1,2,3,4,5), c(0,0,1,0,0))
I$probability[,4] <- recode(I$probability[,4], c(1,2,3,4,5), c(0,1,0,0,0))

I$autism <- recode(I$autism, c(-1,0,1,2), c(0,0,1,1))
print("probabilty & autism recoded")

###recode all temint items (=calc differences to target criteria)
#1
I$temintavers[,2] <- recode(I$temintavers[,2], c(0,1,2), c(0,1,2))
I$temintavers[,3:4] <- recode(I$temintavers[,3:4], c(0,1,2), c(2,1,0))
I$temintavers[,5] <- recode(I$temintavers[,5], c(0,1,2), c(0,1,2))
I$temintavers[,6:7] <- recode(I$temintavers[,6:7], c(0,1,2), c(2,1,0))
I$temintavers[,11] <- recode(I$temintavers[,11], c(0,1,2), c(1,0,1))
I$temintavers[,12] <- recode(I$temintavers[,12], c(0,1,2), c(0,1,2))

#2
I$temintangry[,c(2,5,10,12)] <- recode(I$temintangry[,c(2,5,10,12)], c(0,1,2), c(0,1,2))
I$temintangry[,c(3,4,6,7,9,11)] <- recode(I$temintangry[,c(3,4,6,7,9,11)], c(0,1,2), c(2,1,0))
###################

#3
I$temintfear[,c(1,2,3,8,9,11)] <- recode(I$temintfear[,c(1,2,3,8,9,11)], c(0,1,2), c(2,1,0))
I$temintfear[,c(4,6,12)] <- recode(I$temintfear[,c(4,6,12)], c(0,1,2), c(1,0,1))
I$temintfear[,c(10)] <- recode(I$temintfear[,c(10)], c(0,1,2), c(0,1,2))

#4
I$temintnervous[,c(1,2,3,4,6,7,8,9,11,12)] <- recode(I$temintnervous[,c(1,2,3,4,6,7,8,9,11,12)], c(0,1,2), c(2,1,0))

#5
I$temintsad[,c(1,8,9)] <- recode(I$temintsad[,c(1,8,9)], c(0,1,2), c(2,1,0))
I$temintsad[,c(3,4)] <- recode(I$temintsad[,c(3,4)], c(0,1,2), c(1,0,1))
I$temintsad[,c(5,10,12)] <- recode(I$temintsad[,c(5,10,12)], c(0,1,2), c(0,1,2))

#6
I$temintguilt[,c(1,3)] <- recode(I$temintguilt[,c(1,3)], c(0,1,2), c(2,1,0))
I$temintguilt[,c(5,7,12)] <- recode(I$temintguilt[,c(5,7,12)], c(0,1,2), c(0,1,2))

#7
I$temintjoy[,c(2,5,10,12)] <- recode(I$temintjoy[,c(2,5,10,12)], c(0,1,2), c(2,1,0))
I$temintjoy[,c(1,3,4,6,7,9,11)] <- recode(I$temintjoy[,c(1,3,4,6,7,9,11)], c(0,1,2), c(0,1,2))

#8
I$temintpride[,c(2,4,10,12)] <- recode(I$temintpride[,c(2,4,10,12)], c(0,1,2), c(2,1,0))
I$temintpride[,c(1,3,11)] <- recode(I$temintpride[,c(1,3,11)], c(0,1,2), c(0,1,2))

#9
I$temintaffection[,c(1,2,4,8,9,10,12)] <- recode(I$temintaffection[,c(1,2,4,8,9,10,12)], c(0,1,2), c(2,1,0))
I$temintaffection[,c(11)] <- recode(I$temintaffection[,c(11)], c(0,1,2), c(1,0,1))
I$temintaffection[,c(3,6,7)] <- recode(I$temintaffection[,c(3,6,7)], c(0,1,2), c(0,1,2))

#10
I$temintsurprise[,1] <- recode(I$temintsurprise[,1], c(0,1,2), c(2,1,0))
I$temintsurprise[,4] <- recode(I$temintsurprise[,4], c(0,1,2), c(2,1,0))
I$temintsurprise[,5] <- recode(I$temintsurprise[,5], c(0,1,2), c(2,1,0))
I$temintsurprise[,8] <- recode(I$temintsurprise[,8], c(0,1,2), c(2,1,0))
I$temintsurprise[,10] <- recode(I$temintsurprise[,10], c(1,0,1), c(2,1,0))
I$temintsurprise[,12] <- recode(I$temintsurprise[,12], c(0,1,2), c(1,0,1))

print("all temint variables recoded")

####################


M<- meanscales(I, FALSE)
S<- meanscales(I, TRUE)

G=data.frame(c(1:ncases)) # Grouping Variables
D=data.frame(c(1:ncases)) # Differences


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
C$decisioninall<-plus(S$decisioninteas,S$decisioninrid)
C$selfinall<-plus(S$selfinteas,S$selfinrid)
C$otherinall<-plus(S$actorinteas+S$actorinrid)
C$teasinall<-plus(S$teasinteas+S$teasinrid)
C$ridinall<-plus(S$ridinteas+S$ridinrid)
C$funinall<-plus(S$funinteas+S$funinrid)
C$aversinall<-plus(S$aversinteas+S$aversinrid)

C$decdiff<-plus(M$decisioninrid,-M$decisioninteas)
C$selfdiff<-plus(M$selfinrid,-M$selfinteas)
C$otherdiff<-plus(M$actorinrid,-M$actorinteas)
C$teasdiff<-plus(M$teasinrid,-M$teasinteas)
C$riddiff<-plus(M$ridinrid,-M$ridinteas)
C$fundiff<-plus(M$funinrid,-M$funinteas)
C$aversdiff<-plus(M$aversinrid,-M$aversinteas)


C$joygaininteas<-plus(M$joyinteas,-M$joyinnonsocial)
C$sadgaininteas<-plus(M$sadinteas,-M$sadinnonsocial)
C$angrygaininteas<-plus(M$angryinteas, -M$angryinnonsocial)
C$disgustgaininteas<-plus(M$disgustinteas, -M$disgustinnonsocial)
C$surprisegaininteas<-plus(M$surpriseinteas, -M$surpriseinnonsocial)
C$shamegaininteas<-plus(M$shameinteas, -M$shameinnonsocial)
C$feargaininteas<-plus(M$fearinteas, -M$fearinnonsocial)

C$joygaininrid<-plus(M$joyinrid, -M$joyinnonsocial)
C$sadgaininrid<-plus(M$sadinrid, -M$sadinnonsocial)
C$angrygaininrid<-plus(M$angryinrid, -M$angryinnonsocial)
C$disgustgaininrid<-plus(M$disgustinrid-M$disgustinnonsocial)
C$surprisegaininrid<-plus(M$surpriseinrid-M$surpriseinnonsocial)
C$shamegaininrid<-plus(M$shameinrid-M$shameinnonsocial)
C$feargaininrid<-plus(M$fearinrid-M$fearinnonsocial)

C$joydiff<-plus(M$joyinrid,-M$joyinteas)
C$saddiff<-plus(M$sadinrid,-M$sadgaininteas)
C$angrydiff<-plus(M$angryinrid,-M$angrygaininteas)
C$disgustdiff<-plus(M$disgustinrid,-M$disgustgaininteas)
C$surprisediff<-plus(M$surpriseinrid,-M$surprisegaininteas)
C$shamediff<-plus(M$shameinrid,-M$shamegaininteas)
C$feardiff<-plus(M$fearinrid,-M$feargaininteas)

C$temint<-plus(S$temintfear,plus(S$temintnervous,plus(S$temintsad,plus(S$temintguilt,plus(S$temintjoy,plus(S$temintpride,plus(S$temintaffection,S$temintsurprise)))))))

C$temintneg<-plus(S$temintfear,plus(S$temintnervous,plus(S$temintsad,S$temintguilt)))
C$temintpos<-plus(S$temintjoy,plus(S$temintpride,S$temintaffection))

C$stci<-plus(M$badmood,-M$cheerfulness)
C$epq<-plus(M$N,-M$E)
C$phophikat<-plus(M$pho,-M$phi)
C$both<-plus(M$badmood,plus(-M$cheerfulness,plus(3*M$N,-3*M$E)))
C$all<-plus(M$badmood,plus(-M$cheerfulness,plus(M$pho,plus(-M$phi,plus(3*M$N,-3*M$E)))))

print("means, sums, groups and differences created") 