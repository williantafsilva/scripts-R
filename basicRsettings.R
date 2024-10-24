#######################################################################################
#######################################################################################
#######################################################################################
#Author: WILLIAN T.A.F. SILVA
#E-mail: willian.silva@evobiolab.com
#######################################################################################
#######################################################################################
#######################################################################################

#######################################################################################
#ggplot2 theme.

myggplottheme<-theme(title=element_text(size=10,face="bold"),
                     axis.title=element_text(size=10,face="bold"),
                     axis.text=element_text(size=10),
                     axis.text.x=element_text(angle=60,size=8,vjust=0.5),
                     legend.position="none",
                     legend.title=element_text(size=10,face="bold"),
                     legend.text=element_text(size=10),
                     legend.key=element_blank(),
                     panel.grid=element_line(colour="gray90"),
                     panel.grid.major.x=element_blank(),
                     panel.grid.minor.x=element_blank(),
                     panel.background=element_rect(fill="white",colour="black"),
                     panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                     strip.background=element_rect(colour="black",
                                                   fill="white"))

#######################################################################################
#Greek letters and mathematical symbols (plots).

char_alpha<-intToUtf8(945)
char_beta<-intToUtf8(946)
char_gamma<-intToUtf8(947)
char_delta<-intToUtf8(948)
char_epsilon<-intToUtf8(949)
char_zeta<-intToUtf8(950)
char_eta<-intToUtf8(951)
char_theta<-intToUtf8(952)
char_iota<-intToUtf8(953)
char_kappa<-intToUtf8(954)
char_lambda<-intToUtf8(955)
char_mu<-intToUtf8(956)
char_nu<-intToUtf8(957)
char_xi<-intToUtf8(958)
char_omicron<-intToUtf8(959)
char_pi<-intToUtf8(960)
char_rho<-intToUtf8(961)
char_sigma<-intToUtf8(963)
char_tau<-intToUtf8(964)
char_upsilon<-intToUtf8(965)
char_phi<-intToUtf8(966)
char_chi<-intToUtf8(967)
char_psi<-intToUtf8(968)
char_omega<-intToUtf8(969)
rightarrow<-intToUtf8(8594)
leftarrow<-intToUtf8(8592)
leftrightarrow<-intToUtf8(8596)
infinity<-intToUtf8(8734)
forall<-intToUtf8(8704)
partialdiff<-intToUtf8(8706)
thereexists<-intToUtf8(8707)
thereexistsnot<-intToUtf8(8708)
emptyset<-intToUtf8(8709)
elementof<-intToUtf8(8712)
elementofnot<-intToUtf8(8713)
contains<-intToUtf8(8715)
containsnot<-intToUtf8(8716)
plusorminus<-intToUtf8(177)
cdot<-intToUtf8(8729)
angle<-intToUtf8(8736)
logicaland<-intToUtf8(8743)
logicalor<-intToUtf8(8744)
integral<-intToUtf8(8747)
therefore<-intToUtf8(8756)
because<-intToUtf8(8757)
notsign<-intToUtf8(172)
lessthanorequal<-intToUtf8(8804)
greaterthanorequal<-intToUtf8(8805)
notequal<-intToUtf8(8800)
