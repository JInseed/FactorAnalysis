##탐색적 요인분석
#Part.1 주관적 웰빙
#test(KMO, Bartlett의 구형성 검정)
KMO(pt1)
cortest.bartlett(R1, n=nrow(pt1))

R1=cor(pt1)
R1

pt1.pca=princomp(pt1, cor=T)
summary(pt1.pca)

pt1.fa=principal(pt1, nfactors=5, rotate='oblimin')
pt1.fa

pt1.fa=principal(pt1, nfactors=6, rotate='oblimin')
pt1.fa

#신뢰도 분석
alpha(pt1)

#Part.2 관계영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt2)
cortest.bartlett(R2, n=nrow(pt2))

R2=cor(pt2)
R2

pt2.pca=princomp(pt2, cor=T)
summary(pt2.pca)

pt2.fa=principal(pt2,nfactors = 8 ,rotate='oblimin')
pt2.fa

#신뢰도 분석
alpha(pt2)


#Part.3 건강 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt3)
cortest.bartlett(R3, n=nrow(pt3))

R3=cor(pt3)
R3

pt3.pca=princomp(pt3, cor=T)
summary(pt3.pca)

pt3.fa=principal(pt3,nfactors = 3  ,rotate='oblimin')
pt3.fa

pt3.fa=principal(pt3,nfactors = 4 ,rotate='oblimin')
pt3.fa

#신뢰도 분석
alpha(pt3)


#Part.4 교육 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt4)
cortest.bartlett(R4, n=nrow(pt4))


R4=cor(pt4)
R4

pt4.pca=princomp(pt4, cor=T)
summary(pt4.pca)

pt4.fa=principal(pt4,nfactors = 2 ,rotate='oblimin')
pt4.fa

pt4.fa=principal(pt4,nfactors = 3 ,rotate='oblimin')
pt4.fa

#신뢰도 분석
alpha(pt4)


#Part.5 안전 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt5)
cortest.bartlett(R5, n=nrow(pt5))

R5=cor(pt5)
R5

pt5.pca=princomp(pt5, cor=T)
summary(pt5.pca)

pt5.fa=principal(pt5,nfactors = 3 ,rotate='oblimin')
pt5.fa

pt5.fa=principal(pt5,nfactors = 4 ,rotate='oblimin')
pt5.fa

pt5.fa=principal(pt5,nfactors = 5 ,rotate='oblimin')
pt5.fa

#신뢰도 분석
alpha(pt5)


#Part.6 참여 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt6)
cortest.bartlett(R6, n=nrow(pt6))


R6=cor(pt6)
R6

pt6.pca=princomp(pt6, cor=T)
summary(pt6.pca)

pt6.fa=principal(pt6,nfactors = 3 ,rotate='oblimin')
pt6.fa

pt6.fa=principal(pt6,nfactors = 4 ,rotate='oblimin')
pt6.fa

#신뢰도 분석
alpha(pt6)


#Part.7 활동 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt7)
cortest.bartlett(R7, n=nrow(pt7))


R7=cor(pt7)
R7

pt7.pca=princomp(pt7, cor=T)
summary(pt7.pca)

pt7.fa=principal(pt7,nfactors = 3 ,rotate='oblimin')
pt7.fa

pt7.fa=principal(pt7,nfactors = 4 ,rotate='oblimin')
pt7.fa

#신뢰도 분석
alpha(pt7)


#Part.8 경제 영역+환경 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(pt8)
cortest.bartlett(R8, n=nrow(pt8))

R8=cor(pt8)
R8
pt8.pca=princomp(pt8, cor=T)
summary(pt8.pca)

pt8.fa=principal(pt8,nfactors = 4 ,rotate='oblimin',scores = T)
pt8.fa

#신뢰도 분석
alpha(pt8)

options(max.print=1000000)

#전 영역
#test(KMO, Bartlett의 구형성 검정)
KMO(all_part)
bartlett.test(all_part)          
cortest.bartlett(R_all, n=nrow(all_part))

R_all=cor(all_part)
R_all

all_part.pca=princomp(cd, cor=T)
summary(all_part.pca)

all_part.fa=principal(all_part,nfactors = 22 ,rotate='oblimin')
all_part.fa

all_part.fa=principal(all_part,nfactors = 23 ,rotate='oblimin')
all_part.fa

#신뢰도 분석
alpha(all_part)

