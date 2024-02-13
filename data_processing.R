rm(list=ls())
library(foreign)
library(tidyverse)
library(psych)
#원데이터
df=read.spss('C:/Users/82102/Desktop/sample/청소년행복지표.sav',to.data.frame=T)
colSums(is.na(df))
colnames(df)
view(df)

#요인분석에 사용할 지표 선택
tmp=df %>% 
  select(-c(SEX,QU_AGREE1,QU_AGREE2,Q16:Q16_1M2,BQ1_YEAR:wgt_b))

#결측치 처리
tmp1=tmp[!apply(is.na(tmp),1,any),]

#Part.1 주관적 웰빙
pt1=tmp1 %>% 
  select(c(Q1:Q4A2)) %>% 
  mutate(Q1=as.numeric(Q1)-1,
         Q2A1=as.numeric(Q2A1),
         Q2A2=as.numeric(Q2A2),
         Q2A3=as.numeric(Q2A3),
         Q2A4=6-as.numeric(Q2A4),
         Q2A5=6-as.numeric(Q2A5),
         Q2A6=6-as.numeric(Q2A6),
         Q3A1=as.numeric(Q3A1),
         Q3A2=as.numeric(Q3A2),
         Q3A3=as.numeric(Q3A3),
         Q3A4=as.numeric(Q3A4),
         Q3A5=as.numeric(Q3A5),
         Q3A6=as.numeric(Q3A6),
         Q3A7=as.numeric(Q3A7),
         Q3A8=as.numeric(Q3A8),
         Q3A9=6-as.numeric(Q3A9),
         Q4A1=as.numeric(Q4A1),
         Q4A2=6-as.numeric(Q4A2))

#Part.2 관계영역
pt2=tmp1 %>% 
  select(c(Q5:Q11)) %>% 
  mutate(Q5=as.numeric(Q5)-1,
         Q6A1=as.numeric(Q6A1),
         Q6A2=as.numeric(Q6A2),
         Q6A3=as.numeric(Q6A3),
         Q6A4=as.numeric(Q6A4),
         Q6A5=as.numeric(Q6A5),
         Q7A1=as.numeric(Q7A1),
         Q7A2=as.numeric(Q7A2),
         Q7A3=as.numeric(Q7A3),
         Q7A4=as.numeric(Q7A4),
         Q7A5=as.numeric(Q7A5),
         Q7A6=as.numeric(Q7A6),
         Q7A7=as.numeric(Q7A7),
         Q7A8=as.numeric(Q7A8),
         Q7A9=as.numeric(Q7A9),
         Q7A10=as.numeric(Q7A10),
         Q8A1=as.numeric(Q8A1),
         Q8A2=as.numeric(Q8A2),
         Q8A3=as.numeric(Q8A3),
         Q9=as.numeric(Q9),
         Q10=as.numeric(Q10),
         Q11=as.numeric(Q11))


#Part.3 건강 영역
pt3=tmp1 %>% 
  select(c(Q12:Q15)) %>% 
  mutate(Q12=as.numeric(Q12)-1,
         Q13A1=as.numeric(Q13A1),
         Q13A2=6-as.numeric(Q13A2),
         Q14=6-as.numeric(Q14),
         Q15=as.numeric(Q15)-1)


#Part.4 교육 영역
pt4=tmp1 %>% 
  select(c(Q17:Q18A3)) %>% 
  mutate(Q17=as.numeric(Q17)-1,
         Q18A1=as.numeric(Q18A1),
         Q18A2=as.numeric(Q18A2),
         Q18A3=as.numeric(Q18A3))

#Part.5 안전영역
pt5=tmp1 %>% 
  select(c(Q19:Q20A5)) %>% 
  mutate(Q19=as.numeric(Q19)-1,
         Q20A1=as.numeric(Q20A1),
         Q20A2=as.numeric(Q20A2),
         Q20A3=6-as.numeric(Q20A3),
         Q20A4=as.numeric(Q20A4),
         Q20A5=as.numeric(Q20A5))

#Part.6 참여영역
pt6=tmp1 %>% 
  select(c(Q21:Q22A5)) %>% 
  mutate(Q21=as.numeric(Q21)-1,
         Q22A1=as.numeric(Q22A1),
         Q22A2=as.numeric(Q22A2),
         Q22A3=as.numeric(Q22A3),
         Q22A4=as.numeric(Q22A4),
         Q22A5=as.numeric(Q22A5))

#Part.7 활동영역
pt7=tmp1 %>% 
  select(c(Q23:Q24A4)) %>% 
  mutate(Q23=as.numeric(Q23)-1,
         Q24A1=as.numeric(Q24A1),
         Q24A2=as.numeric(Q24A2),
         Q24A3=as.numeric(Q24A3),
         Q24A4=as.numeric(Q24A4))

#Part.8,9 경제영역, 환경 영역
pt8=tmp1 %>% 
  select(c(Q25:Q27A2)) %>% 
  mutate(Q25=as.numeric(Q25)-1,
         Q26=as.numeric(Q26)-1,
         Q27A1=as.numeric(Q27A1),
         Q27A2=as.numeric(Q27A2))


all_part=cbind(pt1,pt2,pt3,pt4,pt5,pt6,pt7,pt8)

write.csv(cd,file='청소년행복지표(변수).csv',row.names = F)