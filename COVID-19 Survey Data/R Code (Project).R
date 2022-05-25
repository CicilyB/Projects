cdata <- read_excel("Balachandar-Cicily-Dataset (formatted for R).xlsx")


awareness1 <- cdata$`How worried are you about getting the COVID19?`
awareness2 <- cdata$`Do you think that you will get sick from the COVID19?`

knowledge1 <- cdata$`Correctly identified 3 symptoms of the COVID19`
knowledge2 <- cdata$`Correctly identified 3 prevention methods of the COVID19`

preparedness <- cdata$`How prepared do you think you are if there were to be a widespread COVID19 outbreak?`
behav <- cdata$`How much has the COVID19 change your daily routine?`


# Data modification for awareness-knowledge (1)

level11 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness1=="Not worried at all"))

level21 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness1=="A little worried"))

level31 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness1=="Somewhat worried"))  

level41 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness1=="Very worried"))

length(level11)
length(level21)
length(level31)
length(level41)


level12_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                  &(awareness1=="Not worried at all"))
level12_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                    &(awareness1=="Not worried at all"))

level22_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                  &(awareness1=="A little worried"))
level22_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                  &(awareness1=="A little worried"))

level32_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                  &(awareness1=="Somewhat worried"))  
level32_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                  &(awareness1=="Somewhat worried"))  

level42_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                  &(awareness1=="Very worried"))
level42_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                  &(awareness1=="Very worried"))


length(level12_1) + length(level12_2)
length(level22_1) + length(level22_2)
length(level32_1) + length(level32_2)
length(level42_1) + length(level42_2)


level13 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
                  &(awareness1=="Not worried at all"))

level23 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
                  &(awareness1=="A little worried"))

level33 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
                  &(awareness1=="Somewhat worried"))  

level43 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
                  &(awareness1=="Very worried"))

length(level13)
length(level23)
length(level33)
length(level43)



# Data modification for awareness-knowledge (2)

a11 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness2=="Not at all"))

a21 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness2=="its possible"))

a31 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness2=="I probably will"))  

a41 <- which ((knowledge1=="NO")&(knowledge2=="NO")
                  &(awareness2=="I definitely will"))


length(a11)
length(a21)
length(a31)
length(a41)



a12_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
              &(awareness2=="Not at all"))
a12_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(awareness2=="Not at all"))

a22_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
              &(awareness2=="its possible"))
a22_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(awareness2=="its possible"))

a32_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
              &(awareness2=="I probably will"))  
a32_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(awareness2=="I probably will")) 

a42_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
              &(awareness2=="I definitely will"))
a42_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(awareness2=="I definitely will"))

length(a12_1) + length(a12_2)
length(a22_1) + length(a22_2)
length(a32_1) + length(a32_2)
length(a42_1) + length(a42_2)



a13 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(awareness2=="Not at all"))

a23 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(awareness2=="its possible"))

a33 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(awareness2=="I probably will"))  

a43 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(awareness2=="I definitely will"))

length(a13)
length(a23)
length(a33)
length(a43)



  
# Data modification for awareness-preparedness (1)

b11 <- which ((preparedness=="Not prepared")
                  &(awareness1=="Not worried at all"))

b21 <- which ((preparedness=="Not prepared")
                  &(awareness1=="A little worried"))

b31 <- which ((preparedness=="Not prepared")
                  &(awareness1=="Somewhat worried"))  

b41 <- which ((preparedness=="Not prepared")
                  &(awareness1=="Very worried"))



length(b11)
length(b21)
length(b31)
length(b41)




b12 <- which ((preparedness=="A little prepared")
              &(awareness1=="Not worried at all"))

b22 <- which ((preparedness=="A little prepared")
              &(awareness1=="A little worried"))

b32 <- which ((preparedness=="A little prepared")
              &(awareness1=="Somewhat worried"))  

b42 <- which ((preparedness=="A little prepared")
              &(awareness1=="Very worried"))

length(b12)
length(b22)
length(b32)
length(b42)





b13 <- which ((preparedness=="Somewhat prepared")
              &(awareness1=="Not worried at all"))

b23 <- which ((preparedness=="Somewhat prepared")
              &(awareness1=="A little worried"))

b33 <- which ((preparedness=="Somewhat prepared")
              &(awareness1=="Somewhat worried"))  

b43 <- which ((preparedness=="Somewhat prepared")
              &(awareness1=="Very worried"))


length(b13)
length(b23)
length(b33)
length(b43)



b14 <- which ((preparedness=="Very prepared")
              &(awareness1=="Not worried at all"))

b24 <- which ((preparedness=="Very prepared")
              &(awareness1=="A little worried"))

b34 <- which ((preparedness=="Very prepared")
              &(awareness1=="Somewhat worried"))  

b44 <- which ((preparedness=="Very prepared")
              &(awareness1=="Very worried"))

length(b14)
length(b24)
length(b34)
length(b44)


# Data modification for awareness-preparedness (2)


c11 <- which ((preparedness=="Not prepared")
              &(awareness2=="Not at all"))

c21 <- which ((preparedness=="Not prepared")
              &(awareness2=="its possible"))

c31 <- which ((preparedness=="Not prepared")
              &(awareness2=="I probably will"))  

c41 <- which ((preparedness=="Not prepared")
              &(awareness2=="I definitely will"))

length(c11)
length(c21)
length(c31)
length(c41)




c12 <- which ((preparedness=="A little prepared")
              &(awareness2=="Not at all"))

c22 <- which ((preparedness=="A little prepared")
              &(awareness2=="its possible"))

c32 <- which ((preparedness=="A little prepared")
              &(awareness2=="I probably will"))  

c42 <- which ((preparedness=="A little prepared")
              &(awareness2=="I definitely will"))


length(c12)
length(c22)
length(c32)
length(c42)


c13 <- which ((preparedness=="Somewhat prepared")
              &(awareness2=="Not at all"))

c23 <- which ((preparedness=="Somewhat prepared")
              &(awareness2=="its possible"))

c33 <- which ((preparedness=="Somewhat prepared")
              &(awareness2=="I probably will"))  

c43 <- which ((preparedness=="Somewhat prepared")
              &(awareness2=="I definitely will"))

length(c13)
length(c23)
length(c33)
length(c43)


c14 <- which ((preparedness=="Very prepared")
              &(awareness2=="Not at all"))

c24 <- which ((preparedness=="Very prepared")
              &(awareness2=="its possible"))

c34 <- which ((preparedness=="Very prepared")
              &(awareness2=="I probably will"))  

c44 <- which ((preparedness=="Very prepared")
              &(awareness2=="I definitely will"))

length(c14)
length(c24)
length(c34)
length(c44)

  
# Data modification for awareness-related behaviors (1)

d11 <- which ((behav=="Not at all")
              &(awareness1=="Not worried at all"))

d21 <- which ((behav=="Not at all")
              &(awareness1=="A little worried"))

d31 <- which ((behav=="Not at all")
              &(awareness1=="Somewhat worried"))  

d41 <- which ((behav=="Not at all")
              &(awareness1=="Very worried"))

length(d11)
length(d21)
length(d31)
length(d41)


d12 <- which ((behav=="A little")
              &(awareness1=="Not worried at all"))

d22 <- which ((behav=="A little")
              &(awareness1=="A little worried"))

d32 <- which ((behav=="A little")
              &(awareness1=="Somewhat worried"))  

d42 <- which ((behav=="A little")
              &(awareness1=="Very worried"))

length(d12)
length(d22)
length(d32)
length(d42)


d13 <- which ((behav=="Some")
              &(awareness1=="Not worried at all"))

d23 <- which ((behav=="Some")
              &(awareness1=="A little worried"))

d33 <- which ((behav=="Some")
              &(awareness1=="Somewhat worried"))  

d43 <- which ((behav=="Some")
              &(awareness1=="Very worried"))

length(d13)
length(d23)
length(d33)
length(d43)


d14 <- which ((behav=="A lot")
              &(awareness1=="Not worried at all"))

d24 <- which ((behav=="A lot")
              &(awareness1=="A little worried"))

d34 <- which ((behav=="A lot")
              &(awareness1=="Somewhat worried"))  

d44 <- which ((behav=="A lot")
              &(awareness1=="Very worried"))

length(d14)
length(d24)
length(d34)
length(d44)


# Data modification for awareness-related behaviors (2)

e11 <- which ((behav=="Not at all")
              &(awareness2=="Not at all"))

e21 <- which ((behav=="Not at all")
              &(awareness2=="its possible"))

e31 <- which ((behav=="Not at all")
              &(awareness2=="I probably will"))  

e41 <- which ((behav=="Not at all")
              &(awareness2=="I definitely will"))

length(e11)
length(e21)
length(e31)
length(e41)



e12 <- which ((behav=="A little")
              &(awareness2=="Not at all"))

e22 <- which ((behav=="A little")
              &(awareness2=="its possible"))

e32 <- which ((behav=="A little")
              &(awareness2=="I probably will"))  

e42 <- which ((behav=="A little")
              &(awareness2=="I definitely will"))

length(e12)
length(e22)
length(e32)
length(e42)


e13 <- which ((behav=="Some")
              &(awareness2=="Not at all"))

e23 <- which ((behav=="Some")
              &(awareness2=="its possible"))

e33 <- which ((behav=="Some")
              &(awareness2=="I probably will"))  

e43 <- which ((behav=="Some")
              &(awareness2=="I definitely will"))

length(e13)
length(e23)
length(e33)
length(e43)


e14 <- which ((behav=="A lot")
              &(awareness2=="Not at all"))

e24 <- which ((behav=="A lot")
              &(awareness2=="its possible"))

e34 <- which ((behav=="A lot")
              &(awareness2=="I probably will"))  

e44 <- which ((behav=="A lot")
              &(awareness2=="I definitely will"))

length(e14)
length(e24)
length(e34)
length(e44)

# Data modification for knowledge-preparedness

f11 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(preparedness=="Not prepared"))

f21_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
              &(preparedness=="Not prepared"))
f21_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
              &(preparedness=="Not prepared"))  

f31 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(preparedness=="Not prepared"))

length(f11)
length(f21_1) + length(f21_2)
length(f31)



f12 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(preparedness=="A little prepared"))

f22_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(preparedness=="A little prepared"))
f22_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(preparedness=="A little prepared"))  

f32 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(preparedness=="A little prepared"))

length(f12)
length(f22_1) + length(f22_2)
length(f32)



f13 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(preparedness=="Somewhat prepared"))

f23_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(preparedness=="Somewhat prepared"))
f23_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(preparedness=="Somewhat prepared"))  

f33 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(preparedness=="Somewhat prepared"))

length(f13)
length(f23_1) + length(f23_2)
length(f33)



f14 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(preparedness=="Very prepared"))

f24_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(preparedness=="Very prepared"))
f24_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(preparedness=="Very prepared"))  

f34 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(preparedness=="Very prepared"))

length(f14)
length(f24_1) + length(f24_2)
length(f34)

  
# Data modification for knowledge-related behaviors


g11 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(behav=="Not at all"))

g21_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(behav=="Not at all"))
g21_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(behav=="Not at all"))  

g31 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(behav=="Not at all"))


length(g11)
length(g21_1) + length(g21_2)
length(g31)


g12 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(behav=="A little"))

g22_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(behav=="A little"))
g22_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(behav=="A little"))  
g32 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(behav=="A little"))

length(g12)
length(g22_1) + length(g22_2)
length(g32)





g13 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(behav=="Some"))

g23_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(behav=="Some"))
g23_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(behav=="Some"))  

g33 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(behav=="Some"))

length(g13)
length(g23_1) + length(g23_2)
length(g33)


g14 <- which ((knowledge1=="NO")&(knowledge2=="NO")
              &(behav=="A lot"))

g24_1 <- which ((knowledge1=="Yes")&(knowledge2=="NO")
                &(behav=="A lot"))
g24_2 <- which ((knowledge1=="NO")&(knowledge2=="Yes")
                &(behav=="A lot"))  

g34 <- which ((knowledge1=="Yes")&(knowledge2=="Yes")
              &(behav=="A lot"))

length(g14)
length(g24_1) + length(g24_2)
length(g34)

  
# Data modification for preparedness-related behaviors

h11 <- which ((behav=="Not at all")
              &(preparedness=="Not prepared"))

h21 <- which ((behav=="Not at all")
              &(preparedness=="A little prepared"))

h31 <- which ((behav=="Not at all")
              &(preparedness=="Somewhat prepared"))  

h41 <- which ((behav=="Not at all")
              &(preparedness=="Very prepared"))

length(h11)
length(h21)
length(h31)
length(h41)


h12 <- which ((behav=="A little")
              &(preparedness=="Not prepared"))

h22 <- which ((behav=="A little")
              &(preparedness=="A little prepared"))

h32 <- which ((behav=="A little")
              &(preparedness=="Somewhat prepared"))  

h42 <- which ((behav=="A little")
              &(preparedness=="Very prepared"))


length(h12)
length(h22)
length(h32)
length(h42)


h13 <- which ((behav=="Some")
              &(preparedness=="Not prepared"))

h23 <- which ((behav=="Some")
              &(preparedness=="A little prepared"))

h33 <- which ((behav=="Some")
              &(preparedness=="Somewhat prepared"))  

h43 <- which ((behav=="Some")
              &(preparedness=="Very prepared"))


length(h13)
length(h23)
length(h33)
length(h43)


h14 <- which ((behav=="A lot")
              &(preparedness=="Not prepared"))

h24 <- which ((behav=="A lot")
              &(preparedness=="A little prepared"))

h34 <- which ((behav=="A lot")
              &(preparedness=="Somewhat prepared"))  

h44 <- which ((behav=="A lot")
              &(preparedness=="Very prepared"))



length(h14)
length(h24)
length(h34)
length(h44)






