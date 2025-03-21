CI <-
function (sample,data) {

score=0


ld=0.01
lo=0.1
if(data$nitrite[data$code==sample]<lo){
C=data$nitrite[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

ld=0.5
lo=45
if(data$nitrate[data$code==sample]<lo){
C=data$nitrate[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

ld=0.1
lo=7
if(data$phosphate[data$code==sample]<lo){
C= data$phosphate[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

ld=0.01
lo=0.5
if(data$ammonium[data$code==sample]<lo){
C=data$ammonium[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

ld=0.01
lo=1
if(data$tkn[data$code==sample]<lo){
C=data$tkn[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

ld=1
lo=300
if(data$dqo[data$code==sample]<lo){
C=data$dqo[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

ld=0
lo=39.10
if(data$dbo[data$code==sample]<lo){
C= data$dbo[data$code==sample]
componentscore=C/lo
score=score+componentscore
}else{
print('Sample is not acceptable as drinking water')}

if(data$organicmatter[data$code==sample]=="yes"){
score=score+1}

print(paste("Sample score: ",score))
if(score>=0 & score<= 2){
print('Sample is not contaminable')}
if(score>2 & score<= 4){
print('Sample is hardly contaminable')}
if(score>4 & score <= 6){
print('Sample is possibly contaminable')}
if(score>6 & score<= 8){
print('Sample is easily contaminable')}
}
