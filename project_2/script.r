library(cluster)
x = read.csv("Live.csv");
x = x[4:12]


wss=rep(0,10)
for(k in 2:10){
    wss[k]=kmeans(x,k,nstart=20)$tot.withinss
}
wss[1]=wss[2]
plot(wss,type="b",pch=20)




# ----- KMEANS -----
x.km=kmeans(x,3,nstart=20)
x.pca=princomp(scale(x))
plot(x.pca$scores,col=1+x.km$cluster,pch=20)
plot(silhouette(x.km$cluster,dist(x)))


as=rep(0,10)
for(k in 2:10){
    cl=kmeans(x,k,nstart=20)$cluster
    as[k]=mean(silhouette(cl,dist(x))[,3])
}
plot(as,type="b",pch=20)




# ----- PAM -----
pm=pam(x,3)
plot(x.pca$scores,col=pm$cluster,pch=12)
layout(t(1:2))
plot(pm)
layout(1)





sad = rep(0,3);
nsad = rep(0,3);
angry = rep(0,3);
nangry = rep(0,3);
love = rep(0,3);
nlove = rep(0,3);
react = rep(0,3);
nreact = rep(0,3);
ahah = rep(0,3);
nahah = rep(0,3);
like = rep(0,3);
nlike = rep(0,3);
wow = rep(0,3);
nwow = rep(0,3);
comment = rep(0,3);
ncomment = rep(0,3);
for(i in 1:length(x[,1])){
    
    sad[x.km$cluster[i]] = sad[x.km$cluster[i]] + x$num_sads[i];
    nsad[x.km$cluster[i]] = nsad[x.km$cluster[i]] + 1;
    angry[x.km$cluster[i]] = angry[x.km$cluster[i]] + x$num_angrys[i];
    nangry[x.km$cluster[i]] = nangry[x.km$cluster[i]] + 1;
    love[x.km$cluster[i]] = love[x.km$cluster[i]] + x$num_loves[i];
    nlove[x.km$cluster[i]] = nlove[x.km$cluster[i]] + 1;
    react[x.km$cluster[i]] = react[x.km$cluster[i]] + x$num_reactions[i];
    nreact[x.km$cluster[i]] = nreact[x.km$cluster[i]] + 1;
    ahah[x.km$cluster[i]] = ahah[x.km$cluster[i]] + x$num_hahas[i];
    nahah[x.km$cluster[i]] = nahah[x.km$cluster[i]] + 1;
    like[x.km$cluster[i]] = like[x.km$cluster[i]] + x$num_likes[i];
    nlike[x.km$cluster[i]] = nlike[x.km$cluster[i]] + 1;
    wow[x.km$cluster[i]] = wow[x.km$cluster[i]] + x$num_wows[i];
    nwow[x.km$cluster[i]] = nwow[x.km$cluster[i]] + 1;
    comment[x.km$cluster[i]] = comment[x.km$cluster[i]] + x$num_comments[i];
    ncomment[x.km$cluster[i]] = ncomment[x.km$cluster[i]] + 1;
    
}


if(FALSE){
    if(x$num_sads[x.km$cluster[i]] > 0){
        print(i)
        print(x$num_sads[x.km$cluster[i]])
    }
    
    
    
    
j = 0;
for(i in 1:length(x[,1])){
    
    num = x$num_loves + x$num_wows + x$num_hahas + x$num_sads + x$num_angrys;
    if(num > x$num_reactions){
        j = j +1;
    }
}    
    

    
}


sad[1] / nsad[1]
sad[2] / nsad[2]
sad[3] / nsad[3]
sad[4] / nsad[4]
andgry[1] / nangry[1]
angry[1] / nangry[1]
angry[2] / nangry[2]
angry[3] / nangry[3]
angry[4] / nangry[4]
love[1] / nlove[1]
love[2] / nlove[2]
love[3] / nlove[3]
love[4] / nlove[4]
react[1] / nreact[1]
react[2] / nreact[2]
react[3] / nreact[3]
react[4] / nreact[4]
ahah[1] / nahah[1]
ahah[2] / nahah[2]
ahah[3] / nahah[3]
ahah[4] / nahah[4]
like[1] / nlike[1]
like[2] / nlike[2]
like[3] / nlike[3]
like[4] / nlike[4]
wow[1] / nwow[1]
wow[2] / nwow[2]
wow[3] / nwow[3]
wow[4] / nwow[4]
comment[1] / ncomment[1]
comment[2] / ncomment[2]
comment[3] / ncomment[3]
comment[4] / ncomment[4]
