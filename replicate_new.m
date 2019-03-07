pen=@(x)10*min(abs(x),5);
PX=ones(1,15)/15
PY=ones(1,15)/15
Y=-7:7;
X=Y;
Z=-14:14;

opt=first_period_opt(X,PX,Y,PY,pen);

%Now calculate the 1-guess distribution
Zpred=zeros(length(X),length(Z));
for i=1:length(X)
    Zpred(i,i:(i+length(Y)-1))=PY;
end
x1pred=(PX*Zpred)*opt;

[mat2 condX sigdist]=posterior_prob(X,PX,Y,PY,opt,pen);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% TABLE WITH 2-PERIOD BEST RESPONSES %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mat2

ZXdist=zeros(length(Z),length(X));
for i=1:length(X)
ZXdist=ZXdist+PX(i)*sigdist(i,:)'*condX(i,:);
end
ZXdist1=reshape(ZXdist,length(X)*length(Z),1);
index=find(ZXdist1>0);
ZXdist1=ZXdist(index);
W=diag(ZXdist1);
YY=reshape(mat2,length(X)*length(Z),1);
YY=YY(index);
Z1=reshape(Z'*ones(1,length(X)),length(X)*length(Z),1);
Z1=Z1(index);
X21=reshape(ones(length(Z),1)*X,length(X)*length(Z),1);
X21=X21(index);
XX=[ones(length(index),1) Z1 X21 Z1.^2 X21.^2];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% COEFFICIENTS FOR BAYESIAN PLAYERS %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inv(XX'*W*XX)*XX'*W*YY


