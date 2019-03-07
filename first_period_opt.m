function opt=first_period_opt(X,PX,Y,PY,pen)

%INPUTS
%           [X] - values of X
%           [Y] - values of Y
%           [PX] - probabilities of X
%           [PY] - probabilities of Y
%           [pen] - handle for the penalty function

% OUTPUTS
%          [Z] - values of the signal
%           [opt] - returns a matrix with Z rows and X columns. For each
%           value of Z, a best response distribution on X is give (it is
%           either uniform or singular)
%           [PXZ] - returns a matrix with Z rows and X columns. This is the posterior distribution of X given Z

Z=(min(X)+min(Y)):(max(X)+max(Y));
f=zeros(length(Z),length(X)); 

Ptab=PY'*PX;
Zval=ones(length(Y),1)*X+Y'*ones(1,length(X));
PP=pen(X'*ones(1,length(X))-ones(length(X),1)*X);

for i=1:length(Z)
    %X(c) are possible values of x given z==Z(i)
    [r c]=find(Zval==Z(i));
    f(i,c)=Ptab(find(Zval==Z(i)))/sum(Ptab(find(Zval==Z(i))));
    bb=round(PP*f(i,:)');
    opt(i,find(bb==min(bb)))=1;
end

%PXZ=zeros(length(Z),length(X));

%This is the expected payoff in Period 1 for each Z and for each X1
%UZ=-PP*PXZ;

%for i=1:length(Z)
%    nval=min(7,Z(i)+7)-max(-7,Z(i)-7)+1;
%    PXZ(i,max(-7,Z(i)-7)+8:min(7,Z(i)+7)+8)=1/nval*ones(1,nval);
%end

oo=(1./sum(opt,2))*ones(1,length(X));
opt=opt.*oo;

