function [mat2 condX sigdist]=posterior_prob(X,PX,Y,PY,opt,pen)
%
% INPUTS
%           [X] - values of X
%           [Y] - values of Y
%           [PX] - probabilities of X
%           [PY] - probabilities of Y
%           [opt] - the first-period optimal strategy
% OUTPUTS
%        
%           [mat2] - optimal second answer given own signal Z and observed choice of other player, given that the other player acts
%          according to [opt]
%           [condX] - distribution of first-period guesses conditional on
%           state of the world. X corresponds to rows, X1 corresponds to
%           columns
%           [sigdist] - distribution of signals conditional on
%           state of the world. X corresponds to rows, Z1 corresponds to
%           columns

Z=(min(X)+min(Y)):(max(X)+max(Y));
PP=pen(X'*ones(1,length(X))-ones(length(X),1)*X);


sigdist=zeros(length(X),length(Z));
for i=1:length(X)
    sigdist(i,i:(i+length(Y)-1))=PY';
end

condX=sigdist*opt;
% condX(i,:) is the distribution of first-period guesses if X=X(i)


mat2=zeros(length(Z),length(X));

for i=1:length(Z)
    
    for ii=1:length(X)
        
        pp=zeros(1,length(X));
        for iii=1:length(X)
            pp(iii)=PX(iii)*condX(iii,ii)*sigdist(iii,i);
        end
        pp=pp/sum(pp);
        % pp is the conditional distribution of X given X2(ii) and Z(i)
        pen=round(PP*pp');
        mat2(i,ii)=mean(X(find(pen==min(pen))));
    end
end


