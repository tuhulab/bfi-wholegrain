function [dataex,varidinc] = exfeatures(data,classnames,per,thr)

% [dataex] = exzerosnew(data,classnames,per,thr)
% Include the variables that are not present in at least 'per'(e.g. 70%) 
% of the samples in one of the sample groups. Thereshold is the baseline 
% considered as noise or background level
% 
% INPUT 
% data = data matrix (samples X features). dataset object
% per = ratio of . (if you want to use 80% than insert 0.8)
% classname = the classname that you specify for applying 80% rule. (e.g.
% 'Diet')
% thr = threshold 
% OUPUT
% dataex = markers are excl according to 'per' from data (dataset object)
% varidinc = included variables
%
%test
%data=dataex;
%classnames='Test_eat';
%per=0.7
%thr=15

id1 = strcmpi(data.classname(1,:),classnames);
class = data.class{1,id1}; 
classnam = unique(class(class~=0)); % don't consider class=0
varid = false( size( data, 2), length(classnam));
varid2 = true( size( data, 2), length(classnam));

for i = 1:length(classnam) 
    tempX = data.data( class == classnam(i),:); 
    tempid = sum( tempX > thr)/ size( tempX, 1);
    tempid = tempid > per;
    varid(tempid,i) = true;
    %varid2(tempid,i) = true;
end

varidinc = (sum(varid,2)>0);
varidex=(varidinc==0);
dataex = data(:,varidinc);
%varidex = (sum(varid,2)>1);
%ex = data(:,varid2);

end





