 function [dataselvar,mzrtselvar,m10] = arrangevarsel(calst,th,dataPN)
% create  a dastesest for selecetd variables
% calst = structure array with selected variables from different models
% th = include features if they are detected 'th' number of models
mrall = [];
m10 = [];
for i=1:length(calst.beforeVS)
    temp = sortby(calst.afterVS{i}.data,'axisscale',2,3,'descend');    
    mr = cell2mat(temp.axisscale(2,1:2)')';
    mrall = [mrall;mr];  
    m10 = [m10,mr(1:10,1)];
end

[umrall,id] = unique(mrall(:,1));

for k=1:length(umrall);
    id(k) = sum(mrall(:,1)==umrall(k,1));
end
figure,hist(id,10)

%th  = 99% include the variables that are present in at least 2 of the models. You can change it
id_inc=id>th; 

mzrt_sig=umrall(id_inc,:);

mzrt1 = cell2mat(dataPN.axisscale(2,1:2)')';
id_org = ismember(mzrt1(:,1),mzrt_sig(:,1));


dataselvar = dataPN(:,id_org);
mzrtselvar =  mzrt1(id_org,:);


end

