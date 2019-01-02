function [datanew,dub] = dubremove_wo_corr(data,intm,intr)
% INPUTS
% data = dataset with MZ and RT as axisscales
% intm = limit of mass to define dublicates (e.g. 0.01)
% intr = limit of rt to define dublicates (e.g. 0.01)

% OUTPUTS
% data2 =dublicates removed;
% d1 = dataset with correlated features in the range, intm intr
% d2 = dataset with non correlated masses in the range
% mzrt1 = correlated features (just with m/z and rt) in the range, intm intr
% mzrt2 = non correlated features (just with m/z and rt) in the range, intm intr
% S1 = sums correlated dats
% S2 = sums non correlated data

% corrected by gozg : 11.08.17
%%
MZRT = [data.axisscale{2,1}' data.axisscale{2,2}']; % take mass and rt
%data.data(data.data==0)=NaN; % put NaN because corrcoeff works better
datanew = data;
idrem = [];
dub = [];
for i =1:length(MZRT)
 
    com = find(abs(MZRT(:,1)-MZRT(i,1))<intm & abs(MZRT(:,2)-MZRT(i,2))<intr);
  
    %com = intersect(idmz,idrt);
    com = com(~ismember(com,i));
    id1 = length(com);
    MZRT(com,:) = NaN;
    
    if id1>0 
   
        idasil = [com; i];
        %[~,idasil] = intersect(MZRT1(:,1),MZRT(com,1)); % find the fetaures in the original MZRT
        dd = data.data(:,idasil); % take data from original dataset
        [~,id] = max(dd,[],2);
        ddnew = dd(:,mode(id));
        for k=1:size(dd,2)
            ddnew(id==k) = dd(id==k,k);   
        end
        datanew.data(:,i) = ddnew;
        idrem = [idrem; idasil(~ismember(idasil,i))]; 
        dub = [dub;idasil];
        
    else
    end
    
end

%%
% remove dublicates from the dataset


if ~isempty(idrem)
    datanew = delsamps(datanew,idrem,2,2);
else
    datanew = datanew;
end

disp(['!!!!!Number of features removed = ' num2str(length(idrem)) '!!!!']);


