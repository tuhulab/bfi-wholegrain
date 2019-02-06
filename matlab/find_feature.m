function [datasel,varargout] = find_feature(data,mz,rt,masstol,rttol,first_class,group)

% data = dataset object that you search for a specific feature(data.axisscale{2,1:2} should include masses and retention times) 
% mz = m/z of the feature you are trying to find
% rt = rt of the feature you are trying to find
% masstol = mass tolerance 
% rttol = retention time tolerance
% first_class = either 'mz' or 'rt', as the label of the fatures
% group = include the grouped features (only matlab grouping) 1: include

% 19-04-2017
% 10-11-2017 line 33

mzrt = cell2mat(data.axisscale(2,1:2)')';
idd2 = zeros(size(data,2),5);
idd = [];
i=1;
idmzrt = [];
for k=1:length(mz)
    id = find(abs(mzrt(:,1)-mz(k,1)*ones(length(mzrt),1))<masstol &...
        abs(mzrt(:,2)-rt(k,1)*ones(length(mzrt),1))<rttol);
    if ~isempty(id) 
        idd2(i,1:length(id)) = id';
        idd = [idd;id];
        idmzrt = [idmzrt;k*ones(length(id),1)];
        i = i+1;
    else    
    end
end
varargout{1} = unique(idd); % index of data
varargout{2} = idmzrt;%index of mzrt
varargout{3} = idd2(idd2(:,1)~=0);
datasel = data(:,idd);% changed from datasel = data(:,unique(idd));
if ~isempty(datasel)
if ismember(group,1)
    id=ismember(datasel.classname(2,:),'Group');
    g = datasel.class{2,id};    
    idc = ismember(datasel.classname(2,:),'camera_groups' );    
    gc = datasel.class{2,idc};  
    if any(g~=0) && length(gc)==1
        
       gd = data.class{2,id}; 
       idg = ismember(gd,g);
       datasel = data(:,idg);
    elseif any(g==0) && length(gc)>1
        %k2
       gd = data.class{2,idc}; 
       idgc = ismember(gd,gc);
       datasel = data(:,idgc);   
    elseif any(g~=0) && length(gc)>1
       % k3
       gd = data.class{2,idc}; 
       idg = ismember(gd,g);
       idgc = ismember(gd,gc);
       datasel = data(:,idgc+idg>0);   
    else
    disp('Not any other related compounds')
    end
end
end
if ~isempty(datasel)
    if ismember(first_class,'mz')
        datasel.label{2,1} = datasel.label{2,2};
    else
    end
end

    