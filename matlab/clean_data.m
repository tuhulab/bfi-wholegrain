%% blank cleaning
function [dataex, varargout]= clean_data(sample,PS,data_bl,out_sample,th,a,classnames,varargin)
% INPUTS
% data = dataset
% data_bl = blank dataset
% PS =1 if you include pooled samples (else if you don't have PS you can put 0)
% out_sample = add the sample code (e.g.'5531-01-04') . If none type {''}
% th = noise level for removal of specific samples. used for calculating how many of the fatures are above
% th for each sample
% a = multiplicative factor of for confidence interval. for removal of
% samples
% classnames = classnames to apply percent rule

%
% OPTIONAL INPUTS (varargin)
% per =  the percentage of the samples that should be present with an signal higher than the threshold (the next optional input) at least one
% of the sample groups (default =0.7)
% th = noise level (default = 20)

% OUTPUT
% dataex = cleaned dataset

% Blank removal part modeified 13-07-16
% correction of removal of odd masses and percent rule percentage and
% threshold are added as optional input variables 12-12-16
% fixed mistake on matching pooled samples in the final dataset

%% test (by Tu)
% PS=1;
% load 'POSbl.mat'
% data_bl=blank;
% out_sample={''};
% th=5;
% a=10;
% classnames={'Test_eat'};
% varargin={0.7,15};
% sample=dataplNEG;
%% define number of input variables
numvarargs = length(varargin); % what?

%% CHECK if the axislabels and functions are correct

check_massl = find(strcmpi(sample.labelname(2,:),'mass'));
check_massa = find(strcmpi(sample.axisscalename(2,:),'mass'));

if any(check_massl~=2) || isempty(check_massl)
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    disp('Please add the mass information to the 2nd column of column labels')
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    return
elseif any(check_massa~=1) || isempty(check_massa)
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    disp('Please add the mass information to the 1st column of column axisscale')
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    return
end

check_rtl = find(strcmpi(sample.labelname(2,:),'retention time'));
check_rta = find(strcmpi(sample.axisscalename(2,:),'retention time'));

if check_rtl~=1
    disp('!!!!!READ ME!!!!!!!!')
    disp('Please add the RT information to the 1st column of column labels')
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    return
elseif check_rta~=2
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    disp('Please add the RT information to the 2nd column of column axisscale')
    disp('!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    return
end

%% Removal of features that are present in blanks.
if PS==1
    samplePS = sample.PS;
    sample = sample.S;
else    
end

if ~isdataset(data_bl)
    dataex = sample;
else
    mzrt = cell2mat(sample.axisscale(2,:)')';
    mzrt_bl = cell2mat(data_bl.axisscale(2,:)')';
    id2 = [];
    mztol = 0.006;
    rttol = 0.01;
    idb = [];
    for k=1:length(mzrt_bl)
        id1 = find(mzrt(:,1)<mzrt_bl(k,1)+mztol & mzrt(:,1)>mzrt_bl(k,1)-mztol &...
            mzrt(:,2)<mzrt_bl(k,2)+rttol & mzrt(:,2)>mzrt_bl(k,2)-rttol);
        if ~isempty(id1)
            
            [~,i] = min(sum(abs([mzrt_bl(k,:)'*ones(1,length(id1))]'-mzrt(id1,:)),2));
            id2 = [id2,id1(i)];
            idb = [idb,k];
            
        end
    end
    %idd = unique(id2);
    idex = median(sample.data(:,id2))<median(data_bl.data(:,idb))*1.5; % Remove only if they are present in blanks with 1.5 times higher intensity.
    idex = id2(idex);
    idinc = ~ismember(1:size(sample,2),idex);
    disp('------------BLANK_REMOVAL------------------')
    disp(['Number of features removed = ' num2str(length(unique(idex))) '--------'])
    % ass_data_excluded_bl = assign_markers2(datablex,0.02,0.08,'pos','new');
    % varargout{1} = ass_data_excluded_bl;
    dataex = sample(:,idinc);
    
end
%% remove the ones lower 0.3 and higher max_rt-0.5
mx_rt = max(dataex.axisscale{2,2})-0.5;
ex = (dataex.axisscale{2,2}>0.3 & dataex.axisscale{2,2}<mx_rt);
disp(['--------REMOVE rt<0.3 and rt>' num2str(mx_rt) '-------------'])
disp(['Number of features removed = ' num2str(size(dataex,2)-sum(ex)) '--------'])
dataex = dataex(:,ex);

%% remove the odd  early eluting compounds
rt =dataex.axisscale{2,2};
rt1 =0.3;
rt2 = 0.8;
idrt = find(rt>rt1 & rt<rt2);
mz = cellstr(dataex.label{2,2});
mzrt = mz(idrt,:);

nu = [];
for i=1:length(mzrt)
    idd = strfind(mzrt{i},'.');
    if ~isempty(idd)
        nu(i) = str2double(cellstr(mzrt{i}(idd+1)));
    end
end
idex=idrt(find(nu==4 | nu==5 | nu==6 | nu==7 | nu==8));

dataex = dataex(:,setdiff(1:size(dataex,2),idex));
disp('----REMOVE odd early eluting compounds-----------')
disp(['Number of features removed = ' num2str(length(idex)) '--------'])

%% deisotope
[data3,MZRTde] = deisotope(dataex) ;
disp('----REMOVE potential isotopes-----------')
disp(['Number of features removed = ' num2str(size(dataex,2)-size(data3,2)) '--------'])
dataex = data3;
%% remove dublicates
[dataex,dub] = dubremove_wo_corr(dataex,0.05,0.02);

%% Removal of odd samples
% For each sample, compute the number of features over a certain threshold
samp = cellstr(dataex.label{1,1});
initial_size = size(dataex,1);
if ~strcmp(out_sample,{''})
    id_ex = ~ismember(samp,out_sample);
    dataex = dataex(id_ex,:);
    fprintf(['Number of defined samples removed = ',num2str(initial_size-size(dataex,1)), '\n']);
    fprintf('----ExSubject= \n')
else
    disp('---no defined outlier samples---')
end

initial_size = size(dataex,1);
nb = zeros(size(dataex,1),1);
for i=1:size(dataex,1)
    nb(i) = length(find(dataex.data(i,:)>=th));
end
m = mean(nb);
s = std(nb);
% identify the samples with number if features within m+/-s
ids =[];
ex = [];
for i=1:size(dataex,1)
    if nb(i)>=(m-a*s) && nb(i)<=(m+a*s)
        ids = [ids i];
    else
        ex = [ex,i];
    end
end
% keep only those samples
disp('----REMOVE odd Samples-----------')
if ~isempty(ex)
    fprintf(['Number of samples removed = ',num2str(initial_size-length(ids)), '\n']);
    fprintf('----ExSubject= \n')
    dataex.label{1,2}(ex,:)
    dataex = dataex(ids,:);
else
    disp('---no subject is removed---')
end

varargout{2} = sample(setdiff(1:size(dataex,1),ids),:);


%% Percent rule
initial_size = size(dataex,2);
if all(~strcmp(classnames,''))
    
    if length(classnames)>1
        
        class_to_use = cellstr(strcat(classnames{:}));
        newclass = {''};
        for k=1:length(classnames)
            newclass = strcat(newclass,dataex.label{1,strcmpi(dataex.labelname(1,:),classnames(k))});
        end
        
        %num2str(dataex.class{1,strcmp(dataex.classname(1,:),classnames(2))}')));
        unewclass = unique(newclass);
        newclassN = zeros(length(newclass),1);
        for k=1:length(unewclass)
            newclassN(strcmp(newclass,unewclass(k)))= k;
        end
        free_label = length(dataex.labelname(1,:))+1;
        dataex.label{1,free_label} = newclass;
        dataex.labelname{1,free_label} = class_to_use;
        free_class = length(dataex.classname(1,:))+1;
        dataex.classname{1,free_class} = class_to_use;
        dataex.class{1,free_class} = newclassN;
        
    elseif strcmpi(classnames,'All')
        class_to_use = classnames;
        newclassN = ones(size(dataex,1),1);
        free_class = length(dataex.classname(1,:))+1;
        dataex.classname{1,free_class} = classnames;
        dataex.class{1,free_class} = newclassN;
    else
        class_to_use = classnames;
    end
    % exclude features
    if numvarargs>1
        per_ex = varargin(1);
        threshold = varargin(2);
    else
        per_ex = 0.7; % default
        threshold = 10; % default
    end
    dataex = exfeatures(dataex,class_to_use,per_ex,threshold);
    disp('---PERCENT RULE------------')
    fprintf(['Number of fatures removed = ',num2str(initial_size-size(dataex,2)), '\n']);
else
    disp('---PERCENT RULE is not applied------------')
end

%% Combine sample & ps
if PS==1
    [~,ide,idp] = intersect(dataex.axisscale{2,2},samplePS.axisscale{2,2},'stable');
    
    for k=1:length(sample.labelname(2,:))
        samplePS.label{2,k}={''};
        samplePS.labelname{2,k}={''};
    end
    
    dataex = [dataex;samplePS(:,idp)];
else
    
end
