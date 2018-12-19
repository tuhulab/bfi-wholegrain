
function  [data,infosamples,repsamps,missingINsl,missingINd]= Arrange_Data_MZmine2(filename,SampleList,mode,dataform,peakInt,splitter,codenames)

% Blank data arrangement
%mode = 'pos';
%filename = 'NEGurine.xlsx';
%peakInt = 'peak height'; %only mzmine
%dataform = '01.cdf'; % only mzmine
%SampleList = 'newsamplelist.xls';
%no_split_for_codes = 3 %the number of rows for the codes
%splitter = %the deliminatar to split the codes
%OUTPUT
%dataset object with all codes added
%infosamples = labels of samples splitted in to column
%check the missing ATTENTION 2
%last modified 10-04-17 (upper lower case correction-file names are set lower case)
%%

% read data file
[n,t] = xlsread(filename);

% find (m/z,RT) combination; write them into mzrt variable
id = strcmp(t(1,:),'row m/z');
id2 = strcmp(t(1,:),'row retention time');
mzrt(:,1) = n(:,id);
mzrt(:,2) = n(:,id2);

% find intensity; write them into data
t = lower(t);
id = strfind(t,lower(peakInt));
id = ~cellfun(@isempty,id); %mark samples' position
data = n(:,id);

% find filename; write them into file (transposed)
file = lower(t(1,id)');
file = strrep(file,lower([dataform,' ', peakInt]),'');

% dateset (transposed)
data = dataset(data');

% label
data.label{1,1}         = file;
data.labelname{1,1}     = 'files';
data.axisscale{2,1}     = mzrt(:,1);
data.axisscalename{2,1} = 'Mass';
data.axisscale{2,2}     = mzrt(:,2);
data.axisscalename{2,2} = 'Retention Time';
data.label{2,2} = strread(num2str(mzrt(:,1)'),'%s');
data.labelname{2,1} = 'Retention time';
data.label{2,1} = strread(num2str(mzrt(:,2)'),'%s');
data.labelname{2,2} = 'mass';
data.labelname{1,1} = 'File name';

%% load sample list and check if something id odd
    [n1,t1,a1] = xlsread(SampleList);
    % remove mse files    
   
    id = [];
    idmse = strfind(strcat(t1(:,3)),'MSE');%why find mse?
    idinc_notmse = cellfun(@isempty,idmse);%return 1, 'cause everything is empty (no MSE)
      
    t1n = t1(idinc_notmse,:);    
    % remove blank std
    PSinSampleList = {'blank','metstd','standard'};
    clear idstuff % do not have idstuff before....
    
    for i=1:length(PSinSampleList)
        idstuff(:,i) = ~cellfun(@isempty,strfind(lower(t1n(:,2)),PSinSampleList{i}));        
    end
    
    idstuff = sum(idstuff,2);        
   % data_bl = data(ismember(file,t1n(idstuff(:,1)==1,1)),:);
    t1new = t1n(idstuff==0,:);          
    a1new = a1(idstuff==0,:);           
    posneg = t1new(:,3);
    modes = {'pos','neg'};
    
    clear modeinf1 modeinf
    for k = 1:length(modes)
        modeinf(~cellfun(@isempty,strfind(posneg,modes{k}))) = modes(k);    
        modeinf1(~cellfun(@isempty,strfind(posneg,modes{k}))) = k;
    end   
 
    if size(t1new,2)>3
        ids=[find((diff(modeinf1))==-1)';length(t1new)];
        idpl2 = [1;ids(1:length(ids)-1)+1];
        idpl = [idpl2,ids];
        plates = [];
        
        for k=1:size(idpl,1)
            plates(idpl(k,1):idpl(k,2)) = k;
        end
        
        t1new(:,4) = cellstr(num2str(plates'));
    else
    end
    posneg = t1new(:,3); 
    idmode = strfind(posneg,mode);    
    idmode2 = ~cellfun(@isempty,idmode);
    t1new = t1new(idmode2,:);
    a1new = a1new(idmode2,:);
    files = lower(t1new(:,1));
    samples = a1new(:,2);
    for i=1:length(samples)
         if ~iscellstr(samples(i))
             samples{i} = num2str(samples{i});
         end
    end
    
    if  size(t1new,2)>3
        plate = str2double(t1new(:,4));
    end
    
    if sum(strcmp(samples,''))>5
       disp('---------ATTENTION 1-------------')
       disp('--_Check the sample names--------')
       disp(['--Number of Empty rows in sample names =' num2str(sum(strcmp(samples,''))) '-----'])
       disp('--SAMPLE NAMES MAY BE NUMBERs----')
    else
    end    
 
    %% check if any sample is missing
    [~,ii] = setdiff(files,file');
    if ~isempty(ii)
        ex = t1new(ii,:);
        idmode = strfind(ex(:,3),mode);
        idmode = ~cellfun(@isempty,idmode);
        missingINd=ex(idmode,1:4);
        if length(missingINd)>1
            disp('---------ATTENTION 2-----------')
            disp('-------SOME SAMPLES ARE MISSING IN THE PROCEESSED LIST------')
            disp('-------CHECK MISSING OUTPUT-----------------------------')
        else
        end
    else
         missingINd = [];
    end
%%    
    [~,ii] = setdiff(file,files);
       
    missingINsl=file(ii,1);
    if length(missingINsl)>1
        disp('---------ATTENTION 3----------------------------------------')
        disp('ATTENTION!!!!!!ATTENTION!!!!ATTENTION!!!!ALARM!!!!---')
        disp('-------SOME SAMPLES ARE MISSING IN THE PROCEESSED LIST------')
        disp('-------CHECK MISSING OUTPUT----------------------------------')
    else
    end

    %% compare sample list file names with the MZmine processed data so add the sample names
    [~,j,k] = intersect(file,files);
    samples = lower(samples(k));
    files= files(k);
    % check if there is repeated sample codes
    [usamps,u] = unique(samples);
    r = setdiff(1:length(samples),u);
    if ~isempty(r)
        id = ismember(samples,samples(r));
        disp('-----ATTENTION 3--------------------------------------')
        disp('----SOME SAMPLES ARE REPEATED IN THE SAMPLE LIST------')
        disp('-----CHECK OUTPUT REPSAMPS----------------------------')
        repsamps = sortrows([samples(id),files(id)],1);
    else
        repsamps=[];
    end
        
    data = data(j,:);
    data.labelname{1,2} = 'Samples';
    data.label{1,2} = samples;
    
    if size(t1new,2)>3
        plate = plate(k);
        up = unique(plate);
        lp = [];
        for i = 1:length(up)
            lp(i) = sum(ismember(plate,up(i)));
        end
        
        
        figure
        plot(up,lp,'ro','MarkerFaceColor','r')
        title('Number of Samples in Each Plate')
        xlabel('Plate')
        ylabel('Number of samples')
        data.class{1,1} = plate;
        data.classname{1,1} = 'Plate';
      
    end
    %% add the PS sample labels
    PSinSampleList = {'Pool sample','urine','PS','pooled','ps','op','office','qc','processed qc','pool','plasma'};
    CorPS = {'PS','PS','PS','PS','PS','PS','PS','PS','PS','PS','PS'};    
   labelstuff = repmat({'S'},length(samples),1);
    for i=1:length( PSinSampleList)
        id = strncmp(samples,PSinSampleList(i),length(PSinSampleList{i}));
        labelstuff(id) = CorPS(i);
    end
    data.label{1,3} = labelstuff;
    data.labelname{1,3} = 'S and PS';
    
    
    %% add the codes  
    if splitter==0
        infosamples = samples;
        data.class{1,3} = str2double(infosamples);
        data.classname{1,3} = codenames;
    else
        
    samples = strrep(samples,'-',splitter);    
    sp = strfind(samples,splitter);
    ssp = cell2mat(cellfun(@(x) length(x),sp,'UniformOutput',false));
    infosamples = cell(length(samples),max(ssp)+1);
    for k=1:length(samples)
        if strcmp(labelstuff(k),'S')
            spl = strsplit(samples{k},splitter);
            infosamples(k,1:length(spl)) = spl;
        end
    end
      
    for k = 1:size(infosamples,2)
        data.label{1,k+3} = infosamples(:,k);
        data.labelname{1,k+3} = codenames(k);
        data.class{1,k+1} = str2double(infosamples(:,k));
        data.classname{1,k+1} = codenames(k);
    end
    end  
    
