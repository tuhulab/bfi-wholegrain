%% Section 1-Read pre-processed data
filename = 'serum_neg_4.xlsx'; % pre-processed data
SampleList = 'serum_samplelist.xlsx'; % sample list label of the cdf files
mode = 'neg'; %data mode
dataform = '01.cdf'; %data format 
peakInt = 'peak height'; %intensity of the of the peaks
splitter = '_';
code_names = {'Subject','Time'}; %Labels
blank = 'NEGbl.mat';
%% Section 2-Create the dataset from pre-processed data
[dataplNEG,infosamples,repsamps,missingINsl,missingINd] = Arrange_Data_MZmine2(filename,SampleList,mode,dataform,peakInt,splitter,code_names);

%% Section 3-Group(barley or wheat intervention)
% compare the info from sample list and Diet codes
[n,t] = xlsread('DietCodes.xlsx'); % read the file with codes
t1=t(:,1);%add by Tu
t1=cell2str(t(:,1));%add by Tu
for i = 1 : length(t1);%add by Tu
   t2(i,:)=strsplit(t1(i,:),'-');%add by Tu
end;%add by Tu
t1=str2double(t2(:,1));%add by Tu

subj = dataplNEG.class{1,2};
usubj = unique(subj);

sub_codes = t1(:,1);%I replaced Gözde's code (n to t) because my code file does not contain any text

%gender = t(:,1);
%ugender = unique(gender);

test_eat = t(:,2);
utest_eat =unique(test_eat);

% problem:
for k=1:length(utest_eat)
    test_eatd(ismember(test_eat,utest_eat(k))) = k;
    %genderd(ismember(gender,ugender(k))) = k;
end

diet = cell(length(subj),1);
%g = cell(length(subj),1);

%gc = zeros(length(subj),1);
dietc = zeros(length(subj),1);

for k=1:length(usubj)
    id = find(subj==usubj(k));
    idc = find(sub_codes==usubj(k)); %BUG
    if ~isempty(idc)
        diet(id) = test_eat(idc);
        dietc(id) = test_eatd(idc);
        %g(id) = gender(idc);
        %gc(id) = genderd(idc);
    end
end

%% Section 4- Include Test eat information to the dataset object

dataplNEG.class{1,1} = dietc; 
dataplNEG.classname{1,1} = 'Test_eat'; 

%dataplPOS.class{1,4} = gc; 
%dataplPOS.classname{1,4} = 'Gender'; 

dataplNEG.label{1,6} = diet; 
dataplNEG.labelname{1,6} = 'Test_eat'; 

%dataplPOS.label{1,7} = g; 
%dataplPOS.labelname{1,7} = 'Gender'; 

%% clean the noise signals and apply percent rule
sample = dataplNEG;
a=load(blank);blank=a.NEGbl;clear a;
[data_cleaned, varargout]= clean_data(sample,1,blank,{''},5,10,{'Test_eat'},{0.7,15});
data_grouped= group_markers(data_cleaned,0.7,0.01);

%% seperate
%assigned_dataset = assign_markers2(data_cleaned,0.02,0.1,mode,'Method2');
%assigned_dataset = normaliz(assigned_dataset);
%data_cleass_NEG= group_markers(assigned_dataset,0.7,0.01);
%[data_training, data_test] = sep_testcal (data_grouped)

%%variable selection by plsda
SorPS=data_grouped.label{1,3};
for i = 1: length(SorPS)
   PS(i)=strcmp(SorPS(i,:),'S ');
end
dataPLS=data_grouped(PS);

data =dataPLS;
class = data.class{1,1}; %the class of the diet
subject = data.class{1,2};% class of the subject
rep = 12 

[calst,test_val] = varsel_test(data,class,rep,subject);

% ERav=mean(test_val.ER)
% 
% figure,plot(test_val.ER,'o')
% 
data=dataPLS
[dataselvar,mzrtselvarl,m10] = arrangevarsel(calst,9,data);

%% plot
datasel = sortby(dataselvar,'class',1,1);
mzrt = cell2mat(datasel.axisscale(2,1:2)')'
figure,plotgui(datasel)

%% find feature
targetmzrt =[
1.98	357.09
0.93	329.051
2.66	181.041
1.94	153.018
2.91	232.977
0.78	262.9874
1.19	511.0971
1.63	210.041

]

[datasel,varargout] = find_feature(dataplPOS,targetmzrt(:,2),targetmzrt(:,1),0.01,0.1,'mz',0)
mzrt = cell2mat(datasel.axisscale(2,1:2)')'
datasel = sortby(dataselvar,'class',1,1);
figure,plotgui(datasel)