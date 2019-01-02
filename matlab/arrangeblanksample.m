filename = 'serumPOSbl.xlsx'; % pre-processed data
SampleList = 'samplelist.xlsx'; % sample list label of the cdf files
mode = 'pos'; %data mode
dataform = '01.cdf'; %data format 
peakInt = 'peak height'; %intensity of the of the peaks
%no_split_for_codes = 2; %split. seems useless??
splitter = '_';
code_names = {'Subject','Time'}; %Labels

[POSbl,infosamples,repsamps,missingINsl,missingINd] = Arrange_Data_MZmine2_blank(filename,SampleList,mode,dataform,peakInt,splitter,code_names);
