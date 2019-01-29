function [calst,test_val] = varsel_test(datasel,class,rep,subject)
%varsel_test is a function can choose approriate variables in PLSDA

%datasel: data to be analyzed
%class: which class will be used to distinguish two groups
%rep: number of subjects
%subject: subject

%% test
% data=dataPLS;
% class = data.class{1,1};
% subject = data.class{1,2};
% rep = 12;

%%
b = class;
ub = unique(b);
b2 = false(length(b),length(ub));

for i=1:length(ub)
    b2(b==ub (i),i) = true;
    idc(i) = sum(b2(:,i));
end

id_len= round(idc*20/100);%why idc=0.2*idc

if nargin==4
    uper = unique(subject);
end

test = [];
cal = [];

%%

opts.plots = 'none';
opts.structureoutput = 'yes';
opts.discrim  = 'yes';
pre = preprocess('default', 'autoscale');
opts.preprocessing = pre;
opts.display ='no';


if nargin==3
    
    for k = 1:rep
        idtest = [];
        for kk = 1:length(ub) % select test set
            idcp = find(b == ub(kk)); % find indexes of each class
            sel = id_len(kk);
            selr = randperm(length(idcp));
            id1 = idcp(selr(1:sel));
            idtest =[idtest; id1']; % take random samples from each class for test set
        end
        
        idcal = setdiff(1:size(datasel,1),idtest); % cal set indexes
        calc = b2(idcal,:); % dummy matrix cal
        testc = b2(idtest,:); % dummy info for test set
        cal = datasel(idcal,:); % datacal
        test = datasel(idtest,:); % datatest
        
        %apply variable selection
        par_varsel = varselcv(cal,calc);
        dataselvs = par_varsel.afterVS.data;
        model = par_varsel.afterVS.plsdamodel;
        %calst.datasel{k} = sortby(dataselvs,'axisscale',2,3,'descend');
        
        %         mz_sel = dataselvs.axisscale{2,1};
        %         rt_sel = dataselvs.axisscale{2,2};
        %         mz = test.axisscale{2,1};
        %         rt = test.axisscale{2,2};
        %         [~,im,jm] = intersect(mz_sel,mz,'stable');
        %         [~,ir,jr] = intersect(rt_sel,rt,'stable');
        %         j = intersect(jm,jr);
        %         testsel = test(:,j);
        
        mz_sel = dataselvs.axisscale{2,1};
        mz = test.axisscale{2,1};
        [~,i,j] = intersect(mz_sel,mz,'stable');
        testsel = test(:,j);
        
        valid1 = plsda(testsel.data,testc,model,opts);
        ER(:,k) = mean(valid1.detail.classerrp(:,par_varsel.afterVS.ER(2,1)));
        [Xv(:,k),Yv(:,k),Tv(:,k),AUCv(:,k)] = perfcurve(testc(:,1),valid1.pred{2}(:,1),true(1,1));
        calst.beforeVS{k} = par_varsel.beforeVS;
        calst.beforeVS{k}.data = testsel;
        calst.afterVS{k} = par_varsel.afterVS;
    end
    
elseif nargin==4 % if subject is also given
    kk=1;
    for k = 1:rep
        %         k% exclude one subject as test set each time
        %         sum(ismember(subject,uper(k)))
        if sum(ismember(subject,uper(k)))>1
            test = datasel(ismember(subject,uper(k)),:);
            testc = b2(ismember(subject,uper(k)),:);
            urem = setdiff(uper,uper(k));
            cal = datasel(ismember(subject,urem),:);
            calc = b2(ismember(subject,urem),:);
            
            %apply variable selection
            par_varsel = varselcv(cal,calc);
            dataselvs = par_varsel.afterVS.data;
            model = par_varsel.afterVS.plsdamodel;
            %calst.datasel{k} = sortby(dataselvs,'axisscale',2,3,'descend');
            calst.beforeVS{kk} = par_varsel.beforeVS;
            
            calst.afterVS{kk} = par_varsel.afterVS;
            
            mz_sel = dataselvs.axisscale{2,1};
            mz = test.axisscale{2,1};
            [~,i,j] = intersect(mz_sel,mz,'stable');
            testsel = test(:,j);
            
            valid1 = plsda(testsel.data,testc,model,opts)
            ER(:,kk) = mean(valid1.detail.classerrp(:,par_varsel.afterVS.ER(2,1)));
            [Xv(:,kk),Yv(:,kk),Tv(:,kk),AUCv(:,kk)] = perfcurve(testc(:,1),valid1.pred{2}(:,1),true(1,1));
            calst.beforeVS{kk} = par_varsel.beforeVS;
            calst.afterVS{kk} = par_varsel.afterVS;
            kk = kk+1;
        else
        end
    end
    
end 
    
    
    AUCb(:,1) = AUCv;
    test_val.ER = ER;
    test_val.ROC.AUC = AUCb;
    test_val.ROC.Xv = Xv;
    test_val.ROC.Yv = Yv;
    test_val.ROC.Tv = Tv;
    
    
    
    figure
    subplot(1,2,1)
    plot(ER,'bo')
    ylabel('ER')
    title('Test set Results (ER)')
    subplot(1,2,2)
    plot(AUCv,'bo')
    ylabel('AUC')
    title('Test set Results (AUC)')
    
    figure
    for k=1:length(calst.afterVS)
        num_sel(k) =size(calst.afterVS{k}.data,2);
    end
    plot(num_sel,'o')
    ylabel('Number of Selected Variables')
    
