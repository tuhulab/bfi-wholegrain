    function [par_varsel] = varselcv(calnew,class1)
% perform variable selection using selectivity ratio and vip values
% INPUT 
% cal = dataset [nXm]
% class1 = classes [n,1]

% OUTPUT
% par_sel = provides the structure output for the selected variables

% date : 14.09.2016

optscv.preprocessing = 2;
optscv.plots = 'none';
optscv.structureoutput = 'yes';
optscv.discrim  = 'yes';
optscv.display ='no';
opts.plots = 'none';
opts.structureoutput = 'yes';
opts.discrim  = 'yes';
pre = preprocess('default', 'autoscale');
opts.preprocessing =pre;
opts.display ='no';
%opts.priorprob = [0.50 0.25];
%%

vip_th = 0.5;   
sel_th = 0.3;

avgmcit =[5;0];
check = -1;
n = 1;
idex=false(size(calnew,2),1);
clear model
size_cal = zeros(2,1);
nn =2;
callnew1 = cell(1,1);

while check(end)<0.1 && vip_th<1.1 %%&&  size(calnew,2)>200
    calnew = calnew(:,idex==0);
    callnew1{n} = calnew;
    result1{n} = crossval(calnew.data,class1,'sim',{'rnd' round(size(calnew,1)/10) 2},5,optscv);
    mm = mean(result1{n}.classerrcv);
   % disp(['---check = ' num2str(check) '---'])

    mmi = find(mm<0.01);
    [smm,is] = sort(mm);
    dsmm=diff(smm);
    
    id2=dsmm(1)<0.01;
    
    if any(id2) && is(2)<is(1)
        mi=is(2);
        avgm = mm(mi);
    else
        [avgm,mi] = min(mm);
    end
    
    avgmcit = [avgmcit,[mm(mi);mi]];
    
    model{n} = plsda(calnew.data,class1,mi,opts);
    vip1 = vip(model{n});
    calnew.axisscale{2,3} = vip1(:,1);
    calnew.axisscalename{2,3} = 'vip';
    selrat = model{n}.detail.selratio(1,:)';
    idex = vip1(:,1)<=vip_th & selrat<(max(selrat)*sel_th);
    size_cal =[size_cal;size(calnew,2)];
    dsize_cal = diff(size_cal);
    if all(dsize_cal(nn-1:nn)==0) || size(calnew,2)>100
        vip_th = vip_th+0.025;
        sel_th = sel_th+0.025;
 %       disp(['--- vip= ' num2str(vip_th) '---'])
  %      disp(['--- selr= ' num2str(sel_th) '---'])
        if vip_th>1.1
            disp('vip_th>1.1')
 %           disp(['--- vip= ' num2str(vip_th) 'so I break---'])
            break
        end
        nn=nn+1;
        disp(['--' num2str(size(calnew,2)) '--'])
        check = diff(avgmcit(1,:));
        n=n+1;
%     elseif all(dsize_cal(nn-1:nn)==0) && size(calnew,2)<200        
%         break
    elseif size(callnew1{n-1},2)<150 && check(end)<0.01 
        disp('size(callnew1{n-1},2)<150 && check(end)<0.01 ')
        n=n+1;
        nn=nn+1;
        break
    elseif size(callnew1{n-1},2)<20
        disp('size(callnew1{n-1},2)<20 ')

        n=n+1;
        nn=nn+1;
        break
    else
        nn=nn+1;
   %     disp(['--' num2str(size(calnew,2)) '--'])
        check = diff(avgmcit(1,:));
        n=n+1;
    end
end
%%
   
  % cross validated roc[X,Y,T,AUC1]
  class1 = class1';
 %  [Xa,Ya,Ta,AUC1] = perfcurve(class1(:,1),result1{n-1}.cvpred(:,1,avgmcit(2,end-1)),true(1,1));
  % AUCa(:,1) = AUC1;
   %[Xb,Yb,Tb,AUC1] = perfcurve(class1(:,1),result1{1}.cvpred(:,1,avgmcit(2,2)),true(1,1));
   %AUCb(:,1) = AUC1;
    par_varsel.afterVS.ER = avgmcit(:,n-1);
   %par_varsel.afterVS.ROC.AUC = AUCa;  
   %par_varsel.afterVS.ROC.X = Xa;  
   %par_varsel.afterVS.ROC.Y = Ya;
   %par_varsel.afterVS.ROC.T = Ta;
   par_varsel.afterVS.data = callnew1{n-2};   
   par_varsel.afterVS.plsdamodel = model{n-2};   
   
   par_varsel.beforeVS.ER = avgmcit(:,2);
  % par_varsel.beforeVS.ROC.AUC = AUCb;    
  % par_varsel.beforeVS.ROC.X = Xa;  
   %par_varsel.beforeVS.ROC.Y = Ya;
   %par_varsel.beforeVS.ROC.T = Ta;
