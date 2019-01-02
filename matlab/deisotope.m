function [deisotoped,MZRTde] = deisotope(data)

data = sortby(data,'axisscale',2,1,'ascend');
MZRT = [data.axisscale{2,1}' data.axisscale{2,2}'];

id = [];
a= [];
for k=1:size(data,2) %for each feature
    
    idmz1 = MZRT(:,1) <(MZRT(k,1)+1.1) & MZRT(:,1)>(MZRT(k,1)+0.99);
    % find the possibel isotopes within the range
    idmz2 = MZRT(:,1) <(MZRT(k,1)+2*1.1) & MZRT(:,1)>(MZRT(k,1)+2*0.99); % find the possibel isotopes within the range
    idmz =(idmz1+idmz2)==1;
    idrt = MZRT(:,2) <(MZRT(k,2)+0.008) & MZRT(:,2)>(MZRT(k,2)-0.008); % rt should be very close
    id2 = find(sum([idrt idmz],2)==2);
    if ~isempty(id2)
        id3 = data.data(:,id2)<repmat(data.data(:,k),1,size(id2,1)); %potential isotope should have lower intensity and the main peak (kth peak)
        id2 = id2(sum(id3)>(size(data,1)*0.7));
        if ~isempty(id2)
            c = corr(data.data(:,id2),repmat(data.data(:,k),1,size(id2,1)));
            id2 = id2(c(:,1)>0.68);
            if ~isempty(id2) % if the previous line counts for more than 80 % of samples
                id = [id id2']; % take id
                a = [a k];
            else
            end
        end
        
    else
    end
end
deisotoped=delsamps(data,unique(id),2,2);
MZRTde = MZRT;
MZRTde(id,:) = [];