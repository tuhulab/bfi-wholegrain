function assigned_dataset = group_markers(dataset,corr_tol,tr_tol)


%% Get the data for comparison
%Test (by Tu):
%dataset=data_cleaned
%corr_tol=0.7
%tr_tol=0.01

% I need to know where the retention time are in the dataset.
if ~any(any(ismember(lower(dataset.labelname),'retention time')))
    disp('I cannot find the retention times! Please read the help.')
end

for tr_loc = 1:size(dataset.labelname,2)
    if strcmpi(  dataset.labelname{2,tr_loc}  ,  'retention time')
        break
    end
end


% Retention time
tr      =   dataset.label{2,tr_loc};
tr      =   str2num(tr);



%% Matrix of comparisions fitting all criteria
% Get the correlations
dataset2 = dataset.data; dataset2(dataset.data==0)=NaN;
Xcorr = zeros(  length(tr)   ,    length(tr)  );

% Make logical matrix for things close enough
tr_close = false(  length(tr)   ,    length(tr)  );
for i = 1:length(tr)
    for i2 = 1:length(tr)
        
        % only calculate one side of the diagonal
        if i<=i2
            tr_close(i,i2) = 0;
        else
            tr_close(i,i2) = abs(tr(i)-tr(i2))<tr_tol;
        end
               
        
        if tr_close(i,i2) == 1
            Xcorr(i,i2) = corr(dataset2(:,i),dataset2(:,i2),'type','Pearson','rows','pairwise');
        end
        
        
    end
end
clear x y

Xcorr = tril(Xcorr,-1);
corr_matrix = Xcorr; corr_matrix(Xcorr==0)=NaN;Xcorr=corr_matrix;
Xcorr = Xcorr>corr_tol;                % Turn into logical matrix where the coefficients are higher thatn the tolerance set

% The markers need to be present together at least 50 % of the time.
num_vars = size(dataset.data,2);
together = false( num_vars   ,    num_vars  );

for i = 1:num_vars
    for i2 = 1:num_vars
        if Xcorr(i,i2) == 1
            if ~(i<=i2)    % Only one side of the diagonal
                x = dataset.data(:,i);
                y = dataset.data(:,i2);
                
                x_has = x ~= 0 & ~isnan(x);
                y_has = y ~= 0 & ~isnan(y);
                common = x ~= 0 & y ~= 0 & ~isnan(x) & ~isnan(y);
                
                if (   (sum(sum(common)) / sum(sum(x_has))) > 0.8...
                        || (sum(sum(common)) / sum(sum(y_has))) > 0.8   )...
                        && sum(sum(common)) > 9
                    
                    together(i,i2)  = 1;
                end
            end
            clear  x y x_has y_has common
        end
    end
end




% Matrix where all criteria are fulfilled
hit_temp = Xcorr==1 & tr_close==1 & together==1;

% In each row should be everything that correlates to the variables. i.e.
% as if we calculated on both sides of the diagonal.
hit = false(length(hit_temp),length(hit_temp));
for i = 1:length(hit)
    hit(i,:) = hit_temp(i,:)+hit_temp(:,i)';
end





%% Actual grouping
hit_old=false(size(hit));

while ~all(all(hit==hit_old))
    
    hit_old = hit;
    
    for i=1:length(hit)
        where   =   find(   hit(i,:)==1   );       % We find where the row has hits
        [toadd,~]  = find(   hit(:,where)   );     % where it has hits we find what those hits hit
        hit(i,toadd) = true;                       % We set everything to try in the row that was found in the line above
        hit(1:length(hit)+1:length(hit)*length(hit)) = 0;                 % we remove the diagonal
    end
end


hit(eye(size(hit))~=0)=1;                 % we add the diagonal


groups = false(size(hit));
for i=1:size(hit,1)
    index = find(hit(i,:));
    groups(index,i)=true;
end

groups = unique(groups,'rows')';                        % All rows have complete group info. Now we only want one set of each group.


groups   =    groups(:,    sum(groups)>1     );         % Only take groups where there is more than one member
groups   =    bsxfun(@times,1:size(groups,2),groups);   % give group numbers instead of 0/1
groups   =    sum(groups,2);                           % Collaps to vector






%% Writing the groups to the dataset file
% I need to know where to write the group info

free_label_pos = findfree( dataset, 'label', 2);

dataset.label{2,free_label_pos} = num2str(groups); % Put all numbers in a label

for i=1:size(groups,1)                             % Remove label for markers not in a group
    if groups(i)==0
        dataset.label{2,free_label_pos}{i} = ' ';
    end
end

dataset.labelname{2,free_label_pos} = 'Group';     % Give the label a name


% we also want it as a class
% I need to know where to write the group info
free_label_pos = findfree( dataset, 'class', 2);

dataset.class{2,free_label_pos}= groups;
dataset.classname{2,free_label_pos} = 'Group';


assigned_dataset = dataset;

        
     
end
