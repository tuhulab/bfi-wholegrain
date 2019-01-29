%% selection of training and test set samples
function [data_training, data_test] = sep_testcal(data_grouped)
test_drink = data_grouped.class{1,1};
utd = unique(test_drink);
id_test = [];
for k= 1:length(utd)
    id = find(ismember(test_drink,utd(k)));
    sid = length(id);
    id2=randperm(sid)
    id_test = [id_test,id(id2(1:2))];
end
data_training = data_grouped(setdiff(1:length(test_drink),id_test),:);
data_test = data_grouped(id_test,:);
