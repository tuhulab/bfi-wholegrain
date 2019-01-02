
function free_field_pos = findfree( dataset, field, direction)

% I/O: free_field_pos = findfree( datasetset, field, direction)
%
%
%
%

%



switch field
    
    case 'label'
        labels = dataset.label(direction,:);
        free_field_pos = find(cellfun(@isempty, labels));
        
        if isempty(free_field_pos)
            free_field_pos = length(labels)+1;
             else
             free_field_pos = free_field_pos(1);
        end
      
        
                 
    case 'class'
        class = dataset.class(direction,:);
        free_field_pos = find(cellfun(@isempty, class));
       
        if isempty(free_field_pos)
            free_field_pos = length(class)+1;
        else
             free_field_pos = free_field_pos(1);
        end
        
    case 'axisscale'
        axs = dataset.axisscale(direction,:);
        free_field_pos = find(cellfun(@isempty,  axs));
        free_field_pos = free_field_pos(1);
        if isempty(free_field_pos)
            free_field_pos = length(axs)+1;
             else
             free_field_pos = free_field_pos(1);
        end
      
end
        
        %-------------------------------------------------------

end

