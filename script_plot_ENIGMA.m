
% IMPORTANTE: para usar este script hay que haberse descargado la toolbox 
% 'MICA/MNI/ENIGMA' para matlab y establecerla en el path

% SCRIP PARA REPRESENTAR LOS PLOTS DEL ANALISIS DE CENTILES BASALES PAFIP 
% CON LAS FIGURAS CEREBRALES DEL CONSORCIO 'ENIGMA'

%% PLOT RAW BASELINE CENTILES

% CORTICAL

filename = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/data_cortical_centils_352.csv';  % Specify the filename
data.c = readmatrix(filename); % Read the data into a matrix

% Map parcellated data to the surface
CT_d_fsa5 = parcel_to_surface(data.c, 'aparc_fsa5');

% Project the results on the surface brain
f = figure,
    plot_cortical(CT_d_fsa5, 'surface_name', 'fsa5', 'color_range', ...
                  [0.3 0.7], 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_centiles/352 participants';

% Combine folder path and filename
filename = fullfile(folderPath, 'BL_centiles_cortical.png');

% Save the plot as PNG
print(filename, '-dpng', '-r300');


% SUBCORTICAL

filename = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/data_subcortical_centils_352.csv';  % Specify the filename
data.s = readmatrix(filename); % Read the data into a matrix

% Project the results on the surface brain
f = figure,
    plot_subcortical(data.s, 'color_range', [0.3 0.7], 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_centiles/352 participants';

% Combine folder path and filename
filename = fullfile(folderPath, 'BL_centiles_subcortical.png');

% Save the plot as PNG
print(filename, '-dpng', '-r300');

%% PLOT LME CLINICAL INTERACTION (TIME x BASELINE CENTILES)

% CORTICAL

% Define the variables as a cell array of strings
variables = ["SANS_affect_flatt", "SANS_alogia", "SANS_avolition", "SANS_anhedonia", ...
             "SANS_attention", "SANS_total", "SAPS_hallucinations", "SAPS_delusions", ...
             "SAPS_bizarre_behav", "SAPS_formal_thought", "SAPS_total"];

% Loop through each variable
for n = 1:length(variables)
    x = variables{n};
    % Specify the filenames
    filename_beta = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_interaction_beta.csv'];
    filename_sig = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_interaction_sig.csv'];

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Map parcellated data to the surface
CT_d_fsa5 = parcel_to_surface(beta_c, 'aparc_fsa5');

% Set color range based on variable name
if x == "SAPS_hallucinations" || x == "SAPS_delusions" || x == "SAPS_bizarre_behav" || x == "SAPS_formal_thought"
    color_range = [-8 8];
elseif x == "SAPS_total"
    color_range = [-12 12];
else
    color_range = [-5 5];
end

% Project the results on the surface brain
f = figure,
    plot_cortical(CT_d_fsa5, 'surface_name', 'fsa5', 'color_range', ...
                  color_range, 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/INTERACTION';

% Combine folder path and filename
filename = fullfile(folderPath, [char(x) '_interaction.png']); % Convert x to char for concatenation

% Save the plot as PNG
print(filename, '-dpng', '-r300');

end


% SUBCORTICAL

% Define the variables as a cell array of strings
variables = ["SANS_affect_flatt", "SANS_alogia", "SANS_avolition", "SANS_anhedonia", ...
             "SANS_attention", "SANS_total", "SAPS_hallucinations", "SAPS_delusions", ...
             "SAPS_bizarre_behav", "SAPS_formal_thought", "SAPS_total"];

% Loop through each variable
for n = 1:length(variables)
    x = variables{n};
    % Specify the filenames
    filename_beta = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_interaction_beta_subcortical.csv'];
    filename_sig = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_interaction_sig_subcortical.csv'];

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Set color range based on variable name
if x == "SAPS_hallucinations" || x == "SAPS_delusions" || x == "SAPS_bizarre_behav" || x == "SAPS_formal_thought"
    color_range = [-8 8];
elseif x == "SAPS_total"
    color_range = [-12 12];
else
    color_range = [-5 5];
end

% Project the results on the surface brain
f = figure,
    plot_subcortical(beta_c, 'color_range', color_range, 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/+SUBCORTICAL/INTERACTION';

% Combine folder path and filename
filename = fullfile(folderPath, [char(x) '_interaction_subcortical.png']); % Convert x to char for concatenation

% Save the plot as PNG
print(filename, '-dpng', '-r300');

end

%% PLOT LME GAF INTERACTION (TIME x BASELINE CENTILES)

% CORTICAL

% Specify the filenames
filename_beta = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_interaction_beta.csv';
filename_sig = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_interaction_sig.csv';

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Map parcellated data to the surface
CT_d_fsa5 = parcel_to_surface(beta_c, 'aparc_fsa5');

% Project the results on the surface brain
f = figure,
    plot_cortical(CT_d_fsa5, 'surface_name', 'fsa5', 'color_range', ...
                  [-8 8], 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF';

% Combine folder path and filename
filename = fullfile(folderPath, 'GAF_interaction_cortical.png');

% Save the plot as PNG
print(filename, '-dpng', '-r300');

% SUBCORTICAL

% Specify the filenames
filename_beta = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_interaction_beta_subcortical.csv';
filename_sig = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_interaction_sig_subcortical.csv';

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Project the results on the surface brain
f = figure,
    plot_subcortical(beta_c, 'color_range', [-8 8], 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF';

% Combine folder path and filename
filename = fullfile(folderPath, 'GAF_interaction_subcortical.png');

% Save the plot as PNG
print(filename, '-dpng', '-r300');


%% PLOT LME CLINICAL BL CENTILES

% CORTICAL

% Define the variables as a cell array of strings
variables = ["SANS_affect_flatt", "SANS_alogia", "SANS_avolition", "SANS_anhedonia", ...
             "SANS_attention", "SANS_total", "SAPS_hallucinations", "SAPS_delusions", ...
             "SAPS_bizarre_behav", "SAPS_formal_thought", "SAPS_total"];

% Loop through each variable
for n = 1:length(variables)
    x = variables{n};
    % Specify the filenames
    filename_beta = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_BL_centils_beta.csv'];
    filename_sig = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_BL_centils_sig.csv'];

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Map parcellated data to the surface
CT_d_fsa5 = parcel_to_surface(beta_c, 'aparc_fsa5');

% Set color range based on variable name
if x == "SAPS_hallucinations" || x == "SAPS_delusions" || x == "SAPS_bizarre_behav" || x == "SAPS_formal_thought"
    color_range = [-8 8];
elseif x == "SAPS_total"
    color_range = [-8 8];
else
    color_range = [-5 5];
end

% Project the results on the surface brain
f = figure,
    plot_cortical(CT_d_fsa5, 'surface_name', 'fsa5', 'color_range', ...
                  color_range, 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/BL_CENTILES';

% Combine folder path and filename
filename = fullfile(folderPath, [char(x) '_BL_centils.png']); % Convert x to char for concatenation

% Save the plot as PNG
print(filename, '-dpng', '-r300');

end


% SUBCORTICAL

% Define the variables as a cell array of strings
variables = ["SANS_affect_flatt", "SANS_alogia", "SANS_avolition", "SANS_anhedonia", ...
             "SANS_attention", "SANS_total", "SAPS_hallucinations", "SAPS_delusions", ...
             "SAPS_bizarre_behav", "SAPS_formal_thought", "SAPS_total"];

% Loop through each variable
for n = 1:length(variables)
    x = variables{n};
    % Specify the filenames
    filename_beta = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_BL_centils_beta_subcortical.csv'];
    filename_sig = ['/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/' x '_BL_centils_sig_subcortical.csv'];

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Set color range based on variable name
if x == "SAPS_hallucinations" || x == "SAPS_delusions" || x == "SAPS_bizarre_behav" || x == "SAPS_formal_thought"
    color_range = [-8 8];
elseif x == "SAPS_total"
    color_range = [-8 8];
else
    color_range = [-5 5];
end

% Project the results on the surface brain
f = figure,
    plot_subcortical(beta_c, 'color_range', color_range, 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_symptoms/+SUBCORTICAL/BL_CENTILES';

% Combine folder path and filename
filename = fullfile(folderPath, [char(x) '_BL_centils_subcortical.png']); % Convert x to char for concatenation

% Save the plot as PNG
print(filename, '-dpng', '-r300');

end

%% PLOT GAF LME BASELINE CENTILES

% CORTICAL

% Specify the filenames
filename_beta = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_BL_centils_beta.csv';
filename_sig = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_BL_centils_sig.csv';

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Map parcellated data to the surface
CT_d_fsa5 = parcel_to_surface(beta_c, 'aparc_fsa5');

% Project the results on the surface brain
f = figure,
    plot_cortical(CT_d_fsa5, 'surface_name', 'fsa5', 'color_range', ...
                  [-8 8], 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF';

% Combine folder path and filename
filename = fullfile(folderPath, 'GAF_BL_centils_cortical.png');

% Save the plot as PNG
print(filename, '-dpng', '-r300');

% SUBCORTICAL

% Specify the filenames
filename_beta = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_BL_centils_beta_subcortical.csv';
filename_sig = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/export_matlab/GAF_BL_centils_sig_subcortical.csv';

% Read the data into matrices
beta_c = readmatrix(filename_beta); % Read the beta data into a matrix

% Read the sig data into a table
sig_table = readtable(filename_sig); 

% Convert the table to a cell array if it contains non-numeric data
sig_cell = table2cell(sig_table);

% Convert cell array of 'true'/'false' strings to a logical array
sig_logical = false(size(sig_cell));
for i = 1:numel(sig_cell)
    if ischar(sig_cell{i}) || isstring(sig_cell{i})
        sig_logical(i) = strcmp(sig_cell{i}, 'TRUE');
    elseif isnumeric(sig_cell{i})
        sig_logical(i) = sig_cell{i} ~= 0;
    end
end

% Convert elements in beta.c to 0 where sig_logical is false
beta_c(~sig_logical) = 0;

% Traspose beta vector
beta_c = beta_c';

% Project the results on the surface brain
f = figure,
    plot_subcortical(beta_c, 'color_range', [-8 8], 'cmap', 'RdBu_r')

% Specify the folder path where we want to save the plot
folderPath = '/Users/manuelalejandromunozcaracuel/Documents/RIO HORTEGA/Neuroimagen/Images_GAF';

% Combine folder path and filename
filename = fullfile(folderPath, 'GAF_BL_centils_subcortical.png');

% Save the plot as PNG
print(filename, '-dpng', '-r300');

