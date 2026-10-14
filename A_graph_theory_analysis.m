function graph_theory_analysis(input_file,output_dir)
    %% load correlation matrix
    corr_raw = load(input_file);
    corr_raw = corr_raw.FC;
    %% preprocessing
    mask = eye(size(corr_raw)) == 1 | corr_raw < 0;
    corr_raw(mask) = 0;
    corr_raw = double(corr_raw);
    %% 
    sparsity = [0.05:0.01:0.5];
    n_thres = length(sparsity);
    n_nodes = size(corr_raw, 1);
    
    meanFC = zeros(1, n_thres);  
    GE_norm = zeros(1, n_thres);
    LE_norm = zeros(n_nodes, n_thres);
    meanLE_norm = zeros(1, n_thres);
    NE_norm = zeros(n_nodes, n_thres);
    %% graph theory analysis under each sparsity
    for p = 1:n_thres
        %% Thresholding
        corr = threshold_proportional(corr_raw,sparsity(p));
        %% Normalisation
        corr_norm = corr/sum(corr(:));
        %% Calculate GE, LE, mean FCS using BCT and GRETNA
        meanFC(p) = mean(corr(corr > 0)); % mean FCS
        % GE_norm(p) = efficiency_wei(corr_norm, 0); % whole-brain global efficiency using BCT
        [GE_norm(p), NE_norm(:,p)] = gretna_node_global_efficiency_weight(corr_norm); % whole-brain and nodal global efficiency using Gretna
        LE_norm(:,p)= efficiency_wei(corr_norm, 2);% nodal local efficiency
        meanLE_norm(p) = mean(LE_norm(:,p));% whole-brain local efficiency
    end
%% Save results
network_metrics = struct( ...
    'meanFC', meanFC, ...
    'GE_norm', GE_norm, ...
    'LE_norm', LE_norm, ...
    'NE_norm', NE_norm, ...
    'meanLE_norm', meanLE_norm);

[~, filename, ~] = fileparts(input_file);
filename = filename(1:end-5);
output_file = fullfile(output_dir, sprintf('%s.mat', filename));
save(output_file, 'sparsity', 'network_metrics');
end