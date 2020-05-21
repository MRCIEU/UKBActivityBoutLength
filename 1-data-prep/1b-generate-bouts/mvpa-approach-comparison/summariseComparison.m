

datadir = getenv('PROJECT_DATA');
resdir = getenv('RES_DIR');


vennfile = strcat(datadir, '/accel/derived/mvpaCompare/vennData.csv')
venndata = dataset('file', vennfile, 'delimiter', ',');


%%
%% checking

% check all rows sum to 2 days worth of minutes = 2880


venndata.total = venndata.bothMVPA + venndata.bothNotMVPA + venndata.predictedMVPAOnly + venndata.thresholdMVPAOnly;

ix = find(venndata.total ~= 2880);
fprintf('Number of row without 2880 hours (should be zero): %d \n', length(ix));

venndata(ix,:)

%%
%% normalise

venndata.bothMVPA = venndata.bothMVPA/2;
venndata.bothNotMVPA = venndata.bothNotMVPA/2;
venndata.predictedMVPAOnly = venndata.predictedMVPAOnly/2;
venndata.thresholdMVPAOnly = venndata.thresholdMVPAOnly/2;

fs=16;
yl=500;

% plot distribution for each of the for options

h = figure;
histogram(venndata.bothMVPA, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Number of minutes', 'FontSize', fs); ylabel('Num participants', 'FontSize', fs);
xlim([0 1440]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-both.pdf'));

h = figure;
histogram(venndata.bothNotMVPA, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Number of minutes', 'FontSize', fs); ylabel('Num participants', 'FontSize', fs);
xlim([0 1440]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-both-not.pdf'));

h = figure;
histogram(venndata.predictedMVPAOnly, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Number of minutes', 'FontSize', fs); ylabel('Num participants', 'FontSize', fs);
xlim([0 1440]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-pred-only.pdf'));

h = figure;
histogram(venndata.thresholdMVPAOnly, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Number of minutes', 'FontSize', fs); ylabel('Num participants', 'FontSize', fs);
xlim([0 1440]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-thres-only.pdf'));


%% calculate metrics

venndata.predTPR = venndata.bothMVPA./(venndata.bothMVPA+venndata.predictedMVPAOnly);
venndata.predTNR = venndata.bothNotMVPA./(venndata.bothNotMVPA+venndata.thresholdMVPAOnly);

venndata.threshTPR = venndata.bothMVPA./(venndata.bothMVPA+venndata.thresholdMVPAOnly);
venndata.threshTNR = venndata.bothNotMVPA./(venndata.bothNotMVPA+venndata.predictedMVPAOnly);


quantile(venndata.predTPR, [0, 0.25, 0.5, 0.75, 1])
quantile(venndata.predTNR, [0, 0.25, 0.5, 0.75, 1])
quantile(venndata.threshTPR, [0, 0.25, 0.5, 0.75, 1])
quantile(venndata.threshTNR, [0, 0.25, 0.5, 0.75, 1])

h = figure;
histogram(venndata.predTPR, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Proportion (both MVPA of those predicted MVPA)', 'FontSize', fs);ylabel('Num participants', 'FontSize', fs);
xlim([0 1]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-pred-tpr.pdf'));

h = figure;
histogram(venndata.predTNR, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Proportion (both not MVPA of those predicted not MVPA)', 'FontSize', fs);ylabel('Num participants', 'FontSize', fs);
xlim([0 1]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-pred-tnr.pdf'));

h = figure;
histogram(venndata.threshTPR, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Proportion (both MVPA of those threshold MVPA)', 'FontSize', fs); ylabel('Num participants', 'FontSize', fs);
xlim([0 1]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-thresh-tpr.pdf'));

h = figure;
histogram(venndata.threshTNR, 'FaceColor', '[0.8 0.0 0.8]', 'EdgeColor', '[0.4940 0.1840 0.5560]');
box off;
xlabel('Proportion (both not MVPA of those threshold not MVPA)', 'FontSize', fs);ylabel('Num participants', 'FontSize', fs);
xlim([0 1]); ylim([0 yl]);
ax = gca; ax.FontSize = fs;
saveas(h, strcat(resdir, '/venn-mvpa-thresh-tnr.pdf'));





bothyestotal = sum(venndata.bothMVPA)
bothnototal = sum(venndata.bothNotMVPA)
predonlytotal = sum(venndata.predictedMVPAOnly)
threshonlytotal = sum(venndata.thresholdMVPAOnly)

% macro averaged metrics

% TPR assuming predicted MVPA is ground truth
bothyestotal / (bothyestotal + predonlytotal)

% TPR assuming threshold MRVP is ground truth
bothyestotal / (bothyestotal + threshonlytotal)

% TNR assuming predicted MVPA is ground	truth 
bothnototal / (bothnototal + threshonlytotal)

% TNR assuming threshold MVPA is ground truth
bothnototal / (bothnototal + predonlytotal)
