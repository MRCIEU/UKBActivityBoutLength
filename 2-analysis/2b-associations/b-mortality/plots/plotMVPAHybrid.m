
resDir=getenv('RES_DIR');

allx = dataset('file', strcat(resDir, '/results-MVPA-',version,'-',type,'.csv'), 'delimiter', ',');


h=figure('DefaultAxesFontSize',14);

hold on;plot([0.8,3.8], [1, 1], '--');

baselines={'dur1mod100';'dur2mod100';'dur3mod100'};
comparisons={'dur1mod100';'dur2mod100';'dur3mod100';'overall_classSleep';'overall_classSed';'overall_classLight'};

colorx = {'[0.8 0.2 0.2]';'[0.8 0.2 0.2]';'[1.0 1.0 1.0]'; '[0.1 0.1 0.6]'; 'white'; 'cyan'};
markerEdgecolorx = {'[0.8 0.2 0.2]';'[0.4 0.0 0.0]';'[0.4 0.0 0.2]'; '[0.1 0.1 0.6]'; '[0.5 0.8 0.0]'; 'cyan'};
linecolorx = {'[0.8 0.2 0.2]';'[0.8 0.2 0.2]';'[0.8 0.2 0.2]'; '[0.1 0.1 0.6]'; '[0.5 0.8 0.0]'; 'cyan'};

markersx = {'s';'s';'s';'*';'o';'x'};
markersizex = 11;



hx = [];

for i=1:3
	basex = baselines{i};

	count = 1;

	for j=1:6

		posx = i+(count-1)*0.1;

		if (j==i)
			hold on; h1=plot(posx, 1, markersx{j}, 'MarkerFaceColor', colorx{j}, 'MarkerEdgeColor', markerEdgecolorx{j}, 'MarkerSize', markersizex);
		else
			compx = comparisons{j};

			ix = find(strcmp(allx.base,basex)==1 & strcmp(allx.comp,compx)==1 & strcmp(allx.test, 'cox-hr')==1 & strcmp(allx.adjusted,'ALL')==1);

			hold on; h1=plot([posx,posx], [allx.lower(ix), allx.upper(ix)], '-', 'color', linecolorx{j}, 'linewidth', 3);
			hold on; h1=plot(posx, allx.beta(ix), markersx{j}, 'MarkerFaceColor', colorx{j}, 'MarkerEdgeColor', markerEdgecolorx{j}, 'MarkerSize', markersizex);

		end
		
		hx(j) = h1;
		count = count + 1;

	end

end

labelx = {'MVPA 1-15mins'; 'MVPA 16-40mins'; 'MVPA 41+mins'};
set(gca,'XTickLabel', labelx);

set(gca,'XTick', [1.4;2.4;3.4]);
set(gca,'XTickLabelRotation', 90);

lx=legend(hx, {'MVPA 1-15mins'; 'MVPA 16-40mins'; 'MVPA 41+mins'; 'Sleep'; 'Sedentary'; 'Light'}, 'location', 'south');
lx.FontSize = 12;

ylim([0.6, 1.3]);
xlabel('Baseline MVPA bout length stratum', 'FontSize', 14);

ylabel('Hazard ratio');

xlim([0.8, 3.8]);
set(gca, 'YScale', 'log');

saveas(h, strcat(resDir, '/fig-res-MVPA-',version,'-',type,'.pdf'));

