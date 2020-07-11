
resDir=getenv('RES_DIR');

allx = dataset('file', strcat(resDir, '/results-SEDENTARY-',version,'-',type,'.csv'), 'delimiter', ',');



h=figure('DefaultAxesFontSize',14);

hold on;plot([0.8,3.8], [1, 1], '--');

baselines={'dur1sed';'dur2sed';'dur3sed'};
comparisons={'dur1sed';'dur2sed';'dur3sed';'overall_classSleep';'overall_classLight';'overall_100mg'};

colorx = {'[0.5 0.8 0.0]';'[0.5 0.8 0.0]';'[1.0 1.0 1.0]'; '[0.1 0.1 0.6]'; 'cyan'; 'white'};
markerEdgecolorx = {'[0.5 0.8 0.0]';'[0.0 0.5 0.0]';'[0.2 0.5 0.0]'; '[0.1 0.1 0.6]'; 'cyan'; '[0.8 0.2 0.2]'};
linecolorx = {'[0.5 0.8 0.0]';'[0.5 0.8 0.0]';'[0.5 0.8 0.0]'; '[0.1 0.1 0.6]'; 'cyan'; '[0.8 0.2 0.2]'};

markersx = {'o';'o';'o';'*';'x';'s'};


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

			hold on; h1=plot([posx,posx], [allx.lower(ix), allx.upper(ix)], '-', 'color', linecolorx{j}, 'LineWidth', 3);
			hold on; h1=plot(posx, allx.beta(ix), markersx{j}, 'MarkerFaceColor', colorx{j}, 'MarkerEdgeColor', markerEdgecolorx{j}, 'MarkerSize', markersizex);

		end
		
		hx(j) = h1;
		count = count + 1;
	end

end


baselinelabels = {'Sed 1-15mins'; 'Sed 16-40mins'; 'Sed 41+mins'};
set(gca,'XTickLabel', baselinelabels);

set(gca,'XTick', [1.4;2.4;3.4]);
set(gca,'XTickLabelRotation', 90);

lx=legend(hx, {'Sed 1-15mins'; 'Sed 16-40mins'; 'Sed 41+mins'; 'Sleep';'Light';'MVPA'}, 'location', 'southeast');
%lx=legend(hx, comparisons, 'location', 'southeast');
lx.FontSize = 12;

ylim([0.6, 1.3]);
xlabel('Baseline sedentary bout length stratum', 'FontSize', 14);

ylabel('Hazard ratio');

xlim([0.8, 3.8]);
set(gca, 'YScale', 'log');

saveas(h, strcat(resDir, '/fig-res-SED-100mg-',version,'-',type,'.pdf'));

