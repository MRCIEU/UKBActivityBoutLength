
resDir=getenv('RES_DIR');

allx = dataset('file', strcat(resDir, '/results-overall-swapping-',version,'-',type,'.csv'), 'delimiter', ',');



h=figure('DefaultAxesFontSize',14);

hold on;plot([-0.8,6.4], [1, 1], '--');

%baselines={'dur1sed';'dur2sed';'dur3sed'};
comparisons={'overall_classSleep';'overall_classSed';'overall_classWalk';'overall_classLight';'overall_classMod'};

colorx = {'[0.1 0.1 0.6]';'white';'magenta'; 'cyan'; 'white';'blue'};
linecolorx = {'[0.1 0.1 0.6]';'[0.5 0.8 0.0]';'magenta'; 'cyan'; '[0.8 0.2 0.2]';'blue'};
markersx = {'*';'o';'+';'x';'s';'^'};
markersizex = 11;

hx = [];

%ix = find(strcmp(allx.base,'avm')==1 & strcmp(allx.comp,'NULL')==1)
%posx = 0;
%hold on; h1=plot([posx,posx], [-allx.lower(ix), -allx.upper(ix)], '-', 'color', colorx{8});
%hold on; h1=plot(posx, -allx.beta(ix), markersx{8}, 'color', colorx{8});


for i=1:5
	basex = comparisons{i}

	count = 1;

	% all vs basex
	ix = find(strcmp(allx.base,'all')==1 & strcmp(allx.comp,basex)==1 & strcmp(allx.test, 'cox-hr')==1 & strcmp(allx.adjusted,'ALL')==1)

	% NB: these are 1/estimates because here the activity variable in the comparison not the baseline - we want to display as the baseline
	posx = i;
	hold on; h1=plot([posx,posx], [1/allx.lower(ix), 1/allx.upper(ix)], '-', 'color', colorx{6}, 'linewidth', 3);
        hold on; h1=plot(posx, 1/allx.beta(ix), markersx{6}, 'MarkerFaceColor', 'white', 'MarkerEdgeColor', colorx{6}, 'MarkerSize', markersizex);
	hx(1) = h1;


	for j=1:5
		if (j==i)
			continue;
		end

		compx = comparisons{j}

		ix = find(strcmp(allx.base,basex)==1 & strcmp(allx.comp,compx)==1 & strcmp(allx.test, 'cox-hr')==1 & strcmp(allx.adjusted,'ALL')==1);
		posx = i+(count)*0.1;

		hold on; h1=plot([posx,posx], [allx.lower(ix), allx.upper(ix)], '-', 'color', linecolorx{j}, 'linewidth', 3);
		hold on; h1=plot(posx, allx.beta(ix), markersx{j}, 'MarkerFaceColor', colorx{j}, 'MarkerEdgeColor', linecolorx{j}, 'MarkerSize', markersizex);
		
		hx(j+1) = h1;
		count = count + 1;
	end

end


labelsx = {'Sleep';'Sedentary';'Walk';'Light';'Moderate'};
set(gca,'XTickLabel', labelsx);

set(gca,'XTick', [1.4:1:5.4]);
set(gca,'XTickLabelRotation', 90);

labelsx = {'All';'Sleep';'Sedentary';'Walk';'Light';'MVPA'};
lx=legend(hx,labelsx, 'location', 'SouthEast');
lx.FontSize = 12;

ylim([0.8, 1.2]);
xlabel('Baseline activity category', 'FontSize', 14);

ylabel('Hazard ratio');

xlim([0.2, 6.4]);
set(gca, 'YScale', 'log');

saveas(h, strcat(resDir, '/fig-res-overall-',version,'-',type,'.pdf'));

