
% makes id lists of 1000 people


dataDir=getenv('PROJECT_DATA');
sample = dataset('file', strcat(dataDir, '/phenotypes/derived/analysis-dataset-subset.csv'), 'delimiter', ',');

userIds = double(sample.eid);

length(userIds)

numPeople = length(userIds);

rng(1000);
sizeSubs=1000;

for i=1:sizeSubs:numPeople

	% start and end indexes
	sIdx = i;
	eIdx = i+sizeSubs-1;
	if (eIdx>numPeople)
		eIdx=numPeople;
	end

	inSampleUserIds = userIds(sIdx:eIdx);
	dlmwrite(strcat(dataDir, '/accel/samples/sample',num2str(i),'.csv'),inSampleUserIds,'delimiter',',','precision',10);
end
