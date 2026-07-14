% dealing with the Chakravarty_Meerkat matlab files
% I did this with ChatGPT help because I didn't understand what I was
% looking at

load('C:\Users\PC\Documents\EvaluatingVDBA\Data\Accelerometer\Chakravarty_Meerkat\labelledTriaxialAccData.mat')

out = [];

for s = 1:length(sessionWiseAccData_fourBehaviours)

    session = sessionWiseAccData_fourBehaviours{s};

    for b = 1:length(session)

        bouts = session{b};

        for bout = 1:length(bouts)

            acc = bouts{bout};

            n = size(acc,1);

            tmp = [
                repmat(s,n,1), ...
                repmat(b,n,1), ...
                repmat(bout,n,1), ...
                acc
            ];

            out = [out; tmp];

        end

    end
end

T = array2table(out,...
    'VariableNames',{'Session','Behaviour','Bout','X','Y','Z'});

writetable(T,'meerkat_acceleration_labels.csv')