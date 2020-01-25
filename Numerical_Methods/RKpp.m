%% Problem Definition

params.alpha = 1.2;
params.beta = 0.6;
params.delta = 0.3;
params.gamma = 0.8;

f = @(t,y) LotkaVolterraModel(y,params);
y0 = [2 1];

%% Solve the differential equations

t0=0;
tf=20;
%dt=0.01;
hvec = [0.1 0.01 0.001];
for n = 1 : length(hvec)
    dt = hvec(n);
    [y,t] = RK4(f,y0,t0,tf,dt);
    
    %Plot the output
    
    figure;
    
    subplot(1,2,1)
    plot(t,y);
    legend('Prey','Predator')
    xlabel('Time (t)');
    grid on;
    
    subplot(1,2,2)
    plot(y(1,:),y(2,:));
    xlabel('Prey')
    ylabel('Predator')
end