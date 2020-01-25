function ydot = LotkaVolterraModel(y,params)
    
    alpha = params.alpha;
    beta = params.beta;
    delta = params.delta;
    gamma = params.gamma;
    
    ydot = [ alpha*y(1) - beta*y(1)*y(2)
             delta*y(1)*y(2) - gamma*y(2)];
end