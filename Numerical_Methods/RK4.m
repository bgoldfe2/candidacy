function [y, t] = RK4(f, y0, t0, tf, dt)
    
    t = t0:dt:tf;
    nt = numel(t);
    
    ny = numel(y0);
    y = nan(ny,nt);
    
    y(:,1) = y0;
    
    for k = 1:nt-1
        
        k1 = dt*f(t(k),y(:,k));
        k2 = dt*f(t(k)+dt/2,y(:,k)+k1/2);
        k3 = dt*f(t(k)+dt/2,y(:,k)+k2/2);
        k4 = dt*f(t(k)+dt,y(:,k)+k3);
        
        dy = (k1+2*k2+2*k3+k4)/6;
        
        y(:,k+1) = y(:,k) + dy;
    end
    
end
