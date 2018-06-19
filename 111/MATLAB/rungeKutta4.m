function [t,v] = rungeKutta(f, v_o, t_o, t_f, dt)

n_total = ceil((t_f - t_o)/dt);

v = zeros(1, n_total);
t = zeros(1, n_total);

v(1) = v_o;
t(1) = t_o;

n = 1;

while t(n) < t_f
    if t(n) + dt > t_f
        dt = t_f - t(n);
    end
    t(n+1) = t(n) + dt;
    slope = f(t(n), v(n));
    hope = f(t(n)+ 0.5 * dt, v(n) + 0.5 * dt * slope);
    dope =  f(t(n)+ 0.5 * dt, v(n) + 0.5 * dt * hope);
    elope = f(t(n)+ dt, v(n) + dt * dope);
    %nope = f(t(n+1), v(n+1));
    v(n+1) = v(n) + dt * ((slope + 2 * hope + 2 * dope + elope)/6); 
    n = n + 1;
end





end