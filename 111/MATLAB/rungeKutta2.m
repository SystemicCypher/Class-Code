function [t,v] = rungeKutta2(f, v_o, t_o, t_f, dt)

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
    v(n+1) = v(n) + dt * slope;
    nope = f(t(n+1), v(n+1));
    v(n+1) = v(n) + dt * ((slope + nope)/2); 
    n = n + 1;
end





end