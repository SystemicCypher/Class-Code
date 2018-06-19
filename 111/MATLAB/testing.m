clear;
g = 9.8;
m = 75;
cd = 0.25;
v_s = 0;
t_s = 0;
t_f = 15;
dt = 3.3;

f = @(t, v) g - (cd/m)*v.^2;


[t, v_a] = euler_test(f, v_s, t_s, t_f, dt);

%plot(t,v);
[t, v_b] = rungeKutta(f, v_s, t_s, t_f, dt);

[t,v] = rungeKutta4(f, v_s, t_s, t_f, dt);


plot(t,v, 'o', t, v_a, 'o', t, v_b, 'o');