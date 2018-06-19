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
[t, v_b] = rungeKutta2(f, v_s, t_s, t_f, dt);

[t,v] = rungeKutta4(f, v_s, t_s, t_f, dt);

%exact graph
x = [0:1/1000000:15];
y = sqrt(g*m/cd)*tanh(x*sqrt(g*cd/m));


plot(t,v,'o', t, v_a, 'o', t, v_b, 'o', x, y);
xlabel('Time');
ylabel('Velocity');
legend('Runge-Kutta Method (RK4)','Euler Method','Trapezoidal Method (RK2)','Exact', 'location', 'southeast');
title('Time vs Numerical Methods for Velocity');


exact_solution = @(t) sqrt(g*m/cd)*tanh(t*sqrt(g*cd/m));

%error calculation Euler
dt = 0.1;
n_divisions = 4;

error_max_all = zeros(1,n_divisions);
dt_all = zeros(1,n_divisions);

for j=1:n_divisions
    [t, v_a] = euler_test(f, v_s, t_s, t_f, dt);
    
    v_exact = zeros(1,length(t));
    error = zeros(1,length(t));
    
    for i = 1:length(t)
        v_exact(i) = exact_solution(t(i));
        error(i) = abs(v_a(i)-v_exact(i));
    end
        
    error_max_all(j) = max(error);
    
    dt_all(j) = dt;
    dt = dt/2;
end

% calculate order of accuracy
k_all = zeros(1, n_divisions);

for i=2:n_divisions
    k_all(i) = log(error_max_all(i-1)/error_max_all(i))/log(2);
end

% print time-step/error/order table
fprintf('\nEuler Method\n\n');
fprintf('Time-step \t Error \t\t Order\n');
for i = 1:n_divisions
    fprintf('%f \t %f \t %f \n', dt_all(i), error_max_all(i), k_all(i));
end



%error calculation RK2
dt = 0.1;
n_divisions = 4;

error_max_all = zeros(1,n_divisions);
dt_all = zeros(1,n_divisions);

for j=1:n_divisions
    [t, v_b] = rungeKutta2(f, v_s, t_s, t_f, dt);
    
    v_exact = zeros(1,length(t));
    error = zeros(1,length(t));
    
    for i = 1:length(t)
        v_exact(i) = exact_solution(t(i));
        error(i) = abs(v_b(i)-v_exact(i));
    end
        
    error_max_all(j) = max(error);
    
    dt_all(j) = dt;
    dt = dt/2;
end

% calculate order of accuracy
k_all = zeros(1, n_divisions);

for i=2:n_divisions
    k_all(i) = log(error_max_all(i-1)/error_max_all(i))/log(2);
end

% print time-step/error/order table
fprintf('\nTrapezoidal Method\n\n'); 
fprintf('Time-step \t Error \t\t Order\n');
for i = 1:n_divisions
    fprintf('%f \t %f \t %f \n', dt_all(i), error_max_all(i), k_all(i));
end



%error calculation RK4
dt = 0.1;
n_divisions = 4;

error_max_all = zeros(1,n_divisions);
dt_all = zeros(1,n_divisions);

for j=1:n_divisions
    [t, v] = rungeKutta4(f, v_s, t_s, t_f, dt);
    
    v_exact = zeros(1,length(t));
    error = zeros(1,length(t));
    
    for i = 1:length(t)
        v_exact(i) = exact_solution(t(i));
        error(i) = abs(v(i)-v_exact(i));
    end
        
    error_max_all(j) = max(error);
    
    dt_all(j) = dt;
    dt = dt/2;
end

% calculate order of accuracy
k_all = zeros(1, n_divisions);

for i=2:n_divisions
    k_all(i) = log(error_max_all(i-1)/error_max_all(i))/log(2);
end

% print time-step/error/order table
fprintf('\nRunge-Kutta Method\n\n'); 
fprintf('Time-step \t Error \t\t Order\n');
for i = 1:n_divisions
    fprintf('%f \t %g \t %f \n', dt_all(i), error_max_all(i), k_all(i));
end





































%{
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_a(i)-v_exact(i));
end

error_max1 = max(error);


dt = 0.05;
[t, v_a] = euler_test(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_a(i)-v_exact(i));
end

error_max05 = max(error);


dt = 0.025;
[t, v_a] = euler_test(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_a(i)-v_exact(i));
end

error_max025 = max(error);


dt = 0.0125;
[t, v_a] = euler_test(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_a(i)-v_exact(i));
end

error_max0125 = max(error);


%error calculation RK2
dt = 0.1;
[t, v_b] = rungeKutta2(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_b(i)-v_exact(i));
end

error_max1 = max(error);


dt = 0.05;
[t, v_b] = rungeKutta2(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_b(i)-v_exact(i));
end

error_max05 = max(error);


dt = 0.025;
[t, v_b] = rungeKutta2(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_b(i)-v_exact(i));
end

error_max025 = max(error);


dt = 0.0125;
[t, v_b] = rungeKutta2(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v_b(i)-v_exact(i));
end

error_max0125 = max(error);



%error calculation RK4
dt = 0.1;
[t,v] = rungeKutta4(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v(i)-v_exact(i));
end

error_max1 = max(error);


dt = 0.05;
[t,v] = rungeKutta4(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v(i)-v_exact(i));
end

error_max05 = max(error);


dt = 0.025;
[t,v] = rungeKutta4(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v(i)-v_exact(i));
end

error_max025 = max(error);


dt = 0.0125;
[t,v] = rungeKutta4(f, v_s, t_s, t_f, dt);

v_exact = zeros(1,length(t));
error = zeros(1,length(t));

for i = 1:length(t)
    v_exact(i) = exact_solution(t(i));
    error(i) = abs(v(i)-v_exact(i));
end

error_max0125 = max(error);














% display the max error
fprintf('Max error = %f\n', error_max);
%}