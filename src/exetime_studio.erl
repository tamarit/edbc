-module(exetime_studio).

-export([f/1]).

% Original

f_ori(X) -> 
	timer:sleep(1000),
	X + 1.

% Transformed

f(X) -> 
	Limit = 1500, % this comes from ?TIME(500) or something similar
	StartTime = 
		os:timestamp(),
	Res = f_0(X),
	ExeTime = 
		timer:now_diff(os:timestamp(), StartTime) / 1000,
	case ExeTime < Limit of 
		true -> 
			Res;
		false -> 
			ErrorMsg = 
				lists:flatten(
					io_lib:format(
						"The execution of the function took too much time (~p ms).\n", 
						[ExeTime])),
			error(ErrorMsg)
	end.



f_0(X) -> 
	timer:sleep(1000),
	X + 1.