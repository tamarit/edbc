-module(decrease_studio).

-export([f/2]).

% Original

f_ori(M, N) -> 
	io:format("{M, N}: ~p\n", [{M, N}]),
	case M of 
		0 -> 
			f_ori(M + 1, N + 2);
		N -> 
			f_ori(M - 1, N - 1);
		_ ->
			case N of 
				0 -> 
					M;
				_ -> 
					f_ori(M, N - 1)
			end
	end.

% Transformed

f_0(M, N, PrevN) -> 
	case N < PrevN of 
		true -> 
			f(M, N);
		false -> 
			ErrorMsg = 
				lists:flatten(
					io_lib:format(
						"Decreasing condition does not hold.\nPrevious value: ~p\nNew Value: ~p\n", 
						[PrevN, N])),
			error(ErrorMsg)
	end.



f(M, N) -> 
	io:format("{M, N}: ~p\n", [{M, N}]),
	case M of 
		0 -> 
			f_0(M + 1, N + 2, N);
		N -> 
			f_0(M - 1, N - 1, N);
		_ ->
			case N of 
				0 -> 
					M;
				_ -> 
					f_0(M, N - 1, N)
			end
	end.