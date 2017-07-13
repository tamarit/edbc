-compile( [{parse_transform, edbc_parse_transform}]).
-include_lib("stdlib/include/assert.hrl").

% A condition to be held BEFORE performing a call.
-define(PRE(FUN), edbc_pre() -> FUN).

% A condition to be held AFTER performing a call.
-define(POST(FUN), edbc_post() -> FUN).

% Checks whether the state satisfies some conditions. 
% It can be only used in gen_server or other behaviours with a internal state.
-define(INVARIANT(FUN), edbc_invariant() -> FUN).

% Checks that a parameter is decreased in recursive calls (only working for self-recursive function now)
-define(DECREASES(PAR), edbc_decreases() -> PAR).

% Macro for parameters, e.g. P(1) states for the first parameter.
-define(P(N), edbc_p(N)).

% Macro for the result of a function.
-define(R, edbc_r()).


% Not used
% -define(STATE, state()).
% -define(PRE2(FUN), -pre(FUN)).
% -define(PRE_I(FUN), pre(FUN)).
