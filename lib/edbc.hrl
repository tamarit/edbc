-compile( [{parse_transform, edbc_parse_transform}]).
-include_lib("stdlib/include/assert.hrl").

-define(PRE(FUN), edbc_pre() -> FUN).
-define(POST(FUN), edbc_post() -> FUN).
-define(INVARIANT(FUN), edbc_invariant() -> FUN).
% -define(PRE2(FUN), -pre(FUN)).
% -define(PRE_I(FUN), pre(FUN)).
-define(P(N), p(N)).
-define(R, r()).
% -define(STATE, state()).
