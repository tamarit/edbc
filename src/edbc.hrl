-compile( [{parse_transform, edbc_parse_transform}]).

-define(PRE(FUN), pre() -> FUN).
-define(POST(FUN), post() -> FUN).
% -define(PRE2(FUN), -pre(FUN)).
-define(PRE_I(FUN), pre(FUN)).
-define(P(N), p(N)).
-define(R(), r()).