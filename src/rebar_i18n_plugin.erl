-module(rebar_i18n_plugin).

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(LOG(Lvl, Msg, Args), rebar_log:log(Lvl, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

%% standard rebar hooks
-export([preprocess/2, compile/2]).

-on_load(set_vars/0).


%%
%% Plugin API
%%

preprocess(Config, AppFile) ->
	?DEBUG("Preprocess i18n\n", []),
	ok.

last(List, Count) ->
    RevList = lists:reverse(List),
    SubRevList = lists:sublist(RevList, Count),
    lists:reverse(SubRevList).

compile(Config, AppFile) ->
    set_vars(),
    ok.


set_vars() ->
%   ?DEBUG("Set env vars i18n\n", []),
    export_env("CC", "icu-config --cc"),
    export_env("CXX", "icu-config --cxx"),
    export_env("ICU_CFLAGS", "icu-config --cflags"),
    export_env("ICU_CXXFLAGS", "icu-config --cxxflags"),
    export_env("ICU_LDFLAGS", "icu-config --ldflags"),
    export_env("ICU_INC_PATH", "icu-config --cppflags-searchpath",
        fun(X) -> 
            X ++ " -idirafter c_src/include " 
        end),
    case os:getenv("I18N_BUILD_ID") of
    false ->
        {Mega, Secs, _} = os:timestamp(),
        Timestamp = Mega*1000000 + Secs,
        os:putenv("I18N_BUILD_ID", [$.|integer_to_list(Timestamp)]);
    _ -> ok
    end,
    ok.

%%
%% Internal Functions
%%

export_env(Name, Cmd) ->
    FormatFn = fun(X) -> X end,
    export_env(Name, Cmd, FormatFn).
    

export_env(Name, Cmd, FormatFn) ->
	case os:getenv(Name) of
	false ->
		{0, Value} = eunit_lib:command(Cmd),
		os:putenv(Name, FormatFn(Value)),
		ok;
	_ -> ok
	end.

