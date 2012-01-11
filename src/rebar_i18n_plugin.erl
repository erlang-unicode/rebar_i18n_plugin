-module(rebar_i18n_plugin).

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(LOG(Lvl, Msg, Args), rebar_log:log(Lvl, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

%% standard rebar hooks
-export([compile/2]).

-on_load(set_vars/0).


%%
%% Plugin API
%%

compile(Config, AppFile) ->
    set_vars(),
    ok.



%%
%% Internal Functions
%%

set_vars() ->
    case os:getenv("I18N_REBAR") of
    false ->
        ?DEBUG("Set env vars i18n\n", []),
		os:putenv("I18N_REBAR", "true"),

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
        end;

    _ -> 
        ?DEBUG("Env vars for i18n already seted.\n", [])
    end,
        
    ok.


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

