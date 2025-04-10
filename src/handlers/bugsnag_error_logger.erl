-module(bugsnag_error_logger).
-moduledoc "An `m:error_logger` handler to report messages above a certain level to BugSnag".
-deprecated({'_', '_', next_major_release}).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2
]).

% Callbacks

-doc false.
-spec init(_) -> {ok, no_state}.
init(_InitArgs) ->
    {ok, no_state}.

-doc false.
-spec handle_event(term(), no_state) -> {ok, no_state}.
handle_event({error, GroupLeader, {Pid, Format, Data}}, S) ->
    handle_error_msg(GroupLeader, Pid, Format, Data, S);
handle_event({error_report, GroupLeader, {Pid, Type, Report}}, S) ->
    handle_error_report(GroupLeader, Pid, Type, Report, S);
handle_event(_Event, State) ->
    {ok, State}.

-doc false.
-spec handle_call(term(), no_state) -> {ok, term(), no_state}.
handle_call(_Request, State) ->
    {ok, noreply, State}.

-doc false.
-spec handle_info(term(), no_state) -> {ok, no_state}.
handle_info(_Info, State) ->
    {ok, State}.

% Private API
handle_error_msg(_, _, "** Generic server " ++ _, Data, S) ->
    %% gen_server terminate
    [Name, LastMsg, StateData, Reason] = Data,
    notify(
        error,
        Reason,
        "gen_server ~w terminated with reason: ~s",
        [Name, format_reason(Reason)],
        [
            last_message(LastMsg),
            state_data(StateData)
        ]
    ),
    {ok, S};
handle_error_msg(_, _, "** State machine " ++ _, Data, S) ->
    %% gen_fsm terminate
    [Name, LastMsg, StateName, StateData, Reason] = Data,
    notify(
        error,
        Reason,
        "gen_fsm ~w in state ~w terminated with reason: ~s",
        [Name, StateName, format_reason(Reason)],
        [
            last_message(LastMsg),
            state_data(StateData)
        ]
    ),
    {ok, S};
handle_error_msg(_, _, "** gen_event handler" ++ _, Data, S) ->
    %% gen_event handler terminate
    [ID, Name, LastMsg, StateData, Reason] = Data,
    notify(
        error,
        Reason,
        "gen_event ~w installed in ~w terminated with reason: ~s",
        [ID, Name, format_reason(Reason)],
        [
            last_message(LastMsg),
            state_data(StateData)
        ]
    ),
    {ok, S};
handle_error_msg(_, _, "Error in process " ++ _, Data, S) ->
    %% spawn_link'd process terminate
    [Name, Node, Reason] = Data,
    notify(
        error,
        Reason,
        "Process ~w on node ~w terminated with reason: ~s",
        [Name, Node, format_reason(Reason)],
        [{"full_reason", format_term(Reason)}]
    ),
    {ok, S};
handle_error_msg(_, _, Format, Data, S) ->
    notify(error, Format, Format, Data, []),
    {ok, S}.

handle_error_report(_, _, supervisor_report, Report, S) ->
    case lists:sort(Report) of
        [
            {'errorContext', Context},
            {offender, Offender},
            {reason, Reason},
            {supervisor, Name}
        ] ->
            case Reason of
                normal ->
                    % Nothing happens on a normal exit
                    ok;
                _ ->
                    notify(
                        supervisor_report,
                        Reason,
                        "Supervisor ~w had child ~s exit with reason ~s in context ~w",
                        [
                            element(2, Name),
                            format_offender(Offender),
                            format_reason(Reason),
                            Context
                        ],
                        [{"full_reason", format_term(Reason)}]
                    )
            end;
        _ ->
            %% notify(Type, Reason, Format, Args, Extra),
            ok
    end,
    {ok, S};
handle_error_report(_, _, crash_report, [Report, Neighbors], S) ->
    Name =
        case proplists:get_value(registered_name, Report, []) of
            [] -> proplists:get_value(pid, Report);
            Atom -> Atom
        end,
    {Type, Reason, Trace} = proplists:get_value(error_info, Report),
    notify(
        Type,
        {Reason, Trace},
        "Process ~w with ~w neighbors crashed with reason: ~s",
        [Name, length(Neighbors), format_reason(Reason)],
        [{atom_to_list(Key), format_term(Val)} || {Key, Val} <- Report]
    ),
    {ok, S};
handle_error_report(_, _, std_error, _Report, S) ->
    %% notify(Type, Reason, Format, Args, Extra),
    {ok, S};
handle_error_report(_, _, _Type, _Report, S) ->
    %% notify(Type, Reason, Format, Args, Extra),
    {ok, S}.

notify(Type, Reason, Format, Args, Extra) ->
    Message = format_message(Format, Args),
    Request = {atom_to_list(node()), "erlang", format_term(Type), Extra},
    {Class, Trace} = parse_reason(Reason),
    {Module, Line} =
        case Trace of
            [{M, _, _} | _] ->
                {M, 0};
            [{M, _, _, Pos} | _] ->
                {M, proplists:get_value(line, Pos, 0)};
            [] ->
                {unknown, 0}
        end,
    bugsnag:notify(Type, Class, Message, Module, Line, Trace, Request).

parse_reason({Reason, [MFA | _] = Trace}) when
    is_tuple(MFA)
->
    {Atom, DeepTrace} = parse_reason(Reason),
    {Atom, DeepTrace ++ Trace};
parse_reason({Reason, []}) ->
    parse_reason(Reason);
parse_reason({Reason, {_, _, _} = MFA}) ->
    {Atom, DeepTrace} = parse_reason(Reason),
    {Atom, DeepTrace ++ [MFA]};
parse_reason({Reason, {_, _, _, Pos} = MFA}) when
    is_list(Pos)
->
    {Atom, DeepTrace} = parse_reason(Reason),
    {Atom, DeepTrace ++ [MFA]};
parse_reason(Reason) when is_atom(Reason) ->
    {Reason, []};
parse_reason(_Reason) ->
    {unknown, []}.

% Formatters
format_args([], FmtAcc, ArgsAcc) ->
    {string:join(lists:reverse(FmtAcc), ", "), lists:reverse(ArgsAcc)};
format_args([H | T], FmtAcc, ArgsAcc) ->
    format_args(T, ["~p" | FmtAcc], [H | ArgsAcc]).

format_limit([{M, F, _} | _] = Trace) ->
    case {M, F} of
        {erlang, open_port} ->
            "maximum number of ports exceeded";
        {erlang, spawn} ->
            "maximum number of processes exceeded";
        {erlang, spawn_opt} ->
            "maximum number of processes exceeded";
        {erlang, list_to_atom} ->
            "tried to create an atom larger than 255, or maximum atom count exceeded";
        {ets, new} ->
            "maximum number of ETS tables exceeded";
        _ ->
            format_term(Trace)
    end.

format_message(Fmt, Args) ->
    IoList = io_lib:format(Fmt, Args),
    Binary = iolist_to_binary(IoList),
    %% binary_to_list(Binary).
    Binary.

format_mfa({M, F, A}) when is_list(A) ->
    {FmtStr, Args} = format_args(A, [], []),
    io_lib:format("~w:~w(" ++ FmtStr ++ ")", [M, F | Args]);
format_mfa({M, F, A}) when is_integer(A) ->
    io_lib:format("~w:~w/~w", [M, F, A]);
format_mfa({_M, _F, _A} = MFA) ->
    io_lib:format("~w", [MFA]);
format_mfa({M, F, A, _}) ->
    format_mfa({M, F, A});
format_mfa(Other) ->
    io_lib:format("~w", [Other]).

format_offender(Offender) ->
    case proplists:get_value(mfargs, Offender) of
        undefined ->
            %% supervisor_bridge
            Mod = proplists:get_value(mod, Offender),
            Pid = proplists:get_value(pid, Offender),
            io_lib:format("at module ~w at ~w", [Mod, Pid]);
        MFArgs ->
            %% supervisor
            MFA = format_mfa(MFArgs),
            Name = proplists:get_value(name, Offender),
            Pid = proplists:get_value(pid, Offender),
            io_lib:format("~p started with ~s at ~w", [Name, MFA, Pid])
    end.

format_reason({'function not exported', [{M, F, A}, MFA | _]}) ->
    [
        "call to undefined function ",
        format_mfa({M, F, length(A)}),
        " from ",
        format_mfa(MFA)
    ];
format_reason({undef, [MFA | _]}) ->
    ["call to undefined function ", format_mfa(MFA)];
format_reason({bad_return_value, Val}) ->
    ["bad return value: ", format_term(Val)];
format_reason({{bad_return_value, Val}, MFA}) ->
    [
        "bad return value: ",
        format_term(Val),
        " in ",
        format_mfa(MFA)
    ];
format_reason({{case_clause, Val}, [MFA | _]}) ->
    [
        "no case clause matching ",
        format_term(Val),
        " in ",
        format_mfa(MFA)
    ];
format_reason({function_clause, [MFA | _]}) ->
    ["no function clause matching ", format_mfa(MFA)];
format_reason({if_clause, [MFA | _]}) ->
    ["no true branch found while evaluating if expression in ", format_mfa(MFA)];
format_reason({{try_clause, Val}, [MFA | _]}) ->
    [
        "no try clause matching ",
        format_term(Val),
        " in ",
        format_mfa(MFA)
    ];
format_reason({badarith, [MFA | _]}) ->
    ["bad arithmetic expression in ", format_mfa(MFA)];
format_reason({{badmatch, Val}, [MFA | _]}) ->
    [
        "no match of right hand value ",
        format_term(Val),
        " in ",
        format_mfa(MFA)
    ];
format_reason({emfile, _Trace}) ->
    "maximum number of file descriptors exhausted, check ulimit -n";
format_reason({system_limit, [{_, _, _} | _] = Trace}) ->
    ["system limit: ", format_limit(Trace)];
format_reason({badarg, [MFA, MFA2 | _]}) ->
    case MFA of
        {_M, _F, A} when is_list(A) ->
            [
                "bad argument in call to ",
                format_mfa(MFA),
                " in ",
                format_mfa(MFA2)
            ];
        _ ->
            %% Generated by a bad call to a BIF?
            ["bad argument in ", format_mfa(MFA)]
    end;
format_reason({{badarity, {Fun, Args}}, [MFA | _]}) ->
    {arity, Arity} = lists:keyfind(arity, 1, erlang:fun_info(Fun)),
    [
        io_lib:format(
            "fun called with wrong arity of ~w instead of ~w in ",
            [length(Args), Arity]
        ),
        format_mfa(MFA)
    ];
format_reason({noproc, MFA}) ->
    ["no such process or port in call to ", format_mfa(MFA)];
format_reason({{badfun, Term}, [MFA | _]}) ->
    [
        "bad function ",
        format_term(Term),
        " in ",
        format_mfa(MFA)
    ];
format_reason({'EXIT', Reason}) ->
    format_reason(Reason);
format_reason({Reason, Child}) when
    is_tuple(Child) andalso
        element(1, Child) =:= child
->
    format_reason(Reason);
format_reason({Reason, MFA}) when
    is_tuple(MFA)
->
    %% Attempt to capture nested reasons...
    format_reason(Reason);
format_reason(Reason) ->
    format_term(Reason).

format_term(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

%% Misc

last_message(LastMsg) ->
    {"last_message", format_term(LastMsg)}.

state_data(StateData) ->
    {"state_data", format_term(StateData)}.
