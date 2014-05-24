-module(tachyon_kstat_nsq).

-behaviour(ensq_channel_behaviour).

-export([init/0, new_frame/1, response/2, message/3, error/2]).

init() ->
    {ok, {[], []}}.

new_frame(_State) ->
    {ok, {[], []}}.

response(_Msg, State) ->
    {ok, State}.

error(_Msg, State) ->
    {ok, State}.

message(Msg, _, {Hosts, Zones}) ->
    tachyon_mps:provide(),
    case tachyon_kstat_pkg:decode(Msg) of
        {ok, {Host, <<"global">>, _, _, _} = P} ->
            case orddict:find(Host, Hosts) of
                {ok, Pid} ->
                    tachyon_kstat_host:msg(Pid, P),
                    {Hosts, Zones};
                _ ->
                    {ok, Pid} =
                        case global:whereis_name({host, Host}) of
                            undefined ->
                                tachyon_kstat_host_sup:start_child(Host);
                            APid ->
                                {ok, APid}
                        end,
                    tachyon_kstat_host:msg(Pid, P),
                    Hosts1 = orddict:store(Host, Pid, Hosts),
                    {Hosts1, Zones}
            end;
        {ok, {_, Zone, _, _, _} = P} ->
            case orddict:find(Zone, Zones) of
                {ok, Pid} ->
                    tachyon_kstat_zone:msg(Pid, P),
                    {Hosts, Zones};
                _ ->
                    {ok, Pid} =
                        case global:whereis_name({zone, Zone}) of
                            undefined ->
                                tachyon_kstat_zone_sup:start_child(Zone);
                            APid ->
                                {ok, APid}
                        end,
                    tachyon_kstat_zone:msg(Pid, P),
                    Zones1 = orddict:store(Zone, Pid, Zones),
                    {Hosts, Zones1}
            end;
        E ->
            lager:error("[msg] ~p", [E]),
            {Hosts, Zones}
    end.
