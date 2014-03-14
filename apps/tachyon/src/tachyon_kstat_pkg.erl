-module(tachyon_kstat_pkg).

-export([decode/1]).

decode(<<_HostSize:32/integer, Host:_HostSize/binary,
         _ZoneSize:32/integer, Zone:_ZoneSize/binary,
         Time:64/integer,
         _NameSize:32/integer, Name:_NameSize/binary,
         _ModuleSize:32/integer, Module:_ModuleSize/binary,
         _ClassSize:32/integer, Class:_ClassSize/binary,
         Instance:64/integer,
         _KeySize:32/integer, Key:_KeySize/binary,
         Data/binary>>) ->
    case decode_data(Data) of
        {ok, V} ->
            %% {ok, {Host, Zone, trunc(Time/1000000000), {Module, Instance, Name, Class},
            {ok, {Host, Zone, Time, {Module, Instance, Name, Class},
                  {Key, V}}};
        _ ->
            {error, unknown_data, Data}
    end;

decode(B) ->
    {error, unknown_package, B}.

decode_data(<<$i, Integer:64/integer>>) ->
    {ok, Integer};
decode_data(<<$s, String/binary>>) ->
    {ok, String};
decode_data(_) ->
    error.
