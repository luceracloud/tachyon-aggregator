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
         $i, Integer:64/integer>>) ->
    {ok, {Host, Zone, Time, {Module, Instance, Name, Class},
          {Key, Integer}}};

decode(<<_HostSize:32/integer, Host:_HostSize/binary,
         _ZoneSize:32/integer, Zone:_ZoneSize/binary,
         Time:64/integer,
         _NameSize:32/integer, Name:_NameSize/binary,
         _ModuleSize:32/integer, Module:_ModuleSize/binary,
         _ClassSize:32/integer, Class:_ClassSize/binary,
         Instance:64/integer,
         _KeySize:32/integer, Key:_KeySize/binary,
         $s, String/binary>>) ->
    {ok, {Host, Zone, Time, {Module, Instance, Name, Class},
          {Key, String}}};

decode(B) ->
    {error, unknown_package, B}.
