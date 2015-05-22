%% -*- erlang -*-

Definitions.
ERL       = -erl\s.*
WS        = ([\000-\s]|%.*)
Str2      = "([^"]|\.)+"
%"% damn you syntax highlighter
Ignore    = ignore
GZ        = gz
Field     = (host|uuid|name|module|class|key)
Instance  = instance
KW        = [A-Za-z][A-Za-z0-9_@-]*
Arrow     = [-][>]


Rules.
{ERL}       : {token, {erlang,        TokenLine, unerl(TokenChars, TokenLine)}}.
{Comment}   : {token, {comment,       TokenLine}}.
{Ignore}    : {token, {kw_ignore,     TokenLine}}.
{GZ}        : {token, {kw_gz,         TokenLine}}.
{Arrow}     : {token, {kw_arrow,      TokenLine}}.
{Instance}  : {token, {instance,      TokenLine, instance}}.
{Field}     : {token, {field,         TokenLine, a(TokenChars)}}.
{KW}        : {token, {kw,            TokenLine, a(TokenChars)}}.
{Str1}      : S = strip(TokenChars,   TokenLen),
              {token, {str,           TokenLine, S}}.
{Str2}      : S = strip(TokenChars,   TokenLen),
              {token, {str,           TokenLine, S}}.
[(),.[\]=]  :   {token, {a(TokenChars), TokenLine}}.
{WS}+       :   skip_token.

Erlang code.

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
unerl(TokenChars,TokenLen) -> lists:sublist(TokenChars, 6, TokenLen).

a(L) -> list_to_atom(L).
