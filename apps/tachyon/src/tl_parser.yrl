%% -*- erlang -*-
Nonterminals
rules rule ignore matchers matcher match metric metric_elements metric_element.

Terminals '(' ')' ',' '.' '=' '[' ']'
kw_ignore kw_arrow kw str erlang field kw_gz instance.

%%%===================================================================
%%% Root statement
%%%===================================================================
Rootsymbol rules.

rules -> rule                           : ['$1'].
rules -> rule rules                     : ['$1' | '$2'].


rule -> ignore '.'                      : '$1'.
rule -> match  '.'                      : '$1'.
rule -> erlang                          : {fn, unwrap('$1')}.

ignore -> kw_ignore '(' matchers ')'    : {ignore, '$3'}.


matchers -> matcher                     : ['$1'].
matchers -> matcher ',' matchers        : ['$1' | '$3'].

matcher -> field '=' str                : {unwrap('$1'), unwrap('$3')}.
matcher -> kw_gz : gz.

match -> kw '(' matchers ')' kw_arrow metric : {{unwrap('$1'), '$6'}, '$3'}.

metric -> '[' metric_elements ']'       : '$2'.

metric_elements -> metric_element       : ['$1'].
metric_elements -> metric_element ',' metric_elements : ['$1' | '$3'].

metric_element -> field                 : unwrap('$1').
metric_element -> instance              : unwrap('$1').
metric_element -> str                   : unwrap('$1').
metric_element -> kw '(' field ')'      : {unwrap('$1'), unwrap('$3')}.
metric_element -> kw '(' instance ')'   : {unwrap('$1'), unwrap('$3')}.

%%%===================================================================
%%% Erlang code.
%%%===================================================================

Erlang code.
-ignore_xref([format_error/1, parse_and_scan/1, return_error/2]).

unwrap({_,_,V}) -> V;
unwrap({_, V}) -> V.
