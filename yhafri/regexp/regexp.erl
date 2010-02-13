%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(regexp).

%% This module provides a basic set of regular expression functions
%% for strings. The functions provided are taken from AWK.
%%
%% Note that we interpret the syntax tree of a regular expression
%% directly instead of converting it to an NFA and then interpreting
%% that. This method seems to go significantly faster.

-export([sh_to_awk/1,parse/1,format_error/1,match/2,first_match/2,matches/2]).
-export([sub/3,gsub/3,split/2]).

-export([compile/1,make_dfa/1,make_dfa/2]).

-import(string, [substr/2,substr/3]).
-import(lists, [reverse/1]).
-import(lists, [member/2,seq/2,keysearch/3,keysort/2,map/2,foldl/3]).
-import(ordsets, [is_element/2,add_element/2,union/2,subtract/2]).

%% -type matchres() = {match,Start,Length} | nomatch | {error,E}.
%% -type subres() = {ok,RepString,RepCount} | {error,E}.
%% -type splitres() = {ok,[SubString]} | {error,E}.

%%-compile([export_all]).

%% sh_to_awk(ShellRegExp)
%%  Convert a sh style regexp into a full AWK one. The main difficulty is
%%  getting character sets right as the conventions are different.

sh_to_awk(Sh) -> "^(" ++ sh_to_awk_1(Sh).	%Fix the beginning

sh_to_awk_1([$*|Sh]) ->				%This matches any string
    ".*" ++ sh_to_awk_1(Sh);
sh_to_awk_1([$?|Sh]) ->				%This matches any character
    [$.|sh_to_awk_1(Sh)];
sh_to_awk_1([$[,$^,$]|Sh]) ->			%This takes careful handling
    "\\^" ++ sh_to_awk_1(Sh);
sh_to_awk_1("[^" ++ Sh) -> [$[|sh_to_awk_2(Sh, true)];
sh_to_awk_1("[!" ++ Sh) -> "[^" ++ sh_to_awk_2(Sh, false);
sh_to_awk_1([$[|Sh]) -> [$[|sh_to_awk_2(Sh, false)];
sh_to_awk_1([C|Sh]) ->
    %% Unspecialise everything else which is not an escape character.
    case special_char(C) of
	true -> [$\\,C|sh_to_awk_1(Sh)];
	false -> [C|sh_to_awk_1(Sh)]
    end;
sh_to_awk_1([]) -> ")$".			%Fix the end

sh_to_awk_2([$]|Sh], UpArrow) -> [$]|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_2(Sh, UpArrow) -> sh_to_awk_3(Sh, UpArrow).

sh_to_awk_3([$]|Sh], true) -> "^]" ++ sh_to_awk_1(Sh);
sh_to_awk_3([$]|Sh], false) -> [$]|sh_to_awk_1(Sh)];
sh_to_awk_3([C|Sh], UpArrow) -> [C|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_3([], true) -> [$^|sh_to_awk_1([])];
sh_to_awk_3([], false) -> sh_to_awk_1([]).

%% -type special_char(char()) -> bool().
%%  Test if a character is a special character.

special_char($|) -> true;
special_char($*) -> true;
special_char($+) -> true;
special_char($?) -> true;
special_char($() -> true;
special_char($)) -> true;
special_char($\\) -> true;
special_char($^) -> true;
special_char($$) -> true;
special_char($.) -> true;
special_char($[) -> true;
special_char($]) -> true;
special_char($") -> true;
special_char(_C) -> false.

%% parse(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

parse(S) ->
    case catch reg(S) of
	{R,[]} -> {ok,{regexp,R}};
	{_R,[C|_]} -> {error,{illegal,[C]}};
	{error,E} -> {error,E}
    end.

%% format_error(Error) -> String.

format_error({illegal,What}) -> ["illegal character `",What,"'"];
format_error({unterminated,What}) -> ["unterminated `",What,"'"];
format_error({char_class,What}) ->
    ["illegal character class ",io_lib:write_string(What)].

%% -type match(String, RegExp) -> matchres().
%%  Find the longest match of RegExp in String.

match(S, RegExp) when list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> match(S, RE);
	{error,E} -> {error,E}
    end;
match(S, {regexp,RE}) ->
    case match_re(RE, S, 1, 0, -1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	{_Start,_Len} -> nomatch
    end;
match(S, {comp_regexp,RE}) ->
    case match_comp(RE, S, 1, 0, -1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	{_Start,_Len} -> nomatch
    end.

match_re(RE, S, St, Pos, L) ->
    case first_match_re(RE, S, St) of
	{St1,L1} ->
	    Nst = St1 + 1,
	    if L1 > L -> match_re(RE, lists:nthtail(Nst-St, S), Nst, St1, L1);
	       true -> match_re(RE, lists:nthtail(Nst-St, S), Nst, Pos, L)
	    end;
	nomatch -> {Pos,L}
    end.

match_comp(RE, S, St, Pos, L) ->
    case first_match_comp(RE, S, St) of
	{St1,L1} ->
	    Nst = St1 + 1,
	    if L1 > L -> match_comp(RE, lists:nthtail(Nst-St, S), Nst, St1, L1);
	       true -> match_comp(RE, lists:nthtail(Nst-St, S), Nst, Pos, L)
	    end;
	nomatch -> {Pos,L}
    end.

%% -type first_match(String, RegExp) -> matchres().
%%  Find the first match of RegExp in String.

first_match(S, RegExp) when list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> first_match(S, RE);
	{error,E} -> {error,E}
    end;
first_match(S, {regexp,RE}) ->
    case first_match_re(RE, S, 1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	nomatch -> nomatch
    end;
first_match(S, {comp_regexp,RE}) ->
    case first_match_comp(RE, S, 1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	nomatch -> nomatch
    end.

first_match_re(RE, S, St) when S /= [] ->
    case re_apply(S, St, RE) of
	{match,P,_Rest} -> {St,P-St};
	nomatch -> first_match_re(RE, tl(S), St+1)
    end;
first_match_re(_RE, [], _St) -> nomatch.

first_match_comp(RE, S, St) when S /= [] ->
    case comp_apply(S, St, RE) of
	{match,P,_Rest} -> {St,P-St};
	nomatch -> first_match_comp(RE, tl(S), St+1)
    end;
first_match_comp(_RE, [], _St) -> nomatch.

%% -type matches(String, RegExp) -> {match,[{Start,Length}]} | {error,E}.
%%  Return the all the non-overlapping matches of RegExp in String.

matches(S, RegExp) when list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> matches(S, RE);
	{error,E} -> {error,E}
    end;
matches(S, {regexp,RE}) -> {match,matches_re(S, RE, 1)};
matches(S, {comp_regexp,RE}) -> {match,matches_comp(S, RE, 1)}.

matches_re(S, RE, St) ->
    case first_match_re(RE, S, St) of
	{St1,0} -> [{St1,0}|matches_re(substr(S, St1+2-St), RE, St1+1)];
	{St1,L1} -> [{St1,L1}|matches_re(substr(S, St1+L1+1-St), RE, St1+L1)];
	nomatch -> []
    end.

matches_comp(S, RE, St) ->
    case first_match_comp(RE, S, St) of
	{St1,0} -> [{St1,0}|matches_comp(substr(S, St1+2-St), RE, St1+1)];
	{St1,L1} -> [{St1,L1}|matches_comp(substr(S, St1+L1+1-St), RE, St1+L1)];
	nomatch -> []
    end.

%% -type sub(String, RegExp, Replace) -> subsres().
%%  Substitute the first match of the regular expression RegExp with
%%  the string Replace in String. Accept pre-parsed regular
%%  expressions.

sub(String, RegExp, Rep) when list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> sub(String, RE, Rep);
	{error,E} -> {error,E}
    end;
sub(String, {regexp,RE}, Rep) ->
    Ss = sub_match_re(String, RE, 1),
    {ok,sub_repl(Ss, Rep, String, 1),length(Ss)};
sub(String, {comp_regexp,RE}, Rep) ->
    Ss = sub_match_comp(String, RE, 1),
    {ok,sub_repl(Ss, Rep, String, 1),length(Ss)}.

sub_match_re(S, RE, St) ->
    case first_match_re(RE, S, St) of
	{St1,L1} -> [{St1,L1}];
	nomatch -> []
    end.

sub_match_comp(S, RE, St) ->
    case first_match_comp(RE, S, St) of
	{St1,L1} -> [{St1,L1}];
	nomatch -> []
    end.

sub_repl([{St,L}|Ss], Rep, S, Pos) ->
    Rs = sub_repl(Ss, Rep, S, St+L),
    substr(S, Pos, St-Pos) ++ sub_repl(Rep, substr(S, St, L), Rs);
sub_repl([], _Rep, S, Pos) -> substr(S, Pos).

sub_repl([$&|Rep], M, Rest) -> M ++ sub_repl(Rep, M, Rest);
sub_repl("\\&" ++ Rep, M, Rest) -> [$&|sub_repl(Rep, M, Rest)];
sub_repl([C|Rep], M, Rest) -> [C|sub_repl(Rep, M, Rest)];
sub_repl([], _M, Rest) -> Rest.

%% -type gsub(String, RegExp, Replace) -> subres().
%%  Substitute every match of the regular expression RegExp with the
%%  string New in String. Accept pre-parsed regular expressions.

gsub(String, RegExp, Rep) when list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> gsub(String, RE, Rep);
	{error,E} -> {error,E}
    end;
gsub(String, {regexp,RE}, Rep) ->
    Ss = matches_re(String, RE, 1),
    {ok,sub_repl(Ss, Rep, String, 1),length(Ss)};
gsub(String, {comp_regexp,RE}, Rep) ->
    Ss = matches_comp(String, RE, 1),
    {ok,sub_repl(Ss, Rep, String, 1),length(Ss)}.

%% -type split(String, RegExp) -> splitres().
%%  Split a string into substrings where the RegExp describes the
%%  field seperator. The RegExp " " is specially treated.

split(String, " ") ->				%This is really special
    {ok,RE} = parse("[ \t]+"),
    case split_apply_re(String, RE, true) of
	[[]|Ss] -> {ok,Ss};
	Ss -> {ok,Ss}
    end;
split(String, RegExp) when list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> {ok,split_apply_re(String, RE, false)};
	{error,E} -> {error,E}
    end;
split(String, {regexp,RE}) -> {ok,split_apply_re(String, RE, false)};
split(String, {comp_regexp,RE}) -> {ok,split_apply_comp(String, RE, false)}.

split_apply_re(S, RE, Trim) -> split_apply_re(S, 1, RE, Trim, []).

split_apply_re([], _P, _RE, true, []) -> [];
split_apply_re([], _P, _RE, _T, Sub) -> [reverse(Sub)];
split_apply_re(S, P, RE, T, Sub) ->
    case re_apply(S, P, RE) of
	{match,P,_Rest} ->
	    split_apply_re(tl(S), P+1, RE, T, [hd(S)|Sub]);
	{match,P1,Rest} ->
	    [reverse(Sub)|split_apply_re(Rest, P1, RE, T, [])];
	nomatch ->
	    split_apply_re(tl(S), P+1, RE, T, [hd(S)|Sub])
    end.

split_apply_comp(S, RE, Trim) -> split_apply_comp(S, 1, RE, Trim, []).

split_apply_comp([], _P, _RE, true, []) -> [];
split_apply_comp([], _P, _RE, _T, Sub) -> [reverse(Sub)];
split_apply_comp(S, P, RE, T, Sub) ->
    case comp_apply(S, P, RE) of
	{match,P,_Rest} ->
	    split_apply_comp(tl(S), P+1, RE, T, [hd(S)|Sub]);
	{match,P1,Rest} ->
	    [reverse(Sub)|split_apply_comp(Rest, P1, RE, T, [])];
	nomatch ->
	    split_apply_comp(tl(S), P+1, RE, T, [hd(S)|Sub])
    end.

%% This is the regular expression grammar used. It is equivalent to the
%% one used in AWK, except that we allow ^ $ to be used anywhere and fail
%% in the matching.
%%
%% reg -> reg1 : '$1'.
%% reg1 -> reg1 "|" reg2 : {'or','$1','$2'}.
%% reg1 -> reg2 : '$1'.
%% reg2 -> reg2 reg3 : {concat,'$1','$2'}.
%% reg2 -> reg3 : '$1'.
%% reg3 -> reg3 "*" : {kclosure,'$1'}.
%% reg3 -> reg3 "+" : {pclosure,'$1'}.
%% reg3 -> reg3 "?" : {optional,'$1'}.
%% reg3 -> reg4 : '$1'.
%% reg4 -> "(" reg ")" : '$2'.
%% reg4 -> "\\" char : '$2'.
%% reg4 -> "^" : bos.
%% reg4 -> "$" : eos.
%% reg4 -> "." : char.
%% reg4 -> "[" class "]" : {char_class,char_class('$2')}
%% reg4 -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% reg4 -> "\"" chars "\"" : char_string('$2')
%% reg4 -> char : '$1'.
%% reg4 -> empty : epsilon.
%%  The grammar of the current regular expressions. The actual parser
%%  is a recursive descent implementation of the grammar.

reg(S) -> reg1(S).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2
%% reg1' -> empty

reg1(S0) ->
    {L,S1} = reg2(S0),
    reg1p(S1, L).

reg1p([$||S0], L) ->
    {R,S1} = reg2(S0),
    reg1p(S1, {'or',L,R});
reg1p(S, L) -> {L,S}.

%% reg2 -> reg3 reg2'
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0) ->
    {L,S1} = reg3(S0),
    reg2p(S1, L).

reg2p([C|S0], L) when C /= $|, C /= $) ->
    {R,S1} = reg3([C|S0]),
    reg2p(S1, {concat,L,R});
reg2p(S, L) -> {L,S}.

%% reg3 -> reg4 reg3'
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> empty

reg3(S0) ->
    {L,S1} = reg4(S0),
    reg3p(S1, L).

reg3p([$*|S], L) -> reg3p(S, {kclosure,L});
reg3p([$+|S], L) -> reg3p(S, {pclosure,L});
reg3p([$?|S], L) -> reg3p(S, {optional,L});
reg3p(S, L) -> {L,S}.

reg4([$(|S0]) ->
    case reg(S0) of
	{R,[$)|S1]} -> {R,S1};
	{_R,_S} -> throw({error,{unterminated,"("}})
    end;
reg4([$\\,O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
reg4([$\\,C|S]) -> {escape_char(C),S};
reg4([$\\]) -> throw({error,{unterminated,"\\"}});
reg4([$^|S]) -> {bos,S};
reg4([$$|S]) -> {eos,S};
reg4([$.|S]) -> {{comp_class,"\n"},S};
reg4("[^" ++ S0) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{comp_class,Cc},S1};
	{_Cc,_S} -> throw({error,{unterminated,"["}})
    end;
reg4([$[|S0]) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{char_class,Cc},S1};
	{_Cc,_S1} -> throw({error,{unterminated,"["}})
    end;
%reg4([$"|S0]) ->
%    case char_string(S0) of
%	{St,[$"|S1]} -> {St,S1};
%	{St,S1} -> throw({error,{unterminated,"\""}})
%    end;
reg4([C|S]) when C /= $*, C /= $+, C /= $?, C /= $] -> {C,S};
reg4([C|_S]) -> throw({error,{illegal,[C]}});
reg4([]) -> {epsilon,[]}.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPACE
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

char_class([$]|S]) -> char_class(S, [$]]);
char_class(S) -> char_class(S, []).

char($\\, [O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [C|S]) -> {escape_char(C),S};
char(C, S) -> {C,S}.

char_class([C1|S0], Cc) when C1 /= $] ->
    case char(C1, S0) of
	{Cf,[$-,C2|S1]} when C2 /= $] ->
	    case char(C2, S1) of
		{Cl,S2} when Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc]); 
		{Cl,_S2} -> throw({error,{char_class,[Cf,$-,Cl]}})
	    end;
	{C,S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc,S}.

%char_string([C|S]) when C /= $" -> char_string(S, C);
%char_string(S) -> {epsilon,S}.

%char_string([C|S0], L) when C /= $" ->
%    char_string(S0, {concat,L,C});
%char_string(S, L) -> {L,S}.

%% -deftype re_app_res() = {match,RestPos,Rest} | nomatch.

%% re_apply(String, StartPos, RegExp) -> re_app_res().
%%
%%  Apply the (parse of the) regular expression RegExp to String.  If
%%  there is a match return the position of the remaining string and
%%  the string if else return 'nomatch'.
%%
%%  StartPos should be the real start position as it is used to decide
%%  if we are at the beginning of the string.

re_apply(S, St, RE) -> re_apply(RE, [], S, St).

re_apply(epsilon, More, S, P) ->		%This always matches
    re_apply_more(More, S, P);
re_apply({'or',RE1,RE2}, More, S, P) ->
    re_apply_or(re_apply(RE1, More, S, P),
		re_apply(RE2, More, S, P));
re_apply({concat,RE1,RE2}, More, S0, P) ->
    re_apply(RE1, [RE2|More], S0, P);
re_apply({kclosure,CE}, More, S, P) ->
    %% Be careful with the recursion, explicitly do one call before
    %% looping.
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, [{kclosure,CE}|More], S, P));
re_apply({pclosure,CE}, More, S, P) ->
    re_apply(CE, [{kclosure,CE}|More], S, P);
re_apply({optional,CE}, More, S, P) ->
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, More, S, P));
re_apply(bos, More, S, 1) -> re_apply_more(More, S, 1);
re_apply(eos, More, [$\n|S], P) -> re_apply_more(More, S, P);
re_apply(eos, More, [], P) -> re_apply_more(More, [], P);
re_apply({char_class,Cc}, More, [C|S], P) ->
    case in_char_class(C, Cc) of
	true -> re_apply_more(More, S, P+1);
	false -> nomatch
    end;
re_apply({comp_class,Cc}, More, [C|S], P) ->
    case in_char_class(C, Cc) of
	true -> nomatch;
	false -> re_apply_more(More, S, P+1)
    end;
re_apply(C, More, [C|S], P) when integer(C) ->
    re_apply_more(More, S, P+1);
re_apply(_RE, _More, _S, _P) -> nomatch.

%% re_apply_more([RegExp], String, Length) -> re_app_res().

re_apply_more([RE|More], S, P) -> re_apply(RE, More, S, P);
re_apply_more([], S, P) -> {match,P,S}.

%% in_char_class(Char, Class) -> bool().

in_char_class(C, [{C1,C2}|_Cc]) when C >= C1, C =< C2 -> true;
in_char_class(C, [C|_Cc]) -> true;
in_char_class(C, [_|Cc]) -> in_char_class(C, Cc);
in_char_class(_C, []) -> false.

%% re_apply_or(Match1, Match2) -> re_app_res().
%%  If we want the best match then choose the longest match, else just
%%  choose one by trying sequentially.

re_apply_or({match,P1,S1},   {match,P2,_S2}) when P1 >= P2 -> {match,P1,S1};
re_apply_or({match,_P1,_S1}, {match,P2,S2}) -> {match,P2,S2};
re_apply_or(nomatch, R2) -> R2;
re_apply_or(R1, nomatch) -> R1.

-record(nfa_state, {no,edges=[],accept=no}).
-record(dfa_state, {no,nfa=[],trans=[],accept=no}).

%% -deftype re_app_res() = {match,RestPos,Rest} | nomatch.

%% comp_apply(String, StartPos, DFAReg) -> re_app_res().
%% Apply the DFA of a regular expression to a string.  If
%%  there is a match return the position of the remaining string and
%%  the string if else return 'nomatch'.
%%
%%  StartPos should be the real start position as it is used to decide
%%  if we are at the beginning of the string.

comp_apply(Cs, P, {DFA,Start}) ->
    comp_apply(Cs, P, Start, DFA, nomatch).

comp_apply([], P, St, DFA, _Accept) ->
    case element(St, DFA) of
	#dfa_state{accept=true} -> {match,P,[]};
	#dfa_state{accept=false} -> nomatch
    end;
comp_apply([C|Cs]=Cs0, P, St, DFA, Accept) ->
    case element(St, DFA) of
	#dfa_state{accept=false,trans=none} -> Accept;
	#dfa_state{accept=false,trans=Trans} ->
	    comp_apply(Cs, P+1, element(C+1, Trans), DFA, Accept);
%% 	#dfa_state{accept=false,trans={OffS,NoAccept,Trans}} ->
%% 	    I = C + OffS,
%% 	    NextSt = if  I >= 1, I =< size(Trans) -> element(I, Trans);
%% 			 true -> NoAccept
%% 		     end,
%% 	    comp_apply(Cs, P+1, NextSt, DFA, Accept);
	#dfa_state{accept=true,trans=none} ->
	    {match,P,Cs0};
	#dfa_state{accept=true,trans=Trans} ->
	    comp_apply(Cs, P+1, element(C+1, Trans), DFA, {match,P,Cs0})
    end.

%% compile(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

compile(RegExp) ->
    case make_dfa([{RegExp,yes}], 2) of
	{ok,{DFA0,Start}} ->
	    DFA1 = [#dfa_state{no=1,accept=no,trans=[]}|DFA0],
	    DFA = tuplelise_dfa(DFA1, 1),
	    {ok,{comp_regexp,{DFA,Start}}};
	{error,E} -> {error,E}
    end.

tuplelise_dfa(DFA0, NoAccept) ->
    DFA1 = map(fun (#dfa_state{trans=[]}=D) ->
		       D#dfa_state{trans=none,
				   accept=fix_accept(D#dfa_state.accept)};
		   (#dfa_state{trans=Ts0}=D) ->
		       Ts1 = expand_trans(Ts0, NoAccept),
		       D#dfa_state{trans=list_to_tuple(Ts1),
				  accept=fix_accept(D#dfa_state.accept)}
	       end, DFA0),
    list_to_tuple(keysort(#dfa_state.no, DFA1)).

fix_accept({yes,_}) -> true;
fix_accept(no) -> false.

expand_trans(Ts0, NoAccept) ->
    Td = foldl(fun ({C,S}, Ts) -> orddict:store(C, S, Ts) end,
	       map(fun (N) -> {N,NoAccept} end, seq(0, 255)), Ts0),
    map(fun ({_,S}) -> S end, Td).

%% make_dfa(RegExpActions) -> {ok,{DFA,StartState}} | {error,E}.
%% make_dfa(RegExpActions, LowestState) -> {ok,{DFA,StartState}} | {error,E}.
%% Build a complete dfa from a list of {RegExp,Action}. The DFA field
%% accept has values {yes,Action}|no. If multiple Regexps can result
%% in same match string then RegExpActions list define priority.

make_dfa(REAs) -> make_dfa(REAs, 0).

make_dfa(REAs0, Low) ->
    case parse_reas(REAs0) of
	{ok,REAs1} ->
	    {NFA,Start0} = build_combined_nfa(REAs1),
	    {DFA0,Start1} = build_dfa(NFA, Start0),
	    {DFA,Start} = minimise_dfa(DFA0, Start1, Low),
	    {ok,{DFA,Start}};
	{error,E} -> {error,E}
    end.

parse_reas(REAs) -> parse_reas(REAs, []).

parse_reas([{{regexp,R},A}|REAs], S) ->		%Already parsed
    parse_reas(REAs, [{R,A}|S]);
parse_reas([{RegExp,A}|REAs], S) ->
    case parse(RegExp) of
	{ok,{regexp,R}} -> parse_reas(REAs, [{R,A}|S]);
	{error,E} -> {error,E}
    end;
parse_reas([], Stack) -> {ok,reverse(Stack)}.

%% build_combined_nfa(RegExpActionList) -> {NFA,StartState}.
%%  Build the combined NFA using Thompson's construction straight out
%%  of the book. Build the separate NFAs in the same order as the
%%  rules so that the accepting have ascending states have ascending
%%  state numbers.  Start numbering the states from 1 as we put the
%%  states in a tuple with the state number as the index.

build_combined_nfa(REAs) ->
    {NFA0,Starts,Next} = build_nfa_list(REAs, [], [], 1),
    F = #nfa_state{no=Next,edges=epsilon_trans(Starts),accept=no},
    {list_to_tuple(keysort(#nfa_state.no, [F|NFA0])),Next}.

build_nfa_list([{RE,Action}|REAs], NFA0, Starts, Next0) ->
    {NFA1,Next1,Start} = build_nfa(RE, Next0, Action),
    build_nfa_list(REAs, NFA1 ++ NFA0, [Start|Starts], Next1);
build_nfa_list([], NFA, Starts, Next) ->
    {NFA,reverse(Starts),Next}.

epsilon_trans(Sts) -> [ {epsilon,S} || S <- Sts ].

%% build_nfa(RegExp, NextState, Action) -> {NFA,NextFreeState,StartState}.
%%  When building the NFA states for a ??? we don't build the end
%%  state, just allocate a State for it and return this state
%%  number. This allows us to avoid building unnecessary states for
%%  concatenation which would then have to be removed by overwriting
%%  an existing state.

build_nfa(RE, Next, Action) ->
    {NFA,N,E} = build_nfa(RE, Next+1, Next, []),
    {[#nfa_state{no=E,accept={yes,Action}}|NFA],N,Next}.

%% build_nfa(RegExp, NextState, StartState, NFA) -> {NFA,NextState,EndState}.
%%  The NFA is a list of nfa_state is no predefined order. The state
%%  number of the returned EndState is already allocated!

build_nfa({'or',RE1,RE2}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE1, N0+1, N0, NFA0),
    {NFA2,N2,E2} = build_nfa(RE2, N1+1, N1, NFA1),
    E = N2,
    {[#nfa_state{no=S,edges=[{epsilon,N0},{epsilon,N1}]},
      #nfa_state{no=E1,edges=[{epsilon,E}]},
      #nfa_state{no=E2,edges=[{epsilon,E}]}|NFA2],
     N2+1,E};
build_nfa({concat,RE1, RE2}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE1, N0, S, NFA0),
    {NFA2,N2,E2} = build_nfa(RE2, N1, E1, NFA1),
    {NFA2,N2,E2};
build_nfa({kclosure,RE}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,
    {[#nfa_state{no=S,edges=[{epsilon,N0},{epsilon,E}]},
      #nfa_state{no=E1,edges=[{epsilon,N0},{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({pclosure,RE}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,
    {[#nfa_state{no=S,edges=[{epsilon,N0}]},
      #nfa_state{no=E1,edges=[{epsilon,N0},{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({optional,RE}, N0, S, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,
    {[#nfa_state{no=S,edges=[{epsilon,N0},{epsilon,E}]},
      #nfa_state{no=E1,edges=[{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({char_class,Cc}, N, S, NFA) ->
    {[#nfa_state{no=S,edges=[{nfa_char_class(Cc),N}]}|NFA],N+1,N};
build_nfa({comp_class,Cc}, N, S, NFA) ->
    {[#nfa_state{no=S,edges=[{nfa_comp_class(Cc),N}]}|NFA],N+1,N};
build_nfa(C, N, S, NFA) when integer(C) ->
    {[#nfa_state{no=S,edges=[{[C],N}]}|NFA],N+1,N}.

nfa_char_class(Cc) ->
    lists:foldl(fun ({C1,C2}, Set) -> union(seq(C1, C2), Set);
		    (C, Set) -> add_element(C, Set) end, [], Cc).

nfa_comp_class(Cc) -> subtract(seq(0, 255), nfa_char_class(Cc)).

%% build_dfa(NFA, NfaStartState) -> {DFA,DfaStartState}.
%%  Build a DFA from an NFA using "subset construction". The major
%%  difference from the book is that we keep the marked and unmarked
%%  DFA states in seperate lists. New DFA states are added to the
%%  unmarked list and states are marked by moving them to the marked
%%  list. We assume that the NFA accepting state numbers are in
%%  ascending order for the rules and use ordsets to keep this order.

build_dfa(NFA, Start) ->
    D = #dfa_state{no=0,nfa=eclosure([Start], NFA)},
    {build_dfa([D], 1, [], NFA),0}.

%% build_dfa([UnMarked], NextState, [Marked], NFA) -> DFA.
%%  Traverse the unmarked states. Temporarily add the current unmarked
%%  state to the marked list before calculating translation, this is
%%  to avoid adding too many duplicate states. Add it properly to the
%%  marked list afterwards with correct translations.

build_dfa([U|Us0], N0, Ms, NFA) ->
    {Ts,Us1,N1} = build_dfa(255, U#dfa_state.nfa, Us0, N0, [], [U|Ms], NFA),
    M = U#dfa_state{trans=Ts,accept=accept(U#dfa_state.nfa, NFA)},
    build_dfa(Us1, N1, [M|Ms], NFA);
build_dfa([], _N, Ms, _NFA) -> Ms.

%% build_dfa(Char, [NfaState], [Unmarked], NextState, [Transition], [Marked], NFA) ->
%%	{Transitions,UnmarkedStates,NextState}.
%%  Foreach NFA state set calculate the legal translations. N.B. must
%%  search *BOTH* the unmarked and marked lists to check if DFA state
%%  already exists. By test characters downwards and prepending
%%  transitions we get the transition lists in ascending order.

build_dfa(C, Set, Us, N, Ts, Ms, NFA) when C >= 0 ->
    case eclosure(move(Set, C, NFA), NFA) of
	S when S /= [] ->
	    case keysearch(S, #dfa_state.nfa, Us) of
		{value,#dfa_state{no=T}} ->
		    build_dfa(C-1, Set, Us, N, [{C,T}|Ts], Ms, NFA);
		false ->
		    case keysearch(S, #dfa_state.nfa, Ms) of
			{value,#dfa_state{no=T}} ->
			    build_dfa(C-1, Set, Us, N, [{C,T}|Ts], Ms, NFA);
			false ->
			    U = #dfa_state{no=N,nfa=S},
			    build_dfa(C-1, Set, [U|Us], N+1, [{C,N}|Ts], Ms, NFA)
		    end
	    end;
	[] ->
	    build_dfa(C-1, Set, Us, N, Ts, Ms, NFA)
    end;
build_dfa(-1, _Set, Us, N, Ts, _Ms, _NFA) ->
    {Ts,Us,N}.

%% eclosure([State], NFA) -> [State].
%% move([State], Char, NFA) -> [State].
%%  These are straight out of the book. As eclosure uses ordsets then
%%  the generated state sets are in ascending order.

eclosure(Sts, NFA) -> eclosure(Sts, NFA, []).

eclosure([St|Sts], NFA, Ec) ->
    #nfa_state{edges=Es} = element(St, NFA),
    eclosure([ N || {epsilon,N} <- Es,
		    not is_element(N, Ec) ] ++ Sts,
	     NFA, add_element(St, Ec));
eclosure([], _NFA, Ec) -> Ec.

move(Sts, C, NFA) ->
    [St || N <- Sts,
	   {C1,St} <- (element(N, NFA))#nfa_state.edges,
	   list(C1),
	   member(C, C1) ].

%% accept([State], NFA) -> true | false.
%%  Scan down the state list until we find an accepting state.

accept([St|Sts], NFA) ->
    case element(St, NFA) of
	#nfa_state{accept={yes,A}} -> {yes,A};
	#nfa_state{accept=no} -> accept(Sts, NFA)
    end;
accept([], _NFA) -> no.

%% minimise_dfa(DFA, StartState, FirstState) -> {DFA,StartState}.
%%  Minimise the DFA by removing equivalent states. We consider a
%%  state if both the transitions and the their accept state is the
%%  same.  First repeatedly run throught the DFA state list removing
%%  equivalent states and updating remaining transitions with
%%  remaining equivalent state numbers. When no more reductions are
%%  possible then pack the remaining state numbers to get consecutive
%%  states.

minimise_dfa(DFA0, Start, N) ->
    case min_dfa(DFA0) of
	{DFA1,[]} ->				%No reduction!
	    {DFA2,Rs} = pack_dfa(DFA1, N),
	    {min_update(DFA2, Rs),min_new_state(Start, Rs)};
	{DFA1,Rs} ->
	    minimise_dfa(min_update(DFA1, Rs), min_new_state(Start, Rs), N)
    end.

min_dfa(DFA) -> min_dfa(DFA, [], []).

min_dfa([D|DFA0], Rs0, MDFA) ->
    {DFA1,Rs1} = min_delete(DFA0, D#dfa_state.trans, D#dfa_state.accept,
			    D#dfa_state.no, Rs0, []),
    min_dfa(DFA1, Rs1, [D|MDFA]);
min_dfa([], Rs, MDFA) -> {MDFA,Rs}.

min_delete([#dfa_state{no=N,trans=T,accept=A}|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, [{N,NewN}|Rs], MDFA);
min_delete([D|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, Rs, [D|MDFA]);
min_delete([], _T, _A, _NewN, Rs, MDFA) -> {MDFA,Rs}.

min_update(DFA, Rs) ->
    [ D#dfa_state{trans=min_update_trans(D#dfa_state.trans, Rs)} || D <- DFA ].

min_update_trans(Tr, Rs) ->
    [ {C,min_new_state(S, Rs)} || {C,S} <- Tr ].

min_new_state(Old, [{Old,New}|_Reds]) -> New;
min_new_state(Old, [_R|Reds]) -> min_new_state(Old, Reds);
min_new_state(Old, []) -> Old.

pack_dfa(DFA, N) -> pack_dfa(DFA, N, [], []).

pack_dfa([D|DFA], NewN, Rs, PDFA) ->
    pack_dfa(DFA, NewN+1, [{D#dfa_state.no,NewN}|Rs],
	     [D#dfa_state{no=NewN}|PDFA]);
pack_dfa([], _NewN, Rs, PDFA) -> {PDFA,Rs}.
