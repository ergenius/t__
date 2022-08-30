%% -*- coding: utf-8 -*-
%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com> <www.ergenius.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(t__plural).
-author("Madalin Grigore-Enescu").

-include("../include/t__.hrl").

-export([select/3]).

%% @doc Select and return the proper translation based on N and plural-forms
%% If the proper translation can not be determined, the method returns first element of Msg parameter.
select(Language, N, Msg) ->
    Index = n_to_index(Language, N) + 1,
    case Index > erlang:length(Msg) of
        true ->
            ?T__LOG(warning, "Invalid plural term translation",
                [{language, Language}, {msg, Msg}, {n,N}]),
            [Msg1|_] = Msg,
            Msg1;
        false -> lists:nth(Index, Msg)
    end.

%% nplurals: 1
%% C formula: 0
%% cases: ["other"]
formula_0(_N) -> 0.

%% nplurals: 2
%% C formula: n != 1
%% cases: ["one","other"]
formula_1(N) -> 
if
    (N /= 1) -> 1;
    true -> 0
end.

%% nplurals: 2
%% C formula: n != 1 && n != 2 && n != 3 && (n % 10 == 4 || n % 10 == 6 || n % 10 == 9)
%% cases: ["one","other"]
formula_2(N) -> 
if
    ((N /= 1) and (N /= 2) and (N /= 3) and (((N rem 10) == 4) or ((N rem 10) == 6) or ((N rem 10) == 9))) -> 1;
    true -> 0
end.

%% nplurals: 2
%% C formula: n % 10 != 1 || n % 100 == 11
%% cases: ["one","other"]
formula_3(N) -> 
if
    (((N rem 10) /= 1) or ((N rem 100) == 11)) -> 1;
    true -> 0
end.

%% nplurals: 2
%% C formula: n > 1
%% cases: ["one","other"]
formula_4(N) -> 
if
    (N > 1) -> 1;
    true -> 0
end.

%% nplurals: 2
%% C formula: n >= 2 && (n < 11 || n > 99)
%% cases: ["one","other"]
formula_5(N) -> 
if
    ((N >= 2) and ((N < 11) or (N > 99))) -> 1;
    true -> 0
end.

%% nplurals: 3
%% C formula: (n % 10 == 1 && (n % 100 < 11 || n % 100 > 19)) ? 0 : ((n % 10 >= 2 && n % 10 <= 9 && (n % 100 < 11 || n % 100 > 19)) ? 1 : 2)
%% cases: ["one","few","other"]
formula_6(N) -> 
if
    (((N rem 10) == 1) and (((N rem 100) < 11) or ((N rem 100) > 19))) -> 0;
    (((N rem 10) >= 2) and ((N rem 10) =< 9) and (((N rem 100) < 11) or ((N rem 100) > 19))) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n % 10 == 1 && n % 100 != 11) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)
%% cases: ["one","few","other"]
formula_7(N) -> 
if
    (((N rem 10) == 1) and ((N rem 100) /= 11)) -> 0;
    (((N rem 10) >= 2) and ((N rem 10) =< 4) and (((N rem 100) < 12) or ((N rem 100) > 14))) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 0 || n == 1) ? 0 : ((n >= 2 && n <= 10) ? 1 : 2)
%% cases: ["one","few","other"]
formula_8(N) -> 
if
    ((N == 0) or (N == 1)) -> 0;
    ((N >= 2) and (N =< 10)) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 1) ? 0 : ((n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 12 || n % 100 > 14)) ? 1 : 2)
%% cases: ["one","few","other"]
formula_9(N) -> 
if
    (N == 1) -> 0;
    (((N rem 10) >= 2) and ((N rem 10) =< 4) and (((N rem 100) < 12) or ((N rem 100) > 14))) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 19) ? 1 : 2)
%% cases: ["one","few","other"]
formula_10(N) -> 
if
    (N == 1) -> 0;
    ((N == 0) or ((N rem 100) >= 2) and ((N rem 100) =< 19)) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 1) ? 0 : ((n >= 2 && n <= 4) ? 1 : 2)
%% cases: ["one","few","other"]
formula_11(N) -> 
if
    (N == 1) -> 0;
    ((N >= 2) and (N =< 4)) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 0 || n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)
%% cases: ["one","many","other"]
formula_12(N) -> 
if
    ((N == 0) or (N == 1)) -> 0;
    ((N /= 0) and ((N rem 1000000) == 0)) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 1) ? 0 : ((n != 0 && n % 1000000 == 0) ? 1 : 2)
%% cases: ["one","many","other"]
formula_13(N) -> 
if
    (N == 1) -> 0;
    ((N /= 0) and ((N rem 1000000) == 0)) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 1) ? 0 : ((n == 2) ? 1 : 2)
%% cases: ["one","two","other"]
formula_14(N) -> 
if
    (N == 1) -> 0;
    (N == 2) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n % 10 == 0 || n % 100 >= 11 && n % 100 <= 19) ? 0 : ((n % 10 == 1 && n % 100 != 11) ? 1 : 2)
%% cases: ["zero","one","other"]
formula_15(N) -> 
if
    (((N rem 10) == 0) or ((N rem 100) >= 11) and ((N rem 100) =< 19)) -> 0;
    (((N rem 10) == 1) and ((N rem 100) /= 11)) -> 1;
    true -> 2
end.

%% nplurals: 3
%% C formula: (n == 0) ? 0 : ((n == 1) ? 1 : 2)
%% cases: ["zero","one","other"]
formula_16(N) -> 
if
    (N == 0) -> 0;
    (N == 1) -> 1;
    true -> 2
end.

%% nplurals: 4
%% C formula: (n == 1) ? 0 : ((n == 0 || n % 100 >= 2 && n % 100 <= 10) ? 1 : ((n % 100 >= 11 && n % 100 <= 19) ? 2 : 3))
%% cases: ["one","few","many","other"]
formula_17(N) -> 
if
    (N == 1) -> 0;
    ((N == 0) or ((N rem 100) >= 2) and ((N rem 100) =< 10)) -> 1;
    (((N rem 100) >= 11) and ((N rem 100) =< 19)) -> 2;
    true -> 3
end.

%% nplurals: 4
%% C formula: (n % 10 == 1) ? 0 : ((n % 10 == 2) ? 1 : ((n % 100 == 0 || n % 100 == 20 || n % 100 == 40 || n % 100 == 60 || n % 100 == 80) ? 2 : 3))
%% cases: ["one","two","few","other"]
formula_18(N) -> 
if
    ((N rem 10) == 1) -> 0;
    ((N rem 10) == 2) -> 1;
    (((N rem 100) == 0) or ((N rem 100) == 20) or ((N rem 100) == 40) or ((N rem 100) == 60) or ((N rem 100) == 80)) -> 2;
    true -> 3
end.

%% nplurals: 4
%% C formula: (n % 100 == 1) ? 0 : ((n % 100 == 2) ? 1 : ((n % 100 == 3 || n % 100 == 4) ? 2 : 3))
%% cases: ["one","two","few","other"]
formula_19(N) -> 
if
    ((N rem 100) == 1) -> 0;
    ((N rem 100) == 2) -> 1;
    (((N rem 100) == 3) or ((N rem 100) == 4)) -> 2;
    true -> 3
end.

%% nplurals: 4
%% C formula: (n == 1 || n == 11) ? 0 : ((n == 2 || n == 12) ? 1 : ((n >= 3 && n <= 10 || n >= 13 && n <= 19) ? 2 : 3))
%% cases: ["one","two","few","other"]
formula_20(N) -> 
if
    ((N == 1) or (N == 11)) -> 0;
    ((N == 2) or (N == 12)) -> 1;
    ((N >= 3) and (N =< 10) or (N >= 13) and (N =< 19)) -> 2;
    true -> 3
end.

%% nplurals: 4
%% C formula: (n == 1) ? 0 : ((n == 2) ? 1 : ((n > 10 && n % 10 == 0) ? 2 : 3))
%% cases: ["one","two","many","other"]
formula_21(N) -> 
if
    (N == 1) -> 0;
    (N == 2) -> 1;
    ((N > 10) and ((N rem 10) == 0)) -> 2;
    true -> 3
end.

%% nplurals: 5
%% C formula: (n % 10 == 1 && n % 100 != 11 && n % 100 != 71 && n % 100 != 91) ? 0 : ((n % 10 == 2 && n % 100 != 12 && n % 100 != 72 && n % 100 != 92) ? 1 : ((((n % 10 == 3 || n % 10 == 4) || n % 10 == 9) && (n % 100 < 10 || n % 100 > 19) && (n % 100 < 70 || n % 100 > 79) && (n % 100 < 90 || n % 100 > 99)) ? 2 : ((n != 0 && n % 1000000 == 0) ? 3 : 4)))
%% cases: ["one","two","few","many","other"]
formula_22(N) -> 
if
    (((N rem 10) == 1) and ((N rem 100) /= 11) and ((N rem 100) /= 71) and ((N rem 100) /= 91)) -> 0;
    (((N rem 10) == 2) and ((N rem 100) /= 12) and ((N rem 100) /= 72) and ((N rem 100) /= 92)) -> 1;
    (((((N rem 10) == 3) or ((N rem 10) == 4)) or ((N rem 10) == 9)) and (((N rem 100) < 10) or ((N rem 100) > 19)) and (((N rem 100) < 70) or ((N rem 100) > 79)) and (((N rem 100) < 90) or ((N rem 100) > 99))) -> 2;
    ((N /= 0) and ((N rem 1000000) == 0)) -> 3;
    true -> 4
end.

%% nplurals: 5
%% C formula: (n == 1) ? 0 : ((n == 2) ? 1 : ((n >= 3 && n <= 6) ? 2 : ((n >= 7 && n <= 10) ? 3 : 4)))
%% cases: ["one","two","few","many","other"]
formula_23(N) -> 
if
    (N == 1) -> 0;
    (N == 2) -> 1;
    ((N >= 3) and (N =< 6)) -> 2;
    ((N >= 7) and (N =< 10)) -> 3;
    true -> 4
end.

%% nplurals: 6
%% C formula: (n == 0) ? 0 : ((n == 1) ? 1 : (((n % 100 == 2 || n % 100 == 22 || n % 100 == 42 || n % 100 == 62 || n % 100 == 82) || n % 1000 == 0 && (n % 100000 >= 1000 && n % 100000 <= 20000 || n % 100000 == 40000 || n % 100000 == 60000 || n % 100000 == 80000) || n != 0 && n % 1000000 == 100000) ? 2 : ((n % 100 == 3 || n % 100 == 23 || n % 100 == 43 || n % 100 == 63 || n % 100 == 83) ? 3 : ((n != 1 && (n % 100 == 1 || n % 100 == 21 || n % 100 == 41 || n % 100 == 61 || n % 100 == 81)) ? 4 : 5))))
%% cases: ["zero","one","two","few","many","other"]
formula_24(N) -> 
if
    (N == 0) -> 0;
    (N == 1) -> 1;
    ((((N rem 100) == 2) or ((N rem 100) == 22) or ((N rem 100) == 42) or ((N rem 100) == 62) or ((N rem 100) == 82)) or ((N rem 1000) == 0) and (((N rem 100000) >= 1000) and ((N rem 100000) =< 20000) or ((N rem 100000) == 40000) or ((N rem 100000) == 60000) or ((N rem 100000) == 80000)) or (N /= 0) and ((N rem 1000000) == 100000)) -> 2;
    (((N rem 100) == 3) or ((N rem 100) == 23) or ((N rem 100) == 43) or ((N rem 100) == 63) or ((N rem 100) == 83)) -> 3;
    ((N /= 1) and ((((N rem 100) == 1) or ((N rem 100) == 21) or ((N rem 100) == 41) or ((N rem 100) == 61) or ((N rem 100) == 81)))) -> 4;
    true -> 5
end.

%% nplurals: 6
%% C formula: (n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n % 100 >= 3 && n % 100 <= 10) ? 3 : ((n % 100 >= 11 && n % 100 <= 99) ? 4 : 5))))
%% cases: ["zero","one","two","few","many","other"]
formula_25(N) -> 
if
    (N == 0) -> 0;
    (N == 1) -> 1;
    (N == 2) -> 2;
    (((N rem 100) >= 3) and ((N rem 100) =< 10)) -> 3;
    (((N rem 100) >= 11) and ((N rem 100) =< 99)) -> 4;
    true -> 5
end.

%% nplurals: 6
%% C formula: (n == 0) ? 0 : ((n == 1) ? 1 : ((n == 2) ? 2 : ((n == 3) ? 3 : ((n == 6) ? 4 : 5))))
%% cases: ["zero","one","two","few","many","other"]
formula_26(N) -> 
if
    (N == 0) -> 0;
    (N == 1) -> 1;
    (N == 2) -> 2;
    (N == 3) -> 3;
    (N == 6) -> 4;
    true -> 5
end.

-spec n_to_index(Language, N) -> Index | undefined when
    Language :: string(),
    N :: integer(),
    Index :: integer().
%% @doc Returns plural form index for N or 'undefined' if language is unknown to us
n_to_index("bm", N) -> formula_0(N);
n_to_index("bo", N) -> formula_0(N);
n_to_index("dz", N) -> formula_0(N);
n_to_index("hnj", N) -> formula_0(N);
n_to_index("id", N) -> formula_0(N);
n_to_index("ig", N) -> formula_0(N);
n_to_index("ii", N) -> formula_0(N);
n_to_index("ja", N) -> formula_0(N);
n_to_index("jbo", N) -> formula_0(N);
n_to_index("jv", N) -> formula_0(N);
n_to_index("jw", N) -> formula_0(N);
n_to_index("kde", N) -> formula_0(N);
n_to_index("kea", N) -> formula_0(N);
n_to_index("km", N) -> formula_0(N);
n_to_index("ko", N) -> formula_0(N);
n_to_index("lkt", N) -> formula_0(N);
n_to_index("lo", N) -> formula_0(N);
n_to_index("ms", N) -> formula_0(N);
n_to_index("my", N) -> formula_0(N);
n_to_index("nqo", N) -> formula_0(N);
n_to_index("osa", N) -> formula_0(N);
n_to_index("sah", N) -> formula_0(N);
n_to_index("ses", N) -> formula_0(N);
n_to_index("sg", N) -> formula_0(N);
n_to_index("su", N) -> formula_0(N);
n_to_index("th", N) -> formula_0(N);
n_to_index("to", N) -> formula_0(N);
n_to_index("tpi", N) -> formula_0(N);
n_to_index("vi", N) -> formula_0(N);
n_to_index("wo", N) -> formula_0(N);
n_to_index("yo", N) -> formula_0(N);
n_to_index("yue", N) -> formula_0(N);
n_to_index("zh", N) -> formula_0(N);
n_to_index("zh_Hans", N) -> formula_0(N);
n_to_index("zh_Hant", N) -> formula_0(N);
n_to_index("af", N) -> formula_1(N);
n_to_index("an", N) -> formula_1(N);
n_to_index("asa", N) -> formula_1(N);
n_to_index("ast", N) -> formula_1(N);
n_to_index("az", N) -> formula_1(N);
n_to_index("bal", N) -> formula_1(N);
n_to_index("bem", N) -> formula_1(N);
n_to_index("bez", N) -> formula_1(N);
n_to_index("bg", N) -> formula_1(N);
n_to_index("brx", N) -> formula_1(N);
n_to_index("ca", N) -> formula_1(N);
n_to_index("ce", N) -> formula_1(N);
n_to_index("cgg", N) -> formula_1(N);
n_to_index("chr", N) -> formula_1(N);
n_to_index("ckb", N) -> formula_1(N);
n_to_index("da", N) -> formula_1(N);
n_to_index("de", N) -> formula_1(N);
n_to_index("de_AT", N) -> formula_1(N);
n_to_index("de_CH", N) -> formula_1(N);
n_to_index("dv", N) -> formula_1(N);
n_to_index("ee", N) -> formula_1(N);
n_to_index("el", N) -> formula_1(N);
n_to_index("en", N) -> formula_1(N);
n_to_index("en_AU", N) -> formula_1(N);
n_to_index("en_CA", N) -> formula_1(N);
n_to_index("en_GB", N) -> formula_1(N);
n_to_index("en_US", N) -> formula_1(N);
n_to_index("eo", N) -> formula_1(N);
n_to_index("et", N) -> formula_1(N);
n_to_index("eu", N) -> formula_1(N);
n_to_index("fi", N) -> formula_1(N);
n_to_index("fo", N) -> formula_1(N);
n_to_index("fur", N) -> formula_1(N);
n_to_index("fy", N) -> formula_1(N);
n_to_index("gl", N) -> formula_1(N);
n_to_index("gsw", N) -> formula_1(N);
n_to_index("ha", N) -> formula_1(N);
n_to_index("haw", N) -> formula_1(N);
n_to_index("hu", N) -> formula_1(N);
n_to_index("ia", N) -> formula_1(N);
n_to_index("io", N) -> formula_1(N);
n_to_index("jgo", N) -> formula_1(N);
n_to_index("jmc", N) -> formula_1(N);
n_to_index("ka", N) -> formula_1(N);
n_to_index("kaj", N) -> formula_1(N);
n_to_index("kcg", N) -> formula_1(N);
n_to_index("kk", N) -> formula_1(N);
n_to_index("kkj", N) -> formula_1(N);
n_to_index("kl", N) -> formula_1(N);
n_to_index("ks", N) -> formula_1(N);
n_to_index("ksb", N) -> formula_1(N);
n_to_index("ku", N) -> formula_1(N);
n_to_index("ky", N) -> formula_1(N);
n_to_index("lb", N) -> formula_1(N);
n_to_index("lg", N) -> formula_1(N);
n_to_index("lij", N) -> formula_1(N);
n_to_index("mas", N) -> formula_1(N);
n_to_index("mgo", N) -> formula_1(N);
n_to_index("ml", N) -> formula_1(N);
n_to_index("mn", N) -> formula_1(N);
n_to_index("mr", N) -> formula_1(N);
n_to_index("nah", N) -> formula_1(N);
n_to_index("nb", N) -> formula_1(N);
n_to_index("nd", N) -> formula_1(N);
n_to_index("ne", N) -> formula_1(N);
n_to_index("nl", N) -> formula_1(N);
n_to_index("nl_BE", N) -> formula_1(N);
n_to_index("nn", N) -> formula_1(N);
n_to_index("nnh", N) -> formula_1(N);
n_to_index("no", N) -> formula_1(N);
n_to_index("nr", N) -> formula_1(N);
n_to_index("ny", N) -> formula_1(N);
n_to_index("nyn", N) -> formula_1(N);
n_to_index("om", N) -> formula_1(N);
n_to_index("or", N) -> formula_1(N);
n_to_index("os", N) -> formula_1(N);
n_to_index("pap", N) -> formula_1(N);
n_to_index("ps", N) -> formula_1(N);
n_to_index("rm", N) -> formula_1(N);
n_to_index("rof", N) -> formula_1(N);
n_to_index("rwk", N) -> formula_1(N);
n_to_index("saq", N) -> formula_1(N);
n_to_index("sc", N) -> formula_1(N);
n_to_index("scn", N) -> formula_1(N);
n_to_index("sd", N) -> formula_1(N);
n_to_index("sdh", N) -> formula_1(N);
n_to_index("seh", N) -> formula_1(N);
n_to_index("sn", N) -> formula_1(N);
n_to_index("so", N) -> formula_1(N);
n_to_index("sq", N) -> formula_1(N);
n_to_index("ss", N) -> formula_1(N);
n_to_index("ssy", N) -> formula_1(N);
n_to_index("st", N) -> formula_1(N);
n_to_index("sv", N) -> formula_1(N);
n_to_index("sw", N) -> formula_1(N);
n_to_index("sw_CD", N) -> formula_1(N);
n_to_index("syr", N) -> formula_1(N);
n_to_index("ta", N) -> formula_1(N);
n_to_index("te", N) -> formula_1(N);
n_to_index("teo", N) -> formula_1(N);
n_to_index("tig", N) -> formula_1(N);
n_to_index("tk", N) -> formula_1(N);
n_to_index("tn", N) -> formula_1(N);
n_to_index("tr", N) -> formula_1(N);
n_to_index("ts", N) -> formula_1(N);
n_to_index("ug", N) -> formula_1(N);
n_to_index("ur", N) -> formula_1(N);
n_to_index("uz", N) -> formula_1(N);
n_to_index("ve", N) -> formula_1(N);
n_to_index("vo", N) -> formula_1(N);
n_to_index("vun", N) -> formula_1(N);
n_to_index("wae", N) -> formula_1(N);
n_to_index("xh", N) -> formula_1(N);
n_to_index("xog", N) -> formula_1(N);
n_to_index("yi", N) -> formula_1(N);
n_to_index("ceb", N) -> formula_2(N);
n_to_index("fil", N) -> formula_2(N);
n_to_index("tl", N) -> formula_2(N);
n_to_index("is", N) -> formula_3(N);
n_to_index("mk", N) -> formula_3(N);
n_to_index("ak", N) -> formula_4(N);
n_to_index("am", N) -> formula_4(N);
n_to_index("as", N) -> formula_4(N);
n_to_index("bho", N) -> formula_4(N);
n_to_index("bn", N) -> formula_4(N);
n_to_index("doi", N) -> formula_4(N);
n_to_index("fa", N) -> formula_4(N);
n_to_index("fa_AF", N) -> formula_4(N);
n_to_index("ff", N) -> formula_4(N);
n_to_index("gu", N) -> formula_4(N);
n_to_index("guw", N) -> formula_4(N);
n_to_index("hi", N) -> formula_4(N);
n_to_index("hy", N) -> formula_4(N);
n_to_index("kab", N) -> formula_4(N);
n_to_index("kn", N) -> formula_4(N);
n_to_index("ln", N) -> formula_4(N);
n_to_index("mg", N) -> formula_4(N);
n_to_index("nso", N) -> formula_4(N);
n_to_index("pa", N) -> formula_4(N);
n_to_index("pcm", N) -> formula_4(N);
n_to_index("si", N) -> formula_4(N);
n_to_index("ti", N) -> formula_4(N);
n_to_index("wa", N) -> formula_4(N);
n_to_index("zu", N) -> formula_4(N);
n_to_index("tzm", N) -> formula_5(N);
n_to_index("lt", N) -> formula_6(N);
n_to_index("be", N) -> formula_7(N);
n_to_index("bs", N) -> formula_7(N);
n_to_index("hr", N) -> formula_7(N);
n_to_index("ru", N) -> formula_7(N);
n_to_index("sh", N) -> formula_7(N);
n_to_index("sr", N) -> formula_7(N);
n_to_index("sr_ME", N) -> formula_7(N);
n_to_index("uk", N) -> formula_7(N);
n_to_index("shi", N) -> formula_8(N);
n_to_index("pl", N) -> formula_9(N);
n_to_index("mo", N) -> formula_10(N);
n_to_index("ro", N) -> formula_10(N);
n_to_index("ro_MD", N) -> formula_10(N);
n_to_index("cs", N) -> formula_11(N);
n_to_index("sk", N) -> formula_11(N);
n_to_index("fr", N) -> formula_12(N);
n_to_index("fr_CA", N) -> formula_12(N);
n_to_index("fr_CH", N) -> formula_12(N);
n_to_index("pt", N) -> formula_12(N);
n_to_index("pt_BR", N) -> formula_12(N);
n_to_index("es", N) -> formula_13(N);
n_to_index("es_419", N) -> formula_13(N);
n_to_index("es_ES", N) -> formula_13(N);
n_to_index("es_MX", N) -> formula_13(N);
n_to_index("it", N) -> formula_13(N);
n_to_index("pt_PT", N) -> formula_13(N);
n_to_index("iu", N) -> formula_14(N);
n_to_index("naq", N) -> formula_14(N);
n_to_index("sat", N) -> formula_14(N);
n_to_index("se", N) -> formula_14(N);
n_to_index("sma", N) -> formula_14(N);
n_to_index("smi", N) -> formula_14(N);
n_to_index("smj", N) -> formula_14(N);
n_to_index("smn", N) -> formula_14(N);
n_to_index("sms", N) -> formula_14(N);
n_to_index("lv", N) -> formula_15(N);
n_to_index("prg", N) -> formula_15(N);
n_to_index("ksh", N) -> formula_16(N);
n_to_index("lag", N) -> formula_16(N);
n_to_index("mt", N) -> formula_17(N);
n_to_index("gv", N) -> formula_18(N);
n_to_index("dsb", N) -> formula_19(N);
n_to_index("hsb", N) -> formula_19(N);
n_to_index("sl", N) -> formula_19(N);
n_to_index("gd", N) -> formula_20(N);
n_to_index("he", N) -> formula_21(N);
n_to_index("br", N) -> formula_22(N);
n_to_index("ga", N) -> formula_23(N);
n_to_index("kw", N) -> formula_24(N);
n_to_index("ar", N) -> formula_25(N);
n_to_index("ar_001", N) -> formula_25(N);
n_to_index("ars", N) -> formula_25(N);
n_to_index("cy", N) -> formula_26(N);
n_to_index(L, _N) ->
    ?T__LOG(warning, "Unknown/invalid language.", [{language, L}]),
    0.
