-module(base62).

-export([encode/1, encode/2]).

encode(Int) when is_integer(Int) ->
    encode_integer(Int, <<>>).
encode(Subject, PadTo) ->
    case encode(Subject) of
        Bin when size(Bin) < PadTo ->
            << (binary:copy(<<$0>>, PadTo - size(Bin)))/binary, Bin/binary >>;
        Bin -> Bin
    end.

encode_integer(0, Acc) ->
    Acc;
encode_integer(Int, Acc) ->
    encode_integer(Int div 62, << (digit(Int rem 62)), Acc/binary >>).

digit(B) ->
    element(B+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z,$A,$B,$C,$D,$E,$F,$G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,$W,$X,$Y,$Z}).
