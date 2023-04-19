signature Pi_TOKENS =
sig
type ('a,'b) token
type svalue
val TDO:  'a * 'a -> (svalue,'a) token
val TOD:  'a * 'a -> (svalue,'a) token
val TWHILE:  'a * 'a -> (svalue,'a) token
val TFI:  'a * 'a -> (svalue,'a) token
val TELSE:  'a * 'a -> (svalue,'a) token
val TTHEN:  'a * 'a -> (svalue,'a) token
val TIF:  'a * 'a -> (svalue,'a) token
val TPRINT:  'a * 'a -> (svalue,'a) token
val TINTNUM: (BigInt.bigint) *  'a * 'a -> (svalue,'a) token
val TBOOLNUM: (bool) *  'a * 'a -> (svalue,'a) token
val TRATNUM: (Rational.rational) *  'a * 'a -> (svalue,'a) token
val TGE:  'a * 'a -> (svalue,'a) token
val TGT:  'a * 'a -> (svalue,'a) token
val TLE:  'a * 'a -> (svalue,'a) token
val TLT:  'a * 'a -> (svalue,'a) token
val TNE:  'a * 'a -> (svalue,'a) token
val TEQ:  'a * 'a -> (svalue,'a) token
val TOR:  'a * 'a -> (svalue,'a) token
val TAND:  'a * 'a -> (svalue,'a) token
val TNOT:  'a * 'a -> (svalue,'a) token
val TMOD:  'a * 'a -> (svalue,'a) token
val TDIV:  'a * 'a -> (svalue,'a) token
val TMUL:  'a * 'a -> (svalue,'a) token
val TSUB:  'a * 'a -> (svalue,'a) token
val TADD:  'a * 'a -> (svalue,'a) token
val TRATDIV:  'a * 'a -> (svalue,'a) token
val TRATMUL:  'a * 'a -> (svalue,'a) token
val TRATSUB:  'a * 'a -> (svalue,'a) token
val TRATADD:  'a * 'a -> (svalue,'a) token
val TINV:  'a * 'a -> (svalue,'a) token
val TNEG:  'a * 'a -> (svalue,'a) token
val TASSIGN:  'a * 'a -> (svalue,'a) token
val TEOF:  'a * 'a -> (svalue,'a) token
val TCOMMA:  'a * 'a -> (svalue,'a) token
val TRPAREN:  'a * 'a -> (svalue,'a) token
val TLPAREN:  'a * 'a -> (svalue,'a) token
val TRBRACE:  'a * 'a -> (svalue,'a) token
val TLBRACE:  'a * 'a -> (svalue,'a) token
val TSEMI:  'a * 'a -> (svalue,'a) token
val TIDEN: (string) *  'a * 'a -> (svalue,'a) token
val TBOOLEAN:  'a * 'a -> (svalue,'a) token
val TINTEGER:  'a * 'a -> (svalue,'a) token
val TRATIONAL:  'a * 'a -> (svalue,'a) token
end
signature Pi_LRVALS=
sig
structure Tokens : Pi_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
