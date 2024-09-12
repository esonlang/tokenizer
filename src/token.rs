use std::fmt::{Display, Formatter};

use nom::Slice;

use crate::Span;

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TokenPosition {
    pub line: u32,
    pub column: usize,
}

impl<'a> From<Span<'a>> for TokenPosition {
    fn from(span: Span) -> Self {
        TokenPosition {
            line: span.location_line(),
            column: span.get_utf8_column(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TokenRange(pub TokenPosition, pub TokenPosition);

impl Default for TokenRange {
    fn default() -> Self {
        TokenRange(
            TokenPosition { line: 0, column: 0 },
            TokenPosition { line: 0, column: 0 },
        )
    }
}

impl From<(Span<'_>, Span<'_>)> for TokenRange {
    fn from((start, end): (Span, Span)) -> TokenRange {
        TokenRange(TokenPosition::from(start), TokenPosition::from(end))
    }
}

impl From<(Span<'_>, usize)> for TokenRange {
    fn from((start, len): (Span, usize)) -> TokenRange {
        TokenRange(
            TokenPosition::from(start),
            TokenPosition::from(start.slice(len..)),
        )
    }
}

impl From<(Span<'_>, usize, usize)> for TokenRange {
    fn from((s, abs_offset_i, abs_offset_j): (Span, usize, usize)) -> Self {
        TokenRange(
            TokenPosition::from(s.slice(abs_offset_i..)),
            TokenPosition::from(s.slice(abs_offset_j..)),
        )
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum TokenKey {
    Dummy,
    DummySn(usize),
    Sn(usize, TokenRange),
    String(String, TokenRange),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenId(pub String, pub TokenRange);

impl Display for TokenId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TokenId {
    pub fn name(&self) -> &str {
        &self.0.as_str()
    }
}

// TokenDecorator
#[derive(Debug, PartialEq, Clone)]
pub struct TokenDec(pub TokenId, pub Vec<Token>, pub TokenRange);

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Void,
    TokenPrimNull(TokenRange),
    TokenPrimNumberInt(i64, TokenRange),
    TokenPrimNumberFloat(f64, TokenRange),
    TokenPrimString(String, TokenRange),
    TokenPrimBoolean(bool, TokenRange),
    TokenVar(Vec<TokenKey>, TokenRange),
    TokenComment(String, TokenRange),
    TokenRefVarSibling(Vec<TokenKey>, TokenRange),
    TokenRefVarUncle(Vec<TokenKey>, TokenRange),
    TokenRefVarRoot(Vec<TokenKey>, TokenRange),
    TokenFnCall(TokenId, Vec<Token>, TokenRange),
    TokenExprSequence(Vec<Token>, TokenRange),
    TokenFrameExprGroup(Box<Token>, TokenRange),
    TokenFrameRoot(Vec<TokenDec>, Box<Token>, TokenRange), // (decorators, body)
    TokenFrameList(Vec<(TokenKey, Vec<TokenDec>, Token)>, TokenRange), // Vec<(index, decorators, body)>
    TokenFrameDict(Vec<(TokenKey, Vec<TokenDec>, Token)>, TokenRange), // Vec<(index, decorators, body)>
    TokenOpPipe(TokenRange),
    TokenOpEq(TokenRange),
    TokenOpNe(TokenRange),
    TokenOpLe(TokenRange),
    TokenOpGe(TokenRange),
    TokenOpAnd(TokenRange),
    TokenOpOr(TokenRange),
    TokenOpNot(TokenRange),
    TokenOpGt(TokenRange),
    TokenOpLt(TokenRange),
    TokenOpConcat(TokenRange),
    TokenDummyOpConcat,
    TokenOpAdd(TokenRange),
    TokenOpSub(TokenRange),
    TokenOpMul(TokenRange),
    TokenOpDiv(TokenRange),
    TokenOpMod(TokenRange),
    TokenOpQ(TokenRange),
    TokenOpColon(TokenRange),
}

impl Display for TokenKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKey::Dummy => write!(f, "_"),
            TokenKey::DummySn(i) => write!(f, "{}", i),
            TokenKey::Sn(i, ..) => write!(f, "{}", i),
            TokenKey::String(s, ..) => write!(f, "{}", s),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token::Void
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Void => write!(f, "()"),
            Token::TokenPrimNumberInt(v, _) => write!(f, "{}", v),
            Token::TokenPrimNumberFloat(v, _) => write!(f, "{}", v),
            Token::TokenPrimString(v, _) => write!(f, "{}", v),
            Token::TokenPrimBoolean(v, _) => write!(f, "{}", v),
            Token::TokenPrimNull(_) => write!(f, "null"),
            Token::TokenFnCall(n, _, _) => write!(f, "{}(..)", n),
            Token::TokenVar(v, _) => write!(
                f,
                "${}",
                v.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(".")
            ),
            Token::TokenComment(v, _) => write!(f, "/* {} */", v),
            Token::TokenRefVarSibling(v, _) => write!(f, "&sibling.{:?}", v),
            Token::TokenRefVarUncle(v, _) => write!(f, "&uncle.{:?}", v),
            Token::TokenRefVarRoot(v, _) => write!(f, "&root.{:?}", v),
            Token::TokenFrameRoot(_, _, _) => write!(f, "base{{..}}"),
            Token::TokenFrameDict(_, _) => write!(f, "{{..}}"),
            Token::TokenFrameList(_, _) => write!(f, "[..]"),
            Token::TokenExprSequence(s, _) => write!(f, "{:?}", s),
            Token::TokenFrameExprGroup(s, _) => write!(f, "{:?}", s),
            Token::TokenOpPipe(_) => write!(f, "|"),
            Token::TokenOpEq(_) => write!(f, "=="),
            Token::TokenOpNe(_) => write!(f, "!="),
            Token::TokenOpLe(_) => write!(f, "<="),
            Token::TokenOpGe(_) => write!(f, ">="),
            Token::TokenOpAnd(_) => write!(f, "&&"),
            Token::TokenOpOr(_) => write!(f, "||"),
            Token::TokenOpNot(_) => write!(f, "!"),
            Token::TokenOpGt(_) => write!(f, ">"),
            Token::TokenOpLt(_) => write!(f, "<"),
            Token::TokenOpConcat(_) => write!(f, ".."),
            Token::TokenDummyOpConcat => write!(f, ".."),
            Token::TokenOpAdd(_) => write!(f, "+"),
            Token::TokenOpSub(_) => write!(f, "-"),
            Token::TokenOpMul(_) => write!(f, "*"),
            Token::TokenOpDiv(_) => write!(f, "/"),
            Token::TokenOpMod(_) => write!(f, "%"),
            Token::TokenOpQ(_) => write!(f, "?"),
            Token::TokenOpColon(_) => write!(f, ":"),
        }
    }
}

pub trait ReadTokenRange {
    fn range(&self) -> TokenRange;
}

impl ReadTokenRange for Token {
    fn range(&self) -> TokenRange {
        match self {
            Token::Void => TokenRange::default(),
            Token::TokenPrimNumberInt(_, r) => r.clone(),
            Token::TokenPrimNumberFloat(_, r) => r.clone(),
            Token::TokenPrimString(_, r) => r.clone(),
            Token::TokenPrimBoolean(_, r) => r.clone(),
            Token::TokenPrimNull(r) => r.clone(),
            Token::TokenFnCall(_, _, r) => r.clone(),
            Token::TokenVar(_, r) => r.clone(),
            Token::TokenComment(_, r) => r.clone(),
            Token::TokenRefVarSibling(_, r) => r.clone(),
            Token::TokenRefVarUncle(_, r) => r.clone(),
            Token::TokenRefVarRoot(_, r) => r.clone(),
            Token::TokenFrameRoot(_, _, r) => r.clone(),
            Token::TokenFrameDict(_, r) => r.clone(),
            Token::TokenFrameList(_, r) => r.clone(),
            Token::TokenExprSequence(_, r) => r.clone(),
            Token::TokenFrameExprGroup(_, r) => r.clone(),
            Token::TokenOpPipe(r) => r.clone(),
            Token::TokenOpEq(r) => r.clone(),
            Token::TokenOpNe(r) => r.clone(),
            Token::TokenOpLe(r) => r.clone(),
            Token::TokenOpGe(r) => r.clone(),
            Token::TokenOpAnd(r) => r.clone(),
            Token::TokenOpOr(r) => r.clone(),
            Token::TokenOpNot(r) => r.clone(),
            Token::TokenOpGt(r) => r.clone(),
            Token::TokenOpLt(r) => r.clone(),
            Token::TokenOpConcat(r) => r.clone(),
            Token::TokenDummyOpConcat => TokenRange::default(),
            Token::TokenOpAdd(r) => r.clone(),
            Token::TokenOpSub(r) => r.clone(),
            Token::TokenOpMul(r) => r.clone(),
            Token::TokenOpDiv(r) => r.clone(),
            Token::TokenOpMod(r) => r.clone(),
            Token::TokenOpQ(r) => r.clone(),
            Token::TokenOpColon(r) => r.clone(),
        }
    }
}

impl ReadTokenRange for TokenDec {
    fn range(&self) -> TokenRange {
        self.2.clone()
    }
}

impl ReadTokenRange for TokenKey {
    fn range(&self) -> TokenRange {
        match self {
            TokenKey::Dummy => TokenRange::default(),
            TokenKey::DummySn(_) => TokenRange::default(),
            TokenKey::Sn(_, r) => r.clone(),
            TokenKey::String(_, r) => r.clone(),
        }
    }
}

impl ReadTokenRange for TokenId {
    fn range(&self) -> TokenRange {
        self.1.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::TokenDec;

    #[test]
    fn test_read_token_range() {
        use crate::token::TokenPosition;
        use crate::token::{ReadTokenRange, Token, TokenId, TokenKey, TokenRange};
        fn random_token_range() -> TokenRange {
            let i = rand::random::<u8>();
            let m = rand::random::<u16>();
            let j = rand::random::<u8>();
            let n = rand::random::<u16>();

            TokenRange(
                TokenPosition {
                    line: i as u32,
                    column: m as usize,
                },
                TokenPosition {
                    line: (i as u32) + (j as u32),
                    column: n as usize,
                },
            )
        }

        let r = random_token_range();
        let t = Token::TokenPrimNumberInt(1, r.clone());
        assert_eq!(t.range(), r);

        let r = random_token_range();
        let t = Token::TokenPrimString("hello".to_string(), r.clone());
        assert_eq!(t.range(), r);

        let r = random_token_range();
        let t = Token::TokenRefVarSibling(vec![TokenKey::Dummy], r.clone());
        assert_eq!(t.range(), r);

        let r = random_token_range();
        let k = TokenKey::String("hello".to_string(), r.clone());
        assert_eq!(k.range(), r);

        let k = TokenKey::Dummy;
        assert_eq!(k.range(), TokenRange::default());

        let r = random_token_range();
        let i = TokenId("hello".to_string(), r.clone());
        assert_eq!(i.range(), r);

        let r = random_token_range();
        let d = TokenDec(i, vec![], r.clone());
        assert_eq!(d.range(), r);
    }
}
