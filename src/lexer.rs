use std::rc::Rc;
use std::cell::RefCell;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum Token {
  LParen,
  RParen,
  Symbol(String),
  Number(f64),
  Boolean(bool),
  Chr(char),
  Str(Rc<RefCell<String>>)
}

impl Token {
  fn get(ch: char) -> Token {
    match ch {
      "(" => Token::LParen,
      ")" => Token::RParen,
      x   => Token::Chr(x)
    }
  }
}

pub struct TokenIterator<I: Iterator<Item=char>> {
  inner: Peekable<I>
}

impl<I: Iterator<Item=char>> TokenIterator<I> {
  pub fn new(inner: I) -> Self {
    TokenIterator { inner: inner.peekable() }
  }
}

impl<I: Iterator<Item=char>> Iterator for TokenIterator {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    tokenize_single(&mut self.inner)
  }
}

pub fn tokenize_single<I>(iter: &mut Peekable<I>) -> Option<Token>
  where I: Iterator<Item=char>
{
  while parse_whitespace(iter) || parse_comment(iter) {
    continue
  }

  parse_lparen(iter)
    .or_else(|| parse_rparen(iter))
    .or_else(|| parse_string(iter))
    .or_else(|| parse_hash(iter))
    .or_else(|| parse_symbol(iter))
}

pub fn tokenize<I>(iter: &mut Peekable<I>) -> Vec<Token> 
  where I: Iterator<Item=char>
{
  let mut tokens: Vec<Token> = vec![];

  while let Some(x) = tokenize_single(iter) {
    tokens.push(x)
  }
}

fn parse_whitespace<I>(iter: &mut Peekable<I>) -> bool
  where I: Iterator<Item=char>
{
  if check_chr(iter, " ") || check_chr(iter, "\n") {
    iter.next();
    true
  }

  false
}

fn parse_comment<I>(iter: &mut Peekable<I>) -> bool 
  where I: Iterator<Item=char>
{
  if check_chr(iter, ";") {
    iter.take_until(|c| *c != "\n");
    true
  }

  false
}

