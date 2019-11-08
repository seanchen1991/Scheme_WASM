use std::rc::Rc;
use std::cell::RefCell;
use std::iter::Peekable;

use iter::GentleIterator;

#[derive(Debug, Clone)]
pub enum Token {
  LParen,
  RParen,
  Symbol(String),
  Integer(i64),
  Float(f64),
  Boolean(bool),
  Chr(char),
  Str(Rc<RefCell<String>>)
}

impl Token {
  fn get(ch: char) -> Token {
    match ch {
      '(' => Token::LParen,
      ')' => Token::RParen,
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

impl<I: Iterator<Item=char>> Iterator for TokenIterator<I> {
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
    tokens.push(x);
  }

  tokens
}

fn parse_whitespace<I>(iter: &mut Peekable<I>) -> bool
  where I: Iterator<Item=char>
{
  if check_chr(iter, ' ') || check_chr(iter, '\n') {
    iter.next();
    true
  } else {
    false
  }
}

fn parse_comment<I>(iter: &mut Peekable<I>) -> bool 
  where I: Iterator<Item=char>
{
  if check_chr(iter, ';') {
    iter.take_until(|c| *c != '\n');
    true
  } else {
    false
  }
}

fn parse_lparen<I>(iter: &mut Peekable<I>) -> Option<Token>
  where I: Iterator<Item=char>
{
  parse_single(iter, '(')
}

fn parse_rparen<I>(iter: &mut Peekable<I>) -> Option<Token>
  where I: Iterator<Item=char>
{
  parse_single(iter, ')')
}

fn parse_string<I>(iter: &mut Peekable<I>) -> Option<Token>
  where I: Iterator<Item=char>
{
  if !check_chr(iter, '"') {
    return None;
  }

  iter.next();
  let value = iter.take_until(|c| *c != '"').collect();
  iter.next();
  
  Some(Token::Str(Rc::new(RefCell::new(value))))
}

fn parse_hash<I>(iter: &mut Peekable<I>) -> Option<Token>
  where I: Iterator<Item=char>
{
  if !check_chr(iter, '#') {
    return None;
  }

  iter.next();
  match iter.next() {
    Some('t') => Some(Token::Boolean(true)),
    Some('f') => Some(Token::Boolean(false)),
    Some(c)   => panic!("Expected #t or #f, got: #{}", c),
    None      => panic!("Expected char after '#', got none")
  }
}

fn parse_symbol<I>(iter: &mut Peekable<I>) -> Option<Token>
  where I: Iterator<Item=char>
{
  if !check(iter, |_| true) {
    return None;
  }

  let value: String = iter.take_until(|c| *c != ' ' && *c != ')' && *c != '\n').collect();

  parse_number(&value)
    .or_else(|| Some(Token::Symbol(value)))
}

fn parse_number(value: &str) -> Option<Token> {
  value.parse::<i64>().map(Token::Integer)
    .or_else(|_| value.parse::<f64>().map(Token::Float))
    .ok()
}

fn parse_single<I>(iter: &mut Peekable<I>, chr: char) -> Option<Token>
  where I: Iterator<Item=char>
{
  if !check_chr(iter, chr) {
    return None;
  }

  iter.next();
  Some(Token::get(chr))
}

fn check<F, I>(iter: &mut Peekable<I>, f: F) -> bool 
  where F: Fn(char) -> bool,
        I: Iterator<Item=char>
{
  if let Some(&x) = iter.peek() {
    f(x)
  } else {
    false
  }
}

fn check_chr<I>(iter: &mut Peekable<I>, chr: char) -> bool 
  where I: Iterator<Item=char>
{
  check(iter, |x| x == chr)
}
