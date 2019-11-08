use std::vec::IntoIter;
use std::iter::Peekable;

pub trait GentleIterator<I: Iterator> {
  fn take_until<F>(&mut self, pred: F) -> IntoIter<I::Item>
    where F: Fn(&I::Item) -> bool;
}

impl<I: Iterator> GentleIterator<I> for Peekable<I> {
  fn take_until<F>(&mut self, pred: F) -> IntoIter<I::Item>
    where F: Fn(&I::Item) -> bool
  {
    let mut v: Vec<I::Item> = vec![];
    while self.peek().map_or(false, &pred) {
      v.push(self.next().unwrap());
    }

    v.into_iter()
  }
}