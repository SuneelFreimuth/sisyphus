use std::{sync::{Mutex, MutexGuard}, ops::{DerefMut, Deref}};

pub fn equal_within(a: f64, b: f64, err: f64) -> bool {
    (a - b).abs() < err
}

// Dumbest possible wrapper to make a value atomic.
pub struct Guarded<T>(Mutex<T>);

impl<T> Guarded<T> {
    pub fn new(v: T) -> Self {
        Self(Mutex::new(v))
    }

    pub fn get(&self) -> MutexGuard<'_, T> {
        self.0.lock().unwrap()
    }

    pub fn set(&self, v: T) {
        *self.0.lock().unwrap() = v;
    }
}