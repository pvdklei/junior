use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use super::proxy::RefProxy;

pub struct Ref<T> {
    proxy: *mut RefProxy,
    phantom: PhantomData<T>,
}

impl<T> Ref<T> {
    pub fn new(proxy: *mut RefProxy) -> Self {
        Self {
            proxy,
            phantom: PhantomData,
        }
    }

    pub fn duplicate(&self) -> Self {
        unsafe {
            self.proxy.as_mut().unwrap().increase_count();
        }
        Self { ..*self }
    }

    pub fn dbg(&self) {
        unsafe {
            let proxy = self.proxy.as_mut().unwrap();
            println!("{:?}", proxy);
        }
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe {
            (self.proxy.as_ref().unwrap().data() as *mut T)
                .as_ref()
                .unwrap()
        }
    }
}

impl<T> DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            (self.proxy.as_ref().unwrap().data() as *mut T)
                .as_mut()
                .unwrap()
        }
    }
}

impl<T> Drop for Ref<T> {
    fn drop(&mut self) {
        unsafe {
            self.proxy.as_mut().unwrap().decrease_count();
        }
    }
}
