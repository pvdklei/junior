use super::{proxy::RefProxy, reference::Ref};
use std::{
    alloc,
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone)]
pub struct Value<T> {
    pub data: T,
    proxy: *mut RefProxy,
}

impl<T> Value<T> {
    // creates the value wrapper but doesnt create the proxy yet,
    // as thats not needed if no refs are taken
    pub fn new(data: T) -> Self {
        Self {
            data,
            proxy: std::ptr::null_mut(),
        }
    }

    // if the value has moved the mem addr of the proxy must be changed
    pub fn has_moved(&mut self) {
        unsafe {
            self.proxy
                .as_mut()
                .map(|p| p.set_address(self as *mut Self as *mut u8));
        }
    }

    // taking a ref creates a proxy if not done already,
    // and increases the counter
    pub fn take_ref(&mut self) -> Ref<T> {
        self.proxy().increase_count();
        Ref::new(self.proxy)
    }

    pub fn adopt<C>(&mut self, child: *const Value<C>) {
        // safe because a child can never be a constant memory space like a static or const
        let child = unsafe { (child as *mut Value<C>).as_mut().unwrap() };
        self.proxy().adopt_child(child.proxy());
    }

    fn proxy<'a>(&mut self) -> &'a mut RefProxy {
        if self.proxy.is_null() {
            self.init_proxy();
        }
        unsafe { self.proxy.as_mut().unwrap() as &mut RefProxy }
    }

    fn init_proxy(&mut self) {
        self.proxy = RefProxy::new_root(&mut self.data);
        unsafe { self.proxy.as_mut().unwrap().increase_count() };
    }
}

impl<T> Deref for Value<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Value<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Drop for Value<T> {
    fn drop(&mut self) {
        unsafe {
            self.proxy.as_mut().map(|p| {
                if p.refs_left() {
                    p.maybe_move_to_heap(&mut self.data);
                }
                p.decrease_count();
            });
        }
    }
}
