use std::alloc::alloc;
use std::alloc::dealloc;
use std::alloc::Layout;

#[derive(Debug)]
pub struct RefProxy {
    refcount: i32,
    detail: ProxyDetail,
}

#[derive(Debug)]
enum ProxyDetail {
    Root {
        data: *mut u8,
        heaped: Option<Layout>,
    },
    Child {
        parent: *mut RefProxy,
        offset: usize,
    },
}

impl RefProxy {
    pub fn new_root<T>(data: &mut T) -> *mut RefProxy {
        unsafe {
            let ptr = Self::alloc_proxy();
            *ptr = Self {
                refcount: 0,
                detail: ProxyDetail::Root {
                    data: data as *mut T as *mut u8,
                    heaped: None,
                },
            };
            ptr
        }
    }

    fn alloc_proxy() -> *mut RefProxy {
        unsafe { alloc(Layout::new::<RefProxy>()) as *mut RefProxy }
    }

    fn dealloc_proxy(ptr: *mut RefProxy) {
        unsafe { dealloc(ptr as *mut u8, Layout::new::<RefProxy>()) }
    }

    pub fn dealloc(&mut self) {
        Self::dealloc_proxy(self as *mut RefProxy);
    }

    // Is called when a value is moved into a parent struct. This way, the child
    // the childs memory address it taken relative to its parent struct, a value
    // does not have to change the proxies of its children when moved.
    pub fn adopt_child(&mut self, child_root: &mut Self) {
        match child_root.detail {
            ProxyDetail::Root { data, heaped: None } => {
                child_root.detail = ProxyDetail::Child {
                    parent: self as *mut Self,
                    offset: data as usize - self.data() as usize,
                };
            }
            ProxyDetail::Root {
                heaped: Some(_), ..
            } => panic!("Only an onwed value can get adopted, so its proxy cannot have a layout"),
            ProxyDetail::Child { .. } => {
                panic!("A rf::RefProxy can not become some parents child if it already has another parent");
            }
        }
        self.increase_count();
    }

    // Obtains a ptr to the underlying data. This data might be on the
    // stack, owned by a `Value`, or on the heap when the `Value` has
    // been dropped but there are still `Ref`s to it alive.
    pub fn data(&self) -> *mut u8 {
        unsafe {
            let mut parent_ = self;
            let mut total_offset = 0;
            loop {
                match parent_.detail {
                    ProxyDetail::Child { parent, offset, .. } => {
                        total_offset += offset;
                        parent_ = parent.as_ref().unwrap();
                    }
                    ProxyDetail::Root { data, .. } => {
                        return data.add(total_offset);
                    }
                }
            }
        }
    }

    // Is called when the `Value` belonging to this object is moved to another
    // memory addres. When it is moved from a parent struct (destructuring),
    // the proxy becomes a root again.
    pub fn set_address(&mut self, addr: *mut u8) {
        match self.detail {
            ProxyDetail::Root { ref mut data, .. } => *data = addr,
            ProxyDetail::Child { parent, .. } => {
                unsafe {
                    parent.as_mut().unwrap().decrease_count();
                }
                self.detail = ProxyDetail::Root {
                    data: addr,
                    heaped: None, // TODO: something else?
                };
            }
        }
    }

    // Decreases the reference count of the proxy, and drops them at 0.
    pub fn decrease_count(&mut self) {
        self.refcount -= 1;
        if self.refcount == 0 {
            match self.detail {
                ProxyDetail::Child { parent, .. } => unsafe {
                    parent.as_mut().unwrap().decrease_count();
                },
                ProxyDetail::Root {
                    data,
                    heaped: Some(layout),
                } => unsafe {
                    dealloc(data, layout);
                },
                _ => {}
            }
            self.dealloc();
        }
    }
    pub fn increase_count(&mut self) {
        self.refcount += 1;
    }

    pub fn refs_left(&self) -> bool {
        self.refcount > 0
    }

    pub fn maybe_move_to_heap<T>(&mut self, x: &mut T) {
        unsafe {
            if let ProxyDetail::Root { heaped, data, .. } = &mut self.detail {
                let new_layout = Layout::new::<T>();
                let ptr = alloc(new_layout) as *mut T;
                std::ptr::copy::<T>(x, ptr, 1);
                *heaped = Some(new_layout);
                *data = ptr as *mut u8;
            }
        }
    }
}
