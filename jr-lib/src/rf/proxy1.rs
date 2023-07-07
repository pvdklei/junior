use core::panic;
use std::alloc::{alloc, dealloc, Layout};

#[derive(Debug)]
pub enum RefProxy {
    Root {
        data: *mut u8,
        refcount: usize,
        layout: Option<Layout>,
    },
    Child {
        parent: *mut RefProxy,
        refcount: usize,
        offset: usize,
    },
}

struct ProxyPool {
    free: *mut RefProxyChunk,
}
#[repr(C)]
struct RefProxyChunk {
    proxy: RefProxy,
    next: *mut RefProxyChunk,
}
static mut PROXY_POOL: *mut ProxyPool = std::ptr::null_mut();

impl RefProxy {
    const BLOCK_SIZE: usize = 100;

    pub fn new_root<T>(data: &mut T) -> *mut RefProxy {
        unsafe {
            let ptr = Self::alloc_proxy();
            *ptr = Self::Root {
                data: data as *mut T as *mut u8,
                refcount: 0,
                layout: None,
            };
            ptr
        }
    }

    fn pool<'a>() -> &'a mut ProxyPool {
        unsafe { PROXY_POOL.as_mut().expect("proxy pool was not initialized") }
    }

    fn alloc_proxy() -> *mut RefProxy {
        // unsafe {
        //     let pool = Self::pool();
        //     if pool.free.is_null() {
        //         // ran out of free chunks, so creating a block of new ones
        //         let new_block_ptr =
        //             alloc(Layout::array::<RefProxyChunk>(Self::BLOCK_SIZE).unwrap());
        //         for i in 0..Self::BLOCK_SIZE {
        //             let chunk_ptr = (new_block_ptr as usize
        //                 + (i * std::mem::size_of::<RefProxyChunk>()))
        //                 as *mut RefProxyChunk;
        //             (*chunk_ptr).next = pool.free;
        //             pool.free = chunk_ptr;
        //         }
        //     }

        //     // takes a chunk from the begin of the linked list
        //     let next_free = (*pool.free).next;
        //     let free = pool.free;
        //     pool.free = next_free;
        //     free as *mut RefProxy
        // }
        unsafe { alloc(Layout::new::<RefProxy>()) as *mut RefProxy }
    }

    fn dealloc_proxy(ptr: *mut RefProxy) {
        // adds the chunk to the start of the list again
        // unsafe {
        //     let chunk = ptr as *mut RefProxyChunk;
        //     let pool = Self::pool();
        //     (*chunk).next = pool.free;
        //     pool.free = chunk;
        // }
        unsafe { dealloc(ptr as *mut u8, Layout::new::<RefProxy>()) }
    }

    pub fn dealloc(&mut self) {
        Self::dealloc_proxy(self as *mut RefProxy);
    }

    pub fn init_pool() {
        unsafe {
            PROXY_POOL =
                std::alloc::alloc(std::alloc::Layout::new::<ProxyPool>()) as *mut ProxyPool;
            *PROXY_POOL = ProxyPool {
                free: std::ptr::null_mut(),
            };
        }
    }

    // assumes the child already has a relative memeory addres to the parents data
    pub fn adopt_child(&mut self, child_root: &mut Self) {
        match child_root {
            Self::Root {
                refcount,
                data,
                layout: None,
            } => {
                *child_root = Self::Child {
                    parent: self as *mut Self,
                    refcount: *refcount,
                    offset: *data as usize - self.data() as usize,
                };
            }
            Self::Root {
                layout: Some(_), ..
            } => panic!("Only an onwed value can get adopted, so its proxy cannot have a layout"),
            Self::Child { .. } => {
                panic!("A rf::RefProxy can not become some parents child if it already has another parent");
            }
        }
        match self {
            Self::Root { refcount, .. } => *refcount += 1,
            Self::Child { refcount, .. } => *refcount += 1,
        };
    }

    pub fn data(&self) -> *mut u8 {
        unsafe {
            let mut parent_ = self;
            let mut total_offset = 0;
            loop {
                match parent_ {
                    Self::Child { parent, offset, .. } => {
                        total_offset += offset;
                        parent_ = parent.as_ref().unwrap();
                    }
                    Self::Root { data, .. } => return (*data as usize + total_offset) as *mut u8,
                }
            }
        }
    }

    pub fn set_address(&mut self, addr: *mut u8) {
        match self {
            Self::Root { data, .. } => *data = addr,
            Self::Child {
                parent, refcount, ..
            } => {
                unsafe {
                    parent.as_mut().unwrap().decrease_count();
                }
                *self = Self::Root {
                    data: addr,
                    refcount: *refcount,
                    layout: None,
                }
            }
        }
    }
    pub fn decrease_count(&mut self) {
        match self {
            Self::Root {
                refcount: 1,
                layout: Some(layout),
                data,
                ..
            } => unsafe {
                dealloc(*data, *layout);
                self.dealloc();
            },
            Self::Root { refcount, .. } => *refcount -= 1,
            Self::Child {
                refcount: 1,
                parent,
                ..
            } => unsafe {
                parent.as_mut().unwrap().decrease_count();
                self.dealloc();
            },
            Self::Child { refcount, .. } => *refcount -= 1,
        }
    }

    pub fn increase_count(&mut self) {
        match self {
            Self::Root { refcount, .. } => *refcount += 1,
            Self::Child { refcount, .. } => *refcount += 1,
        }
    }
}
