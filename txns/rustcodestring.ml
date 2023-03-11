let str =
  "/// This is an implementation of a general purpose skip list. It was \
   originally\n\
   /// ported from a version of skiplists intended for efficient string handling\n\
   /// found here - https://github.com/josephg/rustrope\n\n\
   /// This implementation is not optimized for strings (there's some string\n\
   /// specific features like unicode handling which have been intentionally\n\
   /// removed for simplicity). But it does have another somewhat unusual \
   feature -\n\
   /// users can specify their own size function, and lookups, inserts and \
   deletes\n\
   /// can use their custom length property to specify offsets.\n\n\n\
   use std::{mem, ptr};\n\
   use std::mem::MaybeUninit;\n\
   use std::ptr::NonNull;\n\
   use std::alloc::{alloc, dealloc, Layout};\n\
   use std::cmp::min;\n\
   use std::marker::PhantomData;\n\
   use std::iter;\n\n\
   use std::fmt;\n\n\
   use rand::{RngCore, Rng, SeedableRng};\n\
   use rand::rngs::SmallRng;\n\n\
   /// The likelyhood a node will have height (n+1) instead of n\n\
   const BIAS: u8 = 100; // out of 256.\n\n\
   /// The number of items in each node. Must fit in a u8 thanks to Node.\n\
   #[cfg(debug_assertions)]\n\
   const NODE_NUM_ITEMS: usize = 2;\n\n\
   #[cfg(not(debug_assertions))]\n\
   const NODE_NUM_ITEMS: usize = 100;\n\n\
   /// List operations will move to linear time after NODE_STR_SIZE * 2 ^\n\
   /// MAX_HEIGHT length. (With a smaller constant the higher this is). On the \
   flip\n\
   /// side, cursors grow linearly with this number; so smaller is marginally\n\
   /// better when the contents are smaller.\n\
   #[cfg(debug_assertions)]\n\
   const MAX_HEIGHT: usize = 5;\n\n\
   #[cfg(not(debug_assertions))]\n\
   const MAX_HEIGHT: usize = 10;\n\n\n\
   const MAX_HEIGHT_U8: u8 = MAX_HEIGHT as u8; // convenience.\n\n\
   pub struct ItemMarker<Item: ListItem> {\n\
  \    pub(super) ptr: *mut Node<Item>,\n\
  \    // _phantom: PhantomData<&'a SkipList<C>>\n\
   }\n\n\
   // Derive traits don't work here.\n\
   impl<Item: ListItem> Clone for ItemMarker<Item> {\n\
  \    fn clone(&self) -> Self { *self }\n\
   }\n\
   impl<Item: ListItem> Copy for ItemMarker<Item> {}\n\
   impl<Item: ListItem> PartialEq for ItemMarker<Item> {\n\
  \    fn eq(&self, other: &Self) -> bool { self.ptr == other.ptr }\n\
   }\n\
   impl<Item: ListItem> Eq for ItemMarker<Item> {}\n\n\
   impl<Item: ListItem> ItemMarker<Item> {\n\
  \    pub fn null() -> ItemMarker<Item> {\n\
  \        ItemMarker { ptr: ptr::null_mut() }\n\
  \    }\n\n\
  \    pub fn is_null(self) -> bool {\n\
  \        self.ptr.is_null()\n\
  \    }\n\
   }\n\n\
   impl<Item: ListItem> Default for ItemMarker<Item> {\n\
  \    fn default() -> Self { Self::null() }\n\
   }\n\n\
   impl<Item: ListItem> fmt::Debug for ItemMarker<Item> {\n\
  \    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {\n\
  \        f.pad(\"ItemMarker\")\n\
  \    }\n\
   }\n\n\n\n\
   pub trait ListItem: Sized {\n\
  \    /// Applications which have custom sizes (or do their own\n\
  \    /// run-length-encoding) can define their own size function for items. \
   When\n\
  \    /// items are inserted or replaced, the position is specified using the\n\
  \    /// custom size defined here.\n\
  \    fn get_usersize(&self) -> usize { 1 }\n\n\
  \    /// An optimized method to calculate the userlen of a slice of ListItems.\n\
  \    /// The default implementation simply calls [`get_usersize`] in a loop.\n\
  \    fn userlen_of_slice(items: &[Self]) -> usize {\n\
  \        items.iter().fold(0, |acc, item| {\n\
  \            acc + Self::get_usersize(item)\n\
  \        })\n\
  \    }\n\n\
  \    /// Split the passed item into a pair of items at some offset.\n\
  \    fn split_item(&self, _at: usize) -> (Self, Self) {\n\
  \    // fn split_item(self, _at: usize) -> (Self, Self) {\n\
  \        unimplemented!(\"Cannot insert in the middle of an item - \
   split_item is not defined in trait\");\n\
  \    }\n\n\
  \    fn merge_from(&mut self, _other: &Self) -> bool {\n\
  \        false\n\
  \    }\n\
   }\n\n\
   // Blanket implementations for some common builtin types, because its \
   impossible\n\
   // to add these later. These make every item have a size of 1.\n\
   impl ListItem for () {}\n\
   impl<X, Y> ListItem for (X, Y) {}\n\
   impl<X, Y, Z> ListItem for (X, Y, Z) {}\n\
   impl<V> ListItem for Option<V> {}\n\
   impl<T, E> ListItem for Result<T, E> {}\n\n\
   impl<X, Y> ListItem for &(X, Y) {}\n\
   impl<X, Y, Z> ListItem for &(X, Y, Z) {}\n\
   impl<V> ListItem for &Option<V> {}\n\
   impl<T, E> ListItem for &Result<T, E> {}\n\n\
   impl ListItem for bool {}\n\
   impl ListItem for char {}\n\
   impl ListItem for u8 {}\n\
   impl ListItem for i8 {}\n\
   impl ListItem for u16 {}\n\
   impl ListItem for i16 {}\n\
   impl ListItem for u32 {}\n\
   impl ListItem for i32 {}\n\
   impl ListItem for usize {}\n\
   impl ListItem for isize {}\n\
   impl ListItem for f32 {}\n\
   impl ListItem for f64 {}\n\n\
   impl ListItem for &bool {}\n\
   impl ListItem for &char {}\n\
   impl ListItem for &u8 {}\n\
   impl ListItem for &i8 {}\n\
   impl ListItem for &u16 {}\n\
   impl ListItem for &i16 {}\n\
   impl ListItem for &u32 {}\n\
   impl ListItem for &i32 {}\n\
   impl ListItem for &usize {}\n\
   impl ListItem for &isize {}\n\
   impl ListItem for &f32 {}\n\
   impl ListItem for &f64 {}\n\n\
   pub trait NotifyTarget<Item: ListItem> {\n\
  \    const USED: bool = true;\n\n\
  \    fn on_set(&mut self, items: &[Item], at_marker: ItemMarker<Item>);\n\
  \    fn on_delete(&mut self, items: &[Item]);\n\
   }\n\n\
   impl<Item: ListItem> NotifyTarget<Item> for () {\n\
  \    const USED: bool = false;\n\
  \    fn on_set(&mut self, _items: &[Item], _at_marker: ItemMarker<Item>) {}\n\
  \    fn on_delete(&mut self, _items: &[Item]) {}\n\
   }\n\n\
   /// This represents a single entry in either the nexts pointers list or in an\n\
   /// iterator.\n\
   #[derive(Debug, PartialEq, Eq)]\n\
   pub(super) struct SkipEntry<Item: ListItem> {\n\
  \    /// The node being pointed to.\n\
  \    node: *mut Node<Item>,\n\n\
  \    /// The number of *items* between the start of the current node and the\n\
  \    /// start of the next node. That means nexts entry 0 contains the \
   length of\n\
  \    /// the current node.\n\
  \    skip_usersize: usize,\n\
   }\n\n\
   // We can't use #[derive()] here for Copy and Clone due to a bug in the rust\n\
   // compiler: https://github.com/rust-lang/rust/issues/26925\n\
   impl<Item: ListItem> Copy for SkipEntry<Item> {}\n\
   impl<Item: ListItem> Clone for SkipEntry<Item> {\n\
  \    fn clone(&self) -> Self { *self }\n\
   }\n\n\
   impl<Item: ListItem> SkipEntry<Item> {\n\
  \    fn new_null() -> Self {\n\
  \        SkipEntry { node: ptr::null_mut(), skip_usersize: 0 }\n\
  \    }\n\
   }\n\n\n\n\
   /// The node structure is designed in a very fancy way which would be more at\n\
   /// home in C or something like that. The basic idea is that the node \
   structure\n\
   /// is fixed size in memory, but the proportion of that space taken up by\n\
   /// characters and by the height differ depending on a node's height. This\n\
   /// results in a lot of `unsafe` blocks. I think the tradeoff is worth it \
   but I\n\
   /// could be wrong here. You probably wouldn't lose much performance in \
   practice\n\
   /// by replacing the inline structure with a smallvec - but that would waste\n\
   /// memory in small nodes, and require extra pointer indirection on large \
   nodes.\n\
   /// It also wouldn't remove all the unsafe here.\n\
   ///\n\
   /// A different representation (which might be better or worse - I can't \
   tell)\n\
   /// would be to have the nodes all be the same size in memory and change the\n\
   /// *proportion* of the node's memory that is used by the string field vs the\n\
   /// next pointers. That might be lighter weight for the allocator because the\n\
   /// struct itself would be a fixed size; but I'm not sure if it would be \
   better.\n\
   #[repr(C)] // Prevent parameter reordering.\n\
   pub(super) struct Node<Item: ListItem> {\n\
  \    /// We start with the items themselves. Only the first `num_items` of \
   this\n\
  \    /// list is in use. The user specified length of the items in the node is\n\
  \    /// stored in nexts[0].skip_items. This is initialized with\n\
  \    /// Default::default() for the type, but when MaybeUninit completely \
   lands,\n\
  \    /// it will be possible to make this a tiny bit faster by leaving the \
   list\n\
  \    /// initially uninitialized.\n\
  \    items: [MaybeUninit<Item>; NODE_NUM_ITEMS],\n\n\
  \    /// Number of items in `items` in use / filled.\n\
  \    num_items: u8,\n\n\
  \    /// Height of nexts array.\n\
  \    height: u8,\n\n\
  \    /// With the heads array as is, we have no way to go from a marker back \
   to a\n\
  \    /// cursor (which is required to insert at that location in the list). \
   For\n\
  \    /// that we need to be able to figure out at each level of the nexts\n\
  \    /// pointers which object points to us, and the offset from that \
   element to\n\
  \    /// the current element. Anyway, for markers to work we need this.\n\
  \    parent: *mut Node<Item>,\n\n\
  \    // #[repr(align(std::align_of::<SkipEntry>()))]\n\
  \    \n\
  \    /// In reality this array has the size of height, allocated using more or\n\
  \    /// less direct calls to malloc() at runtime based on the randomly \
   generated\n\
  \    /// size. The size is always at least 1.\n\
  \    nexts: [SkipEntry<Item>; 0],\n\
   }\n\n\
   // Make sure nexts uses correct alignment. This should be guaranteed by \
   repr(C)\n\
   // This test will fail if this ever stops being true.\n\
   #[test]\n\
   fn test_align() {\n\
  \    struct Item(u8);\n\
  \    impl ListItem for Item {}\n\
  \    #[repr(C)] struct Check([SkipEntry<Item>; 0]);\n\
  \    assert!(mem::align_of::<Check>() >= mem::align_of::<SkipEntry<Item>>());\n\
  \    // TODO: It'd be good to also check the alignment of the nexts field in \
   Node.\n\
   }\n\n\
   fn random_height<R: RngCore>(rng: &mut R) -> u8 {\n\
  \    let mut h: u8 = 1;\n\
  \    // Should I use a csrng here? Does it matter?\n\
  \    while h < MAX_HEIGHT_U8 && rng.gen::<u8>() < BIAS { h+=1; }\n\
  \    h\n\
   }\n\n\
   #[repr(C)]\n\
   pub struct SkipList<Item: ListItem, N: NotifyTarget<Item> = ()> {\n\
  \    // TODO: Consider putting the head item on the heap. For the use case \
   here\n\
  \    // its almost certainly fine either way. The code feels a bit cleaner \
   if its\n\
  \    // on the heap (and then iterators will be able to outlast a move of the\n\
  \    // skiplist parent). But its also very nice having the code run fast for\n\
  \    // small lists. Most lists are small, and it makes sense to optimize for\n\
  \    // that.\n\n\
  \    // TODO: For safety, pointers in to this structure should be Pin<> if we\n\
  \    // ever want to hold on to iterators.\n\n\
  \    /// The total number of items in the skip list. This is not used \
   internally -\n\
  \    /// just here for bookkeeping.\n\
  \    pub(super) num_items: usize,\n\
  \    /// Size of the list in user specified units.\n\
  \    pub(super) num_usercount: usize,\n\n\
  \    /// The RNG we use to generate node heights. Specifying it explicitly \
   allows\n\
  \    /// unit tests and randomizer runs to be predictable, which is very \
   helpful\n\
  \    /// during debugging. I'm still not sure how the type of this should be\n\
  \    /// specified. Should it be a generic parameter? Box<dyn *>?\n\
  \    /// ??\n\
  \    rng: Option<SmallRng>,\n\n\
  \    /// The first node is inline. The height is 1 more than the max height \
   we've\n\
  \    /// ever used. The highest next entry points to {null, total usersize}.\n\
  \    head: Node<Item>,\n\n\
  \    /// This is so dirty. The first node is embedded in SkipList; but we \
   need to\n\
  \    /// allocate enough room for height to get arbitrarily large. I could \
   insist\n\
  \    /// on SkipList always getting allocated on the heap, but for small \
   lists its\n\
  \    /// much better to be on the stack.\n\
  \    ///\n\
  \    /// So this struct is repr(C) and I'm just padding out the struct \
   directly.\n\
  \    /// All accesses should go through head because otherwise I think we \
   violate\n\
  \    /// aliasing rules.\n\
  \    _nexts_padding: [SkipEntry<Item>; MAX_HEIGHT],\n\n\
  \    _phantom: PhantomData<N>\n\
   }\n\n\n\
   impl<Item: ListItem> Node<Item> {\n\
  \    // Do I need to be explicit about the lifetime of the references being \
   tied\n\
  \    // to the lifetime of the node?\n\
  \    fn nexts(&self) -> &[SkipEntry<Item>] {\n\
  \        unsafe {\n\
  \            std::slice::from_raw_parts(self.nexts.as_ptr(), self.height as \
   usize)\n\
  \        }\n\
  \    }\n\n\
  \    fn nexts_mut(&mut self) -> &mut [SkipEntry<Item>] {\n\
  \        unsafe {\n\
  \            std::slice::from_raw_parts_mut(self.nexts.as_mut_ptr(), \
   self.height as usize)\n\
  \        }\n\
  \    }\n\n\
  \    fn layout_with_height(height: u8) -> Layout {\n\
  \        Layout::from_size_align(\n\
  \            mem::size_of::<Node<Item>>() + \
   mem::size_of::<SkipEntry<Item>>() * (height as usize),\n\
  \            mem::align_of::<Node<Item>>()).unwrap()\n\
  \    }\n\n\
  \    fn alloc_with_height(height: u8) -> *mut Node<Item> {\n\
  \        assert!(height >= 1 && height <= MAX_HEIGHT_U8);\n\n\
  \        unsafe {\n\
  \            let node = alloc(Self::layout_with_height(height)) as *mut \
   Node<Item>;\n\
  \            node.write(Node {\n\
  \                items: uninit_items_array(),\n\
  \                num_items: 0,\n\
  \                height,\n\
  \                parent: ptr::null_mut(),\n\
  \                nexts: [],\n\
  \            });\n\n\
  \            for next in (*node).nexts_mut() {\n\
  \                *next = SkipEntry::new_null();\n\
  \            }\n\n\
  \            node\n\
  \        }\n\
  \    }\n\n\
  \    fn alloc<R: RngCore>(rng: &mut R) -> *mut Node<Item> {\n\
  \        Self::alloc_with_height(random_height(rng))\n\
  \    }\n\n\
  \    unsafe fn free(p: *mut Node<Item>) {\n\
  \        ptr::drop_in_place(p); // We could just implement drop here, but \
   this is cleaner.\n\
  \        dealloc(p as *mut u8, Self::layout_with_height((*p).height));\n\
  \    }\n\n\
  \    fn content_slice(&self) -> &[Item] {\n\
  \        let slice = &self.items[..self.num_items as usize];\n\
  \        unsafe { maybeinit_slice_get_ref(slice) }\n\
  \    }\n\n\
  \    // The height is at least 1, so this is always valid.\n\
  \    fn first_skip_entry<'a>(&self) -> &'a SkipEntry<Item> {\n\
  \        unsafe { &*self.nexts.as_ptr() }\n\
  \    }\n\n\
  \    fn first_skip_entry_mut<'a>(&mut self) -> &'a mut SkipEntry<Item> {\n\
  \        unsafe { &mut *self.nexts.as_mut_ptr() }\n\
  \    }\n\n\
  \    // TODO: Rename to len() ?\n\
  \    fn get_userlen(&self) -> usize {\n\
  \        self.first_skip_entry().skip_usersize\n\
  \    }\n\
  \    \n\
  \    fn get_next_ptr(&self) -> *mut Node<Item> {\n\
  \        self.first_skip_entry().node\n\
  \    }\n\n\
  \    pub(crate) fn iter(&self, local_index: usize) -> ListItemIter<Item> {\n\
  \        ListItemIter {\n\
  \            node: Some(&self),\n\
  \            index: local_index,\n\
  \            remaining_items: None\n\
  \        }\n\
  \    }\n\
   }\n\n\
   impl<Item: ListItem> Drop for Node<Item> {\n\
  \    fn drop(&mut self) {\n\
  \        for item in &mut self.items[0..self.num_items as usize] {\n\
  \            // Could instead call assume_init() on each item but this is\n\
  \            // friendlier to the optimizer.\n\
  \            unsafe { ptr::drop_in_place(item.as_mut_ptr()); }\n\
  \        }\n\
  \    }\n\
   }\n\n\
   struct NodeIter<'a, Item: ListItem>(Option<&'a Node<Item>>);\n\
   impl<'a, Item: ListItem> Iterator for NodeIter<'a, Item> {\n\
  \    type Item = &'a Node<Item>;\n\n\
  \    fn next(&mut self) -> Option<&'a Node<Item>> {\n\
  \        let prev = self.0;\n\
  \        if let Some(n) = self.0 {\n\
  \            *self = NodeIter(unsafe { n.get_next_ptr().as_ref() });\n\
  \        }\n\
  \        prev\n\
  \    }\n\
   }\n\n\
   /// This is a set of pointers with metadata into a location in the list \
   needed\n\
   /// to skip ahead, delete and insert in items. A cursor is reasonably heavy\n\
   /// weight - we fill in and maintain as many entries as the height of the \
   list\n\
   /// dictates.\n\
   ///\n\
   /// This is not needed for simply iterating sequentially through nodes and \
   data.\n\
   /// For that look at NodeIter.\n\
   ///\n\
   /// Note most/all methods using cursors are unsafe. This is because cursors \
   use\n\
   /// raw mutable pointers into the list, so when used the following rules \
   have to\n\
   /// be followed:\n\
   ///\n\
   /// - Whenever a write happens (insert/remove/replace), any cursor not \
   passed to\n\
   ///   the write function is invalid.\n\
   /// - While a cursor is held the SkipList struct should be considered \
   pinned and\n\
   ///   must not be moved or deleted\n\
   #[derive(Copy, Clone)]\n\
   pub(crate) struct Cursor<Item: ListItem> {\n\
  \    /// The global user position of the cursor in the entire list. This is \
   used\n\
  \    /// for when the max seen height increases, so we can populate previously\n\
  \    /// unused entries in the cursor and in the head node.\n\
  \    ///\n\
  \    /// This field isn't strictly necessary - earlier versions tacked this \
   on to\n\
  \    /// the last item in entries... I'm still not sure the cleanest way to do\n\
  \    /// this.\n\
  \    pub(super) userpos: usize,\n\n\
  \    /// When the userpos of an entry is 0 (totally valid and useful), a \
   cursor\n\
  \    /// becomes ambiguous with regard to where exactly its pointing in the\n\
  \    /// current entry. This is used to resolve that ambiguity.\n\
  \    pub(super) local_index: usize,\n\n\
  \    pub(super) entries: [SkipEntry<Item>; MAX_HEIGHT],\n\n\
  \    // TODO: The cursor can't outlive the skiplist, but doing this makes it\n\
  \    // tricky to pass cursors around in the Skiplist type. There's probably a\n\
  \    // way out of this mess, but I'm not good enough at rust to figure it \
   out.\n\
  \    // _marker: PhantomData<&'a SkipList<C>>,\n\
   }\n\n\
   impl<Item: ListItem> Cursor<Item> {\n\
  \    pub(super) fn update_offsets(&mut self, height: usize, by: isize) {\n\
  \        for i in 0..height {\n\
  \            unsafe {\n\
  \                // This is weird but makes sense when you realise the nexts \
   in\n\
  \                // the cursor are pointers into the elements that have the\n\
  \                // actual pointers.\n\
  \                // Also adding a usize + isize is awful in rust :/\n\
  \                let skip = &mut \
   (*self.entries[i].node).nexts_mut()[i].skip_usersize;\n\
  \                *skip = skip.wrapping_add(by as usize);\n\
  \            }\n\
  \        }\n\
  \    }\n\n\
  \    /// Move a cursor to the start of the next node. Returns the new node \
   (or a\n\
  \    /// nullptr if this is the end of the list).\n\
  \    fn advance_node(&mut self) -> *mut Node<Item> {\n\
  \        unsafe {\n\
  \            let SkipEntry { node: e, skip_usersize: offset } = \
   self.entries[0];\n\
  \            // offset tells us how far into the current element we are (in\n\
  \            // usersize). We need to increment the offsets by the entry's\n\
  \            // remaining length to get to the start of the next node.\n\
  \            let advance_by = (*e).get_userlen() - offset;\n\
  \            let next = (*e).get_next_ptr();\n\
  \            let height = (*next).height as usize;\n\n\
  \            for i in 0..height {\n\
  \                self.entries[i] = SkipEntry {\n\
  \                    node: next,\n\
  \                    skip_usersize: 0\n\
  \                };\n\
  \            }\n\n\
  \            for i in height..self.entries.len() {\n\
  \                self.entries[i].skip_usersize += advance_by;\n\
  \            }\n\n\
  \            self.userpos += advance_by;\n\
  \            self.local_index = 0;\n\n\
  \            next\n\
  \        }\n\
  \    }\n\n\
  \    pub(super) fn is_at_node_end(&self) -> bool {\n\
  \        self.local_index == unsafe { (*self.here_ptr()).num_items } as usize\n\
  \    }\n\n\
  \    pub(super) fn advance_item(&mut self, height: usize) {\n\
  \        if self.is_at_node_end() { self.advance_node(); }\n\
  \        let usersize = unsafe { self.current_item() \
   }.unwrap().get_usersize();\n\n\
  \        for entry in &mut self.entries[0..height] {\n\
  \            entry.skip_usersize += usersize;\n\
  \        }\n\
  \        self.userpos += usersize;\n\
  \        self.local_index += 1;\n\
  \    }\n\n\
  \    pub(super) fn advance_by_items(&mut self, num: usize, height: usize) {\n\
  \        for _ in 0..num { self.advance_item(height); }\n\
  \    }\n\n\
  \    pub(super) fn move_to_item_start(&mut self, height: usize, offset: \
   usize) {\n\
  \        for entry in &mut self.entries[0..height] {\n\
  \            entry.skip_usersize -= offset;\n\
  \        }\n\
  \        self.userpos -= offset;\n\
  \    }\n\n\
  \    pub(super) unsafe fn prev_item<'a>(&self) -> Option<&'a Item> {\n\
  \        let node = &*self.here_ptr();\n\
  \        if self.local_index == 0 {\n\
  \            assert_eq!(self.userpos, 0, \"Invalid state: Cursor at start of \
   node\");\n\
  \            None\n\
  \        } else {\n\
  \            debug_assert!(self.local_index <= node.num_items as usize);\n\
  \            Some(&*(node.items[self.local_index - 1].as_ptr()))\n\
  \        }\n\
  \    }\n\n\
  \    pub(super) unsafe fn prev_item_mut<'a>(&mut self) -> Option<&'a mut \
   Item> {\n\
  \        let node = &mut *self.here_ptr();\n\
  \        if self.local_index == 0 {\n\
  \            assert_eq!(self.userpos, 0);\n\
  \            None\n\
  \        } else {\n\
  \            debug_assert!(self.local_index <= node.num_items as usize);\n\
  \            Some(&mut *(node.items[self.local_index - 1].as_mut_ptr()))\n\
  \        }\n\
  \    }\n\n\
  \    // Could be Option<NonNull<_>>...\n\
  \    unsafe fn peek_next_node_start(&self) -> Option<*mut Item> {\n\
  \        let next = (*self.here_ptr()).get_next_ptr();\n\
  \        if next.is_null() { None }\n\
  \        else {\n\
  \            debug_assert!((*next).num_items > 0);\n\
  \            Some((*next).items[0].as_mut_ptr())\n\
  \        }\n\
  \    }\n\n\
  \    pub(super) unsafe fn current_item<'a>(&self) -> Option<&'a Item> {\n\
  \        let node = &*self.here_ptr();\n\
  \        if self.local_index < node.num_items as usize {\n\
  \            // Ok - just return the current item.\n\
  \            Some(&*(node.items[self.local_index].as_ptr()))\n\
  \        } else {\n\
  \            // Peek the first item in the next node.\n\
  \            self.peek_next_node_start().map(|ptr| &*ptr)\n\
  \        }\n\
  \    }\n\n\
  \    // pub(super) unsafe fn take_prev<'a>(&mut self) -> Option<Item> {\n\
  \    //     let node = &*self.here_ptr();\n\
  \    //     if self.local_index == 0 { None }\n\
  \    //     else {\n\
  \    //         debug_assert!(self.local_index < node.num_items as usize\n\
  \    //         // Ok - just return the current item.\n\
  \    //         Some(&*(node.items[self.local_index].as_ptr()))\n\
  \    //     }\n\
  \    // }\n\n\
  \    // pub(super) unsafe fn current_item_mut<'a>(&mut self) -> Option<&'a \
   mut C::Item> {\n\
  \    //     let node = &mut *self.here_ptr();\n\
  \    //     if self.local_index < node.num_items as usize {\n\
  \    //         // Ok - just return the current item.\n\
  \    //         Some(&mut *(node.items[self.local_index].as_mut_ptr()))\n\
  \    //     } else {\n\
  \    //         // Peek the first item in the next node.\n\
  \    //         self.peek_next_item().map(|ptr| &mut *ptr)\n\
  \    //     }\n\
  \    // }\n\n\
  \    /// Get the pointer to the cursor's current node\n\
  \    pub(super) fn here_ptr(&self) -> *mut Node<Item> {\n\
  \        self.entries[0].node\n\
  \    }\n\
   }\n\n\
   impl<Item: ListItem> PartialEq for Cursor<Item> {\n\
  \    /// Warning: This returns false if one cursor is at the end of a node, \
   and\n\
  \    /// the other at the start of the next node. Almost all code in this \
   library\n\
  \    /// leaves cursors at the end of nodes, so this shouldn't matter too \
   much in\n\
  \    /// practice.\n\
  \    fn eq(&self, other: &Self) -> bool {\n\
  \        if self.userpos != other.userpos\n\
  \            || self.local_index != other.local_index {return false; }\n\n\
  \        for i in 0..MAX_HEIGHT {\n\
  \            let a = &self.entries[i];\n\
  \            let b = &other.entries[i];\n\
  \            if a.node != b.node || a.skip_usersize != b.skip_usersize { \
   return false; }\n\
  \        }\n\
  \        true\n\
  \    }\n\
   }\n\
   impl<Item: ListItem> Eq for Cursor<Item> {}\n\n\
   impl<Item: ListItem> fmt::Debug for Cursor<Item> {\n\
  \    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {\n\
  \        f.debug_struct(\"Cursor\")\n\
  \            .field(\"userpos\", &self.userpos)\n\
  \            .field(\"local_index\", &self.local_index)\n\
  \            .finish()\n\
  \    }\n\
   }\n\n\
   // None of the rust builtins give me what I want, which is a copy-free \
   iterator\n\
   // to owned items in a MaybeUninit array. Eh; its easy enough to make my own.\n\
   struct UninitOwnedIter<'a, Item: ListItem, N: NotifyTarget<Item>> {\n\
  \    // Based on the core slice IterMut implementation.\n\
  \    ptr: NonNull<Item>,\n\
  \    end: *mut Item,\n\
  \    _marker: PhantomData<&'a SkipList<Item, N>>\n\
   }\n\n\
   impl<'a, Item: ListItem, N: NotifyTarget<Item>> UninitOwnedIter<'a, Item, \
   N> {\n\
  \    /// Make a slice we can iterate from and steal data from without dropping\n\
  \    /// content. This is unsafe:\n\
  \    ///\n\
  \    /// - If the iterator isn't fully drained then remaining items will be\n\
  \    ///   forgotten (they are not dropped).\n\
  \    /// - The slice passed in here must be initialized or undefined behaviour\n\
  \    ///   will hit us.\n\
  \    ///\n\
  \    /// After iterating, the contents are uninit memory.\n\
  \    unsafe fn from_slice(slice: &[MaybeUninit<Item>]) -> Self {\n\
  \        let ptr = slice.as_ptr() as *mut Item; // Safe.\n\
  \        let end = ptr.add(slice.len());\n\n\
  \        UninitOwnedIter {\n\
  \            ptr: NonNull::new_unchecked(ptr),\n\
  \            end,\n\
  \            _marker: PhantomData\n\
  \        }\n\
  \    }\n\
   }\n\n\
   impl<'a, Item: ListItem, N: NotifyTarget<Item>> Iterator for \
   UninitOwnedIter<'a, Item, N> {\n\
  \    type Item = Item;\n\n\
  \    fn next(&mut self) -> Option<Self::Item> {\n\
  \        if self.ptr.as_ptr() == self.end {\n\
  \            None\n\
  \        } else {\n\
  \            let ptr = self.ptr;\n\
  \            self.ptr = unsafe { \
   NonNull::new_unchecked(self.ptr.as_ptr().offset(1)) };\n\
  \            Some(unsafe { ptr.as_ptr().read() })\n\
  \        }\n\
  \    }\n\n\
  \    fn size_hint(&self) -> (usize, Option<usize>) {\n\
  \        let size = (self.end as usize - self.ptr.as_ptr() as usize) / \
   mem::size_of::<Item>();\n\
  \        (size, Some(size))\n\
  \    }\n\
   }\n\n\
   // TODO: Stolen from MaybeUninit::uninit_array. Replace with the real \
   uninit_array when stable.\n\
   #[inline(always)]\n\
   fn uninit_items_array<T>() -> [MaybeUninit<T>; NODE_NUM_ITEMS] {\n\
  \    unsafe { MaybeUninit::<[MaybeUninit<T>; \
   NODE_NUM_ITEMS]>::uninit().assume_init() }\n\
   }\n\n\
   // TODO: Stolen from MaybeUninit::slice_get_ref. Replace when available.\n\
   #[inline(always)]\n\
   unsafe fn maybeinit_slice_get_ref<T>(slice: &[MaybeUninit<T>]) -> &[T] {\n\
  \    // SAFETY: casting slice to a `*const [T]` is safe since the caller \
   guarantees that\n\
  \    // `slice` is initialized, and`MaybeUninit` is guaranteed to have the \
   same layout as `T`.\n\
  \    // The pointer obtained is valid since it refers to memory owned by \
   `slice` which is a\n\
  \    // reference and thus guaranteed to be valid for reads.\n\
  \    &*(slice as *const [MaybeUninit<T>] as *const [T])\n\
   }\n\n\n\
   impl<Item: ListItem, N: NotifyTarget<Item>> SkipList<Item, N> {\n\
  \    pub fn new() -> Self {\n\
  \        SkipList::<Item, N> {\n\
  \            num_items: 0,\n\
  \            num_usercount: 0,\n\
  \            rng: None,\n\
  \            head: Node {\n\
  \                items: uninit_items_array(),\n\
  \                num_items: 0,\n\
  \                height: 1, // Stores max height of list nodes\n\
  \                parent: ptr::null_mut(),\n\
  \                nexts: [],\n\
  \            },\n\
  \            _nexts_padding: [SkipEntry::new_null(); MAX_HEIGHT],\n\
  \            _phantom: PhantomData\n\
  \        }\n\
  \    }\n\n\
  \    pub fn init_rng_from_seed(&mut self, seed: u64) {\n\
  \        self.rng = Some(SmallRng::seed_from_u64(seed));\n\
  \    }\n\n\
  \    fn get_rng(&mut self) -> &mut SmallRng {\n\
  \        // I'm sure there's a nicer way to implement this.\n\
  \        if self.rng.is_none() {\n\
  \            // We'll use a stable RNG in debug mode so the tests are stable.\n\
  \            if cfg!(debug_assertions) {\n\
  \                self.init_rng_from_seed(123);\n\
  \            } else {\n\
  \                self.rng = Some(SmallRng::from_entropy());\n\
  \            }\n\
  \        }\n\
  \        self.rng.as_mut().unwrap()\n\
  \    }\n\n\n\
  \    pub fn len_user(&self) -> usize {\n\
  \        self.num_usercount\n\
  \    }\n\n\
  \    pub fn len_items(&self) -> usize {\n\
  \        self.num_items as usize\n\
  \    }\n\n\
  \    fn node_iter(&self) -> NodeIter<Item> { NodeIter(Some(&self.head)) }\n\
  \    \n\
  \    pub fn iter(&self) -> ListItemIter<Item> {\n\
  \        ListItemIter {\n\
  \            node: Some(&self.head),\n\
  \            index: 0,\n\
  \            remaining_items: Some(self.len_items())\n\
  \        }\n\
  \    }\n\n\
  \    #[inline(always)]\n\
  \    pub(super) fn height(&self) -> usize {\n\
  \        self.head.height as usize\n\
  \    }\n\n\
  \    fn heads_mut(&mut self) -> &mut [SkipEntry<Item>] {\n\
  \        unsafe {\n\
  \            std::slice::from_raw_parts_mut(self.head.nexts.as_mut_ptr(), \
   self._nexts_padding.len())\n\
  \        }\n\
  \    }\n\n\
  \    fn is_head(&self, node: *const Node<Item>) -> bool {\n\
  \        node as *const _ == &self.head as *const _\n\
  \    }\n\n\
  \    #[inline(always)]\n\
  \    fn use_parents() -> bool {\n\
  \        cfg!(debug_assertions) || N::USED\n\
  \    }\n\n\
  \    /// Walk the list and validate internal constraints. This is used for\n\
  \    /// testing the structure itself, and should generally not be called by\n\
  \    /// users.\n\
  \    pub fn check(&self) {\n\
  \        // #[cfg(test)]\n\
  \        {\n\
  \            // self.print();\n\
  \            assert!(self.head.height >= 1);\n\
  \            assert!(self.head.height <= MAX_HEIGHT_U8);\n\n\
  \            let head_ptr = &self.head as *const _ as *mut _;\n\
  \            // let skip_over = self.get_top_entry();\n\
  \            // println!(\"Skip over skip chars {}, num bytes {}\", \
   skip_over.skip_items, self.num_bytes);\n\n\
  \            let mut prev: [*const Node<Item>; MAX_HEIGHT] = [ptr::null(); \
   MAX_HEIGHT];\n\n\
  \            let mut iter = [SkipEntry {\n\
  \                // Bleh.\n\
  \                node: head_ptr,\n\
  \                // The skips will store the total distance travelled since \
   the\n\
  \                // start of this traversal at each height. All the entries \
   above\n\
  \                // head.height are ignored though.\n\
  \                skip_usersize: 0\n\
  \            }; MAX_HEIGHT];\n\n\
  \            let mut num_items = 0;\n\
  \            let mut num_usercount = 0;\n\n\
  \            for (_i, n) in self.node_iter().enumerate() {\n\
  \                // println!(\"visiting {:?}\", n.as_str());\n\
  \                if !self.is_head(n) { assert!(n.num_items > 0); }\n\
  \                assert!(n.height <= MAX_HEIGHT_U8);\n\
  \                assert!(n.num_items as usize <= NODE_NUM_ITEMS);\n\n\
  \                // Make sure the number of items matches the count\n\
  \                let local_count = Item::userlen_of_slice(n.content_slice());\n\
  \                assert_eq!(local_count, n.get_userlen());\n\n\
  \                if Self::use_parents() {\n\
  \                    let expect_parent = if self.is_head(n) {\n\
  \                        ptr::null() // The head's parent is null\n\
  \                    } else if n.height == self.head.height {\n\
  \                        &self.head as *const _ // Max height nodes point \
   back to head\n\
  \                    } else {\n\
  \                        prev[n.height as usize]\n\
  \                    };\n\n\
  \                    assert_eq!(n.parent as *const _, expect_parent, \
   \"invalid parent\");\n\
  \                }\n\
  \                \n\
  \                for (i, entry) in iter[0..n.height as \
   usize].iter_mut().enumerate() {\n\
  \                    assert_eq!(entry.node as *const _, n as *const _);\n\
  \                    assert_eq!(entry.skip_usersize, num_usercount);\n\n\
  \                    // println!(\"replacing entry {:?} with {:?}\", entry, \
   n.nexts()[i].node);\n\
  \                    prev[i] = n;\n\
  \                    entry.node = n.nexts()[i].node;\n\
  \                    entry.skip_usersize += n.nexts()[i].skip_usersize;\n\
  \                }\n\n\
  \                num_items += n.num_items as usize;\n\
  \                num_usercount += n.get_userlen();\n\n\
  \                // Check the value returned by the iterator functions \
   matches.\n\
  \                let (mut normal_iter, local_offset) = \
   self.cursor_at_userpos(num_usercount);\n\
  \                assert_eq!(local_offset, 0);\n\
  \                assert_eq!(normal_iter.userpos, num_usercount);\n\n\
  \                // Dirty hack. If n has 0-sized elements at the end, the \
   normal\n\
  \                // cursor won't be at the end...\n\
  \                if Self::use_parents() {\n\
  \                    while normal_iter.here_ptr() != n as *const _ as *mut _ {\n\
  \                        normal_iter.advance_node();\n\
  \                    }\n\
  \                    normal_iter.local_index = n.num_items as usize;\n\
  \                    let node_iter = unsafe { self.cursor_at_node(n, \
   n.get_userlen(), n.num_items as usize) };\n\
  \                    assert_eq!(normal_iter, node_iter);\n\
  \                }\n\
  \            }\n\n\
  \            for entry in iter[0..self.height()].iter() {\n\
  \                // println!(\"{:?}\", entry);\n\
  \                assert!(entry.node.is_null());\n\
  \                assert_eq!(entry.skip_usersize, num_usercount);\n\
  \            }\n\
  \            \n\
  \            // println!(\"self bytes: {}, count bytes {}\", self.num_bytes, \
   num_bytes);\n\
  \            assert_eq!(self.num_items, num_items);\n\
  \            assert_eq!(self.len_user(), num_usercount);\n\
  \        }\n\
  \    }\n\
  \    \n\
  \    \n\
  \    /// Internal function for creating a cursor at a particular location in \
   the\n\
  \    /// skiplist. The returned cursor contains list of nodes which point past\n\
  \    /// the specified position, as well as offsets of how far into their\n\
  \    /// character lists the specified characters are.\n\
  \    ///\n\
  \    /// Sometimes a call to iter_at_userpos is ambiguous:\n\
  \    ///\n\
  \    /// - The item can contain items with zero usersize. The cursor could \
   point\n\
  \    ///   to any of them.\n\
  \    /// - If the location is at the end of a node, it is equally valid to \
   return\n\
  \    ///   a position at the start of the next node.\n\
  \    ///\n\
  \    /// Because its impossible to move backwards in the list, iter_at_userpos\n\
  \    /// returns the first admissible location with the specified userpos.\n\
  \    /// \n\
  \    /// Returns (cursor, offset into the specified item).\n\
  \    ///\n\
  \    /// TODO: This should be Pin<&self>.\n\
  \    pub(super) fn cursor_at_userpos(&self, target_userpos: usize) -> \
   (Cursor<Item>, usize) {\n\
  \        assert!(target_userpos <= self.len_user());\n\n\
  \        let mut e: *const Node<Item> = &self.head;\n\
  \        let mut height = self.height() - 1;\n\
  \        \n\
  \        let mut offset = target_userpos; // How many more items to skip\n\n\
  \        // We're populating the head node pointer to simplify the case when \
   the\n\
  \        // iterator grows. We could put offset into the skip_usersize but it\n\
  \        // would only be *mostly* correct, not always correct. (Since cursor\n\
  \        // entries above height are not updated by insert.)\n\
  \        let mut cursor = Cursor {\n\
  \            entries: [SkipEntry {\n\
  \                node: &self.head as *const _ as *mut _,\n\
  \                skip_usersize: usize::MAX\n\
  \            }; MAX_HEIGHT],\n\
  \            local_index: 0,\n\
  \            userpos: target_userpos,\n\
  \            // _marker: PhantomData,\n\
  \        };\n\n\
  \        loop { // while height >= 0\n\
  \            let en = unsafe { &*e };\n\
  \            let next = en.nexts()[height];\n\
  \            let skip = next.skip_usersize;\n\
  \            if offset > skip {\n\
  \                // Go right.\n\
  \                debug_assert!(e == &self.head || en.num_items > 0);\n\
  \                offset -= skip;\n\
  \                e = next.node;\n\
  \                assert!(!e.is_null(), \"Internal constraint violation: \
   Reached end prematurely\");\n\
  \            } else {\n\
  \                // Record this and go down.\n\
  \                cursor.entries[height] = SkipEntry {\n\
  \                    skip_usersize: offset,\n\
  \                    node: e as *mut Node<Item>, // This is pretty gross\n\
  \                };\n\n\
  \                if height == 0 { break; } else { height -= 1; }\n\
  \            }\n\
  \        };\n\n\
  \        // We should always land within the node we're pointing to.\n\
  \        debug_assert!(offset <= unsafe { &*cursor.here_ptr() \
   }.get_userlen());\n\n\
  \        // We've found the node. Now look for the index within the node.\n\
  \        let en = unsafe { &*e };\n\
  \        let mut index = 0;\n\n\
  \        while offset > 0 {\n\
  \            assert!(index < en.num_items as usize);\n\
  \            \n\
  \            let usersize = unsafe { &*en.items[index].as_ptr() \
   }.get_usersize();\n\
  \            if usersize > offset { break; } // We're in the middle of an \
   item.\n\
  \            offset -= usersize;\n\
  \            index += 1;\n\
  \        }\n\
  \        cursor.local_index = index;\n\n\
  \        (cursor, offset)\n\
  \    }\n\n\
  \    /// Create a cursor at the specified node, using the parents \
   infrastructure\n\
  \    /// to calculate offsets. The offset and local_index parameters should\n\
  \    /// specify the offset into the current node. They are accepted as-is.\n\
  \    /// Offset *must* be at an item boundary\n\
  \    unsafe fn cursor_at_node(&self, n: *const Node<Item>, mut offset: \
   usize, local_index: usize) -> Cursor<Item> {\n\
  \        assert!(Self::use_parents(), \"cursor_at_node not available if \
   notifications are disabled\");\n\n\
  \        let mut n = n as *mut Node<Item>; // We don't mutate, but we need a \
   mut ptr.\n\n\
  \        let mut cursor = Cursor {\n\
  \            userpos: 0, // We'll set this later.\n\
  \            local_index,\n\
  \            entries: [SkipEntry {\n\
  \                node: &self.head as *const _ as *mut _,\n\
  \                skip_usersize: usize::MAX\n\
  \            }; MAX_HEIGHT],\n\
  \            // _marker: PhantomData\n\
  \        };\n\n\
  \        let mut h = 0;\n\
  \        loop {\n\
  \            while h < (*n).height as usize {\n\
  \                cursor.entries[h] = SkipEntry {\n\
  \                    node: n,\n\
  \                    skip_usersize: offset\n\
  \                };\n\n\
  \                h += 1;\n\
  \            }\n\n\
  \            let parent = (*n).parent;\n\
  \            // Reached the head.\n\
  \            if parent.is_null() { break; }\n\n\
  \            // If we're the same height as the parent its fine.\n\
  \            debug_assert!((*parent).height as usize > h\n\
  \                || (self.is_head(parent) && (*parent).height as usize == \
   h));\n\n\
  \            // Walk from parent back to n, figuring out the offset.\n\
  \            let mut c = parent;\n\
  \            // let walk_height = (*parent).height as usize - 2;\n\
  \            let walk_height = (*n).height as usize - 1;\n\
  \            while c != n {\n\
  \                let elem = (*c).nexts()[walk_height];\n\
  \                offset += elem.skip_usersize;\n\
  \                c = elem.node;\n\
  \            }\n\n\
  \            n = parent;\n\
  \        }\n\n\
  \        cursor.userpos = offset;\n\
  \        cursor\n\
  \    }\n\n\
  \    /// SAFETY: Self must outlast the marker and not have been moved since \
   the\n\
  \    /// marker was created. Self should really be Pin<>!\n\
  \    pub(super) unsafe fn cursor_at_marker<P>(&mut self, marker: \
   ItemMarker<Item>, predicate: P) -> Option<(Cursor<Item>, usize)>\n\
  \    where P: Fn(&Item) -> Option<usize> {\n\
  \        // The marker gives us a pointer into a node. Find the item.\n\
  \        let n = marker.ptr;\n\n\
  \        let mut offset: usize = 0;\n\
  \        let mut local_index = None;\n\
  \        let mut item_offset = 0;\n\
  \        for (i, item) in (*n).content_slice().iter().enumerate() {\n\
  \            if let Some(item_offset_) = predicate(item) {\n\
  \                // offset += item_offset;\n\
  \                item_offset = item_offset_;\n\
  \                local_index = Some(i);\n\
  \                break;\n\
  \            } else {\n\
  \                offset += item.get_usersize();\n\
  \            }\n\
  \        }\n\n\
  \        local_index.map(|local_index| {\n\
  \            (self.cursor_at_node(n, offset, local_index), item_offset)\n\
  \        })\n\
  \    }\n\n\
  \    // Internal fn to create a new node at the specified iterator filled with\n\
  \    // the specified content. The passed cursor should point at the end of \
   the\n\
  \    // previous node. It will be updated to point to the end of the newly\n\
  \    // inserted content.\n\
  \    // unsafe fn insert_node_at(&mut self, cursor: &mut Cursor<Item>, \
   contents: &[C::Item], new_userlen: usize, move_cursor: bool) {\n\
  \    unsafe fn insert_node_at<I>(&mut self, cursor: &mut Cursor<Item>, \
   contents: &mut I, num_items: usize, move_cursor: bool, notify: &mut N)\n\
  \            where I: Iterator<Item=Item> {\n\n\
  \        // println!(\"Insert_node_at {} len {}\", contents.len(), \
   self.num_bytes);\n\
  \        // debug_assert_eq!(new_userlen, C::userlen_of_slice(contents));\n\
  \        assert!(num_items <= NODE_NUM_ITEMS);\n\
  \        debug_assert!(contents.size_hint().0 >= num_items);\n\n\
  \        let new_node_ptr = Node::alloc(self.get_rng());\n\
  \        let new_node = &mut *new_node_ptr;\n\
  \        new_node.num_items = num_items as u8;\n\n\
  \        for (slot, item) in \
   new_node.items[..num_items].iter_mut().zip(contents) {\n\
  \            (slot.as_mut_ptr() as *mut Item).write(item); // Write makes \
   sure we don't drop the old value.\n\
  \        }\n\n\
  \        let new_userlen = Item::userlen_of_slice(new_node.content_slice());\n\n\
  \        let new_height = new_node.height;\n\
  \        let new_height_usize = new_height as usize;\n\n\
  \        let mut head_height = self.height();\n\
  \        while head_height < new_height_usize {\n\
  \            // This seems weird given we're about to overwrite these values\n\
  \            // below. What we're doing is retroactively setting up the cursor\n\
  \            // and head pointers *as if* the height had been this high all\n\
  \            // along. This way we only have to populate the higher head \
   values\n\
  \            // lazily.\n\
  \            let total_userlen = self.num_usercount;\n\
  \            let nexts = self.heads_mut();\n\
  \            nexts[head_height].skip_usersize = total_userlen;\n\
  \            cursor.entries[head_height].skip_usersize = cursor.userpos;\n\n\
  \            head_height += 1; // This is ugly.\n\
  \            self.head.height += 1;\n\
  \        }\n\n\
  \        new_node.parent = if new_height_usize == MAX_HEIGHT {\n\
  \            &self.head as *const _ as *mut _\n\
  \        } else { cursor.entries[new_height_usize].node };\n\n\
  \        for i in 0..new_height_usize {\n\
  \            let prev_skip = &mut (*cursor.entries[i].node).nexts_mut()[i];\n\
  \            let new_nexts = new_node.nexts_mut();\n\n\
  \            // The new node points to the successor (or null)\n\
  \            new_nexts[i] = SkipEntry {\n\
  \                node: prev_skip.node,\n\
  \                skip_usersize: new_userlen + prev_skip.skip_usersize - \
   cursor.entries[i].skip_usersize\n\
  \            };\n\n\
  \            // The previous node points to the new node\n\
  \            *prev_skip = SkipEntry {\n\
  \                node: new_node_ptr,\n\
  \                skip_usersize: cursor.entries[i].skip_usersize\n\
  \            };\n\n\
  \            // Move the iterator to the end of the newly inserted node.\n\
  \            if move_cursor {\n\
  \                cursor.entries[i] = SkipEntry {\n\
  \                    node: new_node_ptr,\n\
  \                    skip_usersize: new_userlen\n\
  \                };\n\
  \            }\n\
  \        }\n\n\
  \        for i in new_height_usize..head_height {\n\
  \            (*cursor.entries[i].node).nexts_mut()[i].skip_usersize += \
   new_userlen;\n\
  \            if move_cursor {\n\
  \                cursor.entries[i].skip_usersize += new_userlen;\n\
  \            }\n\
  \        }\n\n\
  \        // Update parents.\n\
  \        if Self::use_parents() && new_height_usize > 1 {\n\
  \            let mut n = new_node_ptr;\n\
  \            let mut skip_height = 0;\n\n\
  \            loop {\n\
  \                n = (*n).nexts_mut()[skip_height].node;\n\
  \                if n.is_null() || (*n).height >= new_height { break; }\n\
  \                \n\
  \                (*n).parent = new_node_ptr;\n\
  \                skip_height = usize::max(skip_height, (*n).height as usize \
   - 1);\n\
  \            }\n\
  \        }\n\
  \        \n\
  \        self.num_items += num_items;\n\
  \        self.num_usercount += new_userlen;\n\
  \        if move_cursor {\n\
  \            cursor.userpos += new_userlen;\n\
  \            cursor.local_index = num_items;\n\
  \        }\n\n\
  \        notify.on_set(new_node.content_slice(), ItemMarker {\n\
  \            ptr: new_node_ptr,\n\
  \            // _phantom: PhantomData\n\
  \        });\n\
  \    }\n\n\
  \    // unsafe fn insert_at_iter(&mut self, cursor: &mut Cursor<C>, \
   contents: &[C::Item]) {\n\
  \    pub(super) unsafe fn insert_at_iter<I>(&mut self, cursor: &mut \
   Cursor<Item>, contents: &mut I, notify: &mut N)\n\
  \            where I: ExactSizeIterator<Item=Item> {\n\
  \        // iter specifies where to insert.\n\n\
  \        let mut e = cursor.here_ptr();\n\n\
  \        // The insertion offset into the destination node.\n\
  \        assert!(cursor.userpos <= self.num_usercount);\n\
  \        assert!(cursor.local_index <= (*e).num_items as usize);\n\n\
  \        // We might be able to insert the new data into the current node, \
   depending on\n\
  \        // how big it is.\n\
  \        let num_inserted_items = contents.len();\n\n\
  \        // Can we insert into the current node?\n\
  \        let mut insert_here = (*e).num_items as usize + num_inserted_items \
   <= NODE_NUM_ITEMS;\n\n\
  \        // Can we insert into the start of the successor node?\n\
  \        if !insert_here && cursor.local_index == (*e).num_items as usize && \
   num_inserted_items <= NODE_NUM_ITEMS {\n\
  \            // We can insert into the subsequent node if:\n\
  \            // - We can't insert into the current node\n\
  \            // - There _is_ a next node to insert into\n\
  \            // - The insert would be at the start of the next node\n\
  \            // - There's room in the next node\n\
  \            if let Some(next) = (*e).first_skip_entry_mut().node.as_mut() {\n\
  \                if next.num_items as usize + num_inserted_items <= \
   NODE_NUM_ITEMS {\n\
  \                    cursor.advance_node();\n\
  \                    e = next;\n\n\
  \                    insert_here = true;\n\
  \                }\n\
  \            }\n\
  \        }\n\n\
  \        let item_idx = cursor.local_index;\n\
  \        let e_num_items = (*e).num_items as usize; // convenience.\n\n\
  \        if insert_here {\n\
  \            // println!(\"insert_here {}\", contents);\n\
  \            // First push the current items later in the array\n\
  \            let c = &mut (*e).items;\n\
  \            if item_idx < e_num_items {\n\
  \                // Can't use copy_within because Item doesn't necessarily\n\
  \                // implement Copy. Memmove the existing items.\n\
  \                ptr::copy(\n\
  \                    &c[item_idx],\n\
  \                    &mut c[item_idx + num_inserted_items],\n\
  \                    (*e).num_items as usize - item_idx);\n\
  \            }\n\n\
  \            // Then copy in the new items. Can't memcpy from an iterator, but\n\
  \            // the optimizer should make this fast.\n\
  \            let dest_content_slice = &mut c[item_idx..item_idx + \
   num_inserted_items];\n\
  \            for (slot, item) in dest_content_slice.iter_mut().zip(contents) {\n\
  \                // Do not drop the old items - they were only moved.\n\
  \                slot.as_mut_ptr().write(item);\n\
  \            }\n\
  \            let dest_content_slice = \
   maybeinit_slice_get_ref(dest_content_slice);\n\n\
  \            (*e).num_items += num_inserted_items as u8;\n\
  \            self.num_items += num_inserted_items;\n\
  \            let num_inserted_usercount = \
   Item::userlen_of_slice(dest_content_slice);\n\
  \            self.num_usercount += num_inserted_usercount;\n\n\
  \            // .... aaaand update all the offset amounts.\n\
  \            cursor.update_offsets(self.height(), num_inserted_usercount as \
   isize);\n\n\
  \            // Usually the cursor will be discarded after one change, but for\n\
  \            // consistency of compound edits we'll update the cursor to \
   point to\n\
  \            // the end of the new content.\n\
  \            for entry in cursor.entries[0..self.height()].iter_mut() {\n\
  \                entry.skip_usersize += num_inserted_usercount;\n\
  \            }\n\
  \            cursor.userpos += num_inserted_usercount;\n\
  \            cursor.local_index += num_inserted_items;\n\n\
  \            notify.on_set(dest_content_slice, ItemMarker {\n\
  \                ptr: e,\n\
  \                // _phantom: PhantomData\n\
  \            });\n\
  \        } else {\n\
  \            // There isn't room. We'll need to add at least one new node to \
   the\n\
  \            // list. We could be a bit more careful here and copy as much as\n\
  \            // possible into the current node - that would decrease the \
   number\n\
  \            // of new nodes in some cases, but I don't think the performance\n\
  \            // difference will be large enough to justify the complexity.\n\n\
  \            // If we're not at the end of the current node, we'll need to \
   remove\n\
  \            // the end of the current node's data and reinsert it later.\n\
  \            let num_end_items = e_num_items - item_idx;\n\n\
  \            let (end_items, _end_usercount) = if num_end_items > 0 {\n\
  \                // We'll mark the items as deleted from the node, while \
   leaving\n\
  \                // the data itself there for now to avoid a copy.\n\n\
  \                // Note that if we wanted to, it would also be correct (and\n\
  \                // slightly more space efficient) to pack some of the new\n\
  \                // string's characters into this node after trimming it.\n\
  \                let end_items = &(*e).items[item_idx..e_num_items];\n\
  \                (*e).num_items = item_idx as u8;\n\
  \                let end_usercount = (*e).get_userlen() - \
   cursor.entries[0].skip_usersize;\n\n\
  \                cursor.update_offsets(self.height(), -(end_usercount as \
   isize));\n\n\
  \                // We need to trim the size off because we'll add the \
   characters\n\
  \                // back with insert_node_at.\n\
  \                self.num_usercount -= end_usercount;\n\
  \                self.num_items -= num_end_items;\n\n\
  \                (Some(end_items), end_usercount)\n\
  \            } else {\n\
  \                (None, 0)\n\
  \            };\n\n\
  \            // Now we insert new nodes containing the new character data. The\n\
  \            // data is broken into pieces with a maximum size of \
   NODE_NUM_ITEMS.\n\
  \            // As further optimization, we could try and fit the last piece \
   into\n\
  \            // the start of the subsequent node.\n\
  \            let mut items_remaining = num_inserted_items;\n\
  \            while items_remaining > 0 {\n\
  \                let insert_here = usize::min(items_remaining, \
   NODE_NUM_ITEMS);\n\
  \                self.insert_node_at(cursor, contents, insert_here, true, \
   notify);\n\
  \                items_remaining -= insert_here;\n\
  \            }\n\n\
  \            // TODO: Consider recursively calling insert_at_iter() here \
   instead\n\
  \            // of making a whole new node for the remaining content.\n\
  \            if let Some(end_items) = end_items {\n\
  \                // Passing false to indicate we don't want the cursor updated\n\
  \                // after this - it should remain at the end of the newly\n\
  \                // inserted content, which is *before* this end bit.\n\
  \                self.insert_node_at(cursor, &mut UninitOwnedIter::<Item, \
   N>::from_slice(end_items), end_items.len(), false, notify);\n\
  \            }\n\
  \        }\n\
  \    }\n\n\
  \    // unsafe fn insert_at_iter(&mut self, cursor: &mut Cursor<C>, \
   contents: &[C::Item]) {\n\
  \    //     self.insert_at_iter_and_notify(cursor, contents, Self::no_notify);\n\
  \    // }\n\n\
  \    /// Interestingly unlike the original, here we only care about specifying\n\
  \    /// the number of removed items by counting them. We do not use \
   usersize in\n\
  \    /// the deleted item count.\n\
  \    ///\n\
  \    /// If the deleted content occurs at the start of a node, the cursor \
   passed\n\
  \    /// here must point to the end of the previous node, not the start of the\n\
  \    /// current node.\n\
  \    pub(super) unsafe fn del_at_iter(&mut self, cursor: &Cursor<Item>, mut \
   num_deleted_items: usize, notify: &mut N) {\n\
  \        if num_deleted_items == 0 { return; }\n\n\
  \        let mut item_idx = cursor.local_index;\n\
  \        let mut e = cursor.here_ptr();\n\
  \        while num_deleted_items > 0 {\n\
  \            // self.print();\n\
  \            // if cfg!(debug_assertions) { self.check(); }\n\
  \            if item_idx == (*e).num_items as usize {\n\
  \                let entry = (*e).first_skip_entry();\n\
  \                // End of current node. Skip to the start of the next one. \
   We're\n\
  \                // intentionally not updating the iterator because if we \
   delete\n\
  \                // a whole node we need the iterator to point to the previous\n\
  \                // element. And if we only delete here, the iterator doesn't\n\
  \                // need to be moved.\n\
  \                e = entry.node;\n\
  \                if e.is_null() { panic!(\"Cannot delete past the end of the \
   list\"); }\n\
  \                item_idx = 0;\n\
  \            }\n\n\
  \            let e_num_items = (*e).num_items as usize;\n\
  \            let removed_here = min(num_deleted_items, e_num_items - \
   item_idx);\n\
  \            \n\
  \            let height = (*e).height as usize;\n\
  \            let removed_userlen;\n\n\
  \            if removed_here < e_num_items || e as *const _ == &self.head as \
   *const _ {\n\
  \                // Just trim the node down.\n\
  \                let trailing_items = e_num_items - item_idx - removed_here;\n\
  \                \n\
  \                let c = &mut (*e).items;\n\n\
  \                if N::USED {\n\
  \                    \
   notify.on_delete(maybeinit_slice_get_ref(&c[item_idx..item_idx + \
   removed_here]));\n\
  \                }\n\n\
  \                if mem::needs_drop::<Item>() {\n\
  \                    for item in &mut c[item_idx..item_idx + removed_here] {\n\
  \                        ptr::drop_in_place(item.as_mut_ptr());\n\
  \                    }\n\
  \                }\n\n\
  \                removed_userlen = \
   Item::userlen_of_slice(maybeinit_slice_get_ref(&c[item_idx..item_idx + \
   removed_here]));\n\
  \                if trailing_items > 0 {\n\
  \                    ptr::copy(\n\
  \                        &c[item_idx + removed_here],\n\
  \                        &mut c[item_idx],\n\
  \                        trailing_items);\n\
  \                }\n\n\
  \                (*e).num_items -= removed_here as u8;\n\
  \                self.num_items -= removed_here;\n\
  \                self.num_usercount -= removed_userlen;\n\n\
  \                for s in (*e).nexts_mut() {\n\
  \                    s.skip_usersize -= removed_userlen;\n\
  \                }\n\
  \            } else {\n\
  \                // Remove the node from the skip list entirely. e should be \
   the\n\
  \                // next node after the position of the iterator.\n\
  \                assert_ne!(cursor.here_ptr(), e);\n\n\
  \                if N::USED {\n\
  \                    notify.on_delete((*e).content_slice());\n\
  \                }\n\n\
  \                removed_userlen = (*e).get_userlen();\n\
  \                let next = (*e).first_skip_entry().node;\n\n\
  \                // println!(\"removing {:?} contents {:?} height {}\", e, \
   (*e).content_slice(), height);\n\n\
  \                for i in 0..height {\n\
  \                    let s = &mut (*cursor.entries[i].node).nexts_mut()[i];\n\
  \                    s.node = (*e).nexts_mut()[i].node;\n\
  \                    s.skip_usersize += (*e).nexts()[i].skip_usersize - \
   removed_userlen;\n\
  \                }\n\n\
  \                self.num_items -= (*e).num_items as usize;\n\
  \                self.num_usercount -= removed_userlen;\n\n\
  \                // Update parents.\n\
  \                if Self::use_parents() && height > 1 {\n\
  \                    let mut n = e;\n\
  \                    // let new_parent = cursor.entries[height - 1].node;\n\n\
  \                    // If you imagine this node as a big building, we need to\n\
  \                    // update the parent of all the nodes we cast a shadow \
   over.\n\
  \                    // So, if our height is 3 and the next nodes have \
   heights 1\n\
  \                    // and 2, they both need new parents.\n\
  \                    let mut parent_height = 1;\n\
  \                    let cursor_node = cursor.here_ptr();\n\
  \                    let cursor_node_height = (*cursor_node).height as usize;\n\
  \                    let mut new_parent = if height >= cursor_node_height {\n\
  \                        cursor.entries[parent_height].node\n\
  \                    } else {\n\
  \                        cursor_node\n\
  \                    };\n\n\
  \                    loop {\n\
  \                        n = (*n).nexts_mut()[parent_height - 1].node;\n\
  \                        if n.is_null() || (*n).height >= height as u8 { \
   break; }\n\
  \                        let n_height = (*n).height as usize;\n\
  \                        \n\
  \                        assert_eq!((*n).parent, e);\n\
  \                        assert!(n_height >= parent_height - 1);\n\n\
  \                        if n_height > parent_height {\n\
  \                            parent_height = n_height;\n\
  \                            if n_height >= cursor_node_height {\n\
  \                                new_parent = \
   cursor.entries[parent_height].node\n\
  \                            }\n\
  \                        }\n\
  \                        \n\
  \                        (*n).parent = new_parent;\n\
  \                    }\n\
  \                }\n\n\
  \                Node::free(e);\n\
  \                e = next;\n\
  \            }\n\n\
  \            for i in height..self.height() {\n\
  \                let s = &mut (*cursor.entries[i].node).nexts_mut()[i];\n\
  \                s.skip_usersize -= removed_userlen;\n\
  \            }\n\n\
  \            num_deleted_items -= removed_here;\n\n\
  \            // if cfg!(debug_assertions) { self.check(); }\n\
  \        }\n\
  \    }\n\n\n\
  \    pub(super) unsafe fn replace_at_iter<I>(&mut self, cursor: &mut \
   Cursor<Item>, mut removed_items: usize, inserted_content: &mut I, notify: \
   &mut N)\n\
  \            where I: ExactSizeIterator<Item=Item> {\n\
  \        if removed_items == 0 && inserted_content.len() == 0 { return; }\n\n\
  \        // Replace as many items from removed_items as we can with \
   inserted_content.\n\
  \        let mut replaced_items = min(removed_items, inserted_content.len());\n\
  \        removed_items -= replaced_items;\n\n\
  \        while replaced_items > 0 {\n\
  \            debug_assert!(inserted_content.len() >= replaced_items);\n\
  \            let mut e = cursor.here_ptr();\n\
  \            if cursor.local_index == (*e).num_items as usize {\n\
  \                // Move to the next item.\n\
  \                e = cursor.advance_node();\n\
  \                if e.is_null() { panic!(\"Cannot replace past the end of \
   the list\"); }\n\
  \            }\n\n\
  \            let index = cursor.local_index;\n\n\
  \            let e_num_items = (*e).num_items as usize;\n\
  \            let replaced_items_here = min(replaced_items, e_num_items - \
   index);\n\n\
  \            let dest = &mut (*e).items[index..index + replaced_items_here];\n\
  \            let old_usersize = \
   Item::userlen_of_slice(maybeinit_slice_get_ref(dest));\n\n\
  \            // Replace the items themselves. Everything else is commentary.\n\
  \            // Would prefer to use zip() but it wants ownership of \
   inserted_content :/\n\
  \            for slot in dest.iter_mut() {\n\
  \                *slot.as_mut_ptr() = inserted_content.next().unwrap();\n\
  \            }\n\n\
  \            let dest = maybeinit_slice_get_ref(dest);\n\
  \            let new_usersize = Item::userlen_of_slice(dest);\n\
  \            let usersize_delta = new_usersize as isize - old_usersize as \
   isize;\n\n\
  \            if usersize_delta != 0 {\n\
  \                cursor.update_offsets(self.height(), usersize_delta);\n\
  \                // I hate this.\n\
  \                self.num_usercount = \
   self.num_usercount.wrapping_add(usersize_delta as usize);\n\
  \            }\n\n\
  \            replaced_items -= replaced_items_here;\n\
  \            // We'll hop to the next Node at the start of the next loop\n\
  \            // iteration if needed.\n\
  \            cursor.local_index += replaced_items_here;\n\n\
  \            for i in 0..self.height() {\n\
  \                cursor.entries[i].skip_usersize += new_usersize;\n\
  \            }\n\
  \            cursor.userpos += new_usersize;\n\n\
  \            notify.on_set(dest, ItemMarker {\n\
  \                ptr: e,\n\
  \                // _phantom: PhantomData,\n\
  \            });\n\
  \        }\n\n\
  \        // Ok now one of two things must be true. Either we've run out of\n\
  \        // items to remove, or we've run out of items to insert.\n\
  \        if inserted_content.len() > 0 {\n\
  \            // Insert!\n\
  \            debug_assert!(removed_items == 0);\n\
  \            self.insert_at_iter(cursor, inserted_content, notify);\n\
  \        } else if removed_items > 0 {\n\
  \            self.del_at_iter(cursor, removed_items, notify);\n\
  \        }\n\
  \    }\n\n\
  \    pub(super) unsafe fn replace_item(&mut self, cursor: &mut Cursor<Item>, \
   new_item: Item, notify: &mut N) {\n\
  \        // This could easily be optimized.\n\
  \        self.replace_at_iter(cursor, 1, &mut iter::once(new_item), notify);\n\n\
  \        // self.modify_at(start_userpos, Self::no_notify, |item, offset| {\n\
  \        //     assert_eq!(offset, 0, \"replace_at must modify the entire \
   item\");\n\
  \        //     *item = \n\
  \        // })\n\
  \    }\n\n\
  \    // TODO: This is just for debugging. Do not export this.\n\
  \    pub fn print(&self) where Item: std::fmt::Debug {\n\
  \        println!(\"items: {}\\tuserlen: {}, height: {}\", self.num_items, \
   self.len_user(), self.head.height);\n\n\
  \        print!(\"HEAD:\");\n\
  \        for s in self.head.nexts() {\n\
  \            print!(\" |{} \", s.skip_usersize);\n\
  \        }\n\
  \        println!();\n\n\
  \        use std::collections::HashMap;\n\
  \        let mut ptr_to_id = HashMap::new();\n\
  \        // ptr_to_id.insert(std::ptr::null(), usize::MAX);\n\
  \        for (i, node) in self.node_iter().enumerate() {\n\
  \            print!(\"{}:\", i);\n\
  \            ptr_to_id.insert(node as *const _, i);\n\
  \            for s in node.nexts() {\n\
  \                print!(\" |{} \", s.skip_usersize);\n\
  \            }\n\
  \            print!(\"      : {:?}\", node.content_slice());\n\
  \            if let Some(id) = ptr_to_id.get(&(node.parent as *const _)) {\n\
  \                print!(\" (parent: {})\", id);\n\
  \            }\n\
  \            print!(\" (pointer: {:?})\", node as *const _);\n\n\
  \            println!();\n\
  \        }\n\
  \    }\n\
   }\n\n\n\n\
   impl<Item: ListItem, N: NotifyTarget<Item>> SkipList<Item, N> {\n\
  \    pub fn eq_list<Rhs>(&self, other: &[Rhs]) -> bool where Item: \
   PartialEq<Rhs> {\n\
  \        let mut pos = 0;\n\
  \        let other_len = other.len();\n\n\
  \        for node in self.node_iter() {\n\
  \            let my_data = node.content_slice();\n\
  \            let my_len = my_data.len();\n\n\
  \            if pos + my_len > other_len || my_data != &other[pos..pos + \
   my_data.len()] {\n\
  \                return false\n\
  \            }\n\
  \            pos += my_data.len();\n\
  \        }\n\n\
  \        pos == other_len\n\
  \    }\n\
   }\n\n\
   impl<Item: ListItem, N: NotifyTarget<Item>> Drop for SkipList<Item, N> {\n\
  \    fn drop(&mut self) {\n\
  \        let mut node = self.head.first_skip_entry().node;\n\
  \        unsafe {\n\
  \            while !node.is_null() {\n\
  \                let next = (*node).first_skip_entry().node;\n\
  \                Node::free(node);\n\
  \                node = next;\n\
  \            }\n\
  \        }\n\
  \    }\n\
   }\n\n\n\
   // Only if there's no notification target.\n\
   impl<I, Item: ListItem> From<I> for SkipList<Item> where I: \
   ExactSizeIterator<Item=Item> {\n\
  \    fn from(iter: I) -> SkipList<Item> {\n\
  \        SkipList::new_from_iter(iter)\n\
  \    }\n\
   }\n\n\
   // Needs me to relax the ExactSizeIterator constraint on insert.\n\
   // impl<Item: ListItem> iter::FromIterator<Item> for SkipList<Item> {\n\
   //     fn from_iter<T: IntoIterator<Item = Item>>(iter: T) -> Self {\n\
   //         SkipList::new_from_iter(iter)\n\
   //     }\n\
   // }\n\n\
   impl<Item: ListItem, N: NotifyTarget<Item>> Into<Vec<Item>> for \
   &SkipList<Item, N> where Item: Copy {\n\
  \    fn into(self) -> Vec<Item> {\n\
  \        let mut content: Vec<Item> = Vec::with_capacity(self.num_items);\n\n\
  \        for node in self.node_iter() {\n\
  \            content.extend(node.content_slice().iter());\n\
  \        }\n\n\
  \        content\n\
  \    }\n\
   }\n\n\
   impl<Item: ListItem, N: NotifyTarget<Item>> fmt::Debug for SkipList<Item, \
   N> where Item: fmt::Debug {\n\
  \    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {\n\
  \        f.debug_list().entries(self.iter()).finish()\n\
  \    }\n\
   }\n\n\
   impl<Item: ListItem, N: NotifyTarget<Item>> Default for SkipList<Item, N> {\n\
  \    fn default() -> Self {\n\
  \        SkipList::new()\n\
  \    }\n\
   }\n\n\n\
   pub struct ListItemIter<'a, Item: ListItem> {\n\
  \    node: Option<&'a Node<Item>>,\n\
  \    index: usize,\n\
  \    remaining_items: Option<usize> // For size_hint, if known.\n\
   }\n\n\
   impl<'a, Item: ListItem> Iterator for ListItemIter<'a, Item> {\n\
  \    type Item = &'a Item;\n\n\
  \    fn next(&mut self) -> Option<Self::Item> {\n\
  \        if let Some(node) = self.node {\n\
  \            let current = &node.items[self.index];\n\
  \            self.index += 1;\n\
  \            if self.index == node.num_items as usize {\n\
  \                self.index = 0;\n\
  \                self.node = unsafe { node.get_next_ptr().as_ref() };\n\
  \            }\n\n\
  \            Some(unsafe { &*current.as_ptr() })\n\
  \        } else { None }\n\
  \    }\n\n\
  \    fn size_hint(&self) -> (usize, Option<usize>) {\n\
  \        if let Some(r) = self.remaining_items {\n\
  \            (r, Some(r))\n\
  \        } else {\n\
  \            (0, None)\n\
  \        }\n\
  \    }\n\
   }\n\n\n\
   // impl<T: Default + Copy, F: Fn(&T) -> usize> PartialEq for SkipList<T, F> {\n\
   //     // This is quite complicated. It would be cleaner to just write a \
   bytes\n\
   //     // iterator, then iterate over the bytes of both strings comparing \
   along the\n\
   //     // way.\n\
   //     // However, this should be faster because it can memcmp().\n\n\
   //     // Another way to implement this would be to rewrite it as a \
   comparison with\n\
   //     // an iterator over &str. Then the list vs list comparison would be \
   trivial,\n\
   //     // but also we could add comparison functions with a single &str and \
   stuff\n\
   //     // very easily.\n\
   //     fn eq(&self, other: &SkipList<T, F>) -> bool {\n\
   //         if self.num_items != other.num_items\n\
   //                 || self.num_chars() != other.num_chars() {\n\
   //             return false\n\
   //         }\n\n\
   //         let mut other_iter = other.iter().map(|n| { n.as_str() });\n\n\
   //         let mut os = other_iter.next();\n\
   //         let mut opos: usize = 0; // Byte offset in os.\n\
   //         for n in self.iter() {\n\
   //             let s = n.as_str();\n\
   //             let mut pos: usize = 0; // Current byte offset in s\n\
   //             debug_assert_eq!(s.len(), n.num_bytes as usize);\n\n\
   //             // Walk s.len() bytes through the other list\n\
   //             while pos < n.num_bytes as usize {\n\
   //                 if let Some(oss) = os {\n\
   //                     let amt = min(s.len() - pos, oss.len() - opos);\n\
   //                     // println!(\"iter slen {} pos {} osslen {} amt \
   {}\", s.len(), pos, oss.len(), amt);\n\n\
   //                     if &s[pos..pos+amt] != &oss[opos..opos+amt] {\n\
   //                         return false\n\
   //                     }\n\n\
   //                     pos += amt;\n\
   //                     opos += amt;\n\
   //                     debug_assert!(opos <= oss.len());\n\n\
   //                     if opos == oss.len() {\n\
   //                         os = other_iter.next();\n\
   //                         opos = 0;\n\
   //                     }\n\
   //                 } else {\n\
   //                     panic!(\"Internal string length does not match\");\n\
   //                 }\n\
   //             }\n\
   //         }\n\n\
   //         true\n\
   //     }\n\
   // }\n\
   // impl<T: Default + Copy, F: Fn(&T) -> usize> Eq for SkipList<T, F> {}\n\n\
   // impl<T: Default + Copy, F> Clone for SkipList<T, F> where F: Fn(&T) -> \
   usize {\n\
   //     fn clone(&self) -> Self {\n\
   //         let mut r = SkipList::new(self.get_usersize);\n\
   //         r.num_items = self.num_items;\n\
   //         let head_str = self.head.as_str();\n\
   //         \
   r.head.items[..head_str.len()].copy_from_slice(head_str.as_bytes());\n\
   //         r.head.num_bytes = self.head.num_bytes;\n\
   //         r.head.height = self.head.height;\n\
  \        \n\
   //         {\n\
   //             // I could just edit the overflow memory directly, but this \
   is safer\n\
   //             // because of aliasing rules.\n\
   //             let head_nexts = r.head.nexts_mut();\n\
   //             for i in 0..self.height() {\n\
   //                 head_nexts[i].skip_items = self.nexts[i].skip_items;\n\
   //             }\n\
   //         }\n\n\
   //         let mut nodes = [&mut r.head as *mut Node; MAX_HEIGHT];\n\n\
   //         // The first node the iterator will return is the head. Ignore it.\n\
   //         let mut iter = self.iter();\n\
   //         iter.next();\n\
   //         for other in iter {\n\
   //             // This also sets height.\n\
   //             let height = other.height;\n\
   //             let node = Node::alloc_with_height(height);\n\
   //             unsafe {\n\
   //                 (*node).num_bytes = other.num_bytes;\n\
   //                 let len = other.num_bytes as usize;\n\
   //                 \
   (*node).items[..len].copy_from_slice(&other.items[..len]);\n\n\
   //                 let other_nexts = other.nexts();\n\
   //                 let nexts = (*node).nexts_mut();\n\
   //                 for i in 0..height as usize {\n\
   //                     nexts[i].skip_items = other_nexts[i].skip_items;\n\
   //                     (*nodes[i]).nexts_mut()[i].node = node;\n\
   //                     nodes[i] = node;\n\
   //                 }\n\
   //             }\n\
   //         }\n\n\
   //         r\n\
   //     }\n\
   // }\n"
