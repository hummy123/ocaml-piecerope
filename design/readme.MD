# PieceRope-ocaml Design

## Introduction

This data structure consists of two main components: a buffer represented as a Rope-like structure and a Piece Tree, just like a Piece Table but only with one buffer as we are storing all text in RAM in which case the distinction between the add and original bufer is pointless.

All algorithms here are described and implemented recursively (with Continuation Passing Style to avoid stack overflows).

## Buffer

The buffer is represented as a Rope-like structure on top of an AVL Tree. The buffer supports two operations: append and substring.

The difference between the buffer and normal ropes is that there is no distinction between concat and leaf nodes. Every node contains left/right subtree metadata and a string. The difference can be seen in the images below.

[img 1]

[img 2]

The second image is slightly misleading - each node is still annotated with the size of its left and right subtrees and instead of creaing a new node each time we append we concatenate the string we want to append to the rightmost node until (last node's string length + append string's length) is greater than some maximum limit (currently 1024). If we go over that limit, we create a new node for the append string and rebalance as recursion unwinds. 

Keeping the tree short (which the concatenation and lack of distinction between concat and leaf nodes helps us to do) keeps the structure performant.

This isn't the only way to represent the buffer - a plain string can be used (although you pay the cost of allocating and freeing up large objects) and VS Code uses an array of strings which is generally faster but mutable and non-functional (missing all of the benefits of immutability).

Using an array also means storing a number in each node in the Piece Tree about which index it points to in the array. This can fragment the Piece Tree compared to allowing a single piece to address the whole contents of the buffer. So, while VS Code's scheme can be made functional with a HAMT, I prefer this design.

### Operations - Append

1. Go to the rightmost node.
2. Is length of string we want to insert + length of rightmost node under a certain length?
3. If it is under, just concate the insert string to this node.
4. If it is over, create a new node with the insert string and rebalance.

### Operations - Substring

The substring operations are implemented in an unusual way because of OCaml's most performant method of concatenating strings - using the (::) cons operator to create a linked list of strings (which adds to the front of the list each time) and then calling String.concat to concatenate every string in the list.

The order imposed by this is reflected in the following traversal.

1. Is the current node in the substring range? If yes, recurse to the right node and then append the string at this node once we are done recursing. After that, recurse to the left.
2. Is the start of this node (= the end of the substring range) in range? If yes, get the relevant substring, recurse leftwards and then append the substring we got at this current node once leftwards recurse is done.
3. Is the end of this node (= the start of the substring range) in range? If yes, get substring, append it and recurse right.
4. If the substring range is in the middle of this node, we don't need to recurse anywhere else. So get the substring and return it.
5. If the substring range starts before our current index in the node, recurse left.
6. Else, recurse right.

## Piece Tree

The second part of the structure, the Piece Tree, is just like the second image above except, instead of storing a string, we store pointers to the buffer with a start and length property of a range of text to extract (this is the "Piece" in the "Piece Tree" or "Piece Table").

The kind of balanced binary tree used for the Piece Tree is important as some tree invariants are easier to maintain than others. For this reason, I chose AVL Trees which use the subtrees' height for the balancing scheme. 

There are some operations that are quite odd (one insert case means inserting two nodes in one operation and one delete case, instead of deleting from the tree, actually inserts a new node into the tree) and having a balancing algorithm that is the same for both insert and delete operations is helpful as we don't need to propogate this information, and AVL and Weight Balanced Trees are good for this task.

Description of some operations is omitted due to minimal differences with one of the buffer operations described above. 

(These are prepend, which is the same as buffer.append except reverse, append which is the same as for the buffer except that it merges with the rightmost piece if the rightmost piece was the last to be inserted into the tree, indexing, substring and line retrieval.)

### Operations - Insert

1. Are we at an empty node? Then create a new node at this place.
2. Are we trying to insert before the node/index we are currentl at? Then recurse leftwards.
3. Are we trying to insert after the current node/index? Then recurse rightwards.
4. Are we trying to insert at the same index this current node starts at? Then append the node to insert to the left subtree. (If it is understood that an in-order traversal rebuilds the original string, the logic used here makes sense.)
5. If we are trying to insert at the same index at the end of this node, and...
5. a. our last operation was inserting this node, then merge the insertion piece with the piece at this node - optimisation to avoid making tree larger than it needs to be.
5. b. otherwise, prepend the insert node to the start of the right subtree (same logic as 4.).
6. If none of the above cases, that means we need to split the node we are at in the middle. Once we calculate the length of each part of the split node, we append the left part to the end of the left subtree and prepend the right part to the start of the left subtree. Where the previous node was, we will place the node we want to insert.


