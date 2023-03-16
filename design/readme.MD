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



