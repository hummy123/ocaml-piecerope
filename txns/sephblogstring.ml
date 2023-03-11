let str =
  "# 5000x faster CRDTs: An Adventure in Optimization\n\n\
   <span class=post-meta>July 31 2021</span>\n\n\
   A few years ago I was really bothered by an academic paper.\n\n\
   Some researchers in France put together a comparison showing lots of ways \
   you could implement realtime collaborative editing (like Google Docs). They \
   implemented lots of algorithms - CRDTs and OT algorithms and stuff. And \
   they benchmarked them all to see how they perform. (Cool!!) Some algorithms \
   worked reasonably well. But others took upwards of 3 seconds to process \
   simple paste operations from their editing sessions. Yikes!\n\n\
   Which algorithm was that? Well, this is awkward but .. it was mine. I mean, \
   I didn't invent it - but it was the algorithm I was using for ShareJS. The \
   algorithm we used for Google Wave. The algorithm which - hang on - I knew \
   for a fact didn't take 3 seconds to process large paste events. Whats going \
   on here?\n\n\
   I took a closer look at the paper. In their implementation when a user \
   pasted a big chunk of text (like 1000 characters), instead of creating 1 \
   operation with 1000 characters, their code split the insert into 1000 \
   individual operations. And each of those operations needed to be processed \
   separately. Do'h - of course it'll be slow if you do that! This isn't a \
   problem with the operational transformation algorithm. This is just a \
   problem with *their particular implementation*.\n\n\
   The infuriating part was that several people sent me links to the paper and \
   (pointedly) asked me what I think about it. Written up as a Published \
   Science Paper, these speed comparisons seemed like a Fact About The \
   Universe. And not what they really were - implementation details of some \
   java code, written by a probably overstretched grad student. One of a whole \
   bunch of implementations that they needed to code up.\n\n\
   \"Nooo! The peer reviewed science isn't right everybody! Please believe \
   me!\". But I didn't have a published paper justifying my claims. I had \
   working code but it felt like none of the smart computer science people \
   cared about that. Who was I? I was nobody.\n\n\
   ---\n\n\
   Even talking about this stuff we have a language problem. We describe each \
   system as an \"algorithm\". Jupiter is an Algorithm. RGA is an Algorithm. \
   But really there are two very separate aspects:\n\n\
   1. The black-box *behaviour* of concurrent edits. When two clients edit the \
   same region of text at the same time, what happens? Are they merged, and if \
   so in what order? What are the rules?\n\
   2. The white-box *implementation* of the system. What programming language \
   are we using? What data structures? How well optimized is the code?\n\n\
   If some academic's code runs slowly, what does that actually teach us? \
   Maybe it's like tests. A passing test suite *suggests*, but can never \
   *prove* that there are no bugs. Likewise a slow implementation suggests, \
   but can never prove that every implementation of the system will be slow. \
   If you wait long enough, somebody will find more bugs. And, maybe, someone \
   out there can design a faster implementation.\n\n\
   Years ago I translated my old text OT code into C, Javascript, Go, Rust and \
   Swift. Each implementation has the same behaviour, and the same algorithm. \
   But the performance is not even close. In javascript my transform function \
   ran about 100 000 times per second. Not bad! But the same function in C \
   does 20M iterations per second. That's 200x faster. Wow!\n\n\
   Were the academics testing a slow version or the fast version of this code? \
   Maybe, without noticing, they had fast versions of some algorithms and slow \
   versions of others. It's impossible to tell from the paper!\n\n\n\
   ## Making CRDTs fast\n\n\
   So as you may know, I've been getting interested in CRDTs lately. For the \
   uninitiated, CRDTs [(Conflict-Free Replicated Data \
   types)](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type) \
   are fancy programming tools which let multiple users edit the same data at \
   the same time. They let you work locally with no lag. (You don't even have \
   to be online). And when you do sync up with other users & devices, \
   everything just magically syncs up and becomes eventually consistent. The \
   best part of CRDTs is that they can do all that without even needing a \
   centralized computer in the cloud to monitor and control everything.\n\n\
   I want Google Docs without google. I want my apps to seamlessly share data \
   between all my devices, without me needing to rely on some [flakey \
   startup](https://ourincrediblejourney.tumblr.com/)'s servers to still be \
   around in another decade. I think they're the [future of collaborative \
   editing](https://josephg.com/blog/crdts-are-the-future/). And maybe the \
   future of all software - but I'm not ready to talk about that yet.\n\n\
   But most CRDTs you read about in academic papers are crazy slow. A decade \
   ago I decided to stop reading academic papers and dismissed them. I assumed \
   CRDTs had some inherent problem. A GUID for every character? Nought but \
   madness comes from those strange lands! But - and this is awkward to admit \
   - I think I've been making the same mistake as those researchers. I was \
   reading papers which described the *behaviour* of different systems. And I \
   assumed that meant we knew how the best way to *implement* those systems. \
   And wow, I was super wrong.\n\n\
   How wrong? Well. Running [this editing \
   trace](https://github.com/automerge/automerge-perf/), \
   [Automerge](https://github.com/automerge/automerge/) (a popular CRDT, \
   written by [a popular researcher](https://martin.kleppmann.com/)) takes \
   nearly 5 minutes to run. I have a [new \
   implementation](https://github.com/josephg/diamond-types) that can process \
   the same editing trace in 56 milliseconds. Thats 0.056 seconds, which is \
   over 5000x faster. It's the largest speed up I've ever gotten from \
   optimization work - and I'm utterly delighted by it.\n\n\
   Lets talk about why automerge is currently slow, and I'll take you through \
   all the steps toward making it super fast.\n\n\
   Wait, no. First we need to start with:\n\n\n\
   ### What is automerge?\n\n\
   Automerge is a library to help you do collaborative editing. It's written \
   by Martin Kleppmann, who's a little bit famous from his book and [excellent \
   talks](https://martin.kleppmann.com/2020/07/06/crdt-hard-parts-hydra.html). \
   Automerge is based on an algorithm called RGA, which you can read about in \
   an academic paper if you're into that sort of thing.\n\n\
   Martin explains automerge far better than I will in this talk from 2020:\n\n\
   <iframe class=\"youtube\" \
   src=\"https://www.youtube-nocookie.com/embed/x7drE24geUw?start=1237\" \
   title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; \
   autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" \
   allowfullscreen></iframe>\n\n\
   Automerge (and Yjs and other CRDTs) think of a shared document as a list of \
   characters. Each character in the document gets a unique ID, and whenever \
   you insert into the document, you name what you're inserting after.\n\n\
   Imagine I type \"abc\" into an empty document. Automerge creates 3 items:\n\n\
   - Insert *'a'* id `(seph, 0)` after `ROOT`\n\
  \  - Insert *'b'* id `(seph, 1)` after `(seph, 0)`\n\
  \    - Insert *'c'* id `(seph, 2)` after `(seph, 1)`\n\n\
   We can draw this as a tree!\n\n\
   ![tree with \"abc\" inserts](automerge1.drawio.svg)\n\n\
   Lets say Mike inserts an 'X' between *a* and *b*, so we get \"aXbc\". Then \
   we have:\n\n\
   - Insert *'a'* id `(seph, 0)` after `ROOT`\n\
  \  - Insert *'X'* id `(mike, 0)` after `(seph, 0)`\n\
  \  - Insert *'b'* id `(seph, 1)` after `(seph, 0)`\n\
  \    - Insert *'c'* id `(seph, 2)` after `(seph, 1)`\n\n\
   ![tree with \"aXbc\"](automerge2.drawio.svg)\n\n\
   Note the 'X' and 'b' both share the same parent. This will happen when \
   users type concurrently in the same location in the document. But how do we \
   figure out which character goes first? We could just sort using their agent \
   IDs or something. But argh, if we do that the document could end up as \
   *abcX*, even though Mike inserted *X* before the *b*. That would be really \
   confusing.\n\n\
   Automerge (RGA) solves this with a neat hack. It adds an extra integer to \
   each item called a *sequence number*. Whenever you insert something, you \
   set the new item's sequence number to be 1 bigger than the biggest sequence \
   number you've ever seen:\n\n\
   - Insert *'a'* id `(seph, 0)` after `ROOT`, seq: *0*\n\
  \  - Insert *'X'* id `(mike, 0)` after `(seph, 0)`, seq: *3*\n\
  \  - Insert *'b'* id `(seph, 1)` after `(seph, 0)`, seq: *1*\n\
  \    - Insert *'c'* id `(seph, 2)` after `(seph, 1)`, seq: *2*\n\n\
   This is the algorithmic version of \"Wow I saw a sequence number, and it \
   was *this big!*\" \"Yeah? Mine is *even bigger!*\"\n\n\
   The rule is that children are sorted first based on their sequence numbers \
   (bigger sequence number first). If the sequence numbers match, the changes \
   must be concurrent. In that case we can sort them arbitrarily based on \
   their agent IDs. (We do it this way so all peers end up with the same \
   resulting document.)\n\n\
   Yjs - which we'll see more of later - implements a CRDT called YATA. YATA \
   is identical to RGA, except that it solves this problem with a slightly \
   different hack. But the difference isn't really important here.\n\n\
   Automerge (RGA)'s *behaviour* is defined by this algorithm:\n\n\
   - Build the tree, connecting each item to its parent\n\
   - When an item has multiple children, sort them by sequence number then by \
   their ID.\n\
   - The resulting list (or text document) can be made by flattening the tree \
   with a depth-first traversal.\n\n\
   So how should you *implement* automerge? The automerge library does it in \
   the obvious way, which is to store all the data as a tree. (At least I \
   think so - after typing \"abc\" [this is automerge's internal \
   state](https://gist.github.com/josephg/0522c4aec5021cc1dddb60e778828dbe). \
   Uh, uhm, I have no idea whats going on here. And what are all those \
   Uint8Arrays doing all over the place? Whatever.) The automerge library \
   works by building a tree of items.\n\n\
   For a simple benchmark, I'm going to test automerge using [an editing trace \
   Martin himself made](https://github.com/automerge/automerge-perf/). This is \
   a character by character recording of Martin typing up an academic paper. \
   There aren't any concurrent edits in this trace, but users almost never \
   actually put their cursors at exactly the same place and type anyway, so \
   I'm not too worried about that. I'm also only counting the time taken to \
   apply this trace *locally*, which isn't ideal but it'll do. Kevin Jahns \
   (Yjs's author) has a much more [extensive benchmarking suite \
   here](https://github.com/dmonad/crdt-benchmarks) if you're into that sort \
   of thing. All the benchmarks here are done on my chonky ryzen 5800x \
   workstation, with Nodejs v16.1 and rust 1.52 when that becomes appropriate. \
   (Spoilers!)\n\n\
   The editing trace has 260 000 edits, and the final document size is about \
   100 000 characters.\n\n\
   As I said above, automerge takes a little under 5 minutes to process this \
   trace. Thats just shy of 900 edits per second, which is probably fine. But \
   by the time it's done, automerge is using 880 MB of RAM. Whoa! That's 10kb \
   of ram *per key press*. At peak, automerge was using 2.6 GB of RAM!\n\n\
   To get a sense of how much overhead there is, I'll compare this to [a \
   baseline \
   benchmark](https://gist.github.com/josephg/13efc1444660c07870fcbd0b3e917638#file-js_baseline-js) \
   where we just splice all the edits directly into a javascript string. This \
   throws away all the information we need to do collaborative editing, but it \
   gives us a sense of how fast javascript is capable of going. It turns out \
   javascript running on V8 is *fast*:\n\n\
   | Test                              | Time taken | RAM usage |\n\
   |:--------------------------        | ----------:| ---------:|\n\
   | **automerge (v1.0.0-preview2)**   |  291s      | 880 MB    |\n\
   | *Plain string edits in JS*        | 0.61s      | 0.1 MB    |\n\n\
   This is a chart showing the time taken to process each operation throughout \
   the test, averaged in groups of 1000 operations. I think those spikes are \
   V8's garbage collector trying to free up memory.\n\n\
   ![automerge performance chart](am_perf1.svg)\n\n\
   In the slowest spike near the end, a single edit took *1.8 seconds* to \
   process. Oof. In a real application, the whole app (or browser tab) would \
   freeze up for a couple of seconds sometimes while you're in the middle of \
   typing.\n\n\
   The chart is easier to read when we average everything out a bit and zoom \
   the Y axis. We can see the average performance gets gradually (roughly \
   linearly) worse over time.\n\n\
   ![automerge performance chart smoothed out](am_perf1_smooth.svg)\n\n\
   ### Why is automerge slow though?\n\n\
   Automerge is slow for a whole slew of reasons:\n\n\
   1. Automerge's core tree based data structure gets big and slow as the \
   document grows.\n\
   2. Automerge makes heavy use of \
   [Immutablejs](https://immutable-js.github.io/). Immutablejs is a library \
   which gives you clojure-like copy-on-write semantics for javascript \
   objects. This is a cool set of functionality, but the V8 optimizer & GC \
   struggles to optimize code that uses immutablejs. As a result, it increases \
   memory usage and decreases performance.\n\
   3. Automerge treats each inserted character as a separate item. Remember \
   that paper I talked about earlier, where copy+paste operations are slow? \
   Automerge does that too!\n\n\
   Automerge was just never written with performance in mind. Their team is \
   working on a replacement [rust implementation of the \
   algorithm](https://github.com/automerge/automerge-rs/) to run through wasm, \
   but at the time of writing it hasn't landed yet. I got the master branch \
   working, but they have some kinks to work out before it's ready. Switching \
   to the automerge-rs backend doesn't make average performance in this test \
   any faster. (Although it does halve memory usage and smooth out \
   performance.)\n\n\
   ---\n\n\
   There's an old saying with performance tuning:\n\n\
   > You can't make the computer faster. You can only make it do less work.\n\n\
   How do we make the computer do less work here? There's lots of performance \
   wins to be had from going through the code and improving lots of small \
   things. But the automerge team has the right approach. It's always best to \
   start with macro optimizations. Fix the core algorithm and data structures \
   before moving to optimizing individual methods. There's no point optimizing \
   a function when you're about to throw it away in a rewrite.\n\n\
   By far, Automerge's biggest problem is its complex tree based data \
   structure. And we can replace it with something faster.\n\n\n\
   ## Improving the data structure\n\n\
   Luckily, there's a better way to implement CRDTs, pioneered in \
   [Yjs](https://github.com/yjs/yjs). Yjs is another (competing) opensource \
   CRDT implementation made by Kevin Jahns. It's fast, well documented and \
   well made. If I were going to build software which supports collaborative \
   editing today, I'd use Yjs.\n\n\
   Yjs doesn't need a whole blog post talking about how to make it fast \
   because it's already pretty fast, as we'll see soon. It got there by using \
   a clever, obvious data structure \"trick\" that I don't think anyone else \
   in the field has noticed. Instead of implementing the CRDT as a tree like \
   automerge does:\n\n\
   ```javascript\n\
   state = {\n\
  \  { item: 'a', id: ['seph', 0], seq: 0, children: [\n\
  \    { item: 'X', id, seq, children: []},\n\
  \    { item: 'b', id, seq, children: [\n\
  \      { item: 'c', id, seq, children: []}\n\
  \    ]}\n\
  \  ]}\n\
   }\n\
   ```\n\n\
   Yjs just puts all the items in a single flat list:\n\n\
   ```javascript\n\
   state = [\n\
  \  { item: 'a', id: ['seph', 0], seq: 0, parent: null },\n\
  \  { item: 'X', id, seq, parent: ['seph', 0] },\n\
  \  { item: 'b', id, seq, parent: ['seph', 0] },\n\
  \  { item: 'c', id, seq, parent: [..] }\n\
   ]\n\
   ```\n\n\
   That looks simple, but how do you insert a new item into a list? With \
   automerge it's easy:\n\n\
   1. Find the parent item\n\
   2. Insert the new item into the right location in the parents' list of \
   children\n\n\
   But with this list approach it's more complicated:\n\n\
   1. Find the parent item\n\
   2. Starting right after the parent item, iterate through the list until we \
   find the location where the new item should be inserted (?)\n\
   3. Insert it there, splicing into the array\n\n\
   Essentially, this approach is just a fancy insertion sort. We're \
   implementing a list CRDT with a list. Genius!\n\n\
   This sounds complicated - how do you figure out where the new item should \
   go? But it's complicated in the same way *math* is complicated. It's hard \
   to understand, but once you understand it, you can implement the whole \
   insert function in about 20 lines of code:\n\n\
   (But don't be alarmed if this looks confusing - we could probably fit \
   everyone on the planet who understands this code today into a small meeting \
   room.)\n\n\
   ```javascript\n\
   const automergeInsert = (doc, newItem) => {\n\
  \  const parentIdx = findItem(doc, newItem.parent) // (1)\n\n\
  \  // Scan to find the insert location\n\
  \  let i\n\
  \  for (i = parentIdx + 1; i < doc.content.length; i++) {\n\
  \    let o = doc.content[i]\n\
  \    if (newItem.seq > o.seq) break // Optimization.\n\
  \    let oparentIdx = findItem(doc, o.parent)\n\n\
  \    // Should we insert here? (Warning: Black magic part)\n\
  \    if (oparentIdx < parentIdx\n\
  \      || (oparentIdx === parentIdx\n\
  \        && newItem.seq === o.seq\n\
  \        && newItem.id[0] < o.id[0])\n\
  \    ) break\n\
  \  }\n\
  \  // We've found the position. Insert at position *i*.\n\
  \  doc.content.splice(i, 0, newItem) // (2)\n\n\
  \  // .. And do various bookkeeping.\n\
   }\n\
   ```\n\n\
   I implemented both Yjs's CRDT (YATA) and Automerge using this approach in \
   my experimental \
   [*reference-crdts*](https://github.com/josephg/reference-crdts/blob/main/crdts.ts) \
   codebase. [Here's the insert function, with a few more \
   comments](https://github.com/josephg/reference-crdts/blob/fed747255df9d457e11f36575de555b39f07e909/crdts.ts#L401-L459). \
   The Yjs version of this function is in the same file, if you want to have a \
   look. Despite being very different papers, the logic for inserting is \
   almost identical. And even though my code is very different, this approach \
   is *semantically* identical to the actual automerge, and Yjs and sync9 \
   codebases. ([Fuzzer verified \
   (TM)](https://github.com/josephg/reference-crdts/blob/main/reference_test.ts)).\n\n\
   If you're interested in going deeper on this, I gave [a talk about this \
   approach](https://invisiblecollege.s3-us-west-1.amazonaws.com/braid-meeting-10.mp4#t=300) \
   at a [braid](https://braid.org/) meeting a few weeks ago.\n\n\
   The important point is this approach is better:\n\n\
   1. We can use a flat array to store everything, rather than an unbalanced \
   tree. This makes everything smaller and faster for the computer to process.\n\
   2. The code is really simple. Being faster *and* simpler moves the [Pareto \
   efficiency frontier](https://en.wikipedia.org/wiki/Pareto_efficiency). \
   Ideas which do this are rare and truly golden.\n\
   3. You can implement lots of CRDTs like this. Yjs, Automerge, Sync9 and \
   others work. You can implement many list CRDTs in the same codebase. In my \
   reference-crdts codebase I have an implementation of both RGA (automerge) \
   and YATA (Yjs). They share most of their code (everything except this one \
   function) and their performance in this test is identical.\n\n\
   Theoretically this algorithm can slow down when there are concurrent \
   inserts in the same location in the document. But that's really rare in \
   practice - you almost always just insert right after the parent item.\n\n\
   Using this approach, my implementation of automerge's algorithm is about \
   10x faster than the real automerge. And it's 30x more memory-efficient:\n\n\
   | Test                              | Time taken | RAM usage |\n\
   |:--------------------------        | ----------:| ---------:|\n\
   | automerge (v1.0.0-preview2)       |  291s      | 880 MB    |\n\
   | **reference-crdts (automerge / Yjs)** |   31s      |  28 MB    |\n\
   | *Plain string edits in JS*        | 0.61s      | 0.1 MB    |\n\n\
   I wish I could attribute *all* of that difference to this sweet and simple \
   data structure. But a lot of the difference here is probably just \
   immutablejs gumming automerge up.\n\n\
   It's a lot faster than automerge:\n\n\
   ![Automerge is much slower than reference-crdts](ref_vs_am_perf.svg)\n\n\n\
   ## Death by 1000 scans\n\n\
   We're using a clean and fast core data abstraction now, but the \
   implementation is still not *fast*. There are two big performance \
   bottlenecks in this codebase we need to fix:\n\n\
   1. Finding the location to insert, and\n\
   2. Actually inserting into the array\n\n\
   (These lines are marked *(1)* and *(2)* in the code listing above).\n\n\
   To understand why this code is necessary, lets say we have a document, \
   which is a list of items.\n\n\
   ```javascript\n\
   state = [\n\
  \  { item: 'a', isDeleted: false, id: ['seph', 0], seq, parent: null },\n\
  \  { item: 'X', isDeleted: false, id, seq, parent: ['seph', 0] },\n\
  \  { item: 'b', isDeleted: true,  id, seq, parent: ['seph', 0] },\n\
  \  { item: 'c', isDeleted: false, id, seq, parent: ['seph', 1] },\n\
  \  ...\n\
   ]\n\
   ```\n\n\
   And some of those items might have been deleted. I've added an `isDeleted` \
   flag to mark which ones. (Unfortunately we can't just remove them from the \
   array because other inserts might depend on them. Drat! But that's a \
   problem for another day.)\n\n\
   Imagine the document has 150 000 array items in it, representing 100 000 \
   characters which haven't been deleted. If the user types an 'a' in the \
   middle of the document (at *document position* 50 000), what index does \
   that correspond to in our array? To find out, we need to scan through the \
   document (skipping deleted items) to figure out the right array location.\n\n\
   So if the user inserts at position 50 000, we'll probably have to linearly \
   scan past 75 000 items or something to find the insert position. Yikes!\n\n\
   And then when we actually insert, the code does this, which is double \
   yikes:\n\n\
   ```javascript\n\
   doc.content.splice(destIdx, 0, newItem)\n\
   ```\n\n\
   If the array currently has 150 000 items, javascript will need to move \
   every single item *after* the new item once space forward in the array. \
   This part happens in native code, but it's still probably slow when we're \
   moving so many items. (Aside: V8 is actually suspiciously fast at this \
   part, so maybe v8 isn't using an array internally to implement Arrays? Who \
   knows!)\n\n\
   But in general, inserting an item into a document with *n* items will take \
   about *n* steps. Wait, no - it's worse than that because deleted items \
   stick around. Inserting into a document where there have *ever been* *n* \
   items will take *n* steps. This algorithm is reasonably fast, but it gets \
   slower with every keystroke. Inserting *n* characters will take *O(n^2)*.\n\n\
   You can see this if we zoom in on the diagram above. There's a lot going on \
   here because Martin's editing position bounced around the document. But \
   there's a strong linear trend up and to the right, which is what we would \
   expect when inserts take *O(n)* time:\n\n\
   ![reference crdts implementation zoomed in](ref_perf3.svg)\n\n\
   And why this shape in particular? And why does performance get better near \
   the end? If we simply graph *where* each edit happened throughout the \
   editing trace, with the same bucketing and smoothing, the result is a very \
   familiar curve:\n\n\
   ![Edit position throughout document](inspos.svg)\n\n\
   It looks like the time spent applying changes is dominated by the time it \
   takes to scan through the document's array.\n\n\
   ## Changing the data structure\n\n\
   Can we fix this? Yes we can! And by \"we\", I mean Kevin fixed these \
   problems in Yjs. How did he manage that?\n\n\
   So remember, there are two problems to fix:\n\n\
   1. How do we find a specific insert position?\n\
   2. How do we efficiently insert content at that location?\n\n\
   Kevin solved the first problem by thinking about how humans actually edit \
   text documents. Usually while we're typing, we don't actually bounce around \
   a document very much. Rather than scanning the document each time an edit \
   happens, Yjs caches the last *(index, position)* pair where the user made \
   an edit. The next edit will probably be pretty close to the previous edit, \
   so Kevin just scans forwards or backwards from the last editing position. \
   This sounds a little bit dodgy to me - I mean, thats a big assumption to \
   make! What if edits happen randomly?! But people don't actually edit \
   documents randomly, so it works great in practice.\n\n\
   (What if two users are editing different parts of a document at the same \
   time? Yjs actually stores a whole set of cached locations, so there's \
   almost always a cached cursor location near each user no matter where \
   they're making changes in the document.)\n\n\
   Once Yjs finds the target insert location, it needs to insert efficiently, \
   without copying all the existing items. Yjs solves that by using a \
   bidirectional linked list instead of an array. So long as we have an insert \
   position, linked lists allow inserts in constant time.\n\n\
   Yjs does one more thing to improve performance. Humans usually type in runs \
   of characters. So when we type \"hello\" in a document, instead of \
   storing:\n\n\
   ```javascript\n\
   state = [\n\
  \  { item: 'h', isDeleted: false, id: ['seph', 0], seq, parent: null },\n\
  \  { item: 'e', isDeleted: false, id: ['seph', 1], seq, parent: ['seph', 0] },\n\
  \  { item: 'l', isDeleted: false, id: ['seph', 2], seq, parent: ['seph', 1] },\n\
  \  { item: 'l', isDeleted: false, id: ['seph', 3], seq, parent: ['seph', 2] },\n\
  \  { item: 'o', isDeleted: false, id: ['seph', 4], seq, parent: ['seph', 3] },\n\
   ]\n\
   ```\n\n\
   Yjs just stores:\n\n\
   ```javascript\n\
   state = [\n\
  \  { item: 'hello', isDeleted: false, id: ['seph', 0], seq, parent: null },\n\
   ]\n\
   ```\n\n\
   Finally those pesky paste events will be fast too!\n\n\
   This is the same information, just stored more compactly. Unfortunately we \
   can't collapse the whole document into a single item or something like that \
   using this trick. The algorithm can only collapse inserts when the IDs and \
   parents line up sequentially - but that happens whenever a user types a run \
   of characters without moving their cursor. And that happens a lot.\n\n\
   In this data set, using spans reduces the number of array entries by 14x. \
   (180k entries down to 12k).\n\n\
   How fast is it now? This blows me away - Yjs is 30x faster than my \
   reference-crdts implementation in this test. And it only uses about 10% as \
   much RAM. It's *300x faster than automerge!*.\n\n\
   | Test                              | Time taken | RAM usage |\n\
   |:--------------------------        | ----------:| ---------:|\n\
   | automerge (v1.0.0-preview2)       |  291s      | 880 MB    |\n\
   | reference-crdts (automerge / Yjs) |   31s      |  28 MB    |\n\
   | **Yjs (v13.5.5)**                 | 0.97s      | 3.3 MB    |\n\
   | *Plain string edits in JS*        | 0.61s      | 0.1 MB    |\n\n\
   Honestly I'm shocked and a little suspicious of how little ram Yjs uses in \
   this test. I'm sure there's some wizardry in V8 making this possible. It's \
   extremely impressive.\n\n\
   Kevin says he wrote and rewrote parts of Yjs 12 times in order to make this \
   code run so fast. If there was a programmer version of the speedrunning \
   community, they would adore Kevin. I can't even put Yjs on the same scale \
   as the other algorithms because it's so fast:\n\n\
   ![Yjs performance vs other algorithms](yjs_perf4.svg)\n\n\
   If we isolate Yjs, you can see it has *mostly* flat performance. Unlike the \
   other algorithms, it doesn't get slower over time, as the document grows:\n\n\
   ![Yjs performance isolated](yjs_perf5.svg)\n\n\
   But I have no idea what those spikes are near the end. They're pretty small \
   in *absolute* terms, but it's still weird! Maybe they happen when the user \
   moves their cursor around the document? Or when the user deletes chunks? I \
   have no idea.\n\n\
   This is neat, but the real question is: Can we go *even faster*? Honestly I \
   doubt I can make pure javascript run this test any faster than Kevin \
   managed here. But maybe.. just maybe we can be...\n\n\
   ## Faster than Javascript\n\n\
   When I told Kevin that I thought I could make a CRDT implementation that's \
   way faster than Yjs, he didn't believe me. He said Yjs was already so well \
   optimized, going a lot faster probably wasn't possible. \"Maybe a little \
   faster if you just port it to Rust. But not a lot faster! V8 is really fast \
   these days!!\"\n\n\
   But I knew something Kevin didn't know: I knew about memory fragmentation \
   and caches. Rust isn't just *faster*. It's also a lower level language, and \
   that gives us the tools we need to control allocations and memory layout.\n\n\
   > Kevin knows this now too, and he's working on \
   [Yrs](https://github.com/yjs/y-crdt) to see if he can claim the performance \
   crown back.\n\n\
   Imagine one of our document items in javascript:\n\n\
   ```javascript\n\
   var item = {\n\
  \  content: 'hello',\n\
  \  isDeleted: false,\n\
  \  id: ['seph', 10],\n\
  \  seq: 5,\n\
  \  parent: ['mike', 2]\n\
   }\n\
   ```\n\n\
   This object is actually a mess like this in memory:\n\n\
   ![javascript objects fragmented in memory](mem-frag.drawio.svg)\n\n\
   Bad news: *Your computer hates this.*\n\n\
   This is terrible because all the data is fragmented. It's all separated by \
   pointers.\n\n\
   > And yes, I know, V8 tries its hardest to prevent this sort of thing when \
   it can. But its not magic.\n\n\
   To arrange data like this, the computer has to allocate memory one by one \
   for each item. This is slow. Then the garbage collector needs extra data to \
   track all of those objects, which is also slow. Later we'll need to read \
   that data. To read it, your computer will often need to go fetch it from \
   main memory, which - you guessed it - is slow as well.\n\n\
   How slow are main memory reads? [At human \
   scale](https://gist.github.com/hellerbarde/2843375) each L1 cache read \
   takes 0.5 seconds. And a read from main memory takes close to 2 minutes! \
   This is the difference between a single heartbeat, and the time it takes to \
   brush your teeth.\n\n\
   Arranging memory like javascript does would be like writing a shopping \
   list. But instead of \"Cheese, Milk, Bread\", your list is actually a \
   scavenger hunt: \"Under the couch\", \"On top of the fridge\", and so on. \
   Under the couch is a little note mentioning you need toothpaste. Needless \
   to say, this makes doing the grocery shopping a lot of work.\n\n\
   To go faster, we need to squish all the data together so the computer can \
   fetch more information with each read of main memory. (We want a single \
   read of my grocery list to tell us everything we need to know). Linked \
   lists are rarely used in the real world for exactly this reason - *memory \
   fragmentation ruins performance*. I also want to move away from linked \
   lists because the user *does* sometimes hop around the document, which in \
   Yjs has a linear performance cost. Thats probably not a big deal in text \
   editing, but I want this code to be fast in other use cases too. I don't \
   want the program to *ever* need those slow scans.\n\n\
   We can't fix this in javascript. The problem with fancy data structures in \
   javascript is that you end up needing a lot of exotic objects (like fixed \
   size arrays). All those extra objects make fragmentation worse, so as a \
   result of all your work, your programs often end up running slower anyway. \
   This is the same limitation immutablejs has, and why its performance hasn't \
   improved much in the decade since it was released. The V8 optimizer is very \
   clever, but it's not magic and clever tricks only get us so far.\n\n\
   But we're not limited to javascript. Even when making webpages, we have \
   WebAssembly these days. We can code this up in *anything*.\n\n\
   To see how fast we can *really* go, I've been quietly building a CRDT \
   implementation in rust called [Diamond \
   types](https://github.com/josephg/diamond-types). Diamond is almost \
   identical to Yjs, but it uses a [range \
   tree](https://en.wikipedia.org/wiki/Range_tree) instead of a linked list \
   internally to store all of the items.\n\n\
   Under the hood, my range tree is just a slightly modified b-tree. But \
   usually when people talk about b-trees they mean a \
   [BTreeMap](https://doc.rust-lang.org/std/collections/struct.BTreeMap.html). \
   Thats not what I'm doing here. Instead of storing keys, each internal node \
   of the b-tree stores the total number of characters (recursively) in that \
   item's children. So we can look up any item in the document by character \
   position, or insert or delete anywhere in the document in *log(n)* time.\n\n\
   This example shows the tree storing a document which currently has 1000 \
   characters:\n\n\
   ![b-tree diagram](btree.drawio.svg)\n\n\
   > This is a range tree, right? The [wikipedia article on range \
   trees](https://en.wikipedia.org/wiki/Range_tree) is a pretty weak \
   description of what I'm doing here.\n\n\
   This solves both of our linear scanning problems from earlier:\n\n\
   - When we want to find the item at position 200, we can just traverse \
   across and down the tree. In the example above, the item with position 350 \
   must be in the middle leaf node here. Trees are very tidy - we can store \
   Martin's editing trace in just 3 levels in our tree, which means in this \
   benchmark we can find any item in about 3 reads from main memory. In \
   practice, most of these reads will already be in your CPU's cache.\n\
   - Updating the tree is fast too. We update a leaf, then update the \
   character counts at its parent, and its parent, all the way up to the root. \
   So again, after 3 or so steps we're done. Much better than shuffling \
   everything in a javascript array.\n\n\
   We never merge edits from remote peers in this test, but I made that fast \
   too anyway. When merging remote edits we also need to find items by their \
   ID (eg *['seph', 100]*). Diamond has little index to search the b-tree by \
   ID. That codepath doesn't get benchmarked here though. It's fast but for \
   now you'll have to take my word for it.\n\n\
   I'm not using Yjs's trick of caching the last edit location - at least not \
   yet. It might help. I just haven't tried it yet.\n\n\
   Rust gives us total control over the memory layout, so we can pack \
   everything in tightly. Unlike in the diagram, each leaf node in my b-tree \
   stores a block of 32 entries, packed in a fixed size array in memory. \
   Inserting with a structure like this results in a little bit of memcpy-ing, \
   but a little bit of memcpy is fine. Memcpy is always faster than I think it \
   will be - CPUs can copy several bytes per clock cycle. Its not the epic \
   hunt of a main memory lookup.\n\n\
   And why 32 entries? I ran this benchmark with a bunch of different bucket \
   sizes and 32 worked well. I have no idea why that worked out to be the \
   best.\n\n\
   Speaking of fast, how fast does it go?\n\n\
   If we [compile this code to \
   webassembly](https://github.com/josephg/diamond-js) and drive it from \
   javascript like in the other tests, we can now process the whole editing \
   trace in 193 milliseconds. Thats 5x faster than Yjs. And remarkably 3x \
   faster than our baseline test editing a native javascript string, despite \
   doing all the work to support collaborative editing!\n\n\
   Javascript and WASM is now a bottleneck. If we skip javascript and run the \
   benchmark [directly in \
   rust](https://github.com/josephg/diamond-types/blob/42a8bc8fb4d44671147ccaf341eee18d77b2d532/benches/yjs.rs), \
   we can process all 260k edits in this editing trace in just *56 \
   milliseconds*. That's over 5000x faster than where we started with \
   automerge. It can process 4.6 *million* operations every second.\n\n\
   | Test                              | Time taken | RAM usage |\n\
   |:--------------------------        | ----------:| ---------:|\n\
   | automerge (v1.0.0-preview2)       |  291s      | 880 MB    |\n\
   | reference-crdts (automerge / Yjs) |   31s      |  28 MB    |\n\
   | Yjs (v13.5.5)                     | 0.97s      | 3.3 MB    |\n\
   | *Plain string edits in JS*        | 0.61s      | 0.1 MB    |\n\
   | **Diamond (wasm via nodejs)**     | 0.19s      | ???       |\n\
   | **Diamond (native)**              | 0.056s     | 1.1 MB    |\n\n\
   Performance is smooth as butter. A b-tree doesn't care where edits happen. \
   This system is uniformly fast across the whole document. Rust doesn't need \
   a garbage collector to track memory allocations, so there's no mysterious \
   GC spikes. And because memory is so tightly packed, processing this entire \
   data set (all 260 000) only results in 1394 calls to malloc.\n\n\
   ![rust implementation in wasm vs Yjs](rust_perf6.svg)\n\n\
   Oh, what a pity. Its so fast you can barely see it next to yjs \
   (*fleexxxx*). Lets zoom in a bit there and bask in that flat line:\n\n\
   ![rust implementation in wasm](rust_perf7.svg)\n\n\
   Well, a nearly flat line.\n\n\
   And remember, this chart shows the *slow* version. This chart is generated \
   from javascript, calling into rust through WASM. If I run this benchmark \
   natively its another ~4x faster again.\n\n\
   Why is WASM 4x slower than native execution? Are javascript calls to the \
   WASM VM really that slow? Does LLVM optimize native x86 code better? Or do \
   WASM's memory bounds checks slow it down? I'm so curious!\n\n\n\
   ## Struct of arrays or Array of structs?\n\n\
   This implementation has another small, important change - and I'm not sure \
   if I like it.\n\n\
   In rust I'm actually doing something like this:\n\n\
   ```javascript\n\
   doc = {\n\
  \  textContent: RopeyRope { 'hello' },\n\n\
  \  clients: ['seph', 'mike'],\n\n\
  \  items: BTree {[\n\
  \    // Note: No string content!\n\
  \    { len:  5, id: [0, 0], seq, parent: ROOT },\n\
  \    { len: -5, id: [1, 0], seq, parent: [0, 0] }, // negative len means the \
   content was deleted\n\
  \    ...\n\
  \  ]},\n\
   }\n\
   ```\n\n\
   Notice the document's text content doesn't live in the list of items \
   anymore. Now it's in a separate data structure. I'm using a rust library \
   for this called [Ropey](https://crates.io/crates/ropey). Ropey implements \
   *another* b-tree to efficiently manage just the document's text content.\n\n\
   This isn't universally a win. We have unfortunately arrived at the Land of \
   Uncomfortable Engineering Tradeoffs:\n\n\
   - Ropey can to do text-specific byte packing. So with ropey, we use less RAM.\n\
   - When inserting we need to update 2 data structures instead of 1. This \
   makes everything more than twice as slow, and it makes the wasm bundle \
   twice as big  (60kb -> 120kb).\n\
   - For lots of use cases we'll end up storing the document content somewhere \
   else anyway. For example, if you hook this CRDT up to VS Code, the editor \
   will keep a copy of the document at all times anyway. So there's no need to \
   store the document in my CRDT structures as well, at all. This \
   implementation approach makes it easy to just turn that part of the code \
   off.\n\n\
   So I'm still not sure whether I like this approach.\n\n\
   But regardless, my CRDT implementation is so fast at this point that most \
   of the algorithm's time is spent updating the document contents in ropey. \
   Ropey on its own takes 29ms to process this editing trace. What happens if \
   I just ... turn ropey off? How fast can this puppy can really go?\n\n\
   | Test                              | Time taken | RAM usage | Data \
   structure |\n\
   |:--------------------------        | ----------:| \
   ---------:|:---------------|\n\
   | automerge (v1.0.0-preview2)       |  291s      | 880 MB    | Naive \
   tree     |\n\
   | reference-crdts (automerge / Yjs) |   31s      |  28 MB    | \
   Array          |\n\
   | Yjs (v13.5.5)                     | 0.97s      | 3.3 MB    | Linked \
   list    |\n\
   | *Plain string edits in JS*        | 0.61s      | 0.1 MB    | \
   *(none)*       |\n\
   | Diamond (wasm via nodejs)         | 0.20s      | ???       | \
   B-Tree         |\n\
   | Diamond (native)                  | 0.056s     | 1.1 MB    | \
   B-Tree         |\n\
   | *Ropey (rust) baseline*           | 0.029s     | 0.2 MB    | \
   *(none)*       |\n\
   | **Diamond (native, no doc content)** | 0.023s  | 0.96 MB   | \
   B-Tree         |\n\n\
   Boom. This is kind of useless, but it's now 14000x faster than automerge. \
   We're processing 260 000 operations in 23ms. Thats 11 million operations \
   per second. I could saturate my home internet connection with keystrokes \
   and I'd still have CPU to spare.\n\n\
   ---\n\n\
   We can calculate the average speed each algorithm processes edits:\n\n\
   ![](totals.svg)\n\n\
   But these numbers are misleading. Remember, automerge and ref-crdts aren't \
   steady. They're fast at first, then slow down as the document grows. Even \
   though automerge can process about 900 edits per second *on average* (which \
   is fast enough that users won't notice), the slowest edit during this \
   benchmark run stalled V8 for a full 1.8 seconds.\n\n\
   We can put everything in a single, pretty chart if I use a log scale. It's \
   remarkable how tidy this looks:\n\n\
   ![all data in one chart](all_perf.svg)\n\n\
   > Huh - look at the bottom two lines. The jitteryness of yjs and diamond \
   mirror each other. Periods when yjs gets slower, diamond gets faster. I \
   wonder whats going on there!\n\n\
   But log scales are junk food for your intuition. On a linear scale the data \
   looks like this:\n\n\
   ![all data in one chart, with a linear scale](all_perf_linear.svg)\n\n\
   That, my friends, is how you make the computer do a lot less work.\n\n\n\
   ## Conclusion\n\n\
   That silly academic paper I read all those years ago says some CRDTs and OT \
   algorithms are slow. And everyone believed the paper, because it was \
   Published Science. But the paper was wrong. As I've shown, we *can* make \
   CRDTs fast. We can make them *crazy fast* if we get creative with our \
   implementation strategies. With the right approach, we can make CRDTs so \
   fast that we can compete with the performance of native strings. The \
   performance numbers in that paper weren't just wrong. They were \"a \
   billionaire guessing a banana costs $1000\" kind of wrong.\n\n\
   But you know what? I sort of appreciate that paper now. Their mistake is \
   ok. It's *human*. I used to feel inadequate around academics - maybe I'll \
   never be that smart! But this whole thing made me realise something \
   obvious: Scientists aren't gods, sent from the heavens with the gift of \
   Truth. No, they're beautiful, flawed *people* just like the rest of us \
   mooks. Great at whatever we obsess over, but kind of middling everywhere \
   else. I can optimize code pretty well, but I still get zucchini and \
   cucumber mixed up. And, no matter the teasing I get from my friends, thats \
   ok.\n\n\
   A decade ago Google Wave really needed a good quality list CRDT. I got \
   super excited when the papers for CRDTs started to emerge. \
   [LOGOOT](https://hal.inria.fr/inria-00432368/document) and \
   [WOOT](https://hal.inria.fr/inria-00445975/document) seemed like a big \
   deal! But that excitement died when I realised the algorithms were too slow \
   and inefficient to be practically useful. And I made a big mistake - I \
   assumed if the academics couldn't make them fast, nobody could.\n\n\
   But sometimes the best work comes out of a collaboration between people \
   with different skills. I'm terrible at academic papers, I'm pretty good at \
   making code run fast. And yet here, in my own field, I didn't even try to \
   help. The researchers were doing their part to make P2P collaborative \
   editing work. And I just thumbed my nose at them all and kept working on \
   Operational Transform. If I helped out, maybe we would have had fast, \
   workable CRDTs for text editing a decade ago. Oops! It turned out \
   collaborative editing needed a collaboration between all of us. How ironic! \
   Who could have guessed?!\n\n\
   Well, it took a decade, some hard work and some great ideas from a bunch of \
   clever folks. The binary encoding system Martin invented for Automerge is \
   brilliant. The system of avoiding UUIDs by using incrementing (agent id, \
   sequence) tuples is genius. I have no idea who came up with that, but I \
   love it. And of course, Kevin's list representation + insertion approach I \
   describe here makes everything so much faster and simpler. I bet 100 smart \
   people must have walked right past that idea over the last decade without \
   any of them noticing it. I doubt I would have thought of it either. My \
   contribution is using run-length encoded b-trees and clever indexing. And \
   showing Kevin's fast list representation can be adapted to any CRDT \
   algorithm. I don't think anyone noticed that before.\n\n\
   And now, after a decade of waiting, we finally figured out how to make \
   fast, lightweight list CRDT implementations. Practical decentralized \
   realtime collaborative editing? We're coming for you next.\n\n\n\
   ## Appendix A: I want to use a CRDT for my application. What should I do?\n\n\
   If you're building a document based collaborative application today, you \
   should use [Yjs](https://github.com/yjs/yjs). Yjs has solid performance, \
   low memory usage and great support. If you want help implementing Yjs in \
   your application, Kevin Jahns sometimes accepts money in exchange for help \
   integrating Yjs into various applications. He uses this to fund working on \
   Yjs (and adjacent work) full time. Yjs already runs fast and soon it should \
   become even faster.\n\n\
   The automerge team is also fantastic. I've had some great conversations \
   with them about these issues. They're making performance the #1 issue of \
   2021 and they're planning on using a lot of these tricks to make automerge \
   fast. It might already be much faster by the time you're reading this.\n\n\
   Diamond is *really* fast, but there's a lot of work before I have feature \
   parity with Yjs and Automerge. There is a lot more that goes into a good \
   CRDT library than operation speed. CRDT libraries also need to support \
   binary encoding, network protocols, non-list data structures, presence \
   (cursor positions), editor bindings and so on. At the time of writing, \
   diamond does almost none of this.\n\n\
   If you want database semantics instead of document semantics, as far as I \
   know nobody has done this well on top of CRDTs yet. You can use \
   [ShareDB](https://github.com/share/sharedb/), which uses OT. I wrote \
   ShareDB years ago, and it's well used, well maintained and battle tested.\n\n\
   Looking forward, I'm excited for \
   [Redwood](https://github.com/redwood/redwood) - which supports P2P editing \
   and has planned full CRDT support.\n\n\n\
   ## Appending B: Lies, damned lies and benchmarks\n\n\
   Is this for real? Yes. But performance is complicated and I'm not telling \
   the full picture here.\n\n\
   First, if you want to play with any of the benchmarks I ran yourself, you \
   can. But everything is a bit of a mess.\n\n\
   The benchmark code for the JS plain string editing baseline, Yjs, automerge \
   and reference-crdts tests is all in [this github \
   gist](https://gist.github.com/josephg/13efc1444660c07870fcbd0b3e917638). \
   It's a mess; but messy code is better than missing code.\n\n\
   You'll also need `automerge-paper.json.gz` from \
   [josephg/crdt-benchmarks](https://github.com/josephg/crdt-benchmarks) in \
   order to run most of these tests. The reference-crdts benchmark depends on \
   [crdts.ts from josephg/reference-crdts, at this \
   version](https://github.com/josephg/reference-crdts/tree/fed747255df9d457e11f36575de555b39f07e909).\n\n\
   Diamond's benchmarks come from [josephg/diamond-types, at this \
   version](https://github.com/josephg/diamond-types/tree/42a8bc8fb4d44671147ccaf341eee18d77b2d532). \
   Benchmark by running ` RUSTFLAGS='-C target-cpu=native' cargo criterion \
   yjs`. The inline rope structure updates can be enabled or disabled by \
   editing [the constant at the top of \
   src/list/doc.rs](https://github.com/josephg/diamond-types/blob/42a8bc8fb4d44671147ccaf341eee18d77b2d532/src/list/doc.rs#L15). \
   You can look at memory statistics by running `cargo run --release \
   --features memusage --example stats`.\n\n\
   Diamond is compiled to wasm using [this \
   wrapper](https://github.com/josephg/diamond-js/tree/6e8a95670b651c0aaa7701a1a763778d3a486b0c), \
   hardcoded to point to a local copy of diamond-types from git. The wasm \
   bundle is optimized with wasm-opt.\n\n\
   The charts were made on \
   [ObservableHQ](https://observablehq.com/@josephg/crdt-algorithm-performance-benchmarks).\n\n\n\
   ### Are Automerge and Yjs doing the same thing?\n\n\
   Throughout this post I've been comparing the performance of implementations \
   of RGA (automerge) and YATA (Yjs + my rust implementation) \
   interchangeably.\n\n\
   Doing this rests on the assumption that the concurrent merging behaviour \
   for YATA and RGA are basically the same, and that you can swap between CRDT \
   behaviour without changing your implementation, or your implementation \
   performance. This is a novel idea that I think nobody has looked at \
   before.\n\n\
   I feel confident in this claim because I demonstrated it in my [reference \
   CRDT implementation](https://github.com/josephg/reference-crdts), which has \
   identical performance (and an almost identical codepath) when using Yjs or \
   automerge's behaviour. There might be some performance differences with \
   conflict-heavy editing traces - but that's extremely rare in practice.\n\n\
   I'm also confident you could modify Yjs to implement RGA's behaviour if you \
   wanted to, without changing Yjs's performance. You would just need to:\n\n\
   - Change Yjs's *integrate* method (or make an alternative) which used \
   slightly different logic for concurrent edits\n\
   - Store *seq* instead of *originRight* in each *Item*\n\
   - Store *maxSeq* in the document, and keep it up to date and\n\
   - Change Yjs's binary encoding format.\n\n\
   I talked to Kevin about this, and he doesn't see any point in adding RGA \
   support into his library. It's not something anybody actually asks for. And \
   RGA can have weird \
   [interleaving](https://www.cl.cam.ac.uk/~arb33/papers/KleppmannEtAl-InterleavingAnomalies-PaPoC2019.pdf) \
   when prepending items.\n\n\
   For diamond, I make my code accept a type parameter for switching between \
   Yjs and automerge's behaviour. I'm not sure if I want to. Kevin is probably \
   right - I don't think this is something people ask for.\n\n\
   ---\n\n\
   Well, there is one way in which Yjs has a definite edge over automerge: Yjs \
   doesn't record *when* each item in a document has been deleted. Only \
   whether each item has been deleted or not. This has some weird \
   implications:\n\n\
   - Storing when each delete happened has a weirdly large impact on memory \
   usage and on-disk storage size. Adding this data doubles diamond's memory \
   usage from 1.12mb to 2.34mb, and makes the system about 5% slower.\n\
   - Yjs doesn't store enough information to implement per-keystroke editing \
   replays or other fancy stuff like that. (Maybe thats what people want? Is \
   it weird to have every errant keystroke recorded?)\n\
   - Yjs needs to encode information about which items have been deleted into \
   the *version* field. In diamond, versions are tens of bytes. In yjs, \
   versions are ~4kb. And they grow over time as the document grows. Kevin \
   assures me that this information is basically always small in practice. He \
   might be right but this still makes me weirdly nervous.\n\n\
   For now, the master branch of diamond includes temporal deletes. But all \
   benchmarks in this blog post use a [yjs-style branch of \
   diamond-types](https://github.com/josephg/diamond-types/tree/yjs-style), \
   which matches how Yjs works instead. This makes for a fairer comparison \
   with yjs, but diamond 1.0 might have a slightly different performance \
   profile. (There's plenty of puns here about diamond not being polished yet, \
   but I'm not sharp enough for those right now.)\n\n\n\
   ### These benchmarks measure the wrong thing\n\n\
   This post only measures the time taken to replay a local editing trace. And \
   I'm measuring the resulting RAM usage. Arguably accepting incoming changes \
   from the user only needs to happen fast *enough*. Fingers simply don't type \
   very fast. Once a CRDT can handle any local user edit in under about 1ms, \
   going faster probably doesn't matter much. (And automerge usually performs \
   that well already, barring some unlucky GC pauses.)\n\n\
   The *actually important* metrics are:\n\n\
   - How many bytes does a document take on disk or over the network\n\
   - How much time does the document take to save and load\n\
   - How much time it takes to update a document stored at rest (more below)\n\n\
   The editing trace I'm using here also only has a single user making edits. \
   There could be pathological performance cases lurking in the shadows when \
   users make concurrent edits.\n\n\
   I did it this way because I haven't implemented a binary format in my \
   reference-crdts implementation or diamond yet. If I did, I'd probably copy \
   Yjs & automerge's binary formats because they're so compact. So I expect \
   the resulting binary size would be similar between all of these \
   implementations, except for delete operations. Performance for loading and \
   saving will probably approximately mirror the benchmarks I showed above. \
   Maybe. Or maybe I'm wrong. I've been wrong before. It would be fun to find \
   out.\n\n\
   ---\n\n\
   There's one other performance measure I think nobody is taking seriously \
   enough at the moment. And that is, how we update a document at rest (in a \
   database). Most applications aren't collaborative text editors. Usually \
   applications are actually interacting with databases full of tiny objects. \
   Each of those objects is very rarely written to.\n\n\
   If you want to update a single object in a database using Yjs or automerge \
   today you need to:\n\n\
   1. Load the whole document into RAM\n\
   2. Make your change\n\
   3. Save the whole document back to disk again\n\n\
   This is going to be awfully slow. There are better approaches for this - \
   but as far as I know, nobody is working on this at all. We could use your \
   help!\n\n\
   > Edit: Kevin says you can adapt Yjs's providers to implement this in a \
   reasonable way. I'd love to see that in action.\n\n\
   ---\n\n\
   There's another approach to making CRDTs fast, which I haven't mentioned \
   here at all and that is *pruning*. By default, list CRDTs like these only \
   ever grow over time (since we have to keep tombstones for all deleted \
   items). A lot of the performance and memory cost of CRDTs comes from \
   loading, storing and searching that growing data set. There are some \
   approaches which solve this problem by finding ways to shed some of this \
   data entirely. For example, Yjs's GC algorithm, or \
   [Antimatter](https://braid.org/antimatter). That said, git repositories \
   only ever grow over time and nobody seems mind too much. Maybe it doesn't \
   matter so long as the underlying system is fast enough?\n\n\
   But pruning is orthogonal to everything I've listed above. Any good pruning \
   system should also work with all of the algorithms I've talked about \
   here.\n\n\n\
   ### Each step in this journey changes too many variables\n\n\
   Each step in this optimization journey involves changes to multiple \
   variables and I'm not isolating those changes. For example, moving from \
   automerge to my reference-crdts implementation changed:\n\n\
   - The core data structure (tree to list)\n\
   - Removed immutablejs\n\
   - Removed automerge's frontend / backend protocol. And all those \
   Uint8Arrays that pop up throughout automerge for whatever reason are gone \
   too, obviously.\n\
   - The javascript style is totally different. (FP javascript -> imperative)\n\n\
   We got 10x performance from all this. But I'm only guessing how that 10x \
   speedup should be distributed amongst all those changes.\n\n\
   The jump from reference-crdts to Yjs, and from Yjs to diamond are similarly \
   monolithic. How much of the speed difference between diamond and Yjs has \
   nothing to do with memory layout, and everything to do with LLVM's \
   optimizer?\n\n\
   The fact that automerge-rs isn't faster than automerge gives me some \
   confidence that diamond's performance isn't just thanks to rust. But I \
   honestly don't know.\n\n\
   So, yes. This is a reasonable criticism of my approach. If this problem \
   bothers you, I'd *love* for someone to pull apart each of the performance \
   differences between implementations I show here and tease apart a more \
   detailed breakdown. I'd read the heck out of that. I love benchmarking \
   stories. That's normal, right?\n\n\n\
   ## Appendix C: I still don't get it - why is automerge's javascript so \
   slow?\n\n\
   Because it's not trying to be fast. Look at this code [from \
   automerge](https://github.com/automerge/automerge/blob/d2e7ca2e141de0a72f540ddd738907bcde234183/backend/op_set.js#L649-L659):\n\n\
   ```javascript\n\
   function lamportCompare(op1, op2) {\n\
  \  return opIdCompare(op1.get('opId'), op2.get('opId'))\n\
   }\n\n\
   function insertionsAfter(opSet, objectId, parentId, childId) {\n\
  \  let childKey = null\n\
  \  if (childId) childKey = Map({opId: childId})\n\n\
  \  return opSet\n\
  \    .getIn(['byObject', objectId, '_following', parentId], List())\n\
  \    .filter(op => op.get('insert') && (!childKey || lamportCompare(op, \
   childKey) < 0))\n\
  \    .sort(lamportCompare)\n\
  \    .reverse() // descending order\n\
  \    .map(op => op.get('opId'))\n\
   }\n\
   ```\n\n\
   This is called on each insert, to figure out how the children of an item \
   should be sorted. I don't know how hot it is, but there are *so many \
   things* slow about this:\n\n\
   - I can spot 7 allocations in this function. (Though the 2 closures should \
   be hoisted). (Can you find them all?)\n\
   - The items are already sorted reverse-lamportCompare before this method is \
   called. Sorting an anti-sorted list is the slowest way to sort anything. \
   Rather than sorting, then reverse()'ing, this code should just invert the \
   arguments in `lamportCompare` (or negate the return value).\n\
   - The goal is to insert a new item into an already sorted list. You can do \
   that much faster with a for loop.\n\
   - This code wraps childId into an immutablejs Map, just so the argument \
   matches `lamportCompare` - which then unwraps it again. Stop - I'm dying!\n\n\
   But in practice this code is going to be replaced by WASM calls through to \
   [automerge-rs](https://github.com/automerge/automerge-rs). Maybe it already \
   has been replaced with automerge-rs by the time you're reading this! So it \
   doesn't matter. Try not to think about it. Definitely don't submit any PRs \
   to fix all the low hanging fruit. *twitch.*\n\n\n\
   ## Acknowledgements\n\n\
   This post is part of the [Braid project](https://braid.org/) and funded by \
   the [Invisible College](https://invisible.college/). If this is the sort of \
   work you want to contribute towards, get in touch. We're hiring.\n\n\
   Thankyou to everyone who gave feedback before this post went live.\n\n\
   And special thanks to Martin Kleppmann and Kevin Jahns for their work on \
   Automerge and Yjs. Diamond stands on the shoulders of giants.\n\n\
   [Comments on Hacker News](https://news.ycombinator.com/item?id=28017204)\n\n\
   <footer>\n\n\
   [2021 Seph Gentle](https://josephg.com/)\n\n\
   [https://github.com/josephg/](https://github.com/josephg/)\n\n\
   </footer>"
