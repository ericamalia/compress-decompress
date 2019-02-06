#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"Leaf obj"
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]

"Tree obj"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]

"Frequency table array obj"

"Create 1 tree for each ASCII char (256)"

"Read in file (using bitstream obj?) and save frequency in new frequency table (initialize w/ ASCII)"

"order frequency table from lowest-highest freq?"

"Create priority queue and load from frequency table"

"Until 1 tree in the queue, 
	pop two smallest from queue (T1, T2)
	make new tree T3 with frequency = frq(T1) + frq(T2)
	insert T3 into queue (at the end or in order?)" 

"Encoding of each char using DFS, bit string (left is 0, right is 1)"

"make encoding table?- char, freq, encoding string"
"if -t, write out/print? encoding table sorted by character"
"else write out encoding table using post-order traversal one bit at a time"

"compress input file (write out encoded version of each byte) -> output file"


"decompress input file -> output"
	"reconstruct huffman decoding tree, read 1 bit"
	"if 0, read next 8 bits and reconstruct val, check for EOF, push byte onto stack"
	"if 1, pop the 1 subtree from stack, then pop 0 subtree form stack, 
	push new tree of both of these children"
	"continue until reaching last bit/stack is empty"
	"loop over rest of input file to reconstruct"
	"read bit until reaching leaf, write out corresponding byte & reset pointer back to root"
	

