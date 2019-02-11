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

].

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

quit := [:status| ObjectMemory quit: status ].

openfile := [:filename :mode |
    |file|
    [
        file := FileStream open: filename mode: mode
    ] on: SystemExceptions.FileError do: [signal|
        stderr << 'error'
        quit value: 1.
    ].
    file.
].

infilename := Smalltalk arguments at: 1.
outfilename := Smalltalk arguments at: 2.
infile := openfile value: infilename value: FileStream read.
outfile := openfile value: outfilename value: FileStream write.


"Frequency table array obj"

freqtable := Array new: 257.
"Create 1 tree for each ASCII char (256)"
"|i|
i := 0
0 to: 255: do: [freqtable at:i put:new Tree i 0]"

eof := Leaf new: 'EOF' count: 1.
freqtable at: 256 put: 1.

[infile atEnd] whileFalse:[
    	| inputChar ind |
	inputChar := infile nextByte.
	ind := inputChar.
	(freqtable at: ind == 0) ifTrue:[
		freqtable at: ind put: Leaf new: inputChar count: 1.
	] ifFalse:[
		(freqtable at: ind) setChar: inputChar andCount: (freqtable at: ind count)+1.
	].
	stdout << 'index is' << ind << 'count is' << freqtable at: ind count.
].

"Read in file (using bitstream obj?) and save frequency in new frequency table (initialize w/ ASCII)"


"Create priority queue and load from frequency table, if count > 0"
priorQ := SortedCollection new.
priorQ sortBlock:[:a :b | a <= b].

|i|
i := 0
0 to: 256 do:[
	(freqtable at: i ~= 0) ifTrue:[
		priorQ add:(freqtable at: i). 
	].
].

"Huffman tree- result is tree in T3" 
[priorQ isEmpty] whileFalse:[
	| T1 T2 T3 |
	T1 := priorQ removeFirst.
	T2 := priorQ removeFirst.
	T3 := Tree new: T1 char count:(T1 count + T2 count) left: T1 right: T2.
	priorQ add: T3.  
].

"Encoding of each char using DFS, bit string (left is 0, right is 1)"
encodedArray := Array new: 257.
(T3 = nil) ifFalse: [
	T3 inorder: [:char :bitstring| encodedArray at: char put: bitstring].
].
  "char has to be the character at the leaf of the bitstring"

infile close.
outfile close.
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
	
