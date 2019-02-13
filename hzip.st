#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.
outfileExists := false.
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

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

freqtable := Array new: 257.

options := Smalltalk arguments at: 1.
infilename := Smalltalk arguments at: 2.
(Smalltalk arguments size < 3) ifFalse: [
	outfilename := Smalltalk arguments at: 3.
	outfile := openfile value: outfilename value: FileStream write.
	outfileExists := true.
].
infile := openfile value: infilename value: FileStream read.

"Frequency table array obj"

"Create 1 tree for each ASCII char (256)"

freqtable at: 257 put: 1.

[infile atEnd] whileFalse:[
    	| inputChar ind |
	ind := infile nextByte.
	(freqtable at: ind) isNil 
	    ifTrue: [freqtable at: ind put: 0].
	freqtable at: ind put: (freqtable at: ind) + 1.
].

"Read in file (using bitstream obj?) and save frequency in new frequency table (initialize w/ ASCII)"


"Create priority queue and load from frequency table, if count > 0"
priorQ := SortedCollection new.

"|i|
i := 0."
1 to: 257 do: [:i |
	((freqtable at: i) notNil) ifTrue:[
		stdout << 'freqtable at i:' << (freqtable at: i).
		char := i asCharacter.
		priorQ add: (Leaf new: char count: (freqtable at: i)). 
	].
].

"Huffman tree- result is tree in T3" 
[priorQ size == 1] whileFalse:[
	| T1 T2 T3 |
	T1 := priorQ removeFirst.
	T2 := priorQ removeFirst.
	T3 := (Tree new: T1 char count: (T1 count + T2 count) left: T1 right: T2).
	priorQ add: T3.  
].

root := priorQ removeFirst.

"Encoding of each char using DFS, bit string (left is 0, right is 1)"
encodedArray := Array new: 257.
(root = nil) ifFalse: [
	root inorder: [:char :bitstring| encodedArray at: (char asInteger) put: bitstring] prefix: ''.
	1 to: 257 do: [:i |
        	((freqtable at: i) notNil) ifTrue:[ stdout << 'encodedArray at' << (i asCharacter) << ':' << (encodedArray at: i) << nl.]
		.]
	"root inorder: [:char :string| stdout << '[' << char << ']=' << string << nl.] prefix: ''."
].

infile close.
(outfileExists) ifTrue: [outfile close].

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
	
