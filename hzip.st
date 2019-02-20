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
   
   postorder: visitor[
	| ascii |
	stdout << '0'.
	
	(char = (0 asCharacter)) ifTrue: [
		stdout << '0'.
		1 to: 8 do: [:i|
			stdout << '0'.
		].
	].

	(char = (257 asCharacter)) ifTrue:[
		1 to: 8 do: [:i|
			stdout << '0'.
		].
		stdout << '1'.
	] ifFalse: [
		ascii := ((char asciiValue) radix: 2).
		1 to: (10 - (ascii size)) do: [:i |
			stdout << '0'.
		].
		"3 to: (ascii size) do:[:i|
			stdout << (ascii at: i).
		]."
		stdout << char. 
		"print char in bits"
		].
   ]
  
   postorder: visitor file: outfile[	
	|ascii|
	outfile nextPut: $0.
	"stdout << char << '~~'."
	
	"need to add a special case for if char is 0"

	(char = (0 asCharacter)) ifTrue: [
		outfile nextPut: $0.
		1 to: 8 do: [:i|
			outfile nextPut: $0.
		].
	].

	(char = (257 asCharacter)) ifTrue:[
		stdout << 'putting 256 as binary in outfile' << Character nl.
		1 to: 8 do: [:i|
			outfile nextPut: $0.
		].
		outfile nextPut: $1.
	 ]ifFalse:[
		ascii := ((char asciiValue) radix: 2).
		1 to: (10 - (ascii size)) do: [:i |
			outfile nextPut: $0.
		].
		3 to: (ascii size) do:[:i|
			outfile nextPut: (ascii at: i).
		].	
	].
   ]
]

"Tree obj"
Leaf subclass: Tree [
   |left right|
   left [ ^ left]
   right [ ^ right]
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

   postorder: visitor[	
	left postorder: visitor.
	right postorder: visitor.
	
	stdout << '1'.
	"traverse tree keeping track of visitor"
	"visitor is either a leaf(use visitor in leaf class?)
	 or internal node, so we check and if leaf print the char if interior print 1"
   ]

   
   postorder: visitor file: outfile[	
	left postorder: visitor file: outfile.
	right postorder: visitor file: outfile.
	outfile nextPut: $1. 
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
"initialize freqtable"
freqtable := Array new: 257.

"get infile and optional outfile"
option := Smalltalk arguments at: 1.
infilename := Smalltalk arguments at: 2.
(Smalltalk arguments size < 3) ifFalse: [
	outfilename := Smalltalk arguments at: 3.
	outfile := openfile value: outfilename value: FileStream write.
	outfileExists := true.
].
infile := openfile value: infilename value: FileStream read.


(option = '-u') ifTrue: [
	|atEOF endOfHuffman stack bit byte nextBit aLeaf root2 leftTree rightTree newTree|
	stack := OrderedCollection new.
	endOfHuffman := false.
	atEOF := false.
	[endOfHuffman] whileFalse: [
		bit := infile next.
		byte := 16r00.
		(bit = $0) ifTrue: [
			0 to: 7 do: [:i|
			nextBit := ((infile next) asInteger) - 48.
				nextBit := nextBit bitShift: (7-i).
				byte := byte bitOr: nextBit.
				].
			(byte = 0) ifTrue: [
				| extraBit|
				extraBit := infile next.
				(extraBit = $1) ifTrue: [
					byte := byte + 256.
				].
			].
			aLeaf := Leaf new: (byte asCharacter) count: 1.
			stack add: aLeaf.
		] ifFalse: [
			((stack size) = 1) ifFalse: [
				rightTree := stack removeLast.
				leftTree := stack removeLast.
				newTree := Tree new: (leftTree char) count: 1 left: leftTree right: rightTree.
				stack add: newTree. 
			] ifTrue:[
				(newTree isNil) ifTrue: 
					[root2 := stack removeLast.]
				ifFalse:[root2 := newTree.].
				endOfHuffman := true.
			].
			
		].	
	].
	[atEOF] whileFalse: [
		| currNode bit |
		currNode := root2.
		[(currNode isMemberOf: Leaf)] whileFalse: [
			bit := infile next.
		"	stdout << 'bit is' << bit."
			(bit = $0) ifTrue: [
				currNode := (currNode left).
			] ifFalse: [
				currNode := (currNode right).
			].
		].
		"stdout << (currNode isMemberOf: Leaf) << nl."
		((currNode char) = (256 asCharacter)) ifTrue: [
			atEOF := true.
		] ifFalse: [
			(outfileExists) ifTrue: [
				outfile nextPut: (currNode char).
			
			] ifFalse: [
				stdout << (currNode char).
			].
		].
		currNode := root2.
	].
] ifFalse: [

"Frequency table array obj"

"Create 1 tree for each ASCII char (256)"
	freqtable at: 257 put: 1.
	[infile atEnd] whileFalse:[
    		| inputChar ind |
		ind := ((infile nextByte) + 1).
		((freqtable at: ind) isNil) ifTrue: [freqtable at: ind put: 0].
		freqtable at: ind put: (freqtable at: ind) + 1.
	].

	"Read in file and save frequency in new frequency table (initialize w/ ASCII)"
	"Create priority queue and load from frequency table, if count > 0"
	priorQ := SortedCollection new.
	1 to: 257 do: [:i |
		((freqtable at: i) notNil) ifTrue:[
			char := (i-1) asCharacter.
			stdout << 'char is:' << char << nl.
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
		root inorder: [:char :bitstring| encodedArray at: ((char asInteger)+1) put: bitstring] prefix: ''.
	].
].
"if option t, print encoding table sorted by character to output file if exists or stdout"
(option = '-t') ifTrue: [
	1 to: 257 do: [:i |
		(freqtable at: i) notNil ifTrue: [	
			(outfileExists) ifTrue: [
				((i < 33) | ((i > 127) & (i < 257)))ifTrue: [
					|hex|
					outfile nextPut: $x.
					hex := (i-1) radix: 16.
					(hex size < 5) ifTrue: [
						outfile nextPut: $0.
					].
					4 to: hex size do: [:j |
						outfile nextPut: (hex at: j).
					].
				] ifFalse: [
					(i = 257) ifTrue: [outfile nextPut: $E. outfile nextPut: $O. outfile nextPut: $F.]
					ifFalse:[ outfile nextPut: $ . outfile nextPut: ((i-1) asCharacter). outfile nextPut: $ .].	
				].
				
				1 to: (5 - (((freqtable at: i) asString) size)) do: [:variable |
					outfile nextPut: $ .
				].
				1 to: (((freqtable at: i) asString) size) do: [:j |
					outfile nextPut: (((freqtable at: i) asString) at: j).
				].
				outfile nextPut: $ .
				

				1 to: ((encodedArray at: (i-1)) size) do: [:j |
					|charr|
					charr := ((encodedArray at: (i-1)) at: j).
					outfile nextPut: charr.
				].

				outfile nextPut: nl.
			] ifFalse: [	
				((i < 33) | ((i > 127) & (i < 257))) ifTrue: [
					|hex|
					stdout << 'x'.
					hex := (i-1) radix: 16.
					(hex size < 5) ifTrue: [
						stdout << '0'.
					].
					4 to: hex size do: [:j |
						stdout << (hex at: j).
					].
					"stdout << i."
				] ifFalse: [
					(i = 257) ifTrue: [
						stdout << 'EOF'.
					] ifFalse: [	
						stdout << ' '<<((i-1) asCharacter) << ' '.
					].
					"stdout << i."	
				].
				
				1 to: (5 - (((freqtable at: i) asString) size)) do: [:variable |
					stdout << ' ' .
				].
				1 to: (((freqtable at: i) asString) size) do: [:j |
					stdout << (((freqtable at: i) asString) at: j).
				].
				stdout << ' ' .
				

				1 to: ((encodedArray at: i) size) do: [:j |
					|charr|
					charr := ((encodedArray at: i) at: j).
					stdout << charr.
				].

				stdout << nl.
				
			].
		].
	].
] ifFalse: [
	(option = '-u') ifFalse: [
		(outfileExists) ifTrue: [
			root postorder: [] file: outfile.
			outfile nextPut: $1.
		] ifFalse: [
			root postorder: [].
			stdout << '1'.
		].
	].
].
infile close.

"reopen input file"
infile := openfile value: infilename value: FileStream read.

"compress input file (write out encoded version of each byte) -> output file"
(option = '-c') ifTrue: [
	| bitCount |
	bitCount := 0.
	(outfileExists) ifTrue: [
		[infile atEnd] whileFalse:[
        		| ind |
        		ind := ((infile nextByte) + 1).
			1 to: ((encodedArray at: ind) size) do: [:j |
                               |charr|
                               charr := ((encodedArray at: ind) at: j).
         	               outfile nextPut: charr.
				bitCount := (bitCount + 1). 
                        ].
		].
		1 to: ((encodedArray at: 257) size) do: [:j |
                             |charr|
                             charr := ((encodedArray at: 257) at: j).
                             outfile nextPut: charr.
				bitCount := (bitCount + 1).
                ].
		[(bitCount \\ 8) = 0] whileFalse: [
			outfile nextPut: $0.
			bitCount := (bitCount + 1).
		].
		
	]ifFalse:[
		[infile atEnd] whileFalse:[
                        | ind |
                        ind := ((infile nextByte) + 1).
                        1 to: ((encodedArray at: ind) size) do: [:j |
                                       |charr|
                                       charr := ((encodedArray at: ind) at: j).
                                       stdout<< charr.
				       bitCount := (bitCount + 1).
                               ].
                ].
		1 to: ((encodedArray at: 257) size) do: [:j |
                             |charr|
                             charr := ((encodedArray at: 257) at: j).
                             stdout<< charr.
                	     bitCount := (bitCount + 1).
		].
		[(bitCount \\ 8) = 0] whileFalse: [
			stdout << '0'.
			bitCount := (bitCount + 1).
		].
	].
].

infile close. 
(outfileExists) ifTrue: [outfile close].

