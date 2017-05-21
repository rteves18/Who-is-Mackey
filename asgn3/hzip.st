#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"
Ryan Teves | rteves@ucsc.edu
Ky Nguyen | kymnguye@ucsc.edu
CS 112 Spring 2017 - Assignment 3 - Smalltalk
"

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.

flagD := false.
flagC := false.
flagT := false.
flagU := false.

nl := Character nl.
priority := SortedCollection new.

"===============[Object Classes]=================="

"FileStream taken from catfile.st provided in the /examples directory"
FileStream extend [
   nl := Character nl.
   catFile [
      |count textline|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         textline :=self nextLine.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << textline << nl.
      ]
   ]
]

"ZeroArray taken from charfreq.st provided in the /examples directory"
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^  self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

freqtable := ZeroArray new: 257.
freqtable at:256 put:1.

"Leaf taken from tree.st provided in the /examples directory"
Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [^ isLeaf]

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
      isLeaf:=true.
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

   postorder: visitor [
      visitor  value:char value: isLeaf.
   ]
]

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
      isLeaf:=false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor[
      left postorder: visitor .
      right postorder: visitor .
      visitor value: char value:isLeaf.
   ]
]

"BitStream taken from bitstream.st provided in the /examples directory"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"OutBits taken from outbits.st provided in the /examples directory"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]



"=================[Main start of program]==============="

|infile readfile outputfile dcpfile outfilename argc|

argc:=0.
Smalltalk arguments: '-d -t -c -u'

do: [:flag :arg |

   (flag = $d) ifTrue: [ 
	flagD := true. 
	].
   (flag = $t) ifTrue: [ 
	flagT := true. 
	].
   (flag = $c) ifTrue: [ 
	flagC := true. 
	].
   (flag = $u) ifTrue: [ 
	flagU := true. 
	].

  "if arg is not nil or null"
  (arg ~= nil) ifTrue: [

"=================[-d DEGUB FLAG]==================="

     "if argument count is 0"
     argc = 0
        ifTrue:[
          "and if flag -d is turned on"
          flagD ifTrue:[
          stdout <<'Input File: ' << arg <<nl.].

          infile := FileStream open: arg
                    mode: FileStream read.
          readfile := FileStream open: arg
                      mode: FileStream read.
         ]
        "argument count is greater than 0"
        ifFalse:[
           flagD ifTrue:[
           stdout <<'Output File: ' << arg <<nl.].
           outputfile := FileStream open: arg
                         mode: FileStream write.
        ].
     argc := argc + 1.
   ].
].



"=================[ENCODE TABLE]==================="
|tree ExitStatus|
ExitStatus:=0.

"if flag -u is turned off, do not encode into table"
"used charfreq.st as reference"
flagU ifFalse:[
   encodeTable := ZeroArray new: 257.

   [infile atEnd not] whileTrue: [
      |ordChar|
      ordChar := infile next asInteger.
      freqtable incr: ordChar.
   ].

   freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
         priority add: (Leaf new: key count: value).
      ]
   ].

   "Repeatedly removes the two smallest elements from priority queue
   until there is only one tree left and the priority queue is empty"
   [priority size > 1] whileTrue: [
      |leftchild rightchild|

      leftchild := priority removeFirst.
      rightchild := priority removeFirst.
      newtree := ( Tree new: (leftchild char)
                        count: (leftchild count) +  (rightchild count)
                        left: leftchild 
                        right: rightchild ).
      priority add: newtree.
   ].
   tree := priority removeFirst.


   tree inorder: [:char :string |
      encodeTable at: char put: string.
   ] prefix: ''.

   binarytree := ''.
   tree postorder: [:char :isLeaf |
      isLeaf ifFalse:[ binarytree := binarytree,'1'. ]
      ifTrue:[
         bin := ''.
         char = 256
         ifTrue:[ bin := '000000001' ]
         ifFalse:[
            char = 0 
            ifTrue:[ bin := '000000000' ]
            ifFalse:[
               8 to:1 by: -1
               do:[:i | 
                  bin := bin, ( (char bitAt:i)
                  printString ).
               ]
            ].
         ].
         binarytree := (binarytree, '0', bin).
      ].
   ].
]. 

"=================[-t PRINT DECODE TREE FLAG]==================="

flagT ifTrue:[
   stdout <<nl<< 'Decode Tree:' << nl.
   stdout << tree <<nl<<nl.
   stdout <<'Decode Tree Binary:'<<nl<<binarytree << nl.

   stdout <<nl<< 'Huffman Decoding Tree:' << nl.
   stdout <<'Character    Count    Location' << nl.
   encodeTable keysAndValuesDo: [:key :count |
      (count notNil and: [count > 0]) ifTrue: [
         stdout <<  key asCharacter visible << ' .......... '
         << (freqtable at: key) <<' ....... '<<count
         << nl.
      ].
   ].
].

"=================[-c COMPRESS FLAG]==================="
flagC ifTrue: [

   "if outputfile is specified, write to file"
   outputfile~=nil ifTrue:[

      flagD ifTrue: [
         stdout << 'Debug Compress' << nl.
      ].

      bitBybit := OutBits new: outputfile.
      1 to: (binarytree size) by: 1 
      do: [:index | 
         bit:= ((binarytree at: index) asInteger).
         bitBybit writeBit: bit.
      ].

      bitBybit writeBit: 1.
      [readfile atEnd not] whileTrue: [
         |ordChar|
         ordChar := readfile next asInteger.
         run := (encodeTable at: ordChar).
         1 to: (run size) by: 1
         do: [:index | bit := ((run at: index) asInteger).
            bitBybit writeBit: bit.
         ].
      ].

      endFile := (encodeTable at: 256).
      1 to: (endFile size) by: 1
      do: [:index | 
         bit:= ( (endFile at: index) asInteger ).
      ].
      bitBybit flushByte.

   ]

  "if outputfile is not specified"
   ifFalse:[
      flagD ifTrue: [
         stdout << 'No outputfile specified' << nl.].
      stdout <<nl<< 'Decode Tree Binary:' << nl.
      stdout <<binarytree<<nl.
       stdout <<nl<< 'Compression to stdout:' << nl.
      [readfile atEnd not] whileTrue: [
      |ordChar|
      ordChar := readfile next asInteger.
      run := (encodeTable at: ordChar).
     
      stdout << run.
      ].
      endFile := (encodeTable at: 256).
      stdout <<endFile<<nl.
      ].
   ].

"=================[-u DECOMPRESS FLAG]==================="
flagU ifTrue: [
   |isEOF outBytes stack|
   flagD ifTrue: [
      stdout << 'Debug Decompress' << nl.
   ].

   outBytes := OutBits new: outputfile.
   stack := OrderedCollection new.
   fileBits := BitStream new: inputfile.
   isTreeBuilt := false.
   treeRecon := ''.
   node := ''.

   "Read input bit by bit"
   [ (fileBits atEnd not) & (isEOF not) ] whileTrue:[
      treenode := fileBits nextBit.
      learchar := 0.

      "if tree is not built"
      isTreeBuilt ifFalse:[
         "Insert input to leaf"
         treenode = 0 ifTrue:[
            8 to: 1 by: -1 do:[:i |
               bit := fileBits nextBit.
               leafchar := leafchar bitAt: i put: bit.
            ].

            learchar = 0 isTrue:[
               specialbit := fileBits nextBit.
               specialbit = 1 ifTrue:[
                  learchar := learchar bitAt: 9 put: 1.
               ].
            ].

            stack add: (Leaf new: learchar count: '').
         ].

         treenode = 1 ifTrue:[
            
            stack size > 1 ifTrue:[
               rightchild := stack removeLast.
               leftchild := stack removeLast.
               newtree := (Tree new: -1 count: ''
                           left: leftchild right: rightchild).
               stack add: newtree.   
            ] ifFalse:[
               isTreeBuilt := true.
               treeRecon := stack removeLast.
               node := treeRecon.
            ].
         
         ].
      ] ifTrue:[ "if tree is built, decode the data"
         
         node isLeaf ifTrue:[
            
            ( (node char) = 256 ) ifTrue:[
               isEOF := true.
            ] ifFalse:[
               stdout nextPut: (Character value: (node char)).
               node := treeRecon.
            ].
         
         ] ifFalse:[

            treenode = 0 ifTrue:[

               node := node left.
               node isLeaf isTrue:[
                  isEOF not isTrue:[
                     ( (node char) = 256 ) ifTrue:[
                        isEOF := true.
                     ] ifFalse:[
                        outputfile = nil ifTrue:[
                           stdout nextPut: (Character value: (node char)).
                        ] ifFalse:[
                           8 to: 1 by: -1 do: [:index |
                              bit := (node char) bitAt: index.
                              outBytes writeBit: bit.
                           ].
                        ].
                        node := treeRecon.

                     ].
                  ].
               ]

            ] ifFalse:[

               node := node right.
               node isLeaf isTrue:[
                  isEOF not isTrue:[
                     ( (node char) = 256 ) ifTrue:[
                        isEOF := true.
                     ] ifFalse:[
                        outputfile = nil ifTrue:[
                           stdout nextPut: (Character value: (node char)).
                        ] ifFalse:[
                           8 to: 1 by: -1 do: [:index |
                              bit := (node char) bitAt: index.
                              outBytes writeBit: bit.
                           ].
                        ].
                        node := treeRecon.

                     ].
                  ].
               ]

            ].

         ].
      ].
   ].

].
