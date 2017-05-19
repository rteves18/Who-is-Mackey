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
queue := SortedCollection new.

"-------------------Object Classes-------------------"

"----------------------------------------------------------------
FileStream taken from catfile.st provided in the /examples directory
----------------------------------------------------------------"
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

"----------------------------------------------------------------
ZeroArray taken from charfreq.st provided in the /examples directory
----------------------------------------------------------------"
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

"--------------------------------------------------------
Leaf taken from tree.st provided in the /examples directory
--------------------------------------------------------"
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

"------------------------------------------------------------------
BitStream taken from bitstream.st provided in the /examples directory
------------------------------------------------------------------"
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

"--------------------------------------------------------------
OutBits taken from outbits.st provided in the /examples directory
--------------------------------------------------------------"
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

"-------------------Main start of program-------------------"

|inputfile readfile outputfile dcpfile outfilename argc|
argc:=0.

Smalltalk arguments: '-d -t -c -u'

do: [:opt :arg |

   (opt = $d) ifTrue: [ flagD := true. ].
   (opt = $t) ifTrue: [ flagT := true. ].
   (opt = $c) ifTrue: [ flagC := true. ].
   (opt = $u) ifTrue: [ flagU := true. ].

  (arg ~= nil) ifTrue: [

     argc = 0
        ifTrue:[
          flagD ifTrue:[
          stdout <<'input filename: ' << arg <<nl.].

          inputfile := FileStream open: arg
                    mode: FileStream read.
          readfile := FileStream open: arg
                    mode: FileStream read.
         ]

        ifFalse:[
           flagD ifTrue:[
           stdout <<'output filename: ' << arg <<nl.].
            outputfile := FileStream open: arg
                       mode: FileStream write.].

     argc := argc + 1.
   ].
].


"Creates encode table"
|tree ExitStatus|
ExitStatus:=0.
flagU ifFalse:[
    encodeTable := ZeroArray new: 257.

[inputfile atEnd not] whileTrue: [
   |refChar|
   refChar := inputfile next asInteger.
   freqtable incr: refChar.
].

freqtable keysAndValuesDo: [:key :count |
   (count notNil and: [count > 0]) ifTrue: [
      queue add: (Leaf new: key count: count).
   ]
].

[queue size >1 ] whileTrue: [
   |leftchild rightchild|

   leftchild := queue removeFirst.
   rightchild := queue removeFirst.
   newtree := ( Tree new: (leftchild char)
                      count: (leftchild count) +  (rightchild count)
                      left: leftchild right: rightchild).
   queue add: newtree.
].
tree:=queue removeFirst.


tree inorder: [:char :string |
   encodeTable at: char put: string.
] prefix: ''.

binarytree:=''.
tree postorder: [:char :isLeaf |

   isLeaf  ifFalse:[binarytree:=binarytree,'1'.]
                ifTrue:[
                   bin:=''.
                   char = 256
                        ifTrue:[bin:='000000001']
                        ifFalse:[
                           char=0 ifTrue:[bin:='000000000']
                                  ifFalse:[
                                     8 to:1 by: -1 do:[:i|
                                     bin:=bin,((char bitAt:i)
                                                       printString).
                                   ]
                        ].
                 ].
                 binarytree:=(binarytree,'0',bin).
              ].
].
]. "end of flagU ifFalse wasn't here before but was complaining
      ./hzip.st:277: parse error, expected ']'
   "
flagT ifTrue:[
     stdout <<nl<< 'Decode tree' << nl.
     stdout << tree << nl<<nl.
     stdout <<'Binary decode tree: '<<nl<<binarytree << nl.

     stdout <<nl<< 'Encode table' << nl.
     encodeTable keysAndValuesDo: [:key :count |
     (count notNil and: [count > 0]) ifTrue: [
        stdout <<  key asCharacter visible << '     '
               << (freqtable at: key) <<'    '<<count
               << nl.
     ].
    ].
].
