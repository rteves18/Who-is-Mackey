#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Ryan Teves cruzid: rteves | Ky Nguyen cruzid: kymnguye"

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.

optD := false.
optC := false.
optT := false.
optU := false.

nl := Character nl.
queue := SortedCollection new.

"-------------------Object Classes-------------------"

"FileStream taken from catfile.st provided in examples folder"
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

"ZeroArray taken from charfreq.st provided in examples folder"
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


"Leaf taken from tree.st provided in examples folder"
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


"-------------------Main start of program-------------------"

|inputfile readfile outputfile dcpfile outfilename argc|
argc:=0.

Smalltalk arguments: '-d -t -c -u'

do: [:opt :arg |

   (opt = $d) ifTrue: [ optD := true. ].
   (opt = $t) ifTrue: [ optT := true. ].
   (opt = $c) ifTrue: [ optC := true. ].
   (opt = $u) ifTrue: [ optU := true. ].

  (arg ~= nil) ifTrue: [

     argc = 0
        ifTrue:[
          optD ifTrue:[
          stdout <<'input filename: ' << arg <<nl.].

          inputfile := FileStream open: arg
                    mode: FileStream read.
          readfile := FileStream open: arg
                    mode: FileStream read.
         ]

        ifFalse:[
           optD ifTrue:[
           stdout <<'output filename: ' << arg <<nl.].
            outputfile := FileStream open: arg
                       mode: FileStream write.].

     argc := argc + 1.
   ].
].


"Creates encode table"
|tree ExitStatus|
ExitStatus:=0.
optU ifFalse:[
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

optT ifTrue:[
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
