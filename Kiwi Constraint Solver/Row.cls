"Filed out from Dolphin Smalltalk"!

Object subclass: #Row
	instanceVariableNames: 'cells constant'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Row guid: (GUID fromString: '{3bf40c92-5ceb-41ca-92f7-b215c5f57d5f}')!

Row comment: ''!

!Row categoriesForClass!Unclassified! !

!Row methodsFor!

add: value
	constant := constant + value.
	^constant!

allDummies
	cells keysAndValuesDo: [:symbol :value | symbol isDummy ifFalse: [^false]].
	^true!

cells
	^cells!

cells: anObject
	cells := anObject!

coefficientFor: aSymbol
	^cells at: aSymbol ifAbsent: [0.0]!

constant
	^constant!

constant: anObject
	constant := anObject!

copy
	^Row new
		constant: constant;
		cells: (OrderedDictionary newFrom: cells)!

displayString
	| stream |
	stream := ReadWriteStream on: OrderedCollection new.
	cells
		keysAndValuesDo: [:name :value | stream nextPutAll: ' + ' , value displayString , ' * ' , name displayString].
	^stream contents asString!

dumpOn: aReadWriteStream
	cells
		keysAndValuesDo: [:name :value | aReadWriteStream nextPutAll: ' + ' , value displayString , ' * ' , name displayString]!

initialize
	super initialize.
	cells := OrderedDictionary new.
	constant := 0.0!

insertRow: other
	^self insertRow: other coefficient: 1.0!

insertRow: other coefficient: coefficient
	constant := constant + (other constant * coefficient).
	other cells keysAndValuesDo: 
			[:symbol :otherValue |
			| otherCoefficient value temp |
			otherCoefficient := otherValue * coefficient.
			value := cells at: symbol ifAbsentPut: [0.0].
			temp := value + otherCoefficient.
			cells at: symbol put: temp.
			temp nearZero ifTrue: [cells removeKey: symbol]]!

insertSymbol: symbol
	^self insertSymbol: symbol coefficient: 1.0!

insertSymbol: symbol coefficient: number
	| cellValue newCellValue |
	cellValue := cells at: symbol ifAbsentPutValue: 0.0.
	newCellValue := cells at: symbol put: cellValue + number.
	newCellValue nearZero ifTrue: [cells removeKey: symbol]!

isConstant
	^cells isEmpty!

removeSymbol: symbol
	cells removeKey: symbol ifAbsent: [nil]!

reverseSign
	constant := constant negated.
	cells keysAndValuesDo: [:symbol :value | cells at: symbol put: value negated]!

solveFor: symbol
	| cellValue coefficient |
	cellValue := cells removeKey: symbol.
	coefficient := -1.0 / cellValue.
	constant := constant * coefficient.
	cells keysAndValuesDo: [:symbol :value | cells at: symbol put: value * coefficient]!

solveForLhs: lhs rhs: rhs
	self
		insertSymbol: lhs coefficient: -1.0;
		solveFor: rhs!

substitute: symbol row: row
	| cellValue |
	cellValue := cells removeKey: symbol ifAbsent: [nil].
	cellValue ifNotNil: [self insertRow: row coefficient: cellValue]! !

!Row categoriesForMethods!
add:!accessing!public! !
allDummies!accessing!public! !
cells!accessing!public! !
cells:!accessing!private! !
coefficientFor:!accessing!public! !
constant!accessing!public! !
constant:!accessing!private! !
copy!accessing!public! !
displayString!public! !
dumpOn:!public! !
initialize!public! !
insertRow:!accessing!public! !
insertRow:coefficient:!accessing!public! !
insertSymbol:!accessing!public! !
insertSymbol:coefficient:!accessing!public! !
isConstant!accessing!public! !
removeSymbol:!accessing!public! !
reverseSign!accessing!public! !
solveFor:!accessing!public! !
solveForLhs:rhs:!accessing!public! !
substitute:row:!accessing!public! !
!

!Row class methodsFor!

new
	^super new initialize! !

!Row class categoriesForMethods!
new!public! !
!

