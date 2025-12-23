"Filed out from Dolphin Smalltalk"!

Object subclass: #Variable
	instanceVariableNames: 'timestamp name value context'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Variable guid: (GUID fromString: '{aab2deeb-95d5-4cd0-9a19-dfc0b3a92743}')!

Variable comment: ''!

!Variable categoriesForClass!Unclassified! !

!Variable methodsFor!

- aConstraintVariable
	^self minus: aConstraintVariable!

* anInteger
	^self multiply: anInteger!

/ anObject
	^self divide: anObject!

@ aConstraintVariable
	^Point x: self y: aConstraintVariable!

+ anObject
	^self plus: anObject!

<= aVariable
	^timestamp <= aVariable timestamp!

context
	^context!

context: anObject
	context := anObject!

displayString
	^'[' , (name ifNil: ['<unknown>']) , ':' , value displayString , ']'!

divide: aNumber
	^Expression new: (Array with: (Array with: 1 / aNumber with: self))!

dumpOn: aReadWriteStream 
	aReadWriteStream nextPutAll: self displayString!

initialize
	super initialize.
	timestamp := DateAndTime now.
	value := 0.0.!

isArray
	^false!

isExpression
	^false!

isVariable
	^true!

minus: anObject
	^Expression new: (anObject isNumber
				ifTrue: [Array with: self with: anObject negated]
				ifFalse: [Array with: self with: (Array with: -1.0 with: anObject)])!

multiply: aNumber
	^Expression new: (Array with: (Array with: aNumber with: self))!

name
	^name!

name: anObject
	name := anObject!

plus: anObject
	^Expression new: (Array with: self with: anObject)!

printOn: aStream
	aStream nextPutAll: self displayString!

timestamp
	^timestamp!

value
	^value!

value: anObject
	value := anObject! !

!Variable categoriesForMethods!
-!public! !
*!public! !
/!public! !
@!public! !
+!public! !
<=!public! !
context!accessing!public! !
context:!accessing!public! !
displayString!accessing!public! !
divide:!accessing!public! !
dumpOn:!public! !
initialize!public! !
isArray!public! !
isExpression!public! !
isVariable!public! !
minus:!accessing!public! !
multiply:!accessing!public! !
name!accessing!public! !
name:!accessing!public! !
plus:!accessing!public! !
printOn:!accessing!public! !
timestamp!public! !
value!accessing!public! !
value:!accessing!public! !
!

!Variable class methodsFor!

new
	^super new initialize!

withValue: aNumber
	^self new
		value: aNumber! !

!Variable class categoriesForMethods!
new!public! !
withValue:!public! !
!

