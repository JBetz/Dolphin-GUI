"Filed out from Dolphin Smalltalk"!

Object subclass: #VariableSymbol
	instanceVariableNames: 'timestamp type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

VariableSymbol guid: (GUID fromString: '{07f2bb6d-4e4c-43c3-bd12-23fb57eaab48}')!

VariableSymbol comment: ''!

!VariableSymbol categoriesForClass!Unclassified! !

!VariableSymbol methodsFor!

<= aVariableSymbol
	^timestamp <= aVariableSymbol timestamp!

displayString
	^type!

dumpOn: aReadWriteStream 
	aReadWriteStream nextPutAll: self displayString!

initialize
	super initialize.
	timestamp := DateAndTime now!

isDummy
	^type = #dummy!

isError
	^type = #error!

isExternal
	^type = #external!

isInvalid
	^type = #invalid!

isSlack
	^type = #slack!

printOn: aStream
	aStream nextPutAll: self displayString!

timestamp
	^timestamp!

type
	^type!

type: anObject
	type := anObject! !

!VariableSymbol categoriesForMethods!
<=!public! !
displayString!public! !
dumpOn:!public! !
initialize!public! !
isDummy!public! !
isError!public! !
isExternal!public! !
isInvalid!public! !
isSlack!public! !
printOn:!public! !
timestamp!public! !
type!accessing!public! !
type:!accessing!public! !
!

!VariableSymbol class methodsFor!

dummy
	^self new type: #dummy!

error
	^self new type: #error!

external
	^self new type: #external!

invalid
	^self new type: #invalid!

new
	^super new initialize!

slack
	^self new type: #slack! !

!VariableSymbol class categoriesForMethods!
dummy!public! !
error!public! !
external!public! !
invalid!public! !
new!public! !
slack!public! !
!

