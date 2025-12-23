"Filed out from Dolphin Smalltalk"!

Object subclass: #ConstraintSpec
	instanceVariableNames: 'frame constraints strength'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

ConstraintSpec guid: (GUID fromString: '{99634318-5054-4d84-b189-c0f95946ce31}')!

ConstraintSpec comment: ''!

!ConstraintSpec categoriesForClass!Unclassified! !

!ConstraintSpec methodsFor!

<= aConstraint
	^strength <= aConstraint strength!

applyTo: aConstraintSolver
	self compile.
	constraints do: [:each | aConstraintSolver addConstraint: each]!

compile
	self subclassResponsibility!

constraints
	^constraints!

frame
	^frame!

frame: anObject
	frame := anObject!

initialize
	super initialize.
	constraints := OrderedCollection new.
	strength := Strength required!

removeFrom: aConstraintSolver
	constraints do: [:each | aConstraintSolver removeConstraint: each].
	constraints removeAll!

strength
	^strength!

strength: aFloat 
	strength := aFloat! !

!ConstraintSpec categoriesForMethods!
<=!public! !
applyTo:!public! !
compile!public! !
constraints!public! !
frame!accessing!public! !
frame:!accessing!public! !
initialize!public! !
removeFrom:!public! !
strength!public! !
strength:!public! !
!

!ConstraintSpec class methodsFor!

new
	^super new initialize! !

!ConstraintSpec class categoriesForMethods!
new!public! !
!

