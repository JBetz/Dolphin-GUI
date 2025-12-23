"Filed out from Dolphin Smalltalk"!

Object subclass: #Constraint
	instanceVariableNames: 'timestamp expression operator strength'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Constraint guid: (GUID fromString: '{ad06b3a9-6e1e-4f09-9956-6007a60938e7}')!

Constraint comment: ''!

!Constraint categoriesForClass!Kernel-Objects! !

!Constraint methodsFor!

<= aConstraint
	^timestamp <= aConstraint timestamp!

displayString
	^expression displayString , ' ' , operator displayString , ' 0 (' , strength displayString , ')' !

dumpOn: aReadWriteStream 
	aReadWriteStream nextPutAll: self displayString!

expression
	^expression!

expression: aConstraintExpression 
	expression := aConstraintExpression!

initialize
	super initialize.
	timestamp := DateAndTime now.
	strength := Strength required!

operator
	^operator!

operator: anOperator
	operator := anOperator!

printOn: aStream
	aStream nextPutAll: self displayString!

strength
	^strength!

strength: aFloat 
	strength := aFloat!

timestamp
	^timestamp! !

!Constraint categoriesForMethods!
<=!public! !
displayString!public! !
dumpOn:!public! !
expression!public! !
expression:!public! !
initialize!public! !
operator!public! !
operator:!public! !
printOn:!public! !
strength!public! !
strength:!public! !
timestamp!public! !
!

!Constraint class methodsFor!

equals
	^#=!

greaterThanOrEqual
	^#>=!

lessThanOrEqual
	^#<=!

lhs: lhs equalsRhs: rhs
	^self
		lhs: lhs
		equalsRhs: rhs
		strength: Strength required!

lhs: lhs equalsRhs: rhs strength: strength
	^self lhs: lhs operator: Constraint equals rhs: rhs strength: strength!

lhs: lhs greaterThanOrEqualRhs: rhs
	^self
		lhs: lhs
		greaterThanOrEqualRhs: rhs
		strength: Strength required!

lhs: lhs greaterThanOrEqualRhs: rhs strength: strength
	^self
		lhs: lhs
		operator: Constraint greaterThanOrEqual
		rhs: rhs
		strength: strength!

lhs: lhs lessThanOrEqualRhs: rhs
	^self
		lhs: lhs
		lessThanOrEqualRhs: rhs
		strength: Strength required!

lhs: lhs lessThanOrEqualRhs: rhs strength: strength
	^self
		lhs: lhs
		operator: Constraint lessThanOrEqual
		rhs: rhs
		strength: strength!

lhs: lhs operator: operator rhs: rhs
	^self
		lhs: lhs
		operator: operator
		rhs: rhs
		strength: Strength required!

lhs: lhs operator: operator rhs: rhs strength: number
	^self new
		operator: operator;
		strength: (Strength clip: number);
		expression: ((rhs isNil and: [lhs isExpression]) ifTrue: [lhs] ifFalse: [lhs minus: rhs])!

new
	^super new initialize! !

!Constraint class categoriesForMethods!
equals!public! !
greaterThanOrEqual!public! !
lessThanOrEqual!public! !
lhs:equalsRhs:!public! !
lhs:equalsRhs:strength:!public! !
lhs:greaterThanOrEqualRhs:!public! !
lhs:greaterThanOrEqualRhs:strength:!public! !
lhs:lessThanOrEqualRhs:!public! !
lhs:lessThanOrEqualRhs:strength:!public! !
lhs:operator:rhs:!public! !
lhs:operator:rhs:strength:!public! !
new!public! !
!

