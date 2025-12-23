"Filed out from Dolphin Smalltalk"!

Object subclass: #Expression
	instanceVariableNames: 'terms constant'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Expression guid: (GUID fromString: '{b40f9cfe-d017-4eaa-9a3a-5fb1790e0713}')!

Expression comment: ''!

!Expression categoriesForClass!Unclassified! !

!Expression methodsFor!

- anObject
	^self minus: anObject!

* anObject
	^self multiply: anObject!

/ anObject
	^self divide: anObject!

@ aConstraintExpression
	^Point x: self y: aConstraintExpression!

+ anObject
	^self plus: anObject!

constant
	^constant!

constant: aFloat 
	constant := aFloat!

displayString
	| result |
	result := ''.
	terms
		keysAndValuesDo: [:key :value | result := result , value displayString , '*' , key displayString , ' + '].
	result := result , constant displayString.
	^result!

divide: aNumber
	^self class new: (Array with: (Array with: 1 / aNumber with: self))!

isArray
	^false!

isConstant
	^terms isEmpty!

isExpression
	^true!

isVariable
	^false!

minus: anObject
	^self class new: (anObject isNumber
				ifTrue: [Array with: self with: anObject negated]
				ifFalse: [Array with: self with: (Array with: -1.0 with: anObject)])!

multiply: aNumber
	^self class new: (Array with: (Array with: aNumber with: self))!

plus: anObject
	^self class new: (Array with: self with: anObject)!

printOn: aStream
	aStream nextPutAll: self displayString!

terms
	^terms!

terms: aLookupTable
	terms := aLookupTable!

value
	| result |
	result := constant.
	terms keysAndValuesDo: [:variable :multiplier | result := result + (variable value * multiplier)].
	^result! !

!Expression categoriesForMethods!
-!public! !
*!public! !
/!public! !
@!public! !
+!public! !
constant!public! !
constant:!public! !
displayString!public! !
divide:!public! !
isArray!public! !
isConstant!public! !
isExpression!public! !
isVariable!public! !
minus:!public! !
multiply:!public! !
plus:!public! !
printOn:!public! !
terms!public! !
terms:!public! !
value!public! !
!

!Expression class methodsFor!

new: aCollection
	| parsed |
	parsed := self parseArguments: aCollection.
	^self new
		terms: parsed key;
		constant: parsed value;
		yourself!

parseArguments: aCollection
	| constant terms |
	constant := 0.0.
	terms := OrderedDictionary new.
	aCollection do: 
			[:item |
			item isNumber ifTrue: [constant := constant + item].
			item isVariable
				ifTrue: 
					[| term |
					term := terms at: item ifAbsentPutValue: 0.0.
					terms at: item put: term + 1.0].
			item isExpression
				ifTrue: 
					[| itemTerms |
					constant := constant + item constant.
					itemTerms := item terms.
					itemTerms keysAndValuesDo: 
							[:variable :multiplier |
							| itemTerm |
							itemTerm := terms at: variable ifAbsentPutValue: 0.0.
							terms at: variable put: itemTerm + multiplier]].
			item isArray
				ifTrue: 
					[| first second |
					item size ~= 2 ifTrue: [self error: 'array must have length 2'].
					first := item first.
					second := item second.
					first isNumber ifFalse: [self error: 'array item 1 must be a number'].
					second isVariable
						ifTrue: 
							[| term |
							term := terms at: second ifAbsentPutValue: 0.0.
							terms at: second put: term + first]
						ifFalse: 
							[second isExpression
								ifTrue: 
									[| expressionTerms |
									constant := constant + (second constant * first).
									expressionTerms := second terms.
									expressionTerms keysAndValuesDo: 
											[:variable :multiplier |
											| expressionTerm |
											expressionTerm := terms at: variable ifAbsentPutValue: 0.0.
											terms at: variable put: expressionTerm + (multiplier * first)]]
								ifFalse: [self error: 'array item 2 must be a variable or expression']]]].
	^Association key: terms value: constant! !

!Expression class categoriesForMethods!
new:!public! !
parseArguments:!public! !
!

