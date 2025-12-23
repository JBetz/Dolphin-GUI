| package |
package := Package name: 'Kiwi Constraint Solver'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Constraint;
	add: #ConstraintSolver;
	add: #ConstraintSolverTests;
	add: #ConstraintTag;
	add: #EditInfo;
	add: #Expression;
	add: #OrderedDictionary;
	add: #Row;
	add: #RowCreation;
	add: #Strength;
	add: #Variable;
	add: #VariableSymbol;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Desktop\Dolphin Images\Flight Navigator 49\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\Desktop\Dolphin Images\Flight Navigator 49\Core\Object Arts\Dolphin\MVP\Base\Dolphin Basic Geometry'
	'..\..\..\Desktop\Dolphin Images\Flight Navigator 49\Core\Contributions\Camp Smalltalk\SUnit\SUnit').

package!

"Class Definitions"!

Object subclass: #Constraint
	instanceVariableNames: 'timestamp expression operator strength'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ConstraintSolver
	instanceVariableNames: 'currentId constraints rows variables edits infeasibleRows objective artificial'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ConstraintTag
	instanceVariableNames: 'marker other'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #EditInfo
	instanceVariableNames: 'tag constraint constant'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Expression
	instanceVariableNames: 'terms constant'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Row
	instanceVariableNames: 'cells constant'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #RowCreation
	instanceVariableNames: 'row tag'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Strength
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Variable
	instanceVariableNames: 'timestamp name value context'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #VariableSymbol
	instanceVariableNames: 'timestamp type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Collection variableSubclass: #OrderedDictionary
	instanceVariableNames: 'dictionary orderedKeys'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

TestCase subclass: #ConstraintSolverTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

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

ConstraintSolver guid: (GUID fromString: '{3a49431e-c32b-4de8-9cd1-f9560382254b}')!

ConstraintSolver comment: ''!

!ConstraintSolver categoriesForClass!Unclassified! !

!ConstraintSolver methodsFor!

addConstraint: aConstraint
	| rowCreation row tag subject |
	(constraints includesKey: aConstraint) ifTrue: [self error: 'duplicate constraint'].
	rowCreation := self createRow: aConstraint.
	row := rowCreation row.
	tag := rowCreation tag.
	subject := self chooseSubject: row tag: tag.
	(subject isInvalid and: [row allDummies])
		ifTrue: 
			[row constant nearZero
				ifTrue: [subject := tag marker]
				ifFalse: [self error: 'unsatisfiable constraint']].
	subject isInvalid
		ifTrue: [(self addWithArtificialVariable: row) ifFalse: [self error: 'unsatisfiable constraint']]
		ifFalse: 
			[row solveFor: subject.
			self substitute: subject row: row.
			rows at: subject put: row].
	constraints at: aConstraint put: tag.
	self optimize: objective!

addEditVariable: aVariable strength: aNumber
	| strength expression constraint tag info |
	edits at: aVariable ifPresent: [:edit | self error: 'duplicate edit variable'].
	strength := Strength clip: aNumber.
	strength = Strength required ifTrue: [self error: 'edit variable can''''t have required strength'].
	expression := Expression new: (Array with: aVariable).
	constraint := Constraint
				lhs: expression
				equalsRhs: nil
				strength: strength.
	self addConstraint: constraint.
	tag := constraints at: constraint.
	info := EditInfo new
				tag: tag;
				constraint: constraint;
				constant: 0.0.
	edits at: aVariable put: info!

addWithArtificialVariable: aRow
	| tempArtificial success row |
	tempArtificial := VariableSymbol slack.
	rows at: tempArtificial put: aRow copy.
	artificial := aRow copy.
	self optimize: artificial.
	success := artificial constant nearZero.
	artificial := nil.
	row := rows at: tempArtificial ifAbsent: [nil].
	row
		ifNotNil: 
			[| entering |
			rows copy keysAndValuesDo: [:aSymbol :aRow | row = aRow ifTrue: [rows removeKey: aSymbol]].
			row isConstant ifTrue: [^success].
			entering := self anyPivotableSymbol: row.
			entering isInvalid ifTrue: [^false].
			row solveForLhs: tempArtificial rhs: entering.
			self substitute: entering row: row.
			rows at: entering put: row].
	rows keysAndValuesDo: [:symbol :row | row removeSymbol: tempArtificial].
	objective removeSymbol: tempArtificial.
	^success!

anyPivotableSymbol: row
	row cells
		keysAndValuesDo: [:symbol :value | (symbol isSlack or: [symbol isError]) ifTrue: [^symbol]].
	^VariableSymbol invalid!

chooseSubject: aRow tag: aTag
	| marker other |
	aRow cells keysAndValuesDo: [:symbol :value | symbol isExternal ifTrue: [^symbol]].
	marker := aTag marker.
	(marker isSlack or: [marker isError])
		ifTrue: [(aRow coefficientFor: marker) < 0.0 ifTrue: [^marker]].
	other := aTag other.
	(other isSlack or: [other isError]) ifTrue: [(aRow coefficientFor: other) < 0.0 ifTrue: [^other]].
	^VariableSymbol invalid!

constraints
	^constraints!

createConstraintLhs: lhs operator: operator rhs: rhs
	^self
		createConstraintLhs: lhs
		operator: operator
		rhs: rhs
		strength: Strength required!

createConstraintLhs: lhs operator: operator rhs: rhs strength: strength
	| constraint |
	constraint := Constraint
				lhs: lhs
				operator: operator
				rhs: rhs
				strength: strength.
	self addConstraint: constraint.
	^constraint!

createRow: aConstraint
	| expression row terms strength tag operator |
	expression := aConstraint expression.
	row := Row new constant: expression constant.
	terms := expression terms.
	terms keysAndValuesDo: 
			[:variable :coefficient |
			coefficient nearZero
				ifFalse: 
					[| symbol variableRow |
					symbol := self getVariableSymbol: variable.
					variableRow := rows at: symbol ifAbsent: [nil].
					variableRow
						ifNotNil: [row insertRow: variableRow coefficient: coefficient]
						ifNil: [row insertSymbol: symbol coefficient: coefficient]]].
	strength := aConstraint strength.
	tag := ConstraintTag marker: VariableSymbol invalid other: VariableSymbol invalid.
	operator := aConstraint operator.
	(operator = Constraint lessThanOrEqual or: [operator = Constraint greaterThanOrEqual])
		ifTrue: 
			[| coefficient slack |
			coefficient := operator = Constraint lessThanOrEqual ifTrue: [1.0] ifFalse: [-1.0].
			slack := VariableSymbol slack.
			tag marker: slack.
			row insertSymbol: slack coefficient: coefficient.
			strength < Strength required
				ifTrue: 
					[| error |
					error := VariableSymbol error.
					tag other: error.
					row insertSymbol: error coefficient: coefficient negated.
					objective insertSymbol: error coefficient: strength]].
	operator = Constraint equals
		ifTrue: 
			[strength < Strength required
				ifTrue: 
					[| errorPlus errorMinus |
					errorPlus := VariableSymbol error.
					errorMinus := VariableSymbol error.
					tag
						marker: errorPlus;
						other: errorMinus.
					row
						insertSymbol: errorPlus coefficient: -1.0;
						insertSymbol: errorMinus coefficient: 1.0.
					objective
						insertSymbol: errorPlus coefficient: strength;
						insertSymbol: errorMinus coefficient: strength]
				ifFalse: 
					[| dummy |
					dummy := VariableSymbol dummy.
					tag marker: dummy.
					row insertSymbol: dummy]].
	row constant < 0.0 ifTrue: [row reverseSign].
	^RowCreation row: row tag: tag!

dualOptimize
	[infeasibleRows isEmpty not] whileTrue: 
			[| leaving row |
			leaving := infeasibleRows removeLast.
			row := rows at: leaving ifAbsent: [nil].
			(row notNil and: [row constant < 0.0])
				ifTrue: 
					[| entering |
					entering := self getDualEnteringSymbol: row.
					entering ifNil: [self error: 'dual optimize failed'].
					rows removeKey: leaving.
					row solveForLhs: leaving rhs: entering.
					self substitute: entering row: row.
					rows at: entering put: row]]!

dump
	| stream |
	stream := ReadWriteStream on: OrderedCollection new.
	stream cr;
		nextPutAll: 'Objective'; cr;
		nextPutAll: '---------'; cr.
	objective dumpOn: stream.
	stream cr;
		nextPutAll: 'Tableau'; cr;
		nextPutAll: '---------'; cr.
	rows keysAndValuesDo: [:key :value | key dumpOn: stream. stream nextPutAll: ' | '. value dumpOn: stream. stream cr].
	stream cr;
		nextPutAll: 'Infeasible'; cr;
		nextPutAll: '---------'; cr.
	infeasibleRows do: [:each | each dumpOn: stream. stream cr].
	stream cr;
		nextPutAll: 'Variables'; cr;
		nextPutAll: '---------'; cr.
	variables do: [:each | each dumpOn: stream. stream cr].
	stream cr;
		nextPutAll: 'Edit Variables'; cr;
		nextPutAll: '---------'; cr.
	edits keysDo: [:each | each dumpOn: stream. stream cr].
	stream cr;
		nextPutAll: 'Constraints'; cr;
		nextPutAll: '---------'; cr.
	constraints keysDo: [:each | each dumpOn: stream. stream cr].
	^stream contents asString!

editVariables
	^edits!

getDualEnteringSymbol: aRow
	| ratio entering |
	ratio := SmallInteger maximum.
	aRow cells keysAndValuesDo: 
			[:symbol :value |
			(value > 0.0 and: [symbol isDummy not])
				ifTrue: 
					[| coefficient r |
					coefficient := objective coefficientFor: symbol.
					r := coefficient / value.
					r < ratio
						ifTrue: 
							[ratio := r.
							entering := symbol]]].
	^entering!

getEnteringSymbol: aRow
	aRow cells
		keysAndValuesDo: [:symbol :value | (value < 0.0 and: [symbol isDummy not]) ifTrue: [^symbol]].
	^nil!

getLeavingSymbol: aSymbol
	| ratio found |
	ratio := SmallInteger maximum.
	rows keysAndValuesDo: 
			[:symbol :row |
			symbol isExternal
				ifFalse: 
					[| temp |
					temp := row coefficientFor: aSymbol.
					temp < 0.0
						ifTrue: 
							[| tempRatio |
							tempRatio := row constant negated / temp.
							tempRatio < ratio
								ifTrue: 
									[ratio := tempRatio.
									found := symbol]]]].
	^found!

getMarkerLeavingAssociation: marker
	| r1 r2 first second third |
	r1 := Float fmax.
	r2 := Float fmax.
	rows keysAndValuesDo: 
			[:symbol :row |
			| coefficient |
			coefficient := row coefficientFor: marker.
			coefficient = 0.0
				ifFalse: 
					[symbol isExternal
						ifTrue: [third := symbol -> row]
						ifFalse: 
							[coefficient < 0.0
								ifTrue: 
									[| ratio |
									ratio := row constant negated / coefficient.
									ratio < r1
										ifTrue: 
											[r1 := ratio.
											first := symbol -> row]]
								ifFalse: 
									[| ratio |
									ratio := row constant / coefficient.
									ratio < r2
										ifTrue: 
											[r2 := ratio.
											second := symbol -> row]]]]].
	first ifNotNil: [^first].
	second ifNotNil: [^second].
	^third!

getVariableSymbol: aVariable
	^variables at: aVariable ifAbsentPut: [VariableSymbol external]!

hasConstraint: aConstraint
	^constraints includesKey: aConstraint!

hasEditVariable: aVariable
	^edits includesKey: aVariable!

infeasibleRows
	^infeasibleRows!

initialize
	super initialize.
	constraints := OrderedDictionary new.
	rows := OrderedDictionary new.
	variables := OrderedDictionary new.
	edits := OrderedDictionary new.
	infeasibleRows := OrderedCollection new.
	objective := Row new!

objective
	^objective!

optimize: aRow
	[true] whileTrue: 
			[| enteringSymbol leavingSymbol leavingRow |
			enteringSymbol := self getEnteringSymbol: aRow.
			enteringSymbol ifNil: [^self].
			leavingSymbol := self getLeavingSymbol: enteringSymbol.
			leavingSymbol ifNil: [self error: 'the objective is unbounded'].
			leavingRow := rows removeKey: leavingSymbol.
			leavingRow solveForLhs: leavingSymbol rhs: enteringSymbol.
			self substitute: enteringSymbol row: leavingRow.
			rows at: enteringSymbol put: leavingRow]!

removeConstraint: aConstraint
	| constraintTag marker value |
	constraintTag := constraints removeKey: aConstraint ifAbsent: [^self].
	self removeConstraintEffects: aConstraint tag: constraintTag.
	marker := constraintTag marker.
	value := rows removeKey: marker ifAbsent: [nil].
	value
		ifNil: 
			[| association row leaving |
			association := self getMarkerLeavingAssociation: marker.
			association ifNil: [self error: 'failed to find leaving row'].
			leaving := association key.
			row := rows removeKey: leaving.
			row solveForLhs: leaving rhs: marker.
			self substitute: marker row: row].
	self optimize: objective!

removeConstraintEffects: constraint tag: tag
	tag marker isError ifTrue: [self removeMarkerEffects: tag marker strength: constraint strength].
	tag other isError ifTrue: [self removeMarkerEffects: tag other strength: constraint strength]!

removeEditVariable: aVariable
	| edit |
	edit := edits removeKey: aVariable ifAbsent: [self error: 'unknown edit variable'].
	self removeConstraint: edit constraint!

removeMarkerEffects: marker strength: strength
	| row |
	row := rows at: marker ifAbsent: [nil].
	row
		ifNotNil: [objective insertRow: row coefficient: strength negated]
		ifNil: [objective insertSymbol: marker coefficient: strength negated]!

rows
	^rows!

substitute: aSymbol row: aRow
	rows keysAndValuesDo: 
			[:symbol :row |
			row substitute: aSymbol row: aRow.
			(row constant < 0.0 and: [symbol isExternal not]) ifTrue: [infeasibleRows add: symbol]].
	objective substitute: aSymbol row: aRow.
	artificial ifNotNil: [artificial substitute: aSymbol row: aRow]!

suggest: aVariable value: aNumber
	| editInfo delta marker row other |
	editInfo := edits at: aVariable ifAbsent: [self error: 'unknown edit variable'].
	delta := aNumber - editInfo constant.
	editInfo constant: aNumber.
	marker := editInfo tag marker.
	row := rows at: marker ifAbsent: [nil].
	row
		ifNotNil: 
			[(row add: delta negated) < 0.0 ifTrue: [infeasibleRows add: marker]. 
			self dualOptimize.
			^self].
	other := editInfo tag other.
	row := rows at: other ifAbsent: [nil].
	row
		ifNotNil: 
			[(row add: delta) < 0.0 ifTrue: [infeasibleRows add: other].
			self dualOptimize.
			^self].
	rows keysAndValuesDo: 
			[:symbol :row |
			| coefficient |
			coefficient := row coefficientFor: marker.
			((coefficient ~= 0.0 and: [(row add: delta * coefficient) < 0.0]) and: [symbol isExternal not])
				ifTrue: [infeasibleRows add: symbol]].
	self dualOptimize!

tryAddingEditVariable: aVariable strength: aFloat
	(edits includesKey: aVariable) ifFalse: [self addEditVariable: aVariable strength: aFloat]!

updateVariables
	variables keysAndValuesDo: 
			[:variable :symbol |
			| row | 
			row := rows at: symbol ifAbsent: [nil].
			row ifNotNil: [variable value: row constant] ifNil: [variable value: 0.0]]!

variables
	^variables! !

!ConstraintSolver categoriesForMethods!
addConstraint:!public! !
addEditVariable:strength:!public! !
addWithArtificialVariable:!private! !
anyPivotableSymbol:!private! !
chooseSubject:tag:!private! !
constraints!public! !
createConstraintLhs:operator:rhs:!public! !
createConstraintLhs:operator:rhs:strength:!public! !
createRow:!private! !
dualOptimize!private! !
dump!public! !
editVariables!public! !
getDualEnteringSymbol:!private! !
getEnteringSymbol:!private! !
getLeavingSymbol:!private! !
getMarkerLeavingAssociation:!private! !
getVariableSymbol:!private! !
hasConstraint:!public! !
hasEditVariable:!public! !
infeasibleRows!public! !
initialize!public! !
objective!public! !
optimize:!private! !
removeConstraint:!public! !
removeConstraintEffects:tag:!private! !
removeEditVariable:!public! !
removeMarkerEffects:strength:!private! !
rows!public! !
substitute:row:!private! !
suggest:value:!public! !
tryAddingEditVariable:strength:!public! !
updateVariables!public! !
variables!public! !
!

!ConstraintSolver class methodsFor!

example
	| solver left width right |
	solver := ConstraintSolver new.
	left := Variable new.
	width := Variable new.
	solver
		addEditVariable: left strength: Strength strong;
		addEditVariable: width strength: Strength strong;
		suggest: left value: 100;
		suggest: width value: 400.
	right := Variable new.
	solver addConstraint: (Constraint new
				expression: (Expression new: (Array
									with: (Array with: -1 with: right)
									with: left
									with: width));
				operator: #=).
	solver updateVariables.
	^solver!

new
	^super new initialize! !

!ConstraintSolver class categoriesForMethods!
example!public! !
new!public! !
!

ConstraintTag guid: (GUID fromString: '{d60cc730-02df-43be-85fc-358f816cd803}')!

ConstraintTag comment: ''!

!ConstraintTag categoriesForClass!Kernel-Objects! !

!ConstraintTag methodsFor!

marker
	^marker!

marker: anObject
	marker := anObject!

other
	^other!

other: anObject
	other := anObject! !

!ConstraintTag categoriesForMethods!
marker!accessing!public! !
marker:!accessing!public! !
other!accessing!public! !
other:!accessing!public! !
!

!ConstraintTag class methodsFor!

marker: aSymbol other: anotherSymbol
	^self new
		marker: aSymbol;
		other: anotherSymbol! !

!ConstraintTag class categoriesForMethods!
marker:other:!public! !
!

EditInfo guid: (GUID fromString: '{d21c1596-61b2-4fde-8e0e-c7ec34a30475}')!

EditInfo comment: ''!

!EditInfo categoriesForClass!Unclassified! !

!EditInfo methodsFor!

constant
	^constant!

constant: anObject
	constant := anObject!

constraint
	^constraint!

constraint: anObject
	constraint := anObject!

dumpOn: aReadWriteStream 
	^Error notYetImplemented!

tag
	^tag!

tag: anObject
	tag := anObject! !

!EditInfo categoriesForMethods!
constant!accessing!public! !
constant:!accessing!public! !
constraint!accessing!public! !
constraint:!accessing!public! !
dumpOn:!public! !
tag!accessing!public! !
tag:!accessing!public! !
!

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

RowCreation guid: (GUID fromString: '{281e31ac-7f63-4453-99d1-2dd5e0dff035}')!

RowCreation comment: ''!

!RowCreation categoriesForClass!Unclassified! !

!RowCreation methodsFor!

row
	^row!

row: anObject
	row := anObject!

tag
	^tag!

tag: anObject
	tag := anObject! !

!RowCreation categoriesForMethods!
row!accessing!public! !
row:!accessing!public! !
tag!accessing!public! !
tag:!accessing!public! !
!

!RowCreation class methodsFor!

row: aRow tag: aTag
	^self new
		row: aRow;
		tag: aTag! !

!RowCreation class categoriesForMethods!
row:tag:!public! !
!

Strength guid: (GUID fromString: '{0aa4d2df-37c0-4667-b876-367709d848da}')!

Strength comment: ''!

!Strength categoriesForClass!Kernel-Objects! !

!Strength class methodsFor!

clip: aNumber
	^0.0 max: (self required min: aNumber)!

createStrong: strong medium: medium weak: weak 
	^self createStrong: strong medium: medium weak: weak weight: 1.0!

createStrong: strong medium: medium weak: weak weight: weight
	| result |
	result := 0.0.
	result := result + ((0.0 max: (1000.0 min: strong * weight)) * 1000000.0).
	result := result + ((0.0 max: (1000.0 min: medium * weight)) * 1000.0).
	result := result + (0.0 max: (1000.0 min: weak * weight)).
	^result!

medium
	^self createStrong: 0.0 medium: 1.0 weak: 0.0!

required
	^self createStrong: 1000.0 medium: 1000.0 weak: 1000.0!

strong
	^self createStrong: 1.0 medium: 0.0 weak: 0.0!

weak
	^self createStrong: 0.0 medium: 0.0 weak: 1.0! !

!Strength class categoriesForMethods!
clip:!public! !
createStrong:medium:weak:!public! !
createStrong:medium:weak:weight:!public! !
medium!public! !
required!public! !
strong!public! !
weak!public! !
!

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

OrderedDictionary guid: (GUID fromString: '{bf555aad-801c-4bd1-8d55-8f0af176345d}')!

OrderedDictionary comment: ''!

!OrderedDictionary categoriesForClass!Kernel-Objects! !

!OrderedDictionary methodsFor!

= anObject
	"Returns true if the receiver and argument are identical, or if they
	are both some kind of order-preserving dictionary and if they have
	the same associations regardless of order."

	self == anObject ifTrue: [^true].
	(anObject isOrderPreservingDictionary
		and: [self isIdentityDictionary = anObject isIdentityDictionary and: [self size = anObject size]])
			ifFalse: [^false].
	dictionary
		associationsDo: [:each | (anObject at: each key ifAbsent: [^false]) = each value ifFalse: [^false]].
	^true!

add: anAssociation
	| oldSize |
	oldSize := dictionary size.
	dictionary add: anAssociation.
	dictionary size > oldSize
		ifTrue: 
			[orderedKeys size > oldSize ifFalse: [self growOrderedKeys].
			orderedKeys at: oldSize + 1 put: anAssociation key].
	^anAssociation!

addAll: anAssociationCollection
	"Since Collection implements #associationsDo:, this method can accept
	any collection of associations including Arrays and OrderedCollections"

	anAssociationCollection associationsDo: [:each | self add: each].
	^anAssociationCollection!

associationAt: aKey
	^ dictionary associationAt: aKey!

associationAt: aKey ifAbsent: aBlock
	^dictionary associationAt: aKey ifAbsent: aBlock!

associationAt: aKey ifPresent: aBlock
	"Squeak and GS do not have #associationAt:ifPresent: so it
	is reimplemented for portability"

	^aBlock cull: (dictionary associationAt: aKey ifAbsent: [^nil])!

associations
	| associations i |
	associations := Array new: self size.
	i := 1.
	self associationsDo: 
			[:each |
			associations at: i put: each.
			i := i + 1].
	^associations!

associationsDo: aBlock
	self keysDo: [:each | aBlock value: (self associationAt: each)]!

associationsSelect: aBlock
	^self speciesNewFrom: (self associations select: aBlock)!

at: aKey
	^dictionary at: aKey!

at: aKey ifAbsent: aBlock
	^dictionary at: aKey ifAbsent: aBlock!

at: aKey ifAbsentPut: aBlock
	^self at: aKey ifAbsent: [self at: aKey put: aBlock value]!

at: aKey ifAbsentPutValue: aValue
	^(self includesKey: aKey) ifTrue: [self at: aKey] ifFalse: [self at: aKey put: aValue]!

at: aKey ifPresent: aBlock
	"Squeak and GS don't use #cull: for the ifPresent:
	block, so it is reimplemented for portability"

	^aBlock cull: (self at: aKey ifAbsent: [^nil])!

at: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock
	"Squeak and GS don't use #cull: for the ifPresent:
	block, so it is reimplemented for portability"

	self at: aKey ifPresent: [:value | ^aPresentBlock cull: value].
	^anAbsentBlock value!

at: aKey put: aValue
	| oldSize |
	oldSize := dictionary size.
	dictionary at: aKey put: aValue.
	dictionary size > oldSize
		ifTrue: 
			[orderedKeys size > oldSize ifFalse: [self growOrderedKeys].
			orderedKeys at: oldSize + 1 put: aKey].
	^aValue!

capacity
	^dictionary capacity!

collect: aBlock
	^self speciesNewFrom: (self associations collect: [:each | each key -> (aBlock value: each value)])!

copyEmpty
	"Squeak and GS don't have Collection>>#copyEmpty:, so it is
	reimplemented for portability"

	^self species new!

dictionary
	^dictionary!

dictionaryClass
	^Dictionary!

do: aBlock
	self valuesDo: aBlock!

errorInvalidIndex: anIndex
	"Squeak and GS do not have SubscriptOutOfBounds, so Error is used
	for portability"

	self error: 'Invalid index: ' , anIndex printString!

errorValueNotFound: aValue
	"Squeak and GS do not have ValueNotFound, so Error is used
	for portability"

	self error: 'Value not found'!

growOrderedKeys
	orderedKeys := (Array new: ((orderedKeys size * 1.5) asInteger max: 10))
				replaceFrom: 1
				to: orderedKeys size
				with: orderedKeys
				startingAt: 1!

hash
	^dictionary hash!

identityIndexOfKey: aKey
	^self identityIndexOfKey: aKey ifAbsent: [0]!

identityIndexOfKey: aKey ifAbsent: aBlock
	1 to: self size do: [:i | (orderedKeys at: i) == aKey ifTrue: [^i]].
	^aBlock value!

includes: anObject
	^dictionary includes: anObject!

includesAssociation: anAssociation
	"IndentityDictionary>>includesAssociation: works differently on GS
	testing both key and value identity, so it is reimplemented here
	to behave like Pharo/Squeak"

	^(dictionary at: anAssociation key ifAbsent: [^false]) = anAssociation value!

includesIdentity: anObject
	"GS does not have includesIdentity:"

	self valuesDo: [:each | each == anObject ifTrue: [^true]].
	^false!

includesKey: aKey
	^dictionary includesKey: aKey!

indexOfKey: aKey
	^self indexOfKey: aKey ifAbsent: [0]!

indexOfKey: aKey ifAbsent: aBlock
	1 to: self size do: [:i | (orderedKeys at: i) = aKey ifTrue: [^i]].
	^aBlock value!

initialize
	super initialize.
	dictionary := self dictionaryClass!

initialize: aCapacity
	dictionary := self dictionaryClass new: aCapacity.
	orderedKeys := Array new: aCapacity!

isDictionary
	^ true!

isEmpty
	"Squeak's Collection>>#isEmpty is inefficient"

	^self size = 0!

isIdentityDictionary
	^ false!

isOrderPreservingDictionary
	^ true!

keyAtIdentityValue: aValue
	^self keyAtIdentityValue: aValue ifAbsent: [self errorValueNotFound: aValue]!

keyAtIdentityValue: aValue ifAbsent: aBlock
	"GS does not have keyAtIdentityValue:ifAbsent:"

	self keysAndValuesDo: [:key :value | value == aValue ifTrue: [^key]].
	^aBlock value!

keyAtIndex: anIndex
	^self keyAtIndex: anIndex ifAbsent: [self errorInvalidIndex: anIndex]!

keyAtIndex: anIndex ifAbsent: aBlock
	(anIndex > 0 and: [anIndex <= self size])
		ifTrue: [^orderedKeys at: anIndex]
		ifFalse: [^aBlock value]!

keyAtValue: aValue
	^dictionary keyAtValue: aValue!

keyAtValue: aValue ifAbsent: aBlock
	^dictionary keyAtValue: aValue ifAbsent: aBlock!

keyForIdentity: anObject
	"Reimplemented for portability"

	self keysAndValuesDo: [:key :value | value == anObject ifTrue: [^key]].
	^nil!

keys
	^orderedKeys copyFrom: 1 to: self size!

keysAndValuesDo: aBlock
	self keysDo: [:each | aBlock value: each value: (self at: each)]!

keysAndValuesRemove: aTwoArgumentBlock
	| removedAssociations |
	removedAssociations := OrderedCollection new.
	self associationsDo: 
			[:each |
			(aTwoArgumentBlock value: each key value: each value) ifTrue: [removedAssociations add: each]].
	removedAssociations do: [:each | self removeKey: each key]!

keysDo: aBlock
	"Use to:do: for speed"

	1 to: self size do: [:i | aBlock value: (orderedKeys at: i)]!

keysSortedSafely
	"GS's #keysSortedSafely returns a SortedCollection instead of
	an Array, so this is reimplemented directly for portability, and
	'self keys' is used instead of 'dictionary keys', because GS's
	#keys returns a Set which can't be sorted"

	^self keys sort!

orderedKeys
	^orderedKeys!

orderedKeysIdentityIndexOf: aKey
	"GS does not have #identityIndexOf:"

	1 to: orderedKeys size do: [:i | (orderedKeys at: i) == aKey ifTrue: [^i]].
	^0!

orderedKeysIndexOf: aKey
	^orderedKeys indexOf: aKey!

orderedKeysRemove: aRemovedKey
	| index |
	index := self orderedKeysIndexOf: aRemovedKey.

	"shift every remaining key after to the left by one"
	orderedKeys
		replaceFrom: index
		to: self size
		with: orderedKeys
		startingAt: index + 1.

	"one key was removed and the rest shifted, so nil what was the last
	key slot before removing and shifting"
	orderedKeys at: self size + 1 put: nil!

postCopy
	orderedKeys := orderedKeys copy.
	dictionary := dictionary copy!

printElementsOn: aStream
	aStream nextPut: $(.
	self size > 100
		ifTrue: 
			[aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: 
			[self associations withIndexDo: 
					[:each :i |
					aStream
						print: each key;
						nextPutAll: '->';
						print: each value.
					i < self size ifTrue: [aStream space]]].
	aStream nextPut: $)!

remove: anObject ifAbsent: aBlock
	self shouldNotImplement!

removeAll
	1 to: self size do: [:i | orderedKeys at: i put: nil].
	dictionary removeAll!

removeKey: aKey
	| value |
	value := dictionary removeKey: aKey.
	self orderedKeysRemove: aKey.
	^value!

removeKey: aKey ifAbsent: aBlock
	| oldSize value |
	oldSize := dictionary size.
	value := dictionary removeKey: aKey ifAbsent: [nil].
	dictionary size < oldSize ifTrue: [self orderedKeysRemove: aKey].
	^value ifNil: [aBlock value]!

removeKeys: aKeyCollection
	"Fast removal of multiple keys; returns self to avoid
	having to create a removed value collection and does not
	raise errors."

	aKeyCollection size > 1
		ifTrue: 
			[| oldSize newOrderedKeys newOrderedKeysIndex |
			oldSize := self size.
			aKeyCollection do: [:each | dictionary removeKey: each ifAbsent: [nil]].
			newOrderedKeys := Array new: oldSize.
			newOrderedKeysIndex := 0.
			1 to: oldSize
				do: 
					[:i |
					| key |
					(dictionary includesKey: (key := orderedKeys at: i))
						ifTrue: [newOrderedKeys at: (newOrderedKeysIndex := newOrderedKeysIndex + 1) put: key]].
			orderedKeys := newOrderedKeys]
		ifFalse: 
			[aKeyCollection size = 1
				ifTrue: 
					["use #anyOne, because it can be a Set"
					self removeKey: aKeyCollection anyOne ifAbsent: [nil]]]!

select: aBlock
	^self speciesNewFrom: (self associations select: [:each | aBlock value: each value])!

size
	^ dictionary size!

speciesNewFrom: anAssociationCollection
	^self species newFrom: anAssociationCollection!

values
	^self associations collect: [:each | each value]!

valuesDo: aBlock
	self keysDo: [:each | aBlock value: (self at: each)]! !

!OrderedDictionary categoriesForMethods!
=!comparing!public! !
add:!adding!public! !
addAll:!adding!public! !
associationAt:!accessing!public! !
associationAt:ifAbsent:!accessing!public! !
associationAt:ifPresent:!accessing!public! !
associations!accessing!public! !
associationsDo:!enumerating!public! !
associationsSelect:!enumerating!public! !
at:!accessing!public! !
at:ifAbsent:!accessing!public! !
at:ifAbsentPut:!accessing!public! !
at:ifAbsentPutValue:!public! !
at:ifPresent:!accessing!public! !
at:ifPresent:ifAbsent:!accessing!public! !
at:put:!accessing!public! !
capacity!accessing!public! !
collect:!enumerating!public! !
copyEmpty!copying!public! !
dictionary!private! !
dictionaryClass!accessing!public! !
do:!enumerating!public! !
errorInvalidIndex:!private! !
errorValueNotFound:!private! !
growOrderedKeys!private! !
hash!comparing!public! !
identityIndexOfKey:!accessing!public! !
identityIndexOfKey:ifAbsent:!accessing!public! !
includes:!public!testing! !
includesAssociation:!public!testing! !
includesIdentity:!public!testing! !
includesKey:!public!testing! !
indexOfKey:!accessing!public! !
indexOfKey:ifAbsent:!accessing!public! !
initialize!public! !
initialize:!initialization!public! !
isDictionary!public!testing! !
isEmpty!public!testing! !
isIdentityDictionary!public!testing! !
isOrderPreservingDictionary!public!testing! !
keyAtIdentityValue:!accessing!public! !
keyAtIdentityValue:ifAbsent:!accessing!public! !
keyAtIndex:!accessing!public! !
keyAtIndex:ifAbsent:!accessing!public! !
keyAtValue:!accessing!public! !
keyAtValue:ifAbsent:!accessing!public! !
keyForIdentity:!accessing!public! !
keys!accessing!public! !
keysAndValuesDo:!enumerating!public! !
keysAndValuesRemove:!public!removing! !
keysDo:!enumerating!public! !
keysSortedSafely!accessing!public! !
orderedKeys!private! !
orderedKeysIdentityIndexOf:!private! !
orderedKeysIndexOf:!private! !
orderedKeysRemove:!private! !
postCopy!copying!public! !
printElementsOn:!printing!public! !
remove:ifAbsent:!public!removing! !
removeAll!public!removing! !
removeKey:!public!removing! !
removeKey:ifAbsent:!public!removing! !
removeKeys:!public!removing! !
select:!enumerating!public! !
size!accessing!public! !
speciesNewFrom:!private! !
values!private! !
valuesDo:!private! !
!

!OrderedDictionary class methodsFor!

new
	^self new: 10!

new: aCapacity
	^self basicNew initialize: aCapacity!

newFrom: anAssociationCollection
	| newDictionary |
	newDictionary := self new: anAssociationCollection size.
	anAssociationCollection associationsDo: [:each | newDictionary at: each key put: each value].
	^newDictionary!

newFromPairs: aSequenceableCollection
	| newDictionary |
	newDictionary := self new: (aSequenceableCollection size / 2) floor.
	1 to: aSequenceableCollection size - 1
		by: 2
		do: [:i | newDictionary at: (aSequenceableCollection at: i) put: (aSequenceableCollection at: i + 1)].
	^newDictionary! !

!OrderedDictionary class categoriesForMethods!
new!instance creation!public! !
new:!instance creation!public! !
newFrom:!instance creation!public! !
newFromPairs:!instance creation!public! !
!

ConstraintSolverTests guid: (GUID fromString: '{4976a405-aa3e-4025-a455-1dcbff50d798}')!

ConstraintSolverTests comment: ''!

!ConstraintSolverTests categoriesForClass!Kernel-Objects! !

!ConstraintSolverTests methodsFor!

testAddDuplicateEditVariable
	| solver variable |
	solver := ConstraintSolver new.
	variable := Variable new.
	self assert: (solver hasEditVariable: variable) not.
	solver addEditVariable: variable strength: Strength strong.
	self assert: (solver hasEditVariable: variable).
	[solver addEditVariable: variable strength: Strength strong] on: Error
		do: [:exception | self assert: exception messageText equals: 'duplicate edit variable']!

testAddEditVariable
	| solver variable |
	solver := ConstraintSolver new.
	variable := Variable new.
	solver addEditVariable: variable strength: Strength strong.
	self assert: (solver hasEditVariable: variable)!

testAddUnknownEditVariable
	| solver variable |
	solver := ConstraintSolver new.
	variable := Variable new.
	solver addEditVariable: variable strength: Strength strong.
	self assert: (solver hasEditVariable: variable).
	solver removeEditVariable: variable.
	self assert: (solver hasEditVariable: variable) not.
	[solver removeEditVariable: variable] on: Error
		do: [:exception | self assert: exception messageText equals: 'unknown edit variable']!

testCompetingConstraints
	| solver x |
	solver := ConstraintSolver new.
	x := Variable new.
	solver
		addConstraint: (Constraint
					lhs: x
					equalsRhs: 1
					strength: Strength weak);
		addConstraint: (Constraint
					lhs: x
					equalsRhs: 2
					strength: Strength required);
		updateVariables.
	self assert: x value equals: 2!

testConstraintExpression
	| solver expression constraint |
	solver := ConstraintSolver new.
	expression := Expression new: {10}.
	constraint := Constraint lhs: expression equalsRhs: nil.
	self assert: constraint expression equals: expression!

testConstraintNewSyntax
	| solver left width top height right bottom centerX leftOfCenterX |
	solver := ConstraintSolver new.
	left := Variable new.
	width := Variable new.
	top := Variable new.
	height := Variable new.
	right := Variable new.
	bottom := Variable new.
	centerX := Variable new.
	leftOfCenterX := Variable new.
	solver
		addEditVariable: left strength: Strength strong;
		addEditVariable: width strength: Strength strong;
		addEditVariable: top strength: Strength strong;
		addEditVariable: height strength: Strength strong;
		suggest: left value: 0;
		suggest: width value: 500;
		suggest: top value: 0;
		suggest: height value: 300.

	"right == left.plus(width) => 500"
	solver
		addConstraint: (Constraint lhs: right equalsRhs: (left plus: width));
		updateVariables.
	self assert: right value equals: 500.

	"centerX == left.plus(width.divide(2)) => 250"
	solver
		addConstraint: (Constraint lhs: centerX equalsRhs: (left plus: (width divide: 2)));
		updateVariables.
	self assert: centerX value equals: 250.

	"leftOfCenterX == left.plus(width.divide(2)).minus(10) => 240"
	solver
		addConstraint: (Constraint lhs: leftOfCenterX equalsRhs: ((left plus: (width divide: 2)) minus: 10));
		updateVariables.
	self assert: leftOfCenterX value equals: 240.

	"createConstraint(bottom, Operator.Eq, top.plus(height)) => 300"
	solver
		createConstraintLhs: bottom
			operator: #=
			rhs: (top plus: height);
		updateVariables.
	self assert: bottom value equals: 300!

testConstraintOperator
	| constraint |
	constraint := Constraint lhs: (Expression new: {10}) greaterThanOrEqualRhs: nil.
	self assert: constraint operator equals: #>=!

testConstraintRawSyntax
	| solver left leftConstraint width right rightConstraint centerX centerXConstraint |
	solver := ConstraintSolver new.

	"Constant left constraint (10)"
	left := Variable new.
	leftConstraint := Constraint new
				expression: (Expression new: (Array with: (Array with: -1 with: left) with: 10));
				operator: #=.
	solver
		addConstraint: leftConstraint;
		updateVariables.
	self assert: left value equals: 10.

	"Width edit variable (200)"
	width := Variable new.
	solver
		addEditVariable: width strength: Strength strong;
		suggest: width value: 200;
		updateVariables.
	self assert: width value equals: 200.

	"Right === left + width (210)"
	right := Variable new.
	rightConstraint := Constraint new
				expression: (Expression new: (Array
									with: (Array with: -1 with: right)
									with: left
									with: width));
				operator: #=.
	solver
		addConstraint: rightConstraint;
		updateVariables.
	self assert: right value equals: 210.

	"centerX === left + (width / 2) (110)"
	centerX := Variable new.
	centerXConstraint := Constraint new
				expression: (Expression new: (Array
									with: (Array with: -1 with: centerX)
									with: left
									with: (Array with: 0.5 with: width)));
				operator: #=.
	solver
		addConstraint: centerXConstraint;
		updateVariables.
	self assert: centerX value equals: 110!

testConstraintStrengthDefault
	| constraint |
	constraint := Constraint lhs: (Expression new: {1}) equalsRhs: nil.
	self assert: constraint strength equals: Strength required!

testConstraintStrengthMedium
	| constraint |
	constraint := Constraint lhs: (Expression new: {10}) lessThanOrEqualRhs: nil strength: Strength medium.
	self assert: constraint strength equals: Strength medium!

testCreateSolver
	| solver |
	solver := ConstraintSolver new.
	self assert: solver notNil!

testInvalidExpressionArray1
	
	[Expression new: (Array with: (Array with: Variable new with: -1)).
	self assert: false] on: Error
			do: [:exception | self assert: exception messageText equals: 'array item 1 must be a number']!

testInvalidExpressionArray2
	
	[Expression new: (Array with: (Array with: -1 with: 100)).
	self assert: false] on: Error
			do: [:exception | self assert: exception messageText equals: 'array item 2 must be a variable or expression']!

testNewExpression
	self assert: Expression new notNil!

testNewExpressionAdd
	self assert: (Expression new: {Expression new: {10}. Expression new: {20}}) constant equals: 30!

testNewExpressionComplex
	self assert: (Expression new: {20. {0.5. Expression new: {10}}. -10}) constant equals: 15!

testNewExpressionNegatedVariable
	self assert: (Expression new: {{-1. Variable new}}) notNil!

testNewExpressionNegation
	self assert: (Expression new: {{-1. Expression new: {10}}}) constant equals: -10!

testNewExpressionNumbers
	self assert: (Expression new: {10. 20. 30. 40}) constant equals: 100!

testNewExpressionVariable
	self assert: (Expression new: (Array with: Variable new)) notNil!

testNewVariableName
	| testName variable |
	testName := 'somename'.
	variable := Variable new name: testName.
	self assert: testName equals: variable name!

testNewVariableValue
	| variable |
	variable := Variable new.
	self
		assert: variable notNil;
		assert: 0 equals: variable value!

testRemoveEditVariable
	| solver variable |
	solver := ConstraintSolver new.
	variable := Variable new.
	solver addEditVariable: variable strength: Strength strong.
	self assert: (solver hasEditVariable: variable).
	solver removeEditVariable: variable.
	self assert: (solver hasEditVariable: variable) not!

testSolverAddConstraint
	| solver constraint |
	solver := ConstraintSolver new.
	constraint := Constraint lhs: (Expression new: {1. -1}) equalsRhs: nil.
	self assert: (solver hasConstraint: constraint) not.
	solver addConstraint: constraint.
	self assert: (solver hasConstraint: constraint)!

testSolverDuplicateConstraint
	| solver constraint |
	solver := ConstraintSolver new.
	constraint := Constraint lhs: (Expression new: {1. -1}) equalsRhs: nil.
	solver addConstraint: constraint.
	
	[solver addConstraint: constraint.
	self assert: false] on: Error
			do: [:exception | self assert: exception messageText equals: 'duplicate constraint']!

testSolverRemoveConstraint
	| solver constraint |
	solver := ConstraintSolver new.
	constraint := Constraint lhs: (Expression new: {1. -1}) equalsRhs: nil.
	self assert: (solver hasConstraint: constraint) not.
	solver addConstraint: constraint.
	self assert: (solver hasConstraint: constraint).
	solver removeConstraint: constraint.
	self assert: (solver hasConstraint: constraint) not!

testSolverRemoveMultipleConstraints
	| x solver constraint1 constraint2 |
	solver := ConstraintSolver new.
	x := Variable new.
	constraint1 := Constraint lhs: x lessThanOrEqualRhs: 100.
	constraint2 := Constraint lhs: x equalsRhs: 80.
	solver
		addConstraint: constraint1;
		addConstraint: constraint2.
	solver updateVariables.
	self assert: x value = 80.
	solver
		removeConstraint: constraint1;
		removeConstraint: constraint2.
	self
		assert: (solver hasConstraint: constraint1) not;
		assert: (solver hasConstraint: constraint2) not!

testSuggestValue
	| solver variable |
	solver := ConstraintSolver new.
	variable := Variable new.
	solver
		addEditVariable: variable strength: Strength strong;
		suggest: variable value: 200;
		updateVariables.
	self assert: 200 equals: variable value!

testUnsatisfiableConstraint
	| solver width width2 |
	solver := ConstraintSolver new.
	width := Variable new.
	width2 := Variable new.
	solver addConstraint: (Constraint lhs: width equalsRhs: 100).
	[solver addConstraint: (Constraint lhs: width equalsRhs: width2)] on: Error
		do: [:exception | self assert: exception messageText equals: 'unsatisfiable constraint']!

testUnsatisfiableConstraintNonZeroExpression
	| solver constraint |
	solver := ConstraintSolver new.
	constraint := Constraint lhs: (Expression new: {1. -1. 10}) equalsRhs: nil.
	
	[solver addConstraint: constraint.
	self assert: false] on: Error
			do: [:exception | self assert: exception messageText equals: 'unsatisfiable constraint']! !

!ConstraintSolverTests categoriesForMethods!
testAddDuplicateEditVariable!public! !
testAddEditVariable!public! !
testAddUnknownEditVariable!public! !
testCompetingConstraints!public! !
testConstraintExpression!public! !
testConstraintNewSyntax!public! !
testConstraintOperator!public! !
testConstraintRawSyntax!public! !
testConstraintStrengthDefault!public! !
testConstraintStrengthMedium!public! !
testCreateSolver!public! !
testInvalidExpressionArray1!public! !
testInvalidExpressionArray2!public! !
testNewExpression!public! !
testNewExpressionAdd!public! !
testNewExpressionComplex!public! !
testNewExpressionNegatedVariable!public! !
testNewExpressionNegation!public! !
testNewExpressionNumbers!public! !
testNewExpressionVariable!public! !
testNewVariableName!public! !
testNewVariableValue!public! !
testRemoveEditVariable!public! !
testSolverAddConstraint!public! !
testSolverDuplicateConstraint!public! !
testSolverRemoveConstraint!public! !
testSolverRemoveMultipleConstraints!public! !
testSuggestValue!public! !
testUnsatisfiableConstraint!public! !
testUnsatisfiableConstraintNonZeroExpression!public! !
!

"Binary Globals"!

