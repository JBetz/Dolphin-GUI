"Filed out from Dolphin Smalltalk"!

Object subclass: #ConstraintSolver
	instanceVariableNames: 'currentId constraints rows variables edits infeasibleRows objective artificial'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

