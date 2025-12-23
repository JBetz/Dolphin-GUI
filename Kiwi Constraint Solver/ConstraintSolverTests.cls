"Filed out from Dolphin Smalltalk"!

TestCase subclass: #ConstraintSolverTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

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

