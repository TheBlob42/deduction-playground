# Deduction Playground

Deduction Playground is a library for natural deduction written in Clojure. It supports propotional, predicate and temporal logic (LTL) out of the box and is expandable with own deduction rules and logics. You can use Deduction Playground right away in your Clojure REPL or implement it as a part of your project.

## Syntax

Formulas are represented in a clojure-like syntax. Instead of writing for example<br/> *(A &#x22C0; B) &#x22A2; A* you would write *(infer (and A B) A)*. See the following table for the equivalents:

math syntax    | clojure-like syntax
---------------|-----------
a &#x22A2; b   | (infer a b)
a &#x22C0; b   | (and a b)
a &#x22C1; b   | (or a b)
a &#x2192; b   | (impl a b) 
&#x00AC;a      | (not a) 
&#x22A4;       | truth
&#x22A5;       | contradiction
&#x2200;x P(x) | (forall \[x\] (P x))
&#x2203;x P(x) | (exists \[x\] (P x)) 
*i:* a         | (at i a) 
&#x20DE;a      | (always a) 
&#x20DD;a      | (asap a) 
&#x20DF;a      | (sometime a) 
a&#x1D4E4;b    | (until a b) 
(Also you can use *true* and *false* in your formulas)<br/><br/>

You're able to combine these operators, e.g. instead of writing:<br/>
> *(a &#x2192; b) &#x22A2; (&#x00AC;a &#x2192; &#x00AC;a)*

you would write the following:<br/>
> *(infer (impl a b) (impl (not b) (not a)))*

## I wanna start proving

Just load the file "repl.clj" into your Clojure REPL and you're ready to start proving hypotheses. By default it will load the rules for natural deduction with propotional and predicate logic. If you want to change it to temporal logic or your custom rules, just replace the loaded file in line 6 with the one you desire.

### Proof

To start a new proof use the `(proof [premises] conclusion)` function. You can insert no, one or a vector of premises but you always have to provide a conclusion (exactly one). You are the presented with a visual presentation of the proof:

```clojure
=> (proof '(and A B) 'A)
  ----------------------------------------
1:	(and A B)		Premise
2:	...
3:	A
  ----------------------------------------
```
> You have to always quote the formulas or clojure trys to read them as functions and symbols (which will fail obviously)

### Steps

You are now ready to operate on your proof. There are several functions you can call:

`(step-f rule lines)` makes a forward step with the given *rule* and the given *lines*

```clojure
=> (proof '(and a b) 'a)
  ----------------------------------------
1:	(and a b)		Premise
2:	...
3:	a
  ----------------------------------------
=> (step-f "and-e1" 1)
  ----------------------------------------
1:	(and a b)		Premise
2:	a 				"and-e1" (1)
  ----------------------------------------
```

`(step-b rule lines)` makes a backward step with the given *rule* and the given *lines*

```clojure
=> (proof '[(impl a b) (impl b c)] '(impl a c))
  ----------------------------------------
1:	(impl a b)		Premise
2:	(impl b c)		Premise
3:	...
4:	(impl a c)
  ----------------------------------------
  
=> (step-b "impl-i" 4)
  ----------------------------------------
1:	(impl a b)		Premise
2:	(impl b c)		Premise
     -------------------------------------
3:	| a 			Assumption
4:	| ...
5:	| c
	 -------------------------------------
6:	(impl a c)
  ----------------------------------------
```

#### Optional Parameters

Both `step-f` and `step-b` support optional parameters. If there is more than one conclusion, you can feed them parts of the desired result (besides the obligatory parameters) to specify the outcome.<br/><br/>
An example would be the &#x00AC; Elimination:

```clojure
  ----------------------------------------
1:	a				Premise
2:	...
3:	contradiction
  ----------------------------------------
  
=> (step-b "not-e" 3) ; without optional parameters
  ----------------------------------------
1:	a				Premise
2:	...
3:	V1
4:	(not V1)
5:	contradiction	"not-e" (3 4)
  ----------------------------------------
=> (undo) 
[...]
=> (stepb-b "not-e" 3 1) ; with optional parameters
  ----------------------------------------
1:	a				Premise
2:	...
4:	(not a)
5:	contradiction	"not-e" (1 4)
  ----------------------------------------
```
<br/>

`step-f-inside` is a special form of "step-f". It takes a forward rule with exactly one premise and one conclusion and searches inside a formula for matches to replace

```clojure
  ----------------------------------------
1:	(or a (and b c))	Premise
[...]

=> (step-f-inside "and-e1" 1)
  ----------------------------------------
1:	(or a (and b c))	Premise
2:	(or a b)			"and-e1" (1)
[...]
```

`classical` it solves any kind of classical logic inside a given line. Works very similar to "step-f-inside", but uses the theorems in "classical-theorems.clj". So thats the place to look when something isn't working as you expect it.

```clojure
  ----------------------------------------
1:	(or a (and b true))	Premise
[...]

=> (classical 1)
  ----------------------------------------
1:	(or a (and b true))	Premise
2:	(or a b)			"classical" ()
[...]
```

`rename-var` allows you to rename any new inserted or given variable inside the proof. If a rule introduces a new variable into the proof, it will always start with a "V" followed by a number (e.g. V1, V3, V25)

```clojure
------------------------------------------
1	a 					Premise
2	(or a V1)			"or-i1" (1)
[...]

=> (rename-var 'V1 'b)
------------------------------------------
1	a 					Premise
2	(or a b)			"or-i1" (1)
[...]
```

`choose-option` let you decide between different results a rule offers you. Sometimes a rule has different conclusions depending on the order of the parameters. In this case you will be presented with a selection, which let you choose the right result for you.

```clojure
  ----------------------------------------
1:	a 							Premise
2:	b 							Premise
3:	{1 (and a b), 2 (and b a)}	"and-i" (1 2)
[...]

=> (choose-option 3 1)
  ----------------------------------------
1:	a 							Premise
2:	b 							Premise
3:	(and a b)					"and-i" (1 2)
[...]
```

`show` is used to simply print the current state of the proof. Its good to use after you did some other stuff in the REPL and don't want to scroll all the way back to your proof.

`undo` sets your proof one step back. You can use this until you reached the initial state of your proof. But beware that there is no *redo* function.

### Export

It is possible to export a solved proof, in order to use it in other proofs as a theorem. To export a solved proof use `export-theorem`. You have to provide a filename and a name for the theorem to the function. The function will not delete the content of the given file, but instead append the theorem to it. Therefore you can have multiple theorems saved in a single file.

To load all theorems from a certain file, use the `deduction-playground.io/import-theorems` function. In both cases the imported/exported theorems are instantly ready to use. Just use them like normal rules but beware that theorems can only be used forwards.

## Extend the project

There are two ways to extend this project: Write your own rules for existing logics or implement a new one.

### Write your own rules

This is pretty simple. You can either edit an existing rules-file (e.g. "rules-temporal.clj") or write your own. In the last case just make sure you load the file correctly with `deduction-playground.io/import-rules`.

A rule is a simple clojure map with the following keys:

key 		| what does it do?
------------|-----------------|
:name     	|A name for your rule. This is used to apply your rules with functions like `step-f`. The name has to be a string.
:given		|A clojure vector with all the symbols, keywords and/or lists which are premises for your rule.
:conclusion	|A clojure vector with all the symbols, keywords and/or lists which are the conclusion(s) of your rule.
:forwards	|Set this to *true* if this rule should be used forwards, *false* if not*.
:backwards	|Set this to *true* if this rule should be used backwards, *false* if not*.

\*not setting this key is equal to *false*

See the [rules-prop-pred.clj](https://github.com/TheBlob42/deduction-playground/blob/master/resources/rules-prop-prep.clj) or the [rules-temporal.clj](https://github.com/TheBlob42/deduction-playground/blob/master/resources/rules-temporal.clj) file for examples.

The same way you write your own rules, you can also add or change the classical theorems which are used by the `classical` function. These have the same structure as all other rules, but you can skip the *:forwards* and *:backwards* keys since they only can be evaluated forwards.

### Write your own logic

If writing your own rules and classical-theorems isn't enough for your logic you can also edit the code. Search for the keyword **NEW LOGIC** in the comments. This keyword will indicate you where you may have to change or expand the code of this project.
