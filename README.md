# Arithmetic expression evaluation library

This library allows evaluating arithmetic expressions.
```scala
val expression = "10 - (factorial(3) + num)"

val evaluator = new ExpressionEvaluator()
evaluator.setVariable("num", 1)

val result =  ExprEval.evaluate(expression)

// result == 3
``` 

### Docs

ToDo