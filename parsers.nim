import strutils
type
  Parser[T] = proc(input: string): Maybe[(T, string)]
  Maybe*[T] = object
    value: T
    hasValue: bool

proc Just*[T](value: T): Maybe[T] =
  result.hasValue = true
  result.value = value

proc Nothing*[T]: Maybe[T] =
  result.hasValue = false

proc `/`[T](lhs, rhs: Parser[T]): Parser[T] = 
  (proc (input: string): Maybe[(T, string)] = 
    let lresult = lhs(input)
    if lresult.hasValue:
      lresult
    else:
      rhs(input)
  )

proc `+`[T, U](lhs: Parser[T], rhs: Parser[U]): Parser[(T, U)] =
  (proc (input: string): Maybe[((T, U), string)] =
    let lresult = lhs(input)
    if lresult.hasValue:
      let (lvalue, lnext) = lresult.value
      let rresult = rhs(lnext)
      if rresult.hasValue:
        let (rvalue, rnext) = rresult.value
        Just (((lvalue, rvalue), rnext))
      else:
        Nothing[((T, U), string)]()
    else:
      Nothing[((T, U), string)]()
  )

proc s(value: string): Parser[string] =
  (proc (input: string): Maybe[(string, string)] =
    if input.startsWith(value):
      Just ((input[0 .. (value.len - 1)], input[value.len .. input.len]))
    else:
      Nothing[(string, string)]()
  )

proc map[T, U](parser: Parser[T], f: (proc(value: T): U)): Parser[U] =
  (proc (input: string): Maybe[(U, string)] = 
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      Just((f(xvalue), xnext))
    else:
      Nothing[U]()
  )

let ab = s("A") + s("B")

echo ab("AB")
