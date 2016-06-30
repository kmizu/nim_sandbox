import strutils
import lists
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

proc repeat[T](body: Parser[T]): Parser[DoublyLinkedList[T]] =
  (proc (input: string): Maybe[(DoublyLinkedList[T], string)] =
    let list = initDoublyLinkedList[T]()
    var rest = input
    while true:
      let xresult = body(rest)
      if xresult.hasValue:
        let (xvalue, xnext) = xresult.value
        list.append(xvalue)
        rest = xnext
      else:
        return Just(list)
    nil
  )

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
      Nothing[(U, string)]()
  )

proc chainl[T](p: Parser[T], q: Parser[(proc(a: T, b: T): T)]): Parser[T] =
  (p + (q + q).repeat()).map(proc(values) =
    let (x, xs) = values
    var a = x
    for fb in xs:
      let (f, b) = fb
      a = f(a, b)
    a)

let ab: Parser[(string, string)] = (s("A") + s("B"))
let x: Parser[string] = ab.map(proc(v: (string, string)): string =
  let (v1, v2) = v
  v1 & v2
)
