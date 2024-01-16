open RescriptCore

let input = "/input/day3"->Utils.makeInput

type charType = Digit | Symbol

let map = Map.make()

let rec findForward = (line, currentX, prevChar) => {
  switch line->Array.get(currentX + 1) {
  | Some(v) =>
    switch v->Int.fromString {
    | Some(_) => {
        let (str, p) = findForward(line, currentX + 1, prevChar)

        (v ++ str, p)
      }

    | None => ("", currentX)
    }
  | None => ("", currentX + 1)
  }
}

input
->Array.forEachWithIndex((line, y) => {
  line
  ->String.split("")
  ->Array.forEachWithIndex((char, x) => {
    switch (char, char->Int.fromString) {
    | (".", None) => ()
    | (v, Some(_number)) =>
      let (numberString, endPosition) = findForward(line->String.split(""), x, char)

      switch map->Map.get(`${(x - 1)->Int.toString},${y->Int.toString}`) {
      | Some((Digit, Some(_), _, _prevV, _)) =>
        map->Map.set(
          `${x->Int.toString},${y->Int.toString}`,
          (
            Digit,
            // Fixme
            Some(
              switch map->Map.get(`${(x - 2)->Int.toString},${y->Int.toString}`) {
              | Some(_) => x - 2
              | None => x - 1
              },
            ),
            Some(endPosition),
            Some(_prevV->Option.getOr("")),
            y,
          ),
        )
      | _ =>
        map->Map.set(
          `${x->Int.toString},${y->Int.toString}`,
          (Digit, Some(x), Some(endPosition), Some(v ++ numberString), y),
        )
      }
    | (v, None) =>
      map->Map.set(`${x->Int.toString},${y->Int.toString}`, (Symbol, None, None, Some(v), y))
    }
  })
})
->ignore

let r =
  map
  ->Map.keys
  ->Iterator.toArray
  ->Array.map(key => {
    switch map->Map.get(key) {
    | Some((Digit, Some(startX), Some(endX), v, y')) =>
      let startTopLeft = map->Map.get(`${(startX - 1)->Int.toString},${(y' - 1)->Int.toString}`)
      let startTop = map->Map.get(`${startX->Int.toString},${(y' - 1)->Int.toString}`)
      let startTopRight = map->Map.get(`${(startX + 1)->Int.toString},${(y' - 1)->Int.toString}`)
      let startLeft = map->Map.get(`${(startX - 1)->Int.toString},${y'->Int.toString}`)
      let startBottomLeft = map->Map.get(`${(startX - 1)->Int.toString},${(y' + 1)->Int.toString}`)
      let startBottom = map->Map.get(`${startX->Int.toString},${(y' + 1)->Int.toString}`)
      let startBottomRight = map->Map.get(`${(startX + 1)->Int.toString},${(y' + 1)->Int.toString}`)
      let endTop = map->Map.get(`${endX->Int.toString},${(y' - 1)->Int.toString}`)
      let endTopRight = map->Map.get(`${(endX + 1)->Int.toString},${(y' - 1)->Int.toString}`)
      let endRight = map->Map.get(`${(endX + 1)->Int.toString},${y'->Int.toString}`)
      let endBottomRight = map->Map.get(`${(endX + 1)->Int.toString},${(y' + 1)->Int.toString}`)
      let endBottom = map->Map.get(`${endX->Int.toString},${(y' + 1)->Int.toString}`)
      let item =
        [
          startTopLeft,
          startTop,
          startTopRight,
          startLeft,
          startBottomLeft,
          startBottom,
          startBottomRight,
          endTop,
          endTopRight,
          endRight,
          endBottomRight,
          endBottom,
        ]
        ->Array.keepSome
        ->Array.filter(((s, _, _, _, _)) => s == Symbol)

      item->Array.length > 0
        ? Some(`${startX->Int.toString},${y'->Int.toString}`, v->Option.getOr("")->Int.fromString)
        : None
    | _ => None
    }
  })
  ->Array.keepSome
  ->Map.fromArray
  ->Map.values
  ->Iterator.toArray
  ->Array.keepSome
  ->Array.reduce(0, (prev, current) => prev + current)
  // part1
  ->Js.log

let asterisks =
  map
  ->Map.keys
  ->Iterator.toArray
  ->Array.filter(key => {
    switch key->String.split(",") {
    | [_x, _y] =>
      switch map->Map.get(key) {
      | Some((Symbol, _, _, Some("*"), _)) => true
      | _ => false
      }
    | _ => false
    }
  })

let parts = asterisks->Array.map(v => {
  switch v->String.split(",") {
  | [x, y] =>
    switch (x->Int.fromString, y->Int.fromString) {
    | (Some(x'), Some(y')) => {
        let top = map->Map.get(`${x},${(y' - 1)->Int.toString}`)
        let topRight = map->Map.get(`${(x' + 1)->Int.toString},${(y' - 1)->Int.toString}`)
        let right = map->Map.get(`${(x' + 1)->Int.toString},${y'->Int.toString}`)
        let bottomRight = map->Map.get(`${(x' + 1)->Int.toString},${(y' + 1)->Int.toString}`)
        let bottom = map->Map.get(`${x'->Int.toString},${(y' + 1)->Int.toString}`)
        let bottomLeft = map->Map.get(`${(x' - 1)->Int.toString},${(y' + 1)->Int.toString}`)
        let left = map->Map.get(`${(x' - 1)->Int.toString},${y'->Int.toString}`)
        let topLeft = map->Map.get(`${(x' - 1)->Int.toString},${(y' - 1)->Int.toString}`)

        [
          (top, x'),
          (topRight, x' + 1),
          (right, x' + 1),
          (bottomRight, x' + 1),
          (bottom, x'),
          (bottomLeft, x' - 1),
          (left, x' - 1),
          (topLeft, x' - 1),
        ]
        ->Array.filterMap(((v, x)) => {
          switch v {
          | Some(v') => {
              let (s, _, _, _, y) = v'
              switch s {
              | Digit => Some((v', x, y))
              | _ => None
              }
            }
          | _ => None
          }
        })
        ->Array.map(current => {
          let ((_, startX, _, _, _), _, y) = current

          map->Map.get(`${startX->Option.getOr(0)->Int.toString},${y->Int.toString}`)
        })
        ->Array.keepSome
        ->Set.fromArray
        ->Set.values
        ->Iterator.toArray
      }
    | _ => []
    }
  | _ => []
  }
})

type v = Stop | Go

type v2 = [@as("취소") #CANCELED]

let vToString = v =>
  switch v {
  | Stop => ""
  | Go => ""
  }

let vFromString = v =>
  switch v {
  | "stop" => Stop
  | "go" => Go
  | _ => Go
  }

parts
->Array.filter(arr => arr->Array.length == 2)
->Array.reduce(0, (prev, current) => {
  prev +
  current->Array.reduce(1, (prev, current) => {
    let (_, _, _, val, _) = current

    prev * val->Option.getOr("")->Int.fromString->Option.getOr(0)
  })
  // part2
})
->Js.log
