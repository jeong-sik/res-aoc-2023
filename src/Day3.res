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
      | Some((Digit, _, Some(_), _, _)) =>
        map->Map.set(
          `${x->Int.toString},${y->Int.toString}`,
          (
            Digit,
            v ++ numberString,
            // Fixme
            Some(
              switch map->Map.get(`${(x - 2)->Int.toString},${y->Int.toString}`) {
              | Some(_) => x - 2
              | None => x - 1
              },
            ),
            Some(endPosition),
            Some(v ++ numberString),
          ),
        )
      | Some((Digit, _, None, _, _)) =>
        map->Map.set(
          `${x->Int.toString},${y->Int.toString}`,
          (Digit, v ++ numberString, Some(x - 2), Some(endPosition), Some(v ++ numberString)),
        )
      | _ =>
        map->Map.set(
          `${x->Int.toString},${y->Int.toString}`,
          (Digit, v ++ numberString, Some(x), Some(endPosition), Some(v ++ numberString)),
        )
      }
    | (v, None) =>
      map->Map.set(`${x->Int.toString},${y->Int.toString}`, (Symbol, v, None, None, None))
    }
  })
})
->ignore

let r =
  map
  ->Map.keys
  ->Iterator.toArray
  ->Array.filter(key => {
    switch map->Map.get(key) {
    // Symbol 제거
    | Some((Symbol, _, _, _, _)) => false
    | Some((Digit, _, None, _, _)) => false
    | Some((Digit, _, Some(startX), Some(endX), _)) =>
      switch key->String.split(",") {
      | [_, y] =>
        switch y->Int.fromString {
        | Some(y') => {
            let startTopLeft =
              map->Map.get(`${(startX - 1)->Int.toString},${(y' - 1)->Int.toString}`)
            let startTop = map->Map.get(`${startX->Int.toString},${(y' - 1)->Int.toString}`)
            let startTopRight =
              map->Map.get(`${(startX + 1)->Int.toString},${(y' - 1)->Int.toString}`)
            let startLeft = map->Map.get(`${(startX - 1)->Int.toString},${y'->Int.toString}`)
            let startBottomLeft =
              map->Map.get(`${(startX - 1)->Int.toString},${(y' + 1)->Int.toString}`)
            let startBottom = map->Map.get(`${startX->Int.toString},${(y' + 1)->Int.toString}`)
            let startBottomRight =
              map->Map.get(`${(startX + 1)->Int.toString},${(y' + 1)->Int.toString}`)

            let endTop = map->Map.get(`${endX->Int.toString},${(y' - 1)->Int.toString}`)
            let endTopRight = map->Map.get(`${(endX + 1)->Int.toString},${(y' - 1)->Int.toString}`)
            let endRight = map->Map.get(`${(endX + 1)->Int.toString},${y'->Int.toString}`)
            let endBottomRight =
              map->Map.get(`${(endX + 1)->Int.toString},${(y' + 1)->Int.toString}`)
            let endBottom = map->Map.get(`${endX->Int.toString},${(y' + 1)->Int.toString}`)

            switch (
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
            ) {
            | (Some((Symbol, _, _, _, _)), _, _, _, _, _, _, _, _, _, _, _)
            | (_, Some((Symbol, _, _, _, _)), _, _, _, _, _, _, _, _, _, _)
            | (_, _, Some((Symbol, _, _, _, _)), _, _, _, _, _, _, _, _, _)
            | (_, _, _, Some((Symbol, _, _, _, _)), _, _, _, _, _, _, _, _)
            | (_, _, _, _, Some((Symbol, _, _, _, _)), _, _, _, _, _, _, _)
            | (_, _, _, _, _, Some((Symbol, _, _, _, _)), _, _, _, _, _, _)
            | (_, _, _, _, _, _, Some((Symbol, _, _, _, _)), _, _, _, _, _)
            | (_, _, _, _, _, _, _, Some((Symbol, _, _, _, _)), _, _, _, _)
            | (_, _, _, _, _, _, _, _, Some((Symbol, _, _, _, _)), _, _, _)
            | (_, _, _, _, _, _, _, _, _, Some((Symbol, _, _, _, _)), _, _)
            | (_, _, _, _, _, _, _, _, _, _, Some((Symbol, _, _, _, _)), _)
            | (_, _, _, _, _, _, _, _, _, _, _, Some((Symbol, _, _, _, _))) => true
            | _ => false
            }
          }
        | _ => false
        }
      | _ => false
      }
    | _ => false
    }
  })
  ->Array.reduce(0, (prev, current) => {
    switch map->Map.get(current) {
    | Some((_, _, _, _, Some(v))) => prev + v->Int.fromString->Option.getOr(0)
    | _ => prev
    }
    // part1
  })
  ->Js.log

let asterisks =
  map
  ->Map.keys
  ->Iterator.toArray
  ->Array.filter(key => {
    switch key->String.split(",") {
    | [_x, _y] =>
      switch map->Map.get(key) {
      | Some((Symbol, "*", _, _, _)) => true
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
          (top, x', y' - 1),
          (topRight, x' + 1, y' - 1),
          (right, x' + 1, y'),
          (bottomRight, x' + 1, y' + 1),
          (bottom, x', y' + 1),
          (bottomLeft, x' - 1, y' + 1),
          (left, x' - 1, y'),
          (topLeft, x' - 1, y' - 1),
        ]
        ->Array.filterMap(((v, x, y)) => {
          switch v {
          | Some(v') => {
              let (s, _, _, _, _) = v'
              switch s {
              | Digit => Some((v', x, y))
              | _ => None
              }
            }
          | _ => None
          }
        })
        ->Array.map(current => {
          let ((_, _, startX, _, _), _, y) = current

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

parts
->Array.filter(arr => arr->Array.length == 2)
->Array.reduce(0, (prev, current) => {
  prev +
  current->Array.reduce(1, (prev, current) => {
    let (_, val, _, _, _) = current

    prev * val->Int.fromString->Option.getOr(0)
  })
  // part2
})
->Js.log
