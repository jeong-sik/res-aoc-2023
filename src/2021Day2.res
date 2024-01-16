open RescriptCore
let input = "/input/2021day2"->Utils.makeInput

type direction = Forward(int) | Up(int) | Down(int)
type position = Depth(int) | Horizontal(int)
type aim = Aim(int)

let moves =
  input
  ->Array.map(v => {
    let items = v->String.split(" ")
    switch items {
    | [direction, value] =>
      switch (direction, value->Int.fromString) {
      | ("forward", Some(i)) => Some(Forward(i))
      | ("up", Some(i)) => Some(Up(i))
      | ("down", Some(i)) => Some(Down(i))
      | _ => None
      }
    | _ => None
    }
  })
  ->Array.keepSome

moves
->Array.reduce((Horizontal(0), Depth(0)), (acc, current) => {
  let (prevHorizontal, prevDepth) = acc
  switch current {
  | Forward(v) =>
    switch prevHorizontal {
    | Horizontal(prev) => (Horizontal(prev + v), prevDepth)
    | _ => acc
    }
  | Up(v) =>
    switch prevDepth {
    | Depth(prev) => (prevHorizontal, Depth(prev - v))
    | _ => acc
    }
  | Down(v) =>
    switch prevDepth {
    | Depth(prev) => (prevHorizontal, Depth(prev + v))
    | _ => acc
    }
  }
})
->Js.log
// 2091 * 721


// Aim variant 는 필요 없음
moves
->Array.reduce((Horizontal(0), Depth(0), Aim(0)), (acc, current) => {
  let (prevHorizontal, prevDepth, prevAim) = acc
  switch current {
  | Forward(v) =>
    switch (prevHorizontal, prevDepth, prevAim) {
    | (Horizontal(horizontal'), Depth(depth'), Aim(aim')) => (
        Horizontal(horizontal' + v),
        Depth(depth' + aim' * v),
        prevAim,
      )
    | _ => acc
    }
  | Up(v) =>
    switch prevAim {
    | Aim(aim') => (prevHorizontal, prevDepth, Aim(aim' - v))
    }
  | Down(v) =>
    switch prevAim {
    | Aim(aim') => (prevHorizontal, prevDepth, Aim(aim' + v))
    }
  }
})
->Js.log
// part 2
Js.log(2091 * 899375)
