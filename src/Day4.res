open RescriptCore
// Card  13: 68 60 92 16 70 71 15 94 43 83 | 70 59 88 67 27 77 51 54 15 22 49 71 94 60  7 72 47  1 38 39 36 19 68 12 16
let input = "/input/day4"->Utils.makeInput
// let map = Map.make()

input
->Array.map(line =>
  switch line->String.split(":") {
  | [gameInfo, cards] =>
    let _ = gameInfo->String.replaceRegExp(%re("/[a-zA-Z ]/g"), "")
    switch cards->String.split(" | ") {
    | [winningNumbers, gameNumbers] =>
      let winningNumbers =
        winningNumbers
        ->String.split(" ")
        ->Array.filterMap(v => v->String.trim->String.length > 0 ? Some(v) : None)

      let matches =
        gameNumbers
        ->String.split(" ")
        ->Array.filterMap(v => v->String.trim->String.length > 0 ? Some(v) : None)
        ->Array.filter(v => winningNumbers->Array.includes(v))
      matches
    | _ => []
    }
  | _ => []
  }
)
->Array.reduce(0, (prev, array) => {
  switch array->Array.length {
  | 0 => 0 + prev
  | 1 => 1 + prev
  | length => Math.pow(2., ~exp=(length - 1)->Int.toFloat)->Int.fromFloat + prev
  }
})
->Js.log
