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
// part 1
->Js.log

let map = Map.make()

input
->Array.map(line =>
  switch line->String.split(":") {
  | [gameInfo, cards] =>
    let cardsNumber = gameInfo->String.replaceRegExp(%re("/[a-zA-Z ]/g"), "")->Int.fromString

    switch (cardsNumber, cards->String.split(" | ")) {
    | (Some(_cardsNumber'), [winningNumbers, gameNumbers]) =>
      let currentCardNumber' = map->Map.get(_cardsNumber')

      switch currentCardNumber' {
      | Some(v) => map->Map.set(_cardsNumber', v + 1)
      | None => map->Map.set(_cardsNumber', 1)
      }

      let winningNumbers =
        winningNumbers
        ->String.split(" ")
        ->Array.filterMap(v => v->String.trim->String.length > 0 ? Some(v) : None)

      let matches =
        gameNumbers
        ->String.split(" ")
        ->Array.filterMap(v => v->String.trim->String.length > 0 ? Some(v) : None)
        ->Array.filter(v => winningNumbers->Array.includes(v))

      let matchedCardsNumbers = Array.fromInitializer(~length=matches->Array.length, i =>
        i + _cardsNumber' + 1
      )

      matchedCardsNumbers->Array.forEach(cardNumber => {
        let currentCount = map->Map.get(cardNumber)

        switch (map->Map.get(_cardsNumber'), currentCount) {
        | (Some(v'), Some(v)) => map->Map.set(cardNumber, v + v')
        | (Some(v'), None) => map->Map.set(cardNumber, v')
        | (None, None) => ()
        | (_, Some(v)) => map->Map.set(cardNumber, v)
        }
      })

      (_cardsNumber', matches->Array.length, matchedCardsNumbers)->ignore
      matches
    | _ => []
    }
  | _ => []
  }
)
->ignore

map->Map.values->Iterator.toArray->Array.reduce(0, (prev, cur) => prev + cur)->Js.log
