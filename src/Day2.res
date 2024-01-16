open RescriptCore

let input = "/input/day2"->Utils.makeInput

type game = {
  id: int,
  red: array<int>,
  green: array<int>,
  blue: array<int>,
}

type game2 = {
  id: int,
  canGame: array<bool>,
}
// Part 1
let _ =
  input
  ->Array.map(game => {
    switch game->String.split(":") {
    | [into, detail] =>
      into
      ->String.split(" ")
      ->Array.get(1)
      ->Option.flatMap(gameId => gameId->Int.fromString)
      ->Option.map(gameId => {
        // Js.log(_gameId)

        detail
        ->String.trim
        ->String.split(";")
        ->Array.map(v => v->String.trim->String.split(",")->Array.map(String.trim))
        ->Array.flat
        ->Array.reduce(
          {id: gameId, canGame: []},
          (prev, current) => {
            switch current->String.split(" ") {
            | [count, color] =>
              switch color {
              | "red" => {
                  id: gameId,
                  canGame: prev.canGame->Array.concat([
                    count->Int.fromString->Option.getOr(0) <= 12,
                  ]),
                }
              | "green" => {
                  id: gameId,
                  canGame: prev.canGame->Array.concat([
                    count->Int.fromString->Option.getOr(0) <= 13,
                  ]),
                }
              | "blue" => {
                  id: gameId,
                  canGame: prev.canGame->Array.concat([
                    count->Int.fromString->Option.getOr(0) <= 14,
                  ]),
                }
              | _ => prev
              }
            | _ => prev
            }
          },
        )
      })
    | _ => None
    }
  })
  ->Array.keepSome
  ->Array.filterMap(game => game.canGame->Array.every(v => v) ? Some(game) : None)
  // ->Js.log
  ->Array.reduce(0, (prev, current) => prev + current.id)
  ->Js.log

// Part 2
let _ =
  input
  ->Array.map(game => {
    switch game->String.split(":") {
    | [into, detail] =>
      into
      ->String.split(" ")
      ->Array.get(1)
      ->Option.flatMap(gameId => gameId->Int.fromString)
      ->Option.map(gameId => {
        // Js.log(_gameId)

        detail
        ->String.trim
        ->String.split(";")
        ->Array.map(v => v->String.trim->String.split(",")->Array.map(String.trim))
        ->Array.flat
        ->Array.reduce(
          {id: gameId, red: [], green: [], blue: []},
          (prev, current) => {
            switch current->String.split(" ") {
            | [count, color] =>
              // Js.log(current)
              switch color {
              | "red" => {
                  ...prev,
                  red: prev.red->Array.concat([count->Int.fromString->Option.getOr(0)]),
                }
              | "green" => {
                  ...prev,
                  green: prev.green->Array.concat([count->Int.fromString->Option.getOr(0)]),
                }
              | "blue" => {
                  ...prev,
                  blue: prev.blue->Array.concat([count->Int.fromString->Option.getOr(0)]),
                }
              | _ => prev
              }
            | _ => prev
            }
          },
        )
      })
    | _ => None
    }
  })
  ->Array.keepSome
  ->Array.map(game =>
    game.red->Math.Int.maxMany * game.green->Math.Int.maxMany * game.blue->Math.Int.maxMany
  )
  ->Array.reduce(0, (prev, current) => prev + current)
  ->Js.log

open NodeJs


let input =
  Fs.readFileSyncWith(Global.dirname ++ "/input/day2", {encoding: "utf8"})
  ->Buffer.toString
  ->String.split("\n")

let hasRed = 12
let hasGreen = 13
let hasBlue = 14

let sumGameNumber = ref(0)
let v = []

v->Array.push()

Array.forEach(input, gameItem => {
  let game = String.split(gameItem, ": ")
  switch game {
  | [gameInfo, cubeInfo] => {
      let gameNumber = String.split(gameInfo, " ")[1]->Option.getOr("0")->Int.fromString
      let isPossible = ref(true)

      String.split(cubeInfo, "; ")->Array.forEach(gamer =>
        String.split(gamer, ", ")->Array.forEach(
          cube =>
            switch String.split(cube, " ") {
            | [count, color] =>
              switch (count, color) {
              | (_, "red") =>
                if count->Int.fromString->Option.getOr(0) > hasRed {
                  isPossible := false
                }
              | (_, "green") =>
                if count->Int.fromString->Option.getOr(0) > hasGreen {
                  isPossible := false
                }
              | (_, "blue") =>
                if count->Int.fromString->Option.getOr(0) > hasBlue {
                  isPossible := false
                }
              | _ => Js.log("Color")
              }
            | _ => Js.log("Cube")
            }->ignore,
        )
      )

      if isPossible.contents {
        sumGameNumber := sumGameNumber.contents + gameNumber->Option.getOr(0)
      }
    }
  | _ => Js.log("None!!")
  }
})

Js.log(sumGameNumber.contents)

//2023 Day2 Part2
let sumCubeCount = ref(0)

Array.forEach(input, gameItem => {
  let game = String.split(gameItem, ": ")
  switch game {
  | [_, cubeInfo] => {
      let redCount = ref(0)
      let greenCount = ref(0)
      let blueCount = ref(0)

      String.split(cubeInfo, "; ")->Array.forEach(gamer =>
        String.split(gamer, ", ")->Array.forEach(
          cube =>
            switch String.split(cube, " ") {
            | [count, color] =>
              switch (count, color) {
              | (_, "red") =>
                if count->Int.fromString->Option.getOr(0) > redCount.contents {
                  redCount := count->Int.fromString->Option.getOr(0)
                }
              | (_, "green") =>
                if count->Int.fromString->Option.getOr(0) > greenCount.contents {
                  greenCount := count->Int.fromString->Option.getOr(0)
                }
              | (_, "blue") =>
                if count->Int.fromString->Option.getOr(0) > blueCount.contents {
                  blueCount := count->Int.fromString->Option.getOr(0)
                }
              | _ => ()
              }
            | _ => ()
            }->ignore,
        )
      )

      sumCubeCount :=
        sumCubeCount.contents + redCount.contents * greenCount.contents * blueCount.contents
    }
  | _ => Js.log("None!!")
  }
})

Js.log(sumCubeCount.contents)

 
let v = []->Array.getUnsafe(1)

let result = v + 1