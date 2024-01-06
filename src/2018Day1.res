open RescriptCore

let input = "/input/2018day1"->Utils.makeInput

let set = Set.make()
set->Set.add(0)->ignore

let v = None->Option.getOr("")

let rec insert = tuple => {
  let (isFinished', sum') = input->Array.reduce(tuple, ((isFinished, prevSum), current) => {
    switch isFinished {
    | true => (true, prevSum)
    | false =>
      switch current->Int.fromString {
      | Some(v) =>
        set->Set.has(prevSum + v)
          ? {
              (true, prevSum + v)
            }
          : {
              set->Set.add(prevSum + v)
              (false, prevSum + v)
            }

      | None => (true, prevSum)
      }
    }
  })
  if isFinished' {
    (true, sum')
  } else {
    (false, sum')->insert
  }
}
insert((false, 0))->Js.log
