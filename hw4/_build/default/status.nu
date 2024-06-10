let partial = (ls examples/partial/ | get 'name')
let total = (ls examples/total/ | get 'name')
let pgms = $partial | append $total

let pgms = $pgms | wrap 'pgm' | upsert 'type' {|it| if ($it.pgm =~ 'partial') {'partial'} else {'total'} }

let pgms = $pgms | upsert 'expected' {|it| if ($it.pgm =~ 'fail') { 'fail' } else { 'succ' } }

let pgms = $pgms | upsert 'output' {|it|
  let ter = if ($it.type =~ 'partial') {"--partial"} else {"--termination"}
  let a = (dune exec -- ./main.exe --input $it.pgm $ter) | complete
  let res = $a.stdout
  let status = if ($res =~ "succ") {"succ"} else if ($res =~ "fail") {"fail"} else {"ERROR"}
  $status
}

let pgms = $pgms | upsert 'status' {|it|
  if ($it.output == $it.expected) {'✅'} else if ($it.output =~ "ERROR") {'🚧'} else {'❌'}
}

let pgms = $pgms | move status --after pgm

$pgms