(module

  (func $test (param $x i32) (result i32)
    (get_local $x)
  )

  (func $call (result i32)
    (call $test
      (i32.const 5)
    )
  )

  (func $add (param $p1 i32) (param $p2 i32) (result i32)
    (i32.add
      (get_local $p1)
      (get_local $p2)
    )
  )

  (func $sub1 (param $p i32) (result i32)
    (i32.sub
      (get_local $p)
      (i32.const 1)
    )
  )

  (func $eq (param $p i32) (result i32)
    (if (result i32)
      (i32.eq
        (get_local $p)
        (i32.const 1)
      )
      (then
        (get_local $p)
      )
      (else
        (i32.const 99)
      )
    )
  )

  (func $factorial (param $p i32) (result i32)
    (if (result i32)
      (i32.eq
        (get_local $p)
        (i32.const 1)
      )
      (then
        (get_local $p)
      )
      (else
        (i32.mul
          (call $factorial
            (get_local $p)
            (i32.sub
              (get_local $p)
              (i32.const 1)
            )
          )
        )
      )
    )
  )

  

  (export "test" (func $test))
  (export "call" (func $call))
  (export "add" (func $add))
  (export "sub1" (func $sub1))
  (export "eq" (func $eq))
  (export "factorial" (func $factorial))
)
