module AdventOfCode2021.Common.Letters

let private ON = true
let private x_ = false

let fourBySixLetters =
    [|
        ('A',
         [|
             [| x_; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
         |])

        ('B',
         [|
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; x_ |]
         |])

        ('C',
         [|
             [| x_; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; ON |]
             [| x_; ON; ON; x_ |]
         |])

        ('D',
         [|
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; x_ |]
         |])

        ('E',
         [|
             [| ON; ON; ON; ON |]
             [| ON; x_; x_; x_ |]
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; ON; ON; ON |]
         |])

        ('F',
         [|
             [| ON; ON; ON; ON |]
             [| ON; x_; x_; x_ |]
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
         |])

        ('G',
         [|
             [| x_; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; ON; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; ON; ON |]
         |])

        ('H',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
         |])

        ('I',
         [|
             [| ON; ON; ON; x_ |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
             [| ON; ON; ON; x_ |]
         |])

        ('J',
         [|
             [| x_; x_; ON; ON |]
             [| x_; x_; x_; ON |]
             [| x_; x_; x_; ON |]
             [| x_; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; ON; x_ |]
         |])

        ('K',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; ON; x_ |]
             [| ON; ON; x_; x_ |]
             [| ON; x_; ON; x_ |]
             [| ON; x_; ON; x_ |]
             [| ON; x_; x_; ON |]
         |])

        ('L',
         [|
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; ON; ON; ON |]
         |])

        ('M',
         [|
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
         |])

        ('N',
         [|
             [| ON; x_; x_; ON |]
             [| ON; ON; x_; ON |]
             [| ON; x_; ON; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
         |])

        ('O',
         [|
             [| x_; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; ON; x_ |]
         |])

        ('P',
         [|
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; x_; x_; x_ |]
         |])

        ('Q',
         [|
             [| x_; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; ON; x_ |]
             [| x_; ON; x_; ON |]
         |])

        ('R',
         [|
             [| ON; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; x_ |]
             [| ON; x_; ON; x_ |]
             [| ON; x_; x_; ON |]
         |])

        ('S',
         [|
             [| x_; ON; ON; ON |]
             [| ON; x_; x_; x_ |]
             [| x_; ON; ON; x_ |]
             [| x_; x_; x_; ON |]
             [| x_; x_; x_; ON |]
             [| ON; ON; ON; x_ |]
         |])

        ('T',
         [|
             [| ON; ON; ON; ON |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
             [| x_; ON; x_; x_ |]
         |])

        ('U',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; ON; x_ |]
         |])

        ('V',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; x_; ON |]
             [| x_; x_; ON; x_ |]
         |])

        ('W',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; ON; ON; ON |]
             [| ON; x_; x_; ON |]
         |])

        ('X',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; ON; x_ |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
         |])

        ('Y',
         [|
             [| ON; x_; x_; ON |]
             [| ON; x_; x_; ON |]
             [| x_; ON; x_; ON |]
             [| x_; x_; ON; x_ |]
             [| x_; x_; ON; x_ |]
             [| x_; x_; ON; x_ |]
         |])

        ('Z',
         [|
             [| ON; ON; ON; ON |]
             [| x_; x_; x_; ON |]
             [| x_; x_; ON; x_ |]
             [| x_; ON; x_; x_ |]
             [| ON; x_; x_; x_ |]
             [| ON; ON; ON; ON |]
         |])
    |]
