# Rail Fence Cipher

Welcome to Rail Fence Cipher on Exercism's Haskell Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Implement encoding and decoding for the rail fence cipher.

The Rail Fence cipher is a form of transposition cipher that gets its name from
the way in which it's encoded. It was already used by the ancient Greeks.

In the Rail Fence cipher, the message is written downwards on successive "rails"
of an imaginary fence, then moving up when we get to the bottom (like a zig-zag).
Finally the message is then read off in rows.

For example, using three "rails" and the message "WE ARE DISCOVERED FLEE AT ONCE",
the cipherer writes out:

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. E . R . D . S . O . E . E . F . E . A . O . C .
. . A . . . I . . . V . . . D . . . E . . . N . .
```

Then reads off:

```text
WECRLTEERDSOEEFEAOCAIVDEN
```

To decrypt a message you take the zig-zag shape and fill the ciphertext along the rows.

```text
? . . . ? . . . ? . . . ? . . . ? . . . ? . . . ?
. ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? .
. . ? . . . ? . . . ? . . . ? . . . ? . . . ? . .
```

The first row has seven spots that can be filled with "WECRLTE".

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? .
. . ? . . . ? . . . ? . . . ? . . . ? . . . ? . .
```

Now the 2nd row takes "ERDSOEEFEAOC".

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. E . R . D . S . O . E . E . F . E . A . O . C .
. . ? . . . ? . . . ? . . . ? . . . ? . . . ? . .
```

Leaving "AIVDEN" for the last row.

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. E . R . D . S . O . E . E . F . E . A . O . C .
. . A . . . I . . . V . . . D . . . E . . . N . .
```

Distances between elements in first row

first row
=========
nrows dots
1     0
2     1 0
3     3 2
4     5
5     7
6     9

f(n) =      0   if n == 1
       2n - 2   if n > 1

second row
==========
nrows dots
2     1
3     1
4     3
5     5
6     7
7     9
8     11


number of dots in next section
g(n, i) = f(n)          if i == 0 or i == n - 1
          f(n) - 1 - i  otherwise

```text
W . . . . . E . G C . . . R . . . L . . . T . . . E
. E . . . D . F S . O . E . E . F . E . A . O . C .
. . A . C . E I . . . V . . . D . . . E . . . N . .
. . . B . D
. . . . C
```

If you now read along the zig-zag shape you can read the original message.

## Source

### Created by

- @Average-user

### Contributed to by

- @iHiD
- @petertseng
- @ppartarr
- @sshine
- @tejasbubane

### Based on

Wikipedia - https://en.wikipedia.org/wiki/Transposition_cipher#Rail_Fence_cipher
