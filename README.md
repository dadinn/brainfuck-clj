# brainfuck

Brainfuck interpreter written in Clojure

## Usage

``` clj
(require '[clojure.core.async :as ca :refer [to-chan <!!]])
(require '[dadinn.brainfuck :as bf])

(let [in (to-chan [1 2 3 4 5])
      out (bf/exec in ">,[[<+>-],]<.")]
  (<!! out))

(let [in (to-chan [1 2 3 4 5])
      out (bf/pipe in ",[..,]" ">,[[<+>-],]<.")]
  (<!! out))
```

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
