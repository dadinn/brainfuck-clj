(ns dadinn.brainfuck-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.core.async :as ca :refer [go-loop <! >! <!! >!! close! chan]]
            [dadinn.brainfuck :as bf]))

(defn ch2coll
  [ch]
  (lazy-seq
   (if-let [v (<!! ch)]
     (cons v (ch2coll ch)))))

(defn ch2str
  [ch]
  (s/join (map char (ch2coll ch))))


(def helloworld-long "
    +++++ +++               Set Cell #0 to 8

    [

        >++++               Add 4 to Cell #1; this will always set Cell #1 to 4

        [                   as the cell will be cleared by the loop

            >++             Add 2 to Cell #2

            >+++            Add 3 to Cell #3

            >+++            Add 3 to Cell #4ci

            >+              Add 1 to Cell #5

            <<<<-           Decrement the loop counter in Cell #1

        ]                   Loop till Cell #1 is zero; number of iterations is 4

        >+                  Add 1 to Cell #2

        >+                  Add 1 to Cell #3

        >-                  Subtract 1 from Cell #4

        >>+                 Add 1 to Cell #6

        [<]                 Move back to the first zero cell you find; this will

                            be Cell #1 which was cleared by the previous loop

        <-                  Decrement the loop Counter in Cell #0

    ]                       Loop till Cell #0 is zero; number of iterations is 8

     

    The result of this is:

    Cell No :   0   1   2   3   4   5   6

    Contents:   0   0  72 104  88  32   8

    Pointer :   ^

     

    >>.                     Cell #2 has value 72 which is 'H'

    >---.                   Subtract 3 from Cell #3 to get 101 which is 'e'

    +++++ ++..+++.          Likewise for 'llo' from Cell #3

    >>.                     Cell #5 is 32 for the space

    <-.                     Subtract 1 from Cell #4 for 87 to give a 'W'

    <.                      Cell #3 was set to 'o' from the end of 'Hello'

    +++.----- -.----- ---.  Cell #3 for 'rl' and 'd'

    >>+.                    Add 1 to Cell #5 gives us an exclamation point

    >++.                    And finally a newline from Cell #6")

(def helloworld-short
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

(deftest test-exec-helloworld
  (testing "Long \"Hello World\" program returns correct string"
    (let [ch (bf/exec nil helloworld-long)]
      (is (= "Hello World!\n" (ch2str ch)))))
  (testing "Short \"Hello World\" program returns corrent string"
    (let [ch (bf/exec nil helloworld-short)]
      (is (= "Hello World!\n" (ch2str ch))))))

(deftest test-exec-copy-times
  (testing "Makes x copies of number n"
    (are [x n res]
      (= res
         (let [in (chan 1)
               out (bf/exec in ",>,<[>.<-]")]
           (ca/onto-chan in [x n])
           (ch2coll out)))
      3 0 [0 0 0]
      0 6 []
      2 2 [2 2]
      3 4 [4 4 4]
      6 23 [23 23 23 23 23 23])))

(deftest test-twice-addition
  (testing "Adds numbers twice"
    (are [num-vec res]
      (= res
         (let [in (chan 1)
               out (bf/pipe in ",[..,]" ">,[[<+>-],]<.")]
           (go-loop [nums num-vec]
             (if (seq nums)
               (do
                 (>! in (first nums))
                 (recur (rest nums)))
               (close! in)))
           (<!! out)))
      
      [1 2 3 4 5] 30
      [1 2 3 4] 20
      [34 53 2] 178)))
