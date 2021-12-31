(ns compress-test.core)
(use '[clojure.string :only (join split)])

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;The purpore of this test is to compare execution time/amount of resources or number of sequences required to compress a set of data depending on symbol size in number of bits
;To evaluate depending on symbol size -
;number of iterations required to generate encoding
;size of encoding table
;compression performance (total number of bits post compression vs original)
;performance depending on data size

;huffman coding
;need a sequence of symbols to compress
;start with example from wikipedia
(def testSeq "this is an example of a huffman tree")

;start by creating a dictionary based on character frequency and sort by lowest frequency first
(def freq (sort-by last (frequencies (seq testSeq))))
(defn getFreq [dat] (frequencies (seq dat)))

;now generate tree
;define a node structure
(defrecord node [s f l r])
(defn makeNode [x y] (node. x y nil nil))

;make initial list into nodes
(defn makeNodeList [dat]
  (for [x (getFreq dat)] 
    (makeNode (first x) (second x))))

;apply the algorithm. starting with a sorted list, create node from the first two items and return the same list with the two items replaced by the node
(defn doPass [inputList]
  (let [sorted (sort-by :f inputList)
        x (first sorted)
        y (second sorted)]
    (concat 
      (nthrest sorted 2)
      (list (node. "" (+ (:f x) (:f y)) x y)))))

;(println (doPass nodeList))

(defn makeTree [inputList]
  (if (> (count inputList) 1)
    (makeTree (doPass inputList))
    (first inputList)))

;now build table
(defn getCodes [tree]
  (let [[x y] tree]
    (if (= nil (:l x))
      [(:s x) y]
      (concat (getCodes [(:l x) (conj y 0)])
            (getCodes [(:r x) (conj y 1)])))))


(defn makeCodeList [dat] (getCodes [(makeTree (makeNodeList dat)) []]))
(defn makeCodeMap [dat]
  (let [x (take-nth 2 (makeCodeList dat))
        y (take-nth 2 (rest (makeCodeList dat)))]
    (zipmap x y)))

;now this can be used to encode the original message
(defn applyMap [data cMap]
  (for [x (seq data)]
    (get cMap x)))

;currently the code relies on being supplied a series of symbols, in this case a string.
;Want to change this to work with any arbitrary set of binary data
(defn huffman-string [dat]
  (apply concat (applyMap dat (makeCodeMap dat))))

;this function takes data as a binary list and returns its compressed version. symSize determines how many bits each symbol has
(defn huffman-bin [dat symSize]
  (let [partDat (partition symSize dat)
        cmap (makeCodeMap partDat)]
    [(apply concat
      (applyMap partDat
        cmap))
     cmap]))

(defn char2bin [c]
  (let [bin (Integer/toString (int c) 2)]
    (str (join (repeat (- 8 (count bin)) 0)) bin)))
;convert testSeq into binary vector
(defn binVec [dat]
  (flatten
    (let [binStrSeq
          (for [x (seq dat)]
            (char2bin x))]
      (for [x binStrSeq]
        (for [y x]
          y)))))


(println "test")

;(def data (take 10000 (slurp "input.txt")))
(def data (binVec "ABCDABCD"))

;now for arithmetic coding
;need to define in terms of probability. Starting with the simple code ABCDABCD to start
;each symbol has an equal probabilty to appear
;therefore

;(println (huffman-string testSeq))
;(println (huffman-bin (binVec testSeq) 16))
;(def huffTest (huffman-bin (binVec data) 4))
(print "Compressed: ")
;(println (second huffTest))
;(println (count (first huffTest)))
(print "Original: ")
(println (count data))


(defn -main [] 
  (foo "test"))
