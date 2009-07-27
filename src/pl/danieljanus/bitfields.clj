(ns pl.danieljanus.bitfields)

(defn make-byte-part
  "Generates a snippet of code that returns a fragment of (+ I OFS)'th byte
in a Java array of bytes ARR, composed of bits between FROM (inclusive) 
and TO (exclusive)."
  [arr i ofs from to]
  (let [byte `(aget ~arr (unchecked-add (int ~ofs) ~i))] 
    (cond (zero? from)
          `(bit-and (int ~byte) ~(- (bit-shift-left 1 to) 1)),
          
          true
          `(bit-shift-right (bit-and (int ~byte) ~(- (bit-shift-left 1 to) 1)) ~from))))

(defn make-shifted-byte-part 
  "Same as MAKE-BYTE-PART, except the byte fragment is shifted left by SHIFT."
  [seq i ofs from to shift]
  (let [inner (make-byte-part seq i ofs from to)]
    (if (zero? shift)
      inner
      `(bit-shift-left ~inner ~shift)))) 

;; We define a number of functions that are ultimately composed together,
;; starting out with the sequence of bitfield lengths (e.g. [1 10 5])
;; and finally producing a description of sums of byte fragments.

(defn partial-sums
  "Returns a seq of partial sums of the given collection."
  ([coll] (partial-sums coll 0))
  ([coll acc]
     (if (empty? coll)
       coll
       (let [next-acc (+ acc (first coll))]
         (lazy-seq (cons next-acc (partial-sums (rest coll) next-acc)))))))

(defn indexed [coll]
  (map vector (iterate inc 0) coll))

(defn intersperse-bytes 
  "Intersperses an indexed sequence of bit offsets with byte boundaries.
   For example, ([0 1] [1 11] [2 16]) becomes ([0 1] [1 8] [1 11] [2 16])."
  ([coll] (intersperse-bytes coll 8))
  ([coll boundary] 
     (if (empty? coll)
       coll
       (let [[x y :as z] (first coll)]
         (cond 
           (> y boundary) (lazy-seq (cons [x boundary] (intersperse-bytes coll (+ boundary 8))))
           (< y boundary) (lazy-seq (cons z (intersperse-bytes (rest coll) boundary)))
           true (lazy-seq (cons z (intersperse-bytes (rest coll) (+ boundary 8)))))))))

(defn add-starts
  "Takes a sequence of two-element vectors [a end] and transforms it into
   a sequence of vectors of the form [a start end], where each START is equal
   to END of the previous section, or 0 for the first element. For example,
   ([0 1] [1 8] [1 11] [2 16]) becomes ([0 0 1] [1 1 8] [1 8 11] [2 11 16])."
  [coll]
  (cons [(first (first coll)) 0 (second (first coll))]
        (map (fn [[[x1 y1] [x2 y2]]] [x2 y1 y2]) (partition 2 1 coll))))

(defn calculate-sizes
  "Replaces each END with length (i.e. END - START) in the output of ADD-STARTS."
  [coll]
  (map (fn [[x y z]] [x y (- z y)]) coll))

(defn calculate-byte-parts 
  "Takes a list of descriptions of byte fragments of the form 
   [field-number starting-bit length] and transforms each of them to
   [field-number byte-number starting-bit ending-bit]."   
  [coll]
  (map (fn [[x y z]] [x (quot y 8) (rem y 8) (+ z (rem y 8))]) coll))

(defn group-byte-parts 
  "Groups the result of CALCULATE-BYTE-PARTS by FIELD-NUMBER, producing a list of lists
   in which each list consists of the fields that have the same FIELD-NUMBER."
  [coll] 
  (loop [coll coll 
         built () 
         current []
         current-number 0] 
    (if (empty? coll)
      (reverse (cons current built))
      (let [[a b c d] (first coll)]
        (if (= a current-number)
          (recur (rest coll) built (cons [b c d] current) a)
          (recur (rest coll) (cons current built) [[b c d]] a))))))

(defn calculate-shifts 
  "Adds to each sublist element in the result of GROUP-BYTE-PARTS the number of
bits that this part should be shifted left by."
  [coll]
  (letfn [(sublist-shifts
           ([coll] (sublist-shifts (reverse coll) 0))
           ([coll shift]
              (if (empty? coll)
                coll
                (let [[a b c] (first coll)]
                  (lazy-seq
                    (cons [a b c shift] 
                          (sublist-shifts (rest coll) (+ shift (- c b)))))))))]
    (map sublist-shifts coll)))

(def get-byte-parts 
 #^{:arglists '([bitfields])
    :doc "The composition of the above functions."}
  (comp calculate-shifts
        group-byte-parts 
        calculate-byte-parts 
        calculate-sizes 
        add-starts 
        intersperse-bytes 
        indexed 
        partial-sums))

(defn add
  "Generate a series of unchecked-adds of arguments."
  ([] 0)
  ([x] `(int ~x))
  ([x y] `(unchecked-add (int ~x) (int ~y)))
  ([x y & rest] `(unchecked-add (int ~x) ~(apply add y rest))))

(defmacro with-bitfields
  "Execute BODY with variables bound as integer values of bit fields
in a fragment of given Java byte array, starting at position OFS.
BIT-DESCRIPTIONS is a vector containing alternating symbols and
numbers of bits in the corresponding bit field."  
  [arr ofs bit-descriptions & body]
  (letfn [(make-part [[x y z shift]] (make-shifted-byte-part arr x ofs y z shift))
          (make-parts [x] (apply add (map make-part x)))]
    (let [bit-descriptions (partition 2 bit-descriptions) 
          groups (map make-parts (get-byte-parts (map second bit-descriptions)))]
      `(let ~(vec (interleave (map first bit-descriptions) groups))
         ~@body))))