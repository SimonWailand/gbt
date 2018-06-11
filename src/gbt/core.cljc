(ns gbt.core)

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* true))

;; The operations and address representations here based on
;; "An isomorphism between the p-adic integers and a ring
;; associated with a tiling of N-space by permutohedra."
;; by Wei Z. Kitto, Andrew Vince, and David C. Wilson 1994

;; Addresses Representations
;; An address in a generalized balanced ternary space of
;; dimension n has a canonical form of an integer with a base
;; of 2^(n+1)-1. Its standard form is a vector of the digits
;; of that integer represented as bit strings of length n+1
;; e.g. GBT2
;; canonical: 34 (base 7)
;; base10: 25
;; standard: [100 011] (little endian)
;; bit string digits in standard form are currently
;; just the low bits of Longs/Numbers.

(defprotocol Ops
  ;; address constructor
  (int->address [sp x])

  ;; address conversions
  (address->string [sp a])
  (address->int [sp a])

  ;; ensure address is in the given space
  ;; "projects" an address from another space
  (conform [sp a])

  ;; helper function to lookup by offset
  ;; (+ x (* radix y)) and handle nils
  (lookup-add [sp x y])
  (lookup-carry [sp x y])

  ;; unary/binary operations on addresses
  (inv [sp a])
  (add [sp a] [sp a b])
  (sub [sp a] [sp a b])
  (mul [sp a] [sp a b]))

;; Some helpful addresses that are the valid in all dimensions
(def ^:const origin [0])
(def ^:const one [1])
(defn origin? [a] (= a origin))
(defn one? [a] (= a one))

(defrecord Space [dim
                  bits
                  radix
                  digits
                  max-len
                  pows
                  inv-lut
                  add-lut
                  add-carry-lut
                  mul-lut]
  Ops
  (int->address [_ x]
    (if (zero? x)
      origin
      (mapv #(mod (quot x %) radix)
            (take-while #(>= x %) pows))))

  (address->string [_ a] (apply str (rseq a)))
  (address->int [_ a] (apply + (map * a pows)))

  (conform [_ a] (mapv #(bit-and radix %) a))

  (inv [_ a] (mapv #(nth inv-lut %) a))

  (lookup-add [_ x y]
    (cond (nil? x) y
          (nil? y) x
          :else (nth add-lut (+ x (* radix y)))))
  (lookup-carry [_ x y]
    (when (and x y)
      (nth add-carry-lut (+ x (* radix y)))))

  (add [_ a] a)
  (add [sp a b]
    (cond (origin? a) b
          (origin? b) a
          :else
          (loop [a a
                 b b
                 carry nil
                 sum ()]
            ;; Not as elegant as I had initially hoped
            (let [x (first a)
                  y (first b)
                  xy (lookup-add sp x y)
                  xyc (lookup-carry sp x y)
                  curr-sum (lookup-add sp xy carry)
                  next-carry (lookup-add sp xyc (lookup-carry sp xy carry))]
              (if curr-sum
                (recur (rest a)
                       (rest b)
                       next-carry
                       (conj sum curr-sum))
                (if-some [final-sum (seq (drop-while zero? sum))]
                  (vec (reverse final-sum))
                  origin))))))

  (sub [sp a] (inv sp a))
  (sub [sp a b] (add sp a (inv sp b)))

  (mul [_ a] a)
  ; TODO: is there is an algo w/o partial sums?
  (mul [sp a b]
    (cond (origin? a) a
          (origin? b) b
          (one? a)    b
          (one? b)    a
          :else
          (apply (partial add sp)
                 (map-indexed
                  (fn [i d]
                    (let [offset (* d radix)]
                      (vec (concat (repeat i 0)
                                   (map #(nth mul-lut (+ offset %)) b)))))
                  a)))))

;;; Helper functions and constants for defining space properties

;; The integer limit for addresses. Java Long/MAX_VALUE is larger
;; and Clojure supports BigInts, but I'm using Javascript's
;; Number.MAX_SAFE_INTEGER to allow our addresses to be used
;; on the client and server if necessary. Someday maybe use
;; goog.math.Long explicitly? I think it would require a lot of
;; care. Maybe browser support for BigInt will take off.
;; In practice addresses will, at most, be one less than this
;; TODO: currently only used to limit pows seq. Should it be used
;; as a constraint elsewhere? Implement.
(def ^:const int-limit #?(:clj (dec (bit-shift-left 1 53))
                          :cljs (.-MAX_SAFE_INTEGER js/Number)))

(defn- ln [x]
  #?(:clj (Math/log x)
     :cljs (.log js/Math x)))

;; Change of base: log-b(x) = ln(x) / ln(b)
;; There might very well be some floating point
;; precision issues with this. Revisit.
(defn- log-b [x b] (/ (ln x) (ln b)))

(defn- +mod [x y z] (mod (+ x y) z))
(defn- *mod [x y z] (mod (* x y) z))

(defn- bit-rotr
  "Performs a 1 bit right rotate on a bit set of length n."
  [x n]
  (let [rs (unsigned-bit-shift-right x 1)]
    (if (bit-test x 0)
      (bit-flip rs (dec n))
      rs)))

;; The ocean of performance enhancements that could be done here is
;; vast and deep. I'm using look-up-tables for the operations.
;; At the moment we only support dimensions 1, 2, 3, & 4 because
;; GBT4 is base 2^(4+1)-1 = 31 and both Java and Javascript only
;; handle string conversions up to base 36 (GBT5 is base 63).
;; Need a custom string parser to handle Base64 and beyond.
(defn create-space
  "Constructor for a Space record. Takes a dimension
  and then derives several values that are used for
  performing operations on addresses in the space."
  [dim]
  {:pre [(and (int? dim) (<= 1 dim 4))]}
  (let [bits          (inc dim)
        radix         (dec (bit-shift-left 1 bits))
        digits        (vec (range radix))
        max-len       (dec (long (log-b int-limit radix)))
        pows          (into [1] (take max-len (iterate (partial * radix) radix)))
        inv-lut       (into [0] (map (partial bit-and-not radix) (rest digits)))
        combos        (for [x digits y digits] [x y])
        add-lut       (mapv (fn [[x y]] (+mod x y radix)) combos)
        add-carry-lut (mapv (fn [[x y]]
                              (let [sum (nth add-lut (+ x (* y radix)))
                                    xor (bit-xor x y sum)]
                                (if-not (or (zero? xor)
                                            (= radix xor))
                                  (bit-rotr xor bits))))
                            combos)
        mul-lut       (mapv (fn [[x y]] (*mod x y radix)) combos)]
    (->Space dim
             bits
             radix
             digits
             max-len
             pows
             inv-lut
             add-lut
             add-carry-lut
             mul-lut)))

;; TODO:
;; 1) use clojure.spec in testing. Use for fn validation?
;; 2) custom pprint for luts
;; 3) macro for binding functions to a set space
;;    e.g. in some other ns: (def add (partial gbt/add sp))
