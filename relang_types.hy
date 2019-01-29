(require [hy.contrib.walk [let]])

(import [collections [namedtuple]]
        [types [SimpleNamespace]]
        [re [escape :as escape]]

        [functools [partial]]
        [hy [HyExpression HyDict]]

        [parse_interval [hms-pattern :as *hms-pattern*]])


(setv lmap (comp list map))

(setv lrest (comp list rest))


(defn walk [inner outer form]
  "Traverses form, an arbitrary data structure. Applies inner to each
  element of form, building up a data structure of the same type.
  Applies outer to the result."
  (cond
   [(instance? HyExpression form)
    (outer (HyExpression (lmap inner form)))]
   [(instance? HyDict form)
    (HyDict (outer (HyExpression (lmap inner form))))]
   [(instance? list form)
    (outer (HyExpression (lmap inner form)))]
   [(coll? form)
    (walk inner outer (list form))]
   [True (outer form)]))


(defn postwalk [f form]
  "Performs depth-first, post-order traversal of form. Calls f on each
  sub-form, uses f's return value in place of the original."
  (walk (partial postwalk f) f form))


(defn prewalk [f form]
  "Performs depth-first, pre-order traversal of form. Calls f on each
  sub-form, uses f's return value in place of the original."
  (walk (partial prewalk f) identity (f form)))


(defn el? [x]
  (and (coll? x)
       (keyword? (first x))))


(defn cat [xs]
  (try
    (.join "" xs)
    (except [TypeError]
      (raise (ValueError xs)))))


(defclass char []
  [any [:special "."]
   
   whitespace     [:special r"\s"]
   non-whitespace [:special r"\S"]

   digit [:special r"\d"]])


(defclass seq []
  [number [:many char.digit]])


(setv default-transformers
      {:special identity
       :regex identity

       :concat
       (fn [&rest xs]
         (cat xs))

       :maybe
       (fn [x]
         (.format "{}?" x))

       :non-greedy-star
       (fn [x]
         (.format "{}*?" x))

       :group
       (fn [x]
         (.format "(?:{})" x))

       :alternative
       (fn [&rest xs]
         (.join "|" xs))

       :char-class
       (fn [x]
         (.format "[{}]"
                  (.replace x "]" r"\]")))

       :star
       (fn [x]
         (.format "{}*" x))

       :many
       (fn [x]
         (.format "{}+" x))
       
       :named
       (fn [nm x]
         (.format "(?P<{}>{})" nm x))

       :ahead
       (fn [x]
         (.format "(?={})" x))})


(setv default-expanders
      {:group
       (fn [&rest xs]
         [:e/group [:concat #* xs]])

       :maybe
       (fn [&rest xs]
         [:e/maybe [:group #* xs]])

       :non-greedy-star
       (fn [&rest xs]
         [:e/non-greedy-star [:group #* xs]])

       :alternative
       (fn [&rest xs]
         [:group [:e/alternative #* xs]])

       :star
       (fn [&rest xs]
         [:e/star [:group #* xs]])

       :many
       (fn [&rest xs]
         [:e/many [:group #* xs]])

       :named
       (fn [nm &rest xs]
         [:e/named nm [:concat #* xs]])

       :ahead
       (fn [&rest xs]
         [:e/ahead [:concat #* xs]])

       :any-order
       (fn [&rest xs]
         [:alternative
          #* (map (fn [x] [:concat #* x])
                  (permutations xs))])

       })


(setv default-dont-escape
      [:special
       :regex
       :char-class])


(defn expand [expanders nm args]
  (if (not-in nm expanders)
    (raise (ValueError ["There is no expander for" [nm #* args]])))
  (let [f (get expanders nm)]
    (try
      (f #* args)
      (except [TypeError]
        (raise (ValueError ["Signature mismatch for" [nm #* args]]))))))


(defn transform [transformers nm args]
  (if (not-in nm transformers)
    (raise (ValueError ["There is no transformer for" [nm #* args]])))
  (let [f (get transformers nm)]
    (try
      (f #* args)
      (except [TypeError]
        (raise (ValueError ["Signature mismatch for" [nm #* args]]))))))


(defn walk-escape [dont-escape x]
  (cond [(and (el? x) (in     (first x) dont-escape))
         x]
        [(and (el? x) (not-in (first x) dont-escape))
         [(first x) #* (lmap (partial walk-escape dont-escape) (lrest x))]]
        [(string? x)
         (escape x)]
        [True
         (raise (ValueError x))]))


(defn expanded-kw? [k]
  (.startswith (name k) "e/"))


(defn walk-expand [expanders rexpr]
  (if-not (el? rexpr)
    rexpr
    (let [nm (first rexpr)
          args (lrest rexpr)]
      (if (and (in nm expanders) (not (expanded-kw? nm)))
        (walk-expand expanders (expand expanders nm args))
        [nm #* (lmap (partial walk-expand expanders) args)]))))


(defn walk-replace-e-prefix [rexpr]
  (defn replace-e-prefix [kw]
    (-> kw (name) (.replace "e/" "") (keyword)))
  (postwalk
    (fn [x]
      (if (el? x)
        [(replace-e-prefix (first x)) #* (lrest x)]
        x))
    rexpr))


(defn walk-transform [transformers rexpr]
  (postwalk
    (fn [x]
      (if (el? x)
        (transform transformers (first x) (lrest x))
        x))
    rexpr))


(defn rexpr->regex [dont-escape expanders transformers rexpr]
  (as-> rexpr $
      (walk-escape dont-escape $)
      (walk-expand expanders $)
      (walk-replace-e-prefix $)
      (walk-transform transformers $)))


(defn main []
  (defn make-regex [a b c]
    [:concat
      [:ahead seq.number [:char-class [:concat a b c]]]
      [:maybe [:named a seq.number] "h"]
      [:maybe [:named b seq.number] "m"]
      [:maybe [:named c seq.number] "s"]])

  (defn youtube-bot-pattern [timestamp-rexpr end-rexpr]
    [:concat [:non-greedy-star char.any]
      [:named "url"
        [:maybe "http" [:maybe "s"] "://"]
        [:alternative
          "youtu.be/"
          [:concat [:maybe "www"] "youtube.com/watch"]]
        [:star char.non-whitespace]
        [:concat [:char-class "?&"] "t=" timestamp-rexpr]]
      [:many char.whitespace]
      [:named "end" end-rexpr]])
  
  (let [youtube-bot-regex
        (rexpr->regex
          default-dont-escape
          default-expanders
          default-transformers
          (youtube-bot-pattern (make-regex "h" "m" "s")
                               [:many char.non-whitespace]))]
    
    (print youtube-bot-regex)

    (import re
            [pprint [pprint]])
    (pprint (.groupdict
              (re.match youtube-bot-regex
                        "https://youtu.be/urOhWPAS8OI?t=1h18m18s 1h20m3s"))))

  (print
    (rexpr->regex
      default-dont-escape
      default-expanders
      default-transformers
      [:any-order "h" "m" "s"])))


(if (= __name__ "__main__")
  (main))
