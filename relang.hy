(defn cat [patterns]
  (.join "" patterns))

(defn seq [&rest patterns]
  (cat patterns))

(defn group [&rest patterns]
  (.format "(?:{})" (cat patterns)))

(defn capture [&rest patterns]
  (.format "({})" (cat patterns)))

(defn named [name- &rest patterns]
  (.format "(?P<{}>{})" name- (cat patterns)))

(defn -maybe-group [patterns]
  (setv p (cat patterns))
  (if (> (len p) 1)
    (group p)
    p))

(defn many [&rest patterns]
  (.format "{}+" (-maybe-group patterns)))

(defn star [&rest patterns]
  (.format "{}*" (-maybe-group patterns)))

(defn non-greedy-star [&rest patterns]
  (.format "{}*?" (-maybe-group patterns)))

(defn maybe [&rest patterns]
  (.format "{}?" (-maybe-group patterns)))

;; or "char-set"?
(defn char-class [chars]
  (.format "[{}]" chars))

(defn alternative [&rest patterns]
  (group (.join "|" patterns)))

(defn ahead [&rest patterns]
  (.format "(?={})" (cat patterns)))

(defclass char []
  [digit    r"\d"
   any      "."
   whitespace     r"\s"
   non-whitespace r"\S"])

(setv
  start  "^"
  end    "$"
  number (many char.digit))
