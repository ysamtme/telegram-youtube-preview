(.format r"(\D*({}[0-{}]|[0-{}][0-9])\d{2}\D*|.*([ (]|[Нн]ет).*)"
         first second (dec first))

(defn group-regex [first second third]
  [:alternative
    [:concat
      [:star char.non-digit]
      [:alternative
        [:concat first [:char-set [:range "0" second]]]
        [:concat [:range "0" third] [:range "0" "9"]]]
      [:times 2 char.digit]
      [:star char.non-digit]]
    [:concat
      [:star char.any]
      [:alternative
        [:char-set " "  "("]
        [:concat [:char-set "Н" "н"] "ет"]]]])
