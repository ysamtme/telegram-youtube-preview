(import [hy.models [HyKeyword HyExpression]])

(setv HyKeyword.--repr--
      (fn [self]
        (+ ":" self.name))

      HyExpression.--repr--
      (fn [self]
        (.format "<{}>" (list.--repr-- self))))
