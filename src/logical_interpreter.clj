(ns logical-interpreter)

(defn find-first
         [f coll]
         (first (filter f coll)))


(defprotocol Operacion
  (comparar [this query bdd])
  )

(defrecord Fact [name parametros]
  Operacion
  (comparar [this query bdd]
    (and (and (boolean (= (compare (:parametros this) (:parametros query)) 0)) 
         (boolean (= (compare (:name this) (:name query)) 0)))
       (boolean (= (compare (count (:parametros this)) (count(:parametros query))) 0))
         )
             
    )
)

(defrecord Rule [name parametros condicion]
  Operacion
  (comparar [this query bdd]
    ( if (not (and (boolean (= (compare (count (:parametros this)) (count(:parametros query))) 0)) 
         (boolean (= (compare (:name this) (:name query)) 0))))
          false
          (do 
            (def mapa (into {} (map vector (:parametros this) (:parametros query))))
            (def facts  (into [] (map (fn [x] (new Fact (:name x) (into [] (map (fn [y] (get mapa y) ) (:parametros x))))) (:condicion this))) )
            (reduce (fn [x y] ( and x y )) (into [] (map (fn [unFact] (not= (find-first #(comparar unFact % bdd) bdd) nil) ) facts)))
          )    
      )   
    )      
  )

(defn parsear-fact 
      [x]
          
      (def fact (clojure.string/replace x #"," ""))
      (def fact (clojure.string/replace fact #"\." ""))
      (def fact (clojure.string/split (clojure.string/replace fact #"\)" "") #"\(" ))
      (def nombreFact (get fact 0))
      (def paramFact (clojure.string/split (get fact 1) #" "))
      [nombreFact paramFact]
      ;(new Fact nombreFact paramFact)
  )

(defn crearFact [factStr]

  (def nombreParamFact (parsear-fact factStr))
  (new Fact (get nombreParamFact 0) (get nombreParamFact 1))
  )

(defn parsear-rule 
    [ruleStr]

    (def lista (clojure.string/split ruleStr #":-"))
    (def rule (clojure.string/trim (get lista 0)))
    (def rule (crearFact rule))    
    (def facts (clojure.string/trim (get lista 1)))
    (def facts (into [] (map (fn [x] (clojure.string/trim x)) (clojure.string/split facts #"\)") ) ))
    (def facts (into [] (map (fn [x] (crearFact x ) ) facts)))
    [(:name rule) (:parametros rule) facts]
  )

(defn crearRule [RuleStr]

  (def nombreParamFact (parsear-rule RuleStr))
  (new Rule (get nombreParamFact 0) (get nombreParamFact 1) (get nombreParamFact 2))
  )

(defn parsearDataBase 
    [database] 
    (def data (clojure.string/trim database))
    (def data2 (clojure.string/replace data #"\n" ""))
    (def data2 (clojure.string/replace data2 #"\t" ""))
    (def data2 (clojure.string/replace data2 #"," ""))
    ;(def data2 (clojure.string/replace data2 #"\." ""))
    (clojure.string/split data2 #"\." )
)

(defn crearBaseDeDatos [database]

  (def databaseParseada (parsearDataBase database))
  (into [] (map (fn [x] 
                  (if-not (clojure.string/includes? x ":-")
                          (crearFact x)
                          (crearRule x)
                    )
                  )  databaseParseada))
  )

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
    [database query] 
  (try 
    (def bdd (crearBaseDeDatos database))
    (def queryFact (crearFact query))
    (boolean (not= (find-first #(comparar % queryFact bdd) bdd) nil))    
   (catch Exception e nil))
)