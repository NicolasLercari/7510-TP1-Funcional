(ns logical-interpreter)

(defn find-first
         [f coll]
         (first (filter f coll)))


(defprotocol Operacion
  (comparar [this query bdd])
   (esta [query bdd])
  )

(defrecord Fact [name parametros]
  Operacion
  (comparar [this query bdd]
    "compara dos facts, compara el nombre, la cantidad de parametros,
    y que los parametros sean iguales."
    (and (and (boolean (= (compare (:parametros this) (:parametros query)) 0)) 
         (boolean (= (compare (:name this) (:name query)) 0)))
       (boolean (= (compare (count (:parametros this)) (count(:parametros query))) 0))
         )
    )   
  (esta [query bdd]
       "compara cada elemento de la bdd por el query."
       (boolean (not= (find-first #(comparar % query bdd) bdd) nil))    
     ) 
)

(defrecord Rule [name parametros condicion]
  Operacion
  (comparar [this query bdd]
    "compara el nombre de la rule con el nombre del query y la cantidad de parametros, si esto es cierto
     procede a especializar cada fact del lado derecho de la rule con los parametros del query pedido
     y compara con todos estos facts creados.."
    ( if (not (and (boolean (= (compare (count (:parametros this)) (count(:parametros query))) 0)) 
         (boolean (= (compare (:name this) (:name query)) 0))))
          false
          (do 
            (def mapa (into {} (map vector (:parametros this) (:parametros query))))
            (def facts  (into [] (map (fn [x] (new Fact (:name x) (into [] (map (fn [y] (get mapa y) ) (:parametros x))))) (:condicion this))) )
            ;(reduce (fn [x y] ( and x y )) (into [] (map (fn [unFact] (not= (find-first #(comparar % unFact bdd) bdd) nil) ) facts)))
            (reduce (fn [x y] ( and x y )) (into [] (map (fn [unFact] (esta unFact bdd)) facts)))
          )    
      )   
    )
    (esta [query bdd]
       "compara cada elemento de la bdd por el query."
       (boolean (not= (find-first #(comparar % query bdd) bdd) nil))    
     )      
  )



(defn parsear-fact 
      [factStr]
      "recibe un string que representa a un fact con el siguiente formato
       add(one, one, two). retorna una lista con nombre de la operacion 
       del fact y los parametros"        
      (def fact (clojure.string/replace factStr #"," ""))
      (def fact (clojure.string/replace fact #"\." ""))
      (def fact (clojure.string/split (clojure.string/replace fact #"\)" "") #"\(" ))
      (def nombreFact (get fact 0))
      (def paramFact (clojure.string/split (get fact 1) #" "))
      [nombreFact paramFact]
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
    (esta queryFact bdd)
   (catch Exception e nil))
)