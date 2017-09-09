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
          (
            (def mapa (into {} (map vector (:parametros this) (:parametros query))))
            (def facts  (into [] (map (fn [x] (new Fact (:name x) (into [] (map (fn [y] (get mapa y) ) (:parametros x))))) (:condicion this))) )
            (reduce (fn [x y] ( and x y )) (into [] (map (fn [x] (not= (find-first #(comparar x % bdd) bdd) nil) ) facts)))
          )    
      )   
      )      
  )





(defn parsear-fact 
      [x]
          
      (def fact (clojure.string/split (clojure.string/replace x #"\)" "") #"\(" ))
      (def nombreFact (get fact 0))
      (def paramFact (clojure.string/split (get fact 1) #" "))
      (new Fact nombreFact paramFact)
 
  )


(defn parsear-rule [x]

    (def lista (clojure.string/split x #":-"))
    (def rule (clojure.string/trim (get lista 0)))
    (def rule (parsear-fact rule))
    
    (def facts (clojure.string/trim (get lista 1)))
    (def facts (into [] (map (fn [x] (clojure.string/trim x)) (clojure.string/split facts #"\)") ) ))
    (def facts (into [] (map (fn [x] (parsear-fact x ) ) facts)))
    (new Rule (:name rule) (:parametros rule) facts)
 

  )



(defn crearBaseDeDatos [database]
    
    (try
        (into [] (map (fn [x] (if-not (clojure.string/includes? x ":-")
                    (parsear-fact x)
                    (parsear-rule x)
                  ))  database))
    
        (catch Exception e nil))
              
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

(def cantidadDeElementosQuery (int 2))

(defn parsearQuery [x]
    
    (def queryMod (clojure.string/replace x #"," ""))
    (def queryMod (clojure.string/replace queryMod #"\." ""))
    (def fact (clojure.string/split (clojure.string/replace queryMod #"\)" "") #"\(" ))
    (def nombreFact (get fact 0))


    ; PREGUNTAR POR VARIAS LINEAS EN ELSE
    (if (not= (count fact) cantidadDeElementosQuery) 
        nil
        (new Fact nombreFact (clojure.string/split (get fact 1) #" "))
    )
  )


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
    [database query] 

  (def databaseParseada (parsearDataBase database))
  (def bdd (crearBaseDeDatos databaseParseada))
  (def queryFact (parsearQuery query))
  (if (or (= bdd nil) (= queryFact nil))
      nil
      (boolean (not= (find-first #(comparar % queryFact bdd) bdd) nil))    
    )
)