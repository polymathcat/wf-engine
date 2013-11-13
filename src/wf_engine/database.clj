(ns wf-engine.database
  (:gen-class))

;;;;DATABASE

(defn schema? [s]
  (and (map? s)
       (contains? s :type)
       (= (:type s) :schema)))

;schema-eq?

(defn schema-subset? [s1 s2]
  (if (and (schema? s1) (schema? s2))
    (every? #(and (contains? (:schema s2) (first %))
                    ;true)
                    (= (get s2 (first %)) (second %)))
         (:schema s1))
    false))

(defn schema-make [columns]
  {:type   :schema,
   :schema columns})


(defn schema-append [s1 s2]
  {:type   :schema,
   :schema (conj (:schema s1) (:schema s2))})

;tests
(schema? 1)
(schema? {:type :relation, :relation nil})
(schema? [1 :schema 2])
(schema? (schema-make {}))
(schema? (schema-make {:name :sequence, :format :fasta}))

(schema-subset? (schema-make {:name :sequence})
                (schema-make {:name :sequence, :format :fasta}))

(schema-subset? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence, :format :fasta}))

(schema-subset? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence}))
