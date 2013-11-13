;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.database
  (:gen-class)
  (:use
       clojure.data))

;;; procedures

(defn schema? [s]
  "Returns true if s is schema, false otherwise."
  (and (map? s)
       (contains? s :type)
       (= (:type s) :schema)))

(defn schema-subset? [s1 s2]
  "Returns true if the columns of s1 are subset of s2 (with column type preserved), false otherwise."
  (if (and (schema? s1) (schema? s2))
    (every? #(and (contains? (:schema s2) (first %))
                  (= (get (:schema s2) (first %)) (second %)))
             (:schema s1))
    false))

(defn schema-eq? [s1 s2]
  "Returns true if s1 and s2 are schemas that have identical columns and column types, false otherwise."
  (and (schema? s1)
       (schema? s2)
       (schema-subset? s1 s2)
       (schema-subset? s2 s1)))

(defn schema-make [columns]
  "Returns a new schema containing a map of columns. The keys are column names and the values are column types."
  {:type   :schema,
   :schema columns})


(defn schema-join [s1 s2]
  "Returns a schema consisting of s1 and s2 combined. "

  (if (not (schema-eq? s1 s2))
      (throw (Exception. "Unimplemented: schemas must be identical to be joined."))
      {:type   :schema,
       :schema (conj (:schema s1) (:schema s2))}))

;;; tests
;(schema? 1)
;(schema? {:type :relation, :relation nil})
;(schema? [1 :schema 2])
;(schema? (schema-make {}))
;(schema? (schema-make {:name :sequence, :format :fasta}))


(schema-subset? (schema-make {:name :sequence})
                (schema-make {:name :sequence, :format :fasta}))
(schema-subset? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence, :format :fasta}))
(schema-subset? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :unknown, :format :fasta}))
(schema-subset? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence}))


(schema-eq? (schema-make {:name :sequence})
                (schema-make {:name :sequence, :format :fasta}))
(schema-eq? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence, :format :fasta}))
(schema-eq? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :unknown, :format :fasta}))
(schema-eq? (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence}))


(schema-join (schema-make {:name :sequence})
                (schema-make {:name :sequence, :format :fasta}))
(schema-join (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence, :format :fasta}))
(schema-join (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :unknown, :format :fasta}))
(schema-join (schema-make {:name :sequence, :format :fasta})
                (schema-make {:name :sequence}))





