;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.core
  (:gen-class)
  (:use
       wf-engine.database))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Protocol [Layer
                     Type
                     Contents])

(defrecord ExecutionPrimative [ID
                               Title
                               Schema-input
                               Schema-output])

(defrecord ExecutionOperator [ID
                              Title
                              Blocks])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTRUCTORS
(defn execution-make-protocol-primative [id title schema-input schema-output]
  ;will always produce a sound protocol

  (Protocol. :execution
             :block-primative
             (ExecutionPrimative. id
                     title
                     schema-input
                     schema-output)))

(defn execution-make-protocol-fold [id title blocks]
  ;check if an execution of the blocks in the fold would be sound.
   (if (or (= 1 (count blocks))
           (let [
                 input-schemas (map execution-get-schema-input blocks)

                 output-schemas (map execution-get-schema-output blocks)

                 schema-pairs (map vector output-schemas (rest input-schemas))]
                (every? #(schema-subset? (second %)
                                         (first %))
                        schema-pairs)))

       (Protocol. :execution
                  :block-fold
                  (ExecutionOperator. id
                                      title
                                      blocks))

       (throw (Exception. "Cannot build fold protocol from given blocks."))))

(defn execution-make-protocol-mapclone [blocks]
  ;will always produce a sound protocol
  (Protocol. :execution
             :block-mapclone
             (ExecutionOperator. :id-tmp
                                 "Temp"
                                 blocks)))




;;; QUESTIONS

(defn execution-block-primative? [thing]
  (and (instance? Protocol thing)
       (= (.Type thing) :block-primative)))

(defn execution-block-fold? [thing]
  (and (instance? Protocol thing)
       (= (.Type thing) :block-fold)))

(defn execution-block-mapclone? [thing]
  (and (instance? Protocol thing)
       (= (.Type thing) :block-mapclone)))

(defn execution-block-operator? [thing]
  (or (execution-block-fold? thing)
      (execution-block-mapclone? thing)))




;;; INTERROGATION

(defn execution-get-schema-input [protocol]
  (cond (execution-block-primative? protocol)
          (.Schema-input (.Contents protocol))

        (execution-block-fold? protocol)
          (execution-get-schema-input (first (.Blocks (.Contents protocol))))

        (execution-block-mapclone? protocol)
          (reduce schema-join
                  (map execution-get-schema-input (.Blocks (.Contents protocol))))
        :else
          (str "get-schema-input: unknown protocol " protocol)))

(defn execution-get-schema-output [protocol]
  (cond (execution-block-primative? protocol)
          (.Schema-output (.Contents protocol))

        (execution-block-fold? protocol)
          (execution-get-schema-output (last (.Blocks (.Contents protocol))))

        (execution-block-mapclone? protocol)
          (reduce schema-join
                  (conj (map execution-get-schema-output (.Blocks (.Contents protocol)))
                        (execution-get-schema-input protocol)))

        :else
          (str "unknown block type")))


;;; TESTING - bottom up

(defn build-sprouts-execution []
  (execution-make-protocol-fold :fold-1
                                "Fold"
                                [(execution-make-protocol-primative :id-parser
                                                                    "Job Parser"
                                                                    (schema-make {"job_filepath" :string})
                                                                    (schema-make {"pdb_id" :string}))

                                 (execution-make-protocol-mapclone [(execution-make-protocol-primative :id-fetchfasta
                                                                                                       "Fetch FASTA"
                                                                                                       (schema-make {"pdb_id" :string})
                                                                                                       (schema-make {"fasta_filepath" :string}))
                                                                    (execution-make-protocol-primative :id-fetchpdb
                                                                                                       "Fetch PDB"
                                                                                                       (schema-make {"pdb_id" :string})
                                                                                                       (schema-make {"pdb_filepath" :string}))
                                                                    (execution-make-protocol-primative :id-fetchdssp
                                                                                                       "Fetch DSSP"
                                                                                                       (schema-make {"pdb_id" :string})
                                                                                                       (schema-make {"dssp_filepath" :string}))])

                                 (execution-make-protocol-primative :id-fetchentryider
                                                                                     "Entry IDer"
                                                                                     (schema-make {"pdb_id" :string})
                                                                                     (schema-make {"protein_id" :string, "fasta_filepath" :string, "pdb_filepath" :string, "dssp_filepath" :string}))
 ]))

(execution-get-schema-output (build-sprouts-execution))

;top down - split based
(defn execution-split-protocol-to-fold [protocol block1 block2]
  (if (and (schema-subset? (execution-get-schema-input block1)
                           (execution-get-schema-input protocol))
           (schema-subset? (execution-get-schema-input block2)
                           (execution-get-schema-output block1))
           (schema-subset? (execution-get-schema-output protocol)
                           (execution-get-schema-output block2)))

      (execution-make-protocol-fold (.ID (.Contents protocol)) (.Title (.Contents protocol)) [block1 block2])
      (throw (Exception. "Cannot split block into fold operator from given blocks."))))

(defn execution-replace-procotol [protocol id replacement]

  (cond (execution-block-primative? protocol)
          (if (= (.ID (.Contents protocol)) id)
              replacement
              protocol)

        (execution-block-fold? protocol)
          (if (= (.ID (.Contents protocol)) id)
              replacement
              (execution-make-protocol-fold (.ID (.Contents protocol))
                                            (.Title (.Contents protocol))
                                            (map #(execution-replace-procotol % id replacement)
                                                  (.Blocks (.Contents protocol)))))

        (execution-block-mapclone? protocol)
          (if (= (.ID (.Contents protocol)) id)
              replacement
              (execution-make-protocol-mapclone (.ID (.Contents protocol))
                                            (.Title (.Contents protocol))
                                            (map #(execution-replace-procotol % id replacement)
                                                  (.Blocks (.Contents protocol)))))
        :else
          (throw (Exception. "execution-replace-procotol: encountered unknown protocol."))))

(defn execution-contains-id? [protocol id]

  (or (and (execution-block-primative? protocol)
           (= (.ID (.Contents protocol)) id))

      (and (execution-block-operator? protocol)
           (some execution-contains-id? (.Blocks (.Contents protocol))))))



;testing - top down

;to find node in GUI, just path from root to the selected one.

(execution-make-protocol-primative  :root
                                    "Fetch FASTA"
                                    (schema-make {"pdb_id" :string})
                                    (schema-make {}))

;stub needed by the main project
(defn -main
  []
  (println "Hello, World!"))































