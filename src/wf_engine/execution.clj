;;; Copyright ©2013 Ruben Acuna

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAMESPACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns wf-engine.execution
  (:gen-class)
  (:use
       wf-engine.database))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Protocol [Layer
                     Type
                     Contents])

(defn make-protocol [layer type contents]
  (Protocol. layer
             type
             contents))

(defrecord ExecutionPrimative [ID
                               Title
                               Schema-input
                               Schema-output])


(defrecord ExecutionOperator [ID
                              Title
                              Blocks])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCESSORS - TYPE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defn execution-protocol? [thing]
  (or (execution-block-primative? thing)
      (execution-block-operator? thing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCESSORS - STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn execution-get-edges [protocol]
  "Returns a list of ID (keywords) pairs (vectors) that represent connectivity in an execution protocol.
  Recursively, this is the outgoing edges of a protocol combined with the outgoing edges of each nodes it
  is connected to."

  (if (execution-block-primative? protocol)

    ;blocks can't have out going edges
    (list)

    ;otherwise is an operator
    (reduce concat
            (cons
               ;produce local edges
                 (cond ;fold - have to  label edges by order
                       (execution-block-fold? protocol)
                       (first (reduce (fn [state active-protocol]
                                          (let [edge (vector (.ID (.Contents protocol))
                                                             (.ID (.Contents active-protocol))
                                                             (str (second state)))]
                                            (list (conj (first state) edge)
                                                  (inc (second state)))))
                                      (list (list) 0)
                                      (.Blocks (.Contents protocol))))

                     ;map clone - don't need to label edges
                     (execution-block-mapclone? protocol)
                       (map #(vector (.ID (.Contents protocol)) (.ID (.Contents %)) "")
                            (.Blocks (.Contents protocol)))

                     :else
                       (throw (Exception. "execution-get-edges: Don't know how to build edges for operator found.")))

               ;recursive step
               (map execution-get-edges (.Blocks (.Contents protocol)))))))

(defn execution-get-ids [protocol]
  "Returns a list of IDs (keywords) found in an execution protocol."

  (if (execution-block-primative? protocol)
    (list (.ID (.Contents protocol)))

    ;otherwise is an operator
    (reduce concat
            (cons (list (.ID (.Contents protocol)))
             (map execution-get-ids (.Blocks (.Contents protocol)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCESSORS - COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn execution-get-by-id [protocol id]
  "Searches a protocol and returns the sub-protocol with the required ID."

  (if (and (execution-protocol? protocol)
           (= (.ID (.Contents protocol)) id))
        protocol
        (if (execution-block-primative? protocol)
            nil
            ;otherwise is an operator
            (first (filter #(not (nil? %))
                           (map #(execution-get-by-id % id) (.Blocks (.Contents protocol))))))))

(defn execution-contains-id? [protocol id]
  (not (nil? (execution-get-by-id protocol id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERROGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRUCTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(defn execution-make-protocol-mapclone [id title blocks]
  ;will always produce a sound protocol
  (Protocol. :execution
             :block-mapclone
             (ExecutionOperator. id
                                 title
                                 blocks)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REFINEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn execution-split-protocol-to-fold [protocol block1 block2]

  (cond (not (schema-subset? (execution-get-schema-input block1)
                             (execution-get-schema-input protocol)))
          (throw (Exception. "split-protocol-to-fold: Block1 requires data that the original protocol's did not have."))

        (not (schema-subset? (execution-get-schema-input block2)
                             (execution-get-schema-output block1)))
          (throw (Exception. "split-protocol-to-fold: Block1 doesn't produce data sufficient for Block2."))

        (not (schema-subset? (execution-get-schema-output protocol)
                             (execution-get-schema-output block2)))
          (throw (Exception. "split-protocol-to-fold: Block2 doesn't produce data sufficient to take the original protocol's place."))

        :else
          (execution-make-protocol-fold (.ID (.Contents protocol)) (.Title (.Contents protocol)) [block1 block2])))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn build-sprouts-execution []
  (execution-make-protocol-fold :fold-1
                                "Fold"
                                [(execution-make-protocol-primative :job-parser
                                                                    "Job Parser"
                                                                    (schema-make {"job_filepath" :string})
                                                                    (schema-make {"pdb_id" :string}))

                                 (execution-make-protocol-mapclone :mapclone-1
                                                                   "MapClone"
                                                                   [(execution-make-protocol-primative :id-fetchfasta
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

;(execution-get-schema-output (build-sprouts-execution))

(let [protocol-old  (build-sprouts-execution)
      target-id     :id-fetchentryider
      block1        (execution-make-protocol-primative :id-test1
                                   "Test1"
                                   (schema-make {"pdb_id" :string})
                                   (schema-make {"foobar" :string}))
      block2        (execution-make-protocol-primative :id-test2
                                   "Entry IDer"
                                   (schema-make {"foobar" :string})
                                   (schema-make {"protein_id" :string, "fasta_filepath" :string, "pdb_filepath" :string, "dssp_filepath" :string}))
      protocol-part (execution-split-protocol-to-fold (execution-get-by-id protocol-old target-id) block1 block2)
      protocol-new  (execution-replace-procotol protocol-old target-id protocol-part)]

      protocol-new)












