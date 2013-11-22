;;; Copyright ©2013 Ruben Acuna

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAMESPACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns wf-engine.data
  (:gen-class)
  (:use
       wf-engine.database
       wf-engine.execution))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord DataPrimative [Nodes   ;list
                          Edges   ;list
                          Tail])  ;keyword

(defrecord DataNode [ID
                     Title])

(defrecord DataEdge [ID-Start
                     ID-End
                     Title])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRUCTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare data-make-flowgraph)

(defn data-make-flowgraph-primative [protocol]
  (DataPrimative.  (list (DataNode. (.ID (.Contents protocol))
                                    (.Title (.Contents protocol))))
                   (list)
                   (.ID (.Contents protocol))))

(defn data-make-flowgraph-fold [protocol]

  (let [recursive-graphs (map data-make-flowgraph (.Blocks (.Contents protocol)))
        recursive-nodes  (reduce concat (map #(.Nodes %) recursive-graphs))
        recursive-edges  (reduce concat (map #(.Edges %) recursive-graphs))]

      (DataPrimative.  (let [local-nodes (list (DataNode. (.ID (.Contents protocol))
                                                          (.Title (.Contents protocol))))]
                            (concat local-nodes recursive-nodes))

                       (let [fold-to-first (list (DataEdge. (.ID (.Contents protocol))
                                                            (.ID (.Contents (first (.Blocks (.Contents protocol)))))
                                                            "1"))
                             ids-head (map #(.ID (.Contents %))
                                           (.Blocks (.Contents protocol)))
                             ids-tail (map #(.Tail %)
                                           recursive-graphs)
                             id-pairs (map vector ids-tail (rest ids-head))
                             inner-edges (map #(DataEdge. (first %) (second %) "2")
                                              id-pairs)
                             local-edges (concat fold-to-first inner-edges)]
                         (concat local-edges recursive-edges))



                       ;(.ID (.Contents (last (.Blocks (.Contents protocol)))))

                       (.Tail (last recursive-graphs))

                       )))

(defn data-make-flowgraph-mapclone [protocol]
  (let [recursive-graphs (map data-make-flowgraph (.Blocks (.Contents protocol)))
        recursive-nodes  (reduce concat (map #(.Nodes %) recursive-graphs))
        recursive-edges  (reduce concat (map #(.Edges %) recursive-graphs))
        recursive-tails  (map #(.Tail %) recursive-graphs)
        local-tail       (keyword (str (name (.ID (.Contents protocol))) "-join"))]

  (DataPrimative. (let [local-nodes (list (DataNode. (.ID (.Contents protocol))
                                                     (.Title (.Contents protocol)))
                                          (DataNode. local-tail
                                                     "Join"))]
                       (concat local-nodes recursive-nodes))


                   (let [mapclone-to-child-edges (first (reduce (fn [state active-protocol]
                                                                  (let [edge (DataEdge. (.ID (.Contents protocol))
                                                                                        (.ID (.Contents active-protocol))
                                                                                        "")]
                                                                    (list (conj (first state) edge)
                                                                          (inc (second state)))))
                                                                (list (list) 0)
                                                                (.Blocks (.Contents protocol))))

                        tails-to-mapclone-edges (first (reduce (fn [state active-id]
                                                                  (let [edge (DataEdge. active-id
                                                                                        local-tail
                                                                                        "")]
                                                                    (list (conj (first state) edge)
                                                                          (inc (second state)))))
                                                                (list (list) 0)
                                                                recursive-tails))
                        local-edges (concat mapclone-to-child-edges
                                            tails-to-mapclone-edges)]

                       (concat local-edges recursive-edges))

                   local-tail)))


(defn data-make-flowgraph [protocol]
  (cond (execution-block-primative? protocol)
          (data-make-flowgraph-primative protocol)

        (execution-block-fold? protocol)
          (data-make-flowgraph-fold protocol)

        (execution-block-mapclone? protocol)
          (data-make-flowgraph-mapclone protocol)

        :else
          (throw (Exception. "data-make-flowgraph: Don't know how to build graph for operator found."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-primative []
  (data-make-flowgraph
   (execution-make-protocol-primative :id-fetchfasta
                                      "Fetch FASTA"
                                      (schema-make {"pdb_id" :string})
                                      (schema-make {"fasta_filepath" :string}))))

(defn test-mapclone []
  (data-make-flowgraph
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
                                      ])))


(defn test-mapfold []
  (data-make-flowgraph
    (execution-make-protocol-fold :fold-1
                                "Fold"
                                [(execution-make-protocol-primative :job-parser
                                                                    "Job Parser"
                                                                    (schema-make {"job_filepath" :string})
                                                                    (schema-make {"pdb_id" :string}))
                                 (execution-make-protocol-primative :id-fetchentryider
                                                                                     "Entry IDer"
                                                                                     (schema-make {"pdb_id" :string})
                                                                                     (schema-make {"protein_id" :string, "fasta_filepath" :string, "pdb_filepath" :string, "dssp_filepath" :string}))])))





;(.Nodes (test-mapclone))
;(.Edges (test-mapclone))
;(.Tail (test-mapclone))

(.Edges (data-make-flowgraph (build-sprouts-execution)))









