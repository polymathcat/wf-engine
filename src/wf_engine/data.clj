;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

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

(declare data-make-graph)

(defn data-make-graph-primative [protocol]
  (DataPrimative.  (list (DataNode. (.ID (.Contents protocol))
                                    (.Title (.Contents protocol))))
                   (list)
                   (.ID (.Contents protocol))))

(defn data-make-graph-fold [protocol]

  (let [recursive-graphs (map data-make-graph (.Blocks (.Contents protocol)))
        recursive-nodes  (reduce concat (map #(.Nodes %) recursive-graphs))
        recursive-edges  (reduce concat (map #(.Edges %) recursive-graphs))]

      (DataPrimative.  (let [local-nodes (list (DataNode. (.ID (.Contents protocol))
                                                      (.Title (.Contents protocol))))]
                            (concat local-nodes recursive-nodes))

                       (let [fold-to-first (list (DataEdge. (.ID (.Contents protocol))
                                                            (.ID (.Contents (first (.Blocks (.Contents protocol)))))
                                                            ""))
                             ids (map #(.ID (.Contents %))
                                      (.Blocks (.Contents protocol)))
                             id-pairs (map vector ids (rest ids))
                             inner-edges (map #(DataEdge. (first %) (second %) "")
                                              id-pairs)
                             local-edges (concat fold-to-first inner-edges)]
                         (concat local-edges recursive-edges))



                       (.ID (.Contents (last (.Blocks (.Contents protocol))))))))

(defn data-make-graph-mapclone [protocol]
  (let [recursive-graphs (map data-make-graph (.Blocks (.Contents protocol)))
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


(defn data-make-graph [protocol]
  (cond (execution-block-primative? protocol)
          (data-make-graph-primative protocol)

        (execution-block-fold? protocol)
          (data-make-graph-fold protocol)

        (execution-block-mapclone? protocol)
          (data-make-graph-mapclone protocol)

        :else
          (throw (Exception. "data-make-graph: Don't know how to build graph for operator found."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-primative []
  (data-make-graph
   (execution-make-protocol-primative :id-fetchfasta
                                      "Fetch FASTA"
                                      (schema-make {"pdb_id" :string})
                                      (schema-make {"fasta_filepath" :string}))))

(defn test-mapclone []
  (data-make-graph
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
  (data-make-graph
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













