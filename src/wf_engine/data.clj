;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.data
  (:gen-class)
  (:use
       wf-engine.database
       wf-engine.execution))


;edges have the form {keyword:start, keyword:end string:title}

(defrecord DataPrimative [Nodes
                          Edges])

(defrecord DataNode [ID
                     Title])

(defrecord DataEdge [ID-Start
                     ID-End
                     Title])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn data-get-nodes [protocol]
  (if (execution-block-primative? protocol)

    (list     (DataNode. (.ID (.Contents protocol))
               (.Title (.Contents protocol))))

    ;otherwise is an operator
    (reduce concat
            (cons (cond (execution-block-fold? protocol)
                          (list (DataNode. (.ID (.Contents protocol))
                                           (.Title (.Contents protocol))))

                        (execution-block-mapclone? protocol)
                          (list (DataNode. (.ID (.Contents protocol))
                                           (.Title (.Contents protocol)))
                                (DataNode. (keyword (str (name (.ID (.Contents protocol))) "-join"))
                                           "Join"))
                        :else
                          (throw (Exception. "data-get-nodes: Don't know how to build nodes for operator found.")))

            (map data-get-nodes (.Blocks (.Contents protocol)))))))

(defn data-get-edges [protocol]
  (first (data-get-edges-helper protocol)))

(defn data-get-edges-helper [protocol]

  (if (execution-block-primative? protocol)

    ;blocks can't have edges
    (list (list) (.ID (.Contents protocol)))

    ;otherwise is an operator
    (let [recursive-solution (map data-get-edges-helper (.Blocks (.Contents protocol)))
          recursive-edges    (reduce concat (list) (map #(first %) recursive-solution))
          recursive-tails    (map #(second %) recursive-solution)

          local-edges  (cond ;fold - have to  label edges by order
                         ;(execution-block-fold? protocol)
                         ; (list)


                         ;map clone - don't need to label edges
                         (execution-block-mapclone? protocol)
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
                                                                                                 (.ID (.Contents protocol))
                                                                                                 "")]
                                                                                (list (conj (first state) edge)
                                                                                      (inc (second state)))))
                                                                       (list (list) 0)
                                                                       recursive-tails))]

                                (concat mapclone-to-child-edges
                                        tails-to-mapclone-edges))

                         :else
                           (throw (Exception. "execution-list-edges: Don't know how to build edges for operator found.")))]


                  (list (concat local-edges recursive-edges)
                        (.ID (.Contents protocol))))))

(defn data-get-edges [protocol]
  (first (data-get-edges-helper protocol)))



;;; CONSTRUCTORS
(defn data-make-from-execution-protocol [execution-protocol]
  (make-protocol :data
                 :graph
                 (DataPrimative. (data-get-nodes execution-protocol)
                                 (data-get-edges execution-protocol))))


;(data-make-from-execution-protocol (build-sprouts-execution))

(defn tmp []
  (data-make-from-execution-protocol
   (execution-make-protocol-primative :id-fetchfasta
                                      "Fetch FASTA"
                                      (schema-make {"pdb_id" :string})
                                      (schema-make {"fasta_filepath" :string}))))

(defn tmp2 []
  (data-make-from-execution-protocol
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

(.Nodes (.Contents (tmp2)))
(.Edges (.Contents (tmp2)))












