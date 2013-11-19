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
  "Returns a list of IDs (keywords) found in an execution protocol."

  (if (execution-block-primative? protocol)

    (list     (DataNode. (.ID (.Contents protocol))
               (.Title (.Contents protocol))))

    ;otherwise is an operator
    (reduce concat
            (cons (list (DataNode. (.ID (.Contents protocol))
                                   (.Title (.Contents protocol))))

             (map data-get-nodes (.Blocks (.Contents protocol)))))))

(defn data-get-edges [protocol]
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
                       (list)
                       ;(map #(vector (.ID (.Contents protocol)) (.ID (.Contents %)) "")
                       ;     (.Blocks (.Contents protocol)))

                     :else
                       (throw (Exception. "execution-list-edges: Don't know how to build edges for operator found.")))

               ;recursive step
               (map data-get-edges (.Blocks (.Contents protocol)))))))

;;; CONSTRUCTORS
(defn data-make-from-execution-protocol [execution-protocol]
  ;will always produce a sound protocol

  (make-protocol :data
                                 :graph
                                 (DataPrimative. (data-get-nodes execution-protocol)
                                                 (data-get-edges execution-protocol))))


(data-make-from-execution-protocol (build-sprouts-execution))








