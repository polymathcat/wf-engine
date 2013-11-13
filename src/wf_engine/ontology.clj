;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.ontology
  (:gen-class))

;;; procedures

;ONLY IDs should be exposed to the outside world
;hmm... question mark expects everything, other functions assume is okay.

;private

(defn ontology-get-node-byid [id ontology]
  (second (first (filter #(= (first %) id)
                         (:nodes (second ontology))))))

(defn ontology-get-edge-byid [id ontology]
  (second (first (filter #(= (first %) id)
                         (:edges (second ontology))))))

(defn ontology-get-edge-type [edge ontology]
  (:text (first (:labels (:view edge)))))

;(ontology-get-edge-type (ontology-get-edge-byid :uses1 sprouts-ontology) sprouts-ontology)


;public
(defn ontology? [ontology]
  (and (vector? ontology)
       (= (first ontology) :ontology)))

(defn ontology-node-data-centric? [id ontology]
  (if (ontology? ontology)
    (let [node (ontology-get-node-byid id ontology)]
      ;all out edges are labeled predict or uses and one of each exists
      (let [count-predicts (count (filter #(= (ontology-get-edge-type (ontology-get-edge-byid % ontology)
                                                                      ontology)
                                              "predicts")
                                          (:outedges node)))

                count-uses (count (filter #(= (ontology-get-edge-type (ontology-get-edge-byid % ontology)
                                                                      ontology)
                                              "predicts")
                                          (:outedges node)))]
            (and (> count-predicts 0)
                 (> count-uses 0)
                 (= (count (:outedges node)) (+ count-predicts count-uses)))))
    false))


;(ontology-node-data-centric? :mutation sprouts-ontology)
;(ontology-node-data-centric? :interaction-prediction sprouts-ontology)


(defn ontology-node-name [id ontology]
  (:text (first (:labels (:view (ontology-get-node-byid id ontology))))))
