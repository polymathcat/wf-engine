(ns wf-engine.core
  (:gen-class)
  (:use

       lacij.model.graph
       lacij.edit.graph
       lacij.view.graphview
       lacij.layouts.layout
   ;     (lacij.layouts core layout)
   )
  )

;next three are from https://github.com/pallix/lacij/tree/master/src/lacij/layouts
;(defn add-nodes [g & nodes]
;  (reduce (fn [g node]
;            (add-node g node (name node)))
;          g
;          nodes))

;(defn add-edges [g & edges]
;  (reduce (fn [g [src dst]]
;            (let [id (keyword (str (name src) "-" (name dst)))]
;             (add-edge g id src dst)))
;          g
;          edges))

;(defn gen-graph3
;  []
;  (-> (graph :width 800 :height 600)
;      (add-default-node-attrs :width 25 :height 25 :shape :circle)
;      (add-nodes :r :s :t :u :v :w :x :y :t1 :t2 :t3 :t4 :t5
;                 :v1 :v2 :v3 :u1 :u2 :w1 :w2
;                 :x1 :x2 :x3 :x4 :x5 :y1 :y2 :y3)
;      (add-edges [:s :r] [:t :s] [:u :s] [:v :s]
;                 [:t1 :t] [:t2 :t] [:t3 :t] [:t4 :t] [:t5 :t]
;                 [:u1 :u] [:u2 :u] [:v1 :v] [:v2 :v] [:v3 :v]
;                 [:w :r] [:w1 :w] [:w2 :w] [:y :w]
;                 [:y3 :y] [:y2 :y] [:y1 :y]
;                 [:x :r] [:x1 :x] [:x2 :x] [:x3 :x] [:x4 :x] [:x5 :x])))

(def sprouts-ontology (vector :ontology (-> (graph :x 100 :width 1000 :height 1000)
            (add-node :protein "Protein" :x 7 :y 7)
            (add-node :secondary-structure "Secondary Structure" :x 250 :y 31)
            (add-node :interaction-prediction "Interaction Prediction" :x 55 :y 94)
            (add-node :structure "Structure" :x 415 :y 134)
            (add-node :residue "Residue" :x 32 :y 204)
            (add-node :neighbor-distance "Neighbor Distance" :x 212 :y 204)
            (add-node :fragment-prediction "Fragment Prediction" :x 529 :y 215)
            (add-node :sequence "Sequence" :x -135 :y 280)
            (add-node :most-interacting-residue "Most Interacting Residue" :x 101 :y 308)
            (add-node :stability "Stability" :x 301 :y 308) ;:x :y
            (add-node :delta-g "Delta G" :x 301 :y 378)
            (add-node :tighened-end-fragment "Tighened End Fragment" :x 402 :y 395)
            (add-node :mutant "Mutant" :x 88 :y 414)
            (add-node :mutation "Mutation" :x 190 :y 412)
            (add-node :wildtype "Wildtype" :x 90 :y 459)
            (add-node :delta-delta-g-prediction "Delta Delta G Prediction" :x 248 :y 473)

            (add-edge :has-a1 :protein :structure "has a")
            (add-edge :has-a2 :protein :sequence "has a")
            (add-edge :uses1 :interaction-prediction :sequence "uses")
            (add-edge :predicts1 :interaction-prediction :neighbor-distance "predicts")
            (add-edge :attribute1 :structure :secondary-structure "attribute")
            (add-edge :defines1 :structure :neighbor-distance "defines")
            (add-edge :defines1 :structure :neighbor-distance "defines")
            (add-edge :defines2 :structure :stability "defines")
            (add-edge :has-many1 :structure :tighened-end-fragment "has many")
            (add-edge :pair-of :neighbor-distance :residue "pair of ")
            (add-edge :defines3 :neighbor-distance :most-interacting-residue "defines")
            (add-edge :uses2 :fragment-prediction :structure "uses")
            (add-edge :predicts2 :fragment-prediction :tighened-end-fragment "predicts")
            (add-edge :has-a3 :sequence :wildtype "has a")
            (add-edge :has-many2 :sequence :mutant "has many")
            (add-edge :requires-proper :stability :most-interacting-residue "requires proper")
            (add-edge :affects1 :delta-g :stability "affects")
            (add-edge :has-a4 :mutant :mutation "has a")
            (add-edge :changes1 :mutation :delta-g "changes")
            (add-edge :uses3 :delta-delta-g-prediction :wildtype "uses")
            (add-edge :uses4 :delta-delta-g-prediction :mutant "uses")
            (add-edge :predicts-change :delta-delta-g-prediction :delta-g "predicts change")

            ;(layout :radial :radius 90)
            ;(layout :hierarchical)
            ;(layout :naive)
            (build))))

(export sprouts-ontology "simple.svg" :indent "yes")

(:nodes (second sprouts-ontology))
(first (:nodes (second sprouts-ontology)))
(:id (first (:nodes (second sprouts-ontology))))
(first (first (:nodes (second sprouts-ontology))))
 (:text (first (:labels (:view (second (first (:nodes (second sprouts-ontology))))))))

;;;;DATABASES

;schema?

;incomplete
(defn schema-subset? [s1 s2]
  true)

(defn schema-append [s1 s2]
  [])


;;;;ONTOLOGY

;ONLY IDs should be exposed to the outside world
;hmm... question mark expects everything, other functions assume is okay?.

;private

(defn ontology-get-node-byid [id ontology]
  (second (first (filter #(= (first %) id)
                         (:nodes (second ontology))))))

(defn ontology-get-edge-byid [id ontology]
  (second (first (filter #(= (first %) id)
                         (:edges (second ontology))))))

;(:outedges (ontology-get-node-byid :interaction-prediction sprouts-ontology))
;(:text (first (:labels (:view (ontology-get-edge-byid :predicts1 sprouts-ontology)))))
;(count (:outedges (ontology-get-node-byid :interaction-prediction sprouts-ontology)))
;(:text (first (:labels (:view (ontology-get-edge-byid :uses1 sprouts-ontology)))))

(defn ontology-get-edge-type [edge ontology]
  (:text (first (:labels (:view edge)))))

(ontology-get-edge-type (ontology-get-edge-byid :uses1 sprouts-ontology) sprouts-ontology)


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


;;;;PROCOTOLS
(defn protocol? [p]
  (or (protocol-design? p)
      false))

;;;;DESIGN PROCOTOLS

(defn design-operator? [p]
  (and (vector? p)
       (or (= (first p) :design-operator-sequential)
           (= (first p) :design-operator-parallel))))

(defn design-task? [p]
  (and (vector? p)
       (= (first p) :design-task)))

(defn protocol-design? [p]
  (or (design-operator? p)
      (design-task? p)))


(defn design-make-task [id ontology]
  (if (ontology-node-data-centric? id ontology)
    ;(vector 'design-task (ontology-node-name id ontology))
    (vector :design-task id)
    (throw (Exception. "Ontological node is not valid for a design protocol task"))))

(defn design-make-sequential [p1 p2]
  (if (and (protocol-design? p1)
           (protocol-design? p2)
           (schema-subset? (design-output p1) (design-output p2)))
    (vector :design-operator-sequential p1 p2)
    (throw (Exception. "Cannot build sequential operation."))))

(defn design-make-parallel [p1 p2]
  (if (and (protocol-design? p1)
           (protocol-design? p2))
    (vector :design-operator-parallel p1 p2)
    (throw (Exception. "Cannot build parallel operation."))))


;incomplete
(defn design-input [p]
  [])


;schema-append

;incomplete
(defn design-output [p]
[])


(design-make-task :delta-delta-g-prediction sprouts-ontology)


(design-make-task :interaction-prediction sprouts-ontology)
(design-make-task :fragment-prediction sprouts-ontology)

(design-make-parallel (design-make-task :interaction-prediction sprouts-ontology)
                      (design-make-task :fragment-prediction sprouts-ontology))


;stub needed by the main project
(defn -main
  []
  (println "Hello, World!"))








