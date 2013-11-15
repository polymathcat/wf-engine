;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.design
  (:gen-class)
  (:use
       wf-engine.database
       wf-engine.ontology))

;;; procedures
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

;incomplete
(defn protocol? [p]
  (or (protocol-design? p)
      false))

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




;incomplete
(defn design-output [p]
[])


;(design-make-task :delta-delta-g-prediction sprouts-ontology)
;(design-make-task :interaction-prediction sprouts-ontology)
;(design-make-task :fragment-prediction sprouts-ontology)

;(design-make-parallel (design-make-task :interaction-prediction sprouts-ontology)
;                      (design-make-task :fragment-prediction sprouts-ontology))




