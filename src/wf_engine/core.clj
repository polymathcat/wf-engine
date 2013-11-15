(ns wf-engine.core
  (:gen-class)
  (:use
       wf-engine.database
       ;wf-engine.design
       wf-engine.ontology
       lacij.model.graph
       lacij.edit.graph
       lacij.edit.dynamic
       lacij.view.graphview
       lacij.layouts.layout
       (tikkba swing dom core)
       tikkba.utils.xml)

  (:import (javax.swing JFrame JPanel JButton BoxLayout SwingUtilities)
           (java.awt.event ActionListener)
           (java.awt BorderLayout Color)
           java.awt.Component))

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


;;; VISUALIZATION SUPPORT

(defn execution-list-ids [protocol]
  "Returns a list of IDs (keywords) found in an execution protocol."

  (if (execution-block-primative? protocol)
    (list (.ID (.Contents protocol)))

    ;otherwise is an operator
    (reduce concat
            (cons (list (.ID (.Contents protocol)))
             (map execution-list-ids (.Blocks (.Contents protocol)))))))

(defn execution-list-edges [protocol]
  "Returns a list of ID (keywords) pairs (vectors) that represent connectivity in an execution protocol.
  Recursively, this is the outgoing edges of a protocol combined with the outgoing edges of each nodes it
  is connected to."

  (if (execution-block-primative? protocol)
    (list)

    ;otherwise is an operator
    (reduce concat
            (cons
             (map #(vector (.ID (.Contents protocol)) (.ID (.Contents %)))
                  (.Blocks (.Contents protocol)))


             (map execution-list-edges (.Blocks (.Contents protocol))))))

  )

;;; VISUALIZATION

;; copied from swing utils
(defn add-action-listener [component f & args]
  "Adds an ActionLister to component. When the action fires, f will be
invoked with the event as its first argument followed by args.
Returns the listener."

  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))

(defn on-action [event svgcanvas g]
  (do-batik
   svgcanvas
   (-> g
       (add-node! :appolon "Appolon" :x 50 :y 350)
       (add-edge! :appolon-athena :appolon :athena)
       )))

(defn gen-graph []
  (-> (graph)
      (add-node :athena "Athena" :x 10 :y 30)
      (add-node :zeus "Zeus" :x 200 :y 150)
      (add-node :hera "Hera" :x 500 :y 150)
      (add-node :ares "Ares" :x 350 :y 250)
      (add-node :matrimony "<3" :x 400 :y 170 :shape :circle)
      (add-edge :father1 :athena :zeus)
      (add-edge :zeus-matrimony :zeus :matrimony)
      (add-edge :hera-matrimony :hera :matrimony)
      (add-edge :son-zeus-hera :ares :matrimony)
      (build)))


;modified from lacij examples
(defn add-nodes [g nodes]
  (reduce (fn [g node]
              (add-node g node (name node)))
          g
          nodes))

;modified from lacij examples
(defn add-edges [g edges]
  (reduce (fn [g [src dst]]
            (let [id (keyword (str (name src) "-" (name dst)))]
             (add-edge g id src dst)))
          g
          edges))

(defn build-execution-graph [protocol]
  (-> (graph :width 800 :height 600)
      (add-default-node-attrs :width 30 :height 30 :shape :circle)
      (add-nodes (execution-list-ids protocol))
      (add-edges (execution-list-edges protocol))
      (layout :hierarchical)
      (build)))


(defn gen-graph-sprouts []
   (->
    (build-execution-graph (build-sprouts-execution))

    ;(add-node! :appolon "Appolon" :x 50 :y 350)
    ;(add-edge! :appolon-athena :appolon :athena)
    ))

(defn create-frame [svgcanvas active-graph]
  (let [frame  (JFrame.)

        panel-ontology (JPanel.)
        panel-protocol (JPanel.)

        button (JButton. "Actionzzz")
        pane   (.getContentPane frame)]

    ;set up main JFrame
    (.setLayout pane nil)
    (.setSize frame (+ 1280 16) (+ 720 38))
    (add-action-listener button on-action svgcanvas active-graph)


    ;set up the ontology panel
    (.setLocation panel-ontology 0 0)
    (.setSize panel-ontology 640 720)
    (.setBackground panel-ontology Color/BLACK)
    (.add panel-ontology button)
    (.add pane panel-ontology)

    ;set up the protocol panel
    (.setLocation panel-protocol 640 0)
    (.setSize panel-protocol 640 720)
    (.setLayout panel-protocol nil)
    (.setBackground panel-protocol Color/BLUE)
    (.add panel-protocol svgcanvas)
      (.setSize svgcanvas 640 600)
      (.setLocation svgcanvas 0 0)
    (.add pane panel-protocol)

    frame))

(defn zz []
  (let [;active-graph  (second sprouts-ontology)
        ;active-graph  (gen-graph)
        active-graph  (gen-graph-sprouts)
        ;doc           (:xmldoc active-graph)
        svgcanvas     (:svgcanvas active-graph)
        frame         (create-frame svgcanvas active-graph)]
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))

(zz)





























