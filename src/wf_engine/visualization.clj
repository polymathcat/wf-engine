;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.visualization
  (:gen-class)
  (:use
       wf-engine.execution
       wf-engine.database
       lacij.model.graph
       lacij.edit.graph
       lacij.edit.dynamic
       lacij.view.graphview
       lacij.layouts.layout
       (tikkba swing dom core)
       tikkba.utils.xml)

  (:import (javax.swing JFrame JOptionPane JPanel JButton JList BoxLayout SwingUtilities JTextArea JTextField JScrollPane)
           (java.awt.event ActionListener MouseListener)
           (java.awt BorderLayout Color)
           java.awt.Component))

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

;(export sprouts-ontology "simple.svg" :indent "yes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ID STRUCTURE INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LACIJ GRAPH GENERATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;modified from lacij examples
(defn add-nodes [g nodes protocol]
  (reduce (fn [g node]
              (let [active-protocol (get-protocol-by-id protocol node)]
                (add-node g
                          node

                          ;node display string
                          (.Title (.Contents active-protocol))

                          ;node display shape
                          :shape (if (execution-block-primative? active-protocol)
                                     :square
                                     :circle)

                          ;node width and height
                          :width (if (execution-block-primative? active-protocol)
                                     70
                                     60)
                          :height (if (execution-block-primative? active-protocol)
                                     30
                                     60))))
          g
          nodes))

;modified from lacij examples
(defn add-edges [g edges protocol]
  (reduce (fn [g [src dst]]
            (let [id (keyword (str (name src) "-" (name dst)))]
             (add-edge g id src dst)))
          g
          edges))

(defn build-execution-graph [protocol]
  (-> (graph :width 800 :height 600)
      (add-nodes (execution-list-ids protocol) protocol)
      (add-edges (execution-list-edges protocol) protocol)
      (layout :hierarchical)
      (build)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISUALIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defn *node-listener*
  [event id]

  (let [title (.Title (.Contents (get-protocol-by-id (deref *execution-protocol*) id)))
        in    (execution-get-schema-input (get-protocol-by-id (deref *execution-protocol*) id))
        out   (execution-get-schema-output (get-protocol-by-id (deref *execution-protocol*) id))]

    (.setText (:text-title (deref *frame-components*)) (str "Name: "title))
    (.setText (:text-id (deref *frame-components*)) (str "ID: "(name id)))

    (.setText (:textarea-in (deref *frame-components*)) (str "In: " (schema-string in)))
    (.setText (:textarea-out (deref *frame-components*)) (str "Out: "  (schema-string out)))))

(defn insert-node-listeners [graph protocol]

  (reduce (fn [g nodeid]
              (add-listener g nodeid "click" #(*node-listener* % nodeid)))
          graph
          (execution-list-ids protocol)))

;http://stackoverflow.com/questions/1558852/learning-resources-and-tutorials-for-using-the-java-batik-library

;; copied from swing utils
(defn add-action-listener [component f & args]
  "Adds an ActionLister to component. When the action fires, f will be
invoked with the event as its first argument followed by args.
Returns the listener."

  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))

(defn on-action [event svgcanvas graph]
  ;(JOptionPane/showMessageDialog nil "Hello World")
  (do-batik
   svgcanvas
   (-> graph
       (add-node! :appolon "Appolon" :x 50 :y 350)
       (add-edge! :appolon-athena :appolon :athena)
       (layout :hierarchical)

       )))

(def ^{:dynamic true} *execution-protocol* (atom nil))
(def ^{:dynamic true} *execution-graph* (atom nil))
(def ^{:dynamic true} *frame-components* (atom nil))

(defn create-frame [graph-other execution-svgcanvas]
  (let [frame                (JFrame.)
        svgcanvas-other      (:svgcanvas graph-other)

        panel-ontology       (JPanel.)
        panel-protocol       (JPanel.)

        button               (JButton. "Unimplemented")
        button-split-fold    (JButton. "Split to Fold")

        components      {:text-title           (JTextField. "N/A")
                         :text-id              (JTextField. "N/A")
                         :textarea-in          (JTextArea. "N/A")
                         :textarea-out         (JTextArea. "N/A")}

        pane                 (.getContentPane frame)]

    (reset! *frame-components* components)

    ;set up main JFrame
    (.setLayout pane nil)
    (.setSize frame (+ 1280 16) (+ 720 38)); offset is for window frames.
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (add-action-listener button on-action svgcanvas-other graph-other)

    ;ONTOLOGY PANEL
    (.setLocation panel-ontology 0 0)
    (.setSize panel-ontology 640 720)
    (.setLayout panel-ontology nil)

    (.add panel-ontology svgcanvas-other)
     (.setSize svgcanvas-other 640 600)
     (.setLocation svgcanvas-other 0 0)

    (.add panel-ontology button)
      (.setSize button 100 20)
      (.setLocation button 100 650)

    (.add pane panel-ontology)

    ;PROTOCOL PANEL
    (.setLocation panel-protocol 640 0)
    (.setSize panel-protocol 640 720)
    (.setLayout panel-protocol nil)

    ;top
    (.add panel-protocol execution-svgcanvas)
      (.setSize execution-svgcanvas 640 600)
      (.setLocation execution-svgcanvas 0 0)

    ;bottom

      (.setSize (:text-title components) 100 20)
      (.setLocation (:text-title components) 140 610)

     (.add panel-protocol (:text-id components))
      (.setSize (:text-id components) 100 20)
      (.setLocation (:text-id components) 140 630)

     (.add panel-protocol (:textarea-in components))
      (.setSize (:textarea-in components) 200 20)
      (.setLocation (:textarea-in components) 140 650)

     (.add panel-protocol (:textarea-out components))
      (.setSize (:textarea-out components) 200 20)
      (.setLocation (:textarea-out components) 140 (+ 650 20))

    (.add panel-protocol button-split-fold)
      (.setSize button-split-fold 100 20)
      (.setLocation button-split-fold 400 650)

    (.add pane panel-protocol)

    frame))





(defn create-window []

  (reset! *execution-protocol* (build-sprouts-execution))
  (let [;graph-other    (second sprouts-ontology)
        graph-other     (gen-graph)
        graph-protocol  (build-execution-graph (deref *execution-protocol*))
        frame           (create-frame graph-other
                                      (:svgcanvas graph-protocol))
        graph-protocol  (insert-node-listeners graph-protocol (deref *execution-protocol*))
        ]

    (reset! *execution-graph* graph-protocol)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))

(create-window)

;(JOptionPane/showMessageDialog nil "Hello World")
