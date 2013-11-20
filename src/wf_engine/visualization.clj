;;; Copyright ©2013 Ruben Acuna

;;; namespace and externals

(ns wf-engine.visualization
  (:gen-class)
  (:use
       wf-engine.database
       wf-engine.execution
       wf-engine.data
       lacij.model.graph
       lacij.edit.graph
       lacij.edit.dynamic
       lacij.view.graphview
       lacij.layouts.layout
       (tikkba swing dom core)
       tikkba.utils.xml)

  (:import (javax.swing JFrame JOptionPane JPanel JButton JLabel BoxLayout SwingUtilities JTextArea JTextField JScrollPane)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXECUTION GRAPH EXTRACTION
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
                       (throw (Exception. "execution-list-edges: Don't know how to build edges for operator found.")))

               ;recursive step
               (map execution-list-edges (.Blocks (.Contents protocol)))))))


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

                          :style (cond (execution-block-fold? active-protocol)
                                         {:fill "#00CCFF"}
                                       (execution-block-mapclone? active-protocol)
                                         {:fill "#99F893"}
                                       :else
                                         {:fill "#FFF5A2"})

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
  (reduce (fn [g [src dst label]]
            (let [id (keyword (str (name src) "-" (name dst)))]
             (add-edge g id src dst label)))
          g
          edges))

(defn build-execution-graph [protocol]
  (-> (graph :width 800 :height 600)
      (add-nodes (execution-list-ids protocol) protocol)
      (add-edges (execution-list-edges protocol) protocol)
      ;(layout :hierarchical)
      (layout :radial :radius 90)
      (build)))

;;; testing
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWING UTILITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copied from swing utils
(defn add-action-listener [component f & args]
  "Adds an ActionLister to component. When the action fires, f will be
invoked with the event as its first argument followed by args.
Returns the listener."

  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:dynamic true} *execution-protocol* (atom nil))
(def ^{:dynamic true} *execution-graph* (atom nil))
(def ^{:dynamic true} *execution-svgcanvas* (atom nil))
(def ^{:dynamic true} *frame-components* (atom nil))
(def ^{:dynamic true} *container-svgcanvas* (atom nil))
(def ^{:dynamic true} *execution-active-id* (atom nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI STATE UPDATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gui-populate-block-info []
  (if (nil? (deref *execution-active-id*))

    ;node not selected, clear
    (do (.setText (:text-title (deref *frame-components*))   "N/A")
        (.setText (:text-id (deref *frame-components*))      "N/A")
        (.setText (:textarea-in (deref *frame-components*))  "N/A")
        (.setText (:textarea-out (deref *frame-components*)) "N/A"))

    ;node selected, populate
    (let [protocol   (deref *execution-protocol*)
          components (deref *frame-components*)

          id    (deref *execution-active-id*)
          title (.Title (.Contents (get-protocol-by-id protocol id)))
          in    (execution-get-schema-input (get-protocol-by-id protocol id))
          out   (execution-get-schema-output (get-protocol-by-id protocol id))]

          (do (.setText (:text-title components) title)
              (.setText (:text-id components) (name id))
              (.setText (:textarea-in components) (schema-string in))
              (.setText (:textarea-out components) (schema-string out))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAPH LISTENERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn node-listener!
  [event id]

  (reset! *execution-active-id* id)

  ;update GUI
  (let [title (.Title (.Contents (get-protocol-by-id (deref *execution-protocol*) id)))
        in    (execution-get-schema-input (get-protocol-by-id (deref *execution-protocol*) id))
        out   (execution-get-schema-output (get-protocol-by-id (deref *execution-protocol*) id))]

    (gui-populate-block-info)

    ;set defaults for split.
    (.setText (:block1-text-title (deref *frame-components*)) (str title " (1)"))
    (.setText (:block1-text-id (deref *frame-components*)) (str (name id) "-1"))
    (.setText (:block1-textarea-in (deref *frame-components*)) (schema-string in))
    (.setText (:block1-textarea-out (deref *frame-components*)) (schema-string (schema-make {"foobar" :string})))

    (.setText (:block2-text-title (deref *frame-components*)) (str title " (2)"))
    (.setText (:block2-text-id (deref *frame-components*)) (str (name id) "-2"))
    (.setText (:block2-textarea-in (deref *frame-components*)) (schema-string (schema-make {"foobar" :string})))
    (.setText (:block2-textarea-out (deref *frame-components*)) (schema-string out))

))

(declare create-and-attach-graph!)
(defn listener-button-split-fold! [event]
  (try
    (let [block1-id    (keyword (.getText (:block1-text-id (deref *frame-components*))))
          block1-title (.getText (:block1-text-title (deref *frame-components*)))
          block1-in    (schema-parse (.getText (:block1-textarea-in (deref *frame-components*))))
          block1-out   (schema-parse (.getText (:block1-textarea-out (deref *frame-components*))))
          block2-id    (keyword (.getText (:block2-text-id (deref *frame-components*))))
          block2-title (.getText (:block2-text-title (deref *frame-components*)))
          block2-in    (schema-parse (.getText (:block2-textarea-in (deref *frame-components*))))
          block2-out   (schema-parse (.getText (:block2-textarea-out (deref *frame-components*))))]

      (cond (execution-contains-id? (deref *execution-protocol*) block1-id)
              (throw (Exception. "Block1 cannot use an ID already present in the protocol."))

            (execution-contains-id? (deref *execution-protocol*) block1-id)
              (throw (Exception. "Block1 cannot use an ID already present in the protocol."))

            :else
              (let [protocol-old   (deref *execution-protocol*)
                    target-id      (deref *execution-active-id*)                                                                             ;:id-fetchentryider
                    block1         (execution-make-protocol-primative block1-id block1-title block1-in block1-out)
                    block2         (execution-make-protocol-primative block2-id block2-title block2-in block2-out)
                    protocol-part  (execution-split-protocol-to-fold (get-protocol-by-id protocol-old target-id) block1 block2)
                    protocol-new   (execution-replace-procotol protocol-old target-id protocol-part)]

              (reset! *execution-protocol* protocol-new)

              (reset! *execution-active-id* nil)

              (create-and-attach-graph!))))
  (catch Exception e
    (JOptionPane/showMessageDialog nil (str "" (.getMessage e))))))


(defn listener-button-export [event]
  (export (deref *execution-graph*) "graph.svg" :indent "yes"))


;;; testing
(defn on-action [event svgcanvas graph]
  (do-batik
   svgcanvas
   (-> graph
       (add-node! :appolon "Appolon" :x 50 :y 350)
       (add-edge! :appolon-athena :appolon :athena)

       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAPH INTERACTION SET UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn insert-node-listeners [graph protocol]

  (reduce (fn [g nodeid]
              (add-listener g nodeid "click" #(node-listener! % nodeid)))
          graph
          (execution-list-ids protocol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISUALIZATION SET UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-frame [graph-other]
  (let [frame                (JFrame.)
        svgcanvas-other      (:svgcanvas graph-other)

        panel-ontology       (JPanel.)
        panel-protocol       (JPanel.)

        button               (JButton. "Unimplemented")
        button-export        (JButton. "Save Image")

        label-title (JLabel. "Title:")
        label-id    (JLabel. "ID:")
        label-in    (JLabel. "Input:")
        label-out   (JLabel. "Output:")

        components      {:text-title           (JTextField. "")
                         :text-id              (JTextField. "")
                         :textarea-in          (JTextField. "")
                         :textarea-out         (JTextField. "")

                         :block1-text-title           (JTextField. "")
                         :block1-text-id              (JTextField. "")
                         :block1-textarea-in          (JTextField. "")
                         :block1-textarea-out         (JTextField. "")

                         :block2-text-title           (JTextField. "")
                         :block2-text-id              (JTextField. "")
                         :block2-textarea-in          (JTextField. "")
                         :block2-textarea-out         (JTextField. "")

                         :button-split-fold    (JButton. "To Fold")
                         }

        pane                 (.getContentPane frame)]

    (reset! *frame-components* components)
    (reset! *container-svgcanvas* panel-protocol)

    ;set up main JFrame
    (.setLayout pane nil)
    (.setSize frame (+ 1280 16) (+ 720 38)); offset is for window frames.
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (add-action-listener button            on-action svgcanvas-other graph-other)
    (add-action-listener button-export     listener-button-export)

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

    ;bottom
    (.add panel-protocol label-title)
      (.setSize label-title 100 20)
      (.setLocation label-title 0 610)
    (.add panel-protocol (:text-title components))
      (.setSize (:text-title components) 100 20)
      (.setLocation (:text-title components) 50 610)
      (.setEditable (:text-title components) false)

     (.add panel-protocol label-id)
      (.setSize label-id 100 20)
      (.setLocation label-id 0 635)
     (.add panel-protocol (:text-id components))
      (.setSize (:text-id components) 100 20)
      (.setLocation (:text-id components) 50 635)
      (.setEditable (:text-id components) false)

     (.add panel-protocol label-in)
      (.setSize label-in 200 20)
      (.setLocation label-in 0 660)
     (.add panel-protocol (:textarea-in components))
      (.setSize (:textarea-in components) 150 20)
      (.setLocation (:textarea-in components) 50 660)
      (.setEditable (:textarea-in components) false)

     (.add panel-protocol label-out)
      (.setSize label-out 200 20)
      (.setLocation label-out 0 690)
     (.add panel-protocol (:textarea-out components))
      (.setSize (:textarea-out components) 150 20)
      (.setLocation (:textarea-out components) 50 690)
      (.setEditable (:textarea-out components) false)

    ;block1
    (let [x-shift 200]
      (.add panel-protocol (:block1-text-title components))
        (.setSize (:block1-text-title components) 100 20)
        (.setLocation (:block1-text-title components) (+ 50 x-shift) 610)

       (.add panel-protocol (:block1-text-id components))
        (.setSize (:block1-text-id components) 100 20)
        (.setLocation (:block1-text-id components) (+ 50 x-shift) 635)

       (.add panel-protocol (:block1-textarea-in components))
        (.setSize (:block1-textarea-in components) 150 20)
        (.setLocation (:block1-textarea-in components) (+ 50 x-shift) 660)

       (.add panel-protocol (:block1-textarea-out components))
        (.setSize (:block1-textarea-out components) 150 20)
        (.setLocation (:block1-textarea-out components) (+ 50 x-shift) 690))

    ;block2
    (let [x-shift 350]
      (.add panel-protocol (:block2-text-title components))
        (.setSize (:block2-text-title components) 100 20)
        (.setLocation (:block2-text-title components) (+ 50 x-shift (+ 50)) 610)

       (.add panel-protocol (:block2-text-id components))
        (.setSize (:block2-text-id components) 100 20)
        (.setLocation (:block2-text-id components) (+ 50 x-shift (+ 50)) 635)

       (.add panel-protocol (:block2-textarea-in components))
        (.setSize (:block2-textarea-in components) 150 20)
        (.setLocation (:block2-textarea-in components) (+ 50 x-shift) 660)

       (.add panel-protocol (:block2-textarea-out components))
        (.setSize (:block2-textarea-out components) 150 20)
        (.setLocation (:block2-textarea-out components) (+ 50 x-shift) 690))

    (.add panel-protocol button-export)
      (.setSize button-export 100 20)
      (.setLocation button-export 540 700)

    (.add panel-protocol (:button-split-fold components))
      (.setSize (:button-split-fold components) 80 20)
      (.setLocation (:button-split-fold components) 360 600)

    (.add pane panel-protocol)

    frame))

(defn create-and-attach-graph! []

  (reset! *execution-graph* (build-execution-graph (deref *execution-protocol*)))

  ;remove any existing svgcanvas
  (if (nil? (deref *execution-svgcanvas*))
      nil
      (.remove (deref *container-svgcanvas*) (deref *execution-svgcanvas*)))

  ;there is already a partial svgcanvas assoicated with the graph, must finish it.
  (reset! *execution-graph* (insert-node-listeners (deref *execution-graph*) (deref *execution-protocol*)))

  ;set new svgcanvas
  (reset! *execution-svgcanvas* (:svgcanvas (deref *execution-graph*)))

  ;lock it into the GUI
    (.add (deref *container-svgcanvas*) (deref *execution-svgcanvas*))
      (.setSize (deref *execution-svgcanvas*) 640 600)
      (.setLocation (deref *execution-svgcanvas*) 0 0)

  ;attach to buttons
  (.removeActionListener (:button-split-fold (deref *frame-components*))
                         (first (.getActionListeners (:button-split-fold (deref *frame-components*)))))
  (add-action-listener (:button-split-fold (deref *frame-components*)) #(listener-button-split-fold! %))

  ;update swing component state
  (gui-populate-block-info)

  nil)

(defn create-window []

  (reset! *execution-protocol* (build-sprouts-execution))

  (let [;graph-other    (second sprouts-ontology)
        graph-other     (gen-graph)
        frame           (create-frame graph-other)
        ]
    (create-and-attach-graph!)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))


;(create-window)

;(JOptionPane/showMessageDialog nil "Hello World")

;http://stackoverflow.com/questions/1558852/learning-resources-and-tutorials-for-using-the-java-batik-library












