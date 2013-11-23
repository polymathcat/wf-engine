;;; Copyright ©2013 Ruben Acuna

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NAMESPACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; EXECUTION GRAPH GENERATION (modified from lacij examples)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;modified from lacij examples
(defn vizexec-graph-add-nodes [g nodes protocol]
  (reduce (fn [g node]
              (let [active-protocol (execution-get-by-id protocol node)]
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


(defn vizexec-graph-add-edges [g edges protocol]
  (reduce (fn [g [src dst label]]
            (let [id (keyword (str (name src) "-" (name dst)))]
             (add-edge g id src dst label)))
          g
          edges))


(defn vizexec-graph-build [protocol width height]
  (-> (graph :width width :height height)
      (vizexec-graph-add-nodes (execution-get-ids protocol) protocol)
      (vizexec-graph-add-edges (execution-get-edges protocol) protocol)
      (layout :radial :radius 90
                      :root (.ID (.Contents protocol))
              )
      (build)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA GRAPH EXTRACTION (modified from lacij examples)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vizdata-graph-add-nodes [g nodes]
  (reduce (fn [g node]
                (add-node g
                          (.ID node)

                          ;node display string
                          (.Title node)

                          ;node display shape
                          :shape :square

                          :style {:fill "#FFF5A2"}

                          ;node width and height
                          :width 60
                          :height 30))
          g
          nodes))


(defn vizdata-graph-add-edges [g edges]
  (reduce (fn [g edge]
            (let [id (keyword (str (name (.ID-Start edge)) "-" (name (.ID-End edge) )))]
             (add-edge g
                       id
                       (.ID-Start edge)
                       (.ID-End edge)
                       (.Title edge))))
          g
          edges))


(defn vizdata-graph-build [protocol width height]
  (let [flowgraph (data-make-flowgraph protocol)]
    (-> (graph :width width :height height)
        (vizdata-graph-add-nodes (.Nodes flowgraph))
        (vizdata-graph-add-edges (.Edges flowgraph))
        (layout :hierarchical
                :layer-space 75)

        (build))))


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
;;; EXECUTION GUI STATE UPDATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn vizexec-gui-update-block-info []
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
          title (.Title (.Contents (execution-get-by-id protocol id)))
          in    (execution-get-schema-input (execution-get-by-id protocol id))
          out   (execution-get-schema-output (execution-get-by-id protocol id))]

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
  (let [title (.Title (.Contents (execution-get-by-id (deref *execution-protocol*) id)))
        in    (execution-get-schema-input (execution-get-by-id (deref *execution-protocol*) id))
        out   (execution-get-schema-output (execution-get-by-id (deref *execution-protocol*) id))]

    (vizexec-gui-update-block-info)

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
                    protocol-part  (execution-split-protocol-to-fold (execution-get-by-id protocol-old target-id) block1 block2)
                    protocol-new   (execution-replace-procotol protocol-old target-id protocol-part)]

              (reset! *execution-protocol* protocol-new)

              (reset! *execution-active-id* nil)

              (create-and-attach-graph!))))
  (catch Exception e
    (JOptionPane/showMessageDialog nil (str "" (.getMessage e))))))


(defn listener-button-export [event]
  (export (deref *execution-graph*) "graph.svg" :indent "yes"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAPH INTERACTION SET UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn insert-node-listeners [graph protocol]

  (reduce (fn [g nodeid]
              (add-listener g nodeid "click" #(node-listener! % nodeid)))
          graph
          (execution-get-ids protocol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISUALIZATION SET UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-frame-execution [graph-other]
  (let [frame                (JFrame.)
        svgcanvas-other      (:svgcanvas graph-other)

        panel-protocol       (JPanel.)

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

    (add-action-listener button-export     listener-button-export)

    (.add pane svgcanvas-other)
     (.setSize svgcanvas-other 640 600)
     (.setLocation svgcanvas-other 0 0)

    ;PROTOCOL PANEL
    (.setLocation panel-protocol 640 0)
    (.setSize panel-protocol 640 720)
    (.setLayout panel-protocol nil)

    ;block info
    (let [x-shift 0
          y-shift 610]
      (.add pane label-title)
        (.setSize label-title 100 20)
        (.setLocation label-title (+ 0 x-shift) (+ 0 y-shift))
      (.add pane (:text-title components))
        (.setSize (:text-title components) 100 20)
        (.setLocation (:text-title components) (+ 50 x-shift) (+ 0 y-shift))
        (.setEditable (:text-title components) false)

       (.add pane label-id)
        (.setSize label-id 100 20)
        (.setLocation label-id (+ 0 x-shift) (+ 25 y-shift))
       (.add pane (:text-id components))
        (.setSize (:text-id components) 100 20)
        (.setLocation (:text-id components) (+ 50 x-shift) (+ 25 y-shift))
        (.setEditable (:text-id components) false)

       (.add pane label-in)
        (.setSize label-in 200 20)
        (.setLocation label-in (+ 0 x-shift) (+ 50 y-shift))
       (.add panel-protocol (:textarea-in components))
        (.setSize (:textarea-in components) 150 20)
        (.setLocation (:textarea-in components) (+ 50 x-shift) (+ 50 y-shift))
        (.setEditable (:textarea-in components) false)

       (.add pane label-out)
        (.setSize label-out 200 20)
        (.setLocation label-out (+ 0 x-shift) (+ 75 y-shift))
       (.add pane (:textarea-out components))
        (.setSize (:textarea-out components) 150 20)
        (.setLocation (:textarea-out components) (+ 50 x-shift) (+ 75 y-shift))
        (.setEditable (:textarea-out components) false))

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

  (reset! *execution-graph* (vizexec-graph-build (deref *execution-protocol*)
                                                 640 600))

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
  (vizexec-gui-update-block-info)

  nil)


(defn create-window []

  (reset! *execution-protocol* (build-sprouts-execution))

  (let [;graph-other    (second sprouts-ontology)
        graph-other     (vizdata-graph-build (deref *execution-protocol*) 640 600)
        frame           (create-frame-execution graph-other)]
    (create-and-attach-graph!)
    (SwingUtilities/invokeAndWait
     (fn [] (.setVisible frame true)))))


(create-window)

;(JOptionPane/showMessageDialog nil "Hello World")




















