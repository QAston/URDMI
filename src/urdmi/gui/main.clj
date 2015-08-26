(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [clojure.zip :as zip])
  (:import
    (javafx.scene.layout AnchorPane Region VBox Priority HBox)
    (javafx.geometry Pos Insets)
    (javafx.scene.text Font TextAlignment)
    (javafx.scene.paint Color)
    (urdmi.core Project)
    (javafx.util Callback StringConverter)
    (javafx.scene.control.cell TextFieldTreeCell)))

(defn- file-names-recursively [zipiter proj-key]
  (let [path (into [proj-key] (conj (mapv :name (rest (zip/path zipiter))) (:name (zip/node zipiter))))
        name (:name (zip/node zipiter))]
    (if (zip/branch? zipiter)
      (vector
        (keep identity (cons [:name (:name (zip/node zipiter)) :path path]
                             (if-let [childiter (zip/down zipiter)]
                               (loop [iter childiter
                                      children []
                                      ]
                                 (if-not (nil? iter)
                                   (recur (zip/right iter) (conj children (file-names-recursively iter proj-key)))
                                   children))

                               (list)
                               ))))
      {:name name :path path})))

(defn- get-file-names [^Project p proj-key display-name]
  (vec (cons {:name display-name :path [proj-key]}
             (rest (first (file-names-recursively (core/file-model-zipper (get-in p (core/dir-keys proj-key))) proj-key))))))

(defn generate-menu-viewmodel [^Project p]
  (let [
        relations (get-file-names p core/relations-keyname "Relations")
        working-dir (get-file-names p core/workdir-keyname "Working dir")
        outputs (get-file-names p core/output-keyname "Output")
        additions (get-file-names p core/output-keyname "Additions")
        settings (get-file-names p core/output-keyname "Settings")
        ]
    [{:name "Project" :path []} relations working-dir outputs additions settings]))

(defn- build-file-menu-entry-widget [data]
  (if-not (vector? data)
    (fx/tree-item {:value data})
    (let [dir-data (first data)]
      (doto (fx/tree-item {:value dir-data})
        (.. getChildren
            (setAll
              (for [entry (rest data)]
                (build-file-menu-entry-widget entry))))
        ))))

(defn update-main-file-menu
  [view]
  (set-widget-children (fx/lookup view :#file-selection)
                       (list (doto (fx/tree-view
                                     {:cell-factory (reify
                                                      Callback
                                                      (call [this tree-view]
                                                        (TextFieldTreeCell. (proxy
                                                                              [StringConverter] []
                                                                              (toString [obj]
                                                                                (:name obj)
                                                                                )))))
                                      :root         (doto (build-file-menu-entry-widget [{:name "Project", :path []}
                                                                                         [{:name "Relations", :path [:relations]}
                                                                                          {:name "towar_6.pl", :path [:relations "towar_6.pl"]}
                                                                                          {:name "produkcja_5.pl", :path [:relations "produkcja_5.pl"]}
                                                                                          {:name "pracownik_7.pl", :path [:relations "pracownik_7.pl"]}
                                                                                          {:name "pracownikpersonalia_8.pl",
                                                                                           :path [:relations "pracownikpersonalia_8.pl"]}
                                                                                          {:name "klient_9.pl", :path [:relations "klient_9.pl"]}
                                                                                          {:name "zamowienieszczegoly_4.pl",
                                                                                           :path [:relations "zamowienieszczegoly_4.pl"]}
                                                                                          {:name "pracownikprodukcja_7.pl",
                                                                                           :path [:relations "pracownikprodukcja_7.pl"]}
                                                                                          {:name "zamowienie_5.pl", :path [:relations "zamowienie_5.pl"]}
                                                                                          {:name "dzial_6.pl", :path [:relations "dzial_6.pl"]}]
                                                                                         [{:name "Working dir", :path [:working-dir]}
                                                                                          {:name "pracownik.b", :path [:working-dir "pracownik.b"]}
                                                                                          {:name "pracownik.f", :path [:working-dir "pracownik.f"]}
                                                                                          {:name "pracownik.n", :path [:working-dir "pracownik.n"]}]
                                                                                         [{:name "Output", :path [:output]}
                                                                                          {:name "result.edn", :path [:output "result.edn"]}]
                                                                                         [{:name "Additions", :path [:output]}
                                                                                          {:name "result.edn", :path [:output "result.edn"]}]
                                                                                         [{:name "Settings", :path [:output]}
                                                                                          {:name "result.edn", :path [:output "result.edn"]}]])
                                                      (.setExpanded true))}
                                     )
                               (VBox/setVgrow Priority/ALWAYS))))
  )

(defn new-main-view []
  (let [font (Font/font 11.0)
        text-fill (Color/color 0.625 0.625 0.625)
        menu-events (chan)
        file-events (chan)

        view (fx/v-box {:pref-height 600
                        :pref-width  900
                        }
                       (doto (fx/menu-bar
                               (fx/menu {:text "Project"}
                                        (fx/menu-item {:text "New" :on-action menu-events})
                                        (fx/menu-item {:text "Open..." :on-action menu-events})))
                         (VBox/setVgrow Priority/NEVER))
                       (doto (fx/split-pane {:divider-positions (double-array [0.25])
                                             :focus-traversable true
                                             }

                                            (fx/v-box :#file-selection {:focus-traversable true})

                                            (fx/scroll-pane :#content {:fit-to-height true
                                                                       :fit-to-width  true}
                                                            ))

                         (VBox/setVgrow Priority/ALWAYS))
                       (doto (fx/h-box :#hbox {:alignment Pos/CENTER_LEFT
                                               :spacing   5.0
                                               :padding   (Insets. 3 3 3 3)}
                                       (doto (fx/label {:max-height 1.8
                                                        :max-width  Region/USE_COMPUTED_SIZE
                                                        :text       "Left status"
                                                        :font       font
                                                        :text-fill  text-fill})
                                         (HBox/setHgrow Priority/ALWAYS))
                                       (doto (fx/pane {:pref-height Region/USE_COMPUTED_SIZE
                                                       :pref-width  Region/USE_COMPUTED_SIZE})
                                         (HBox/setHgrow Priority/ALWAYS))
                                       (doto (fx/label {:max-height 1.8
                                                        :max-width  Region/USE_COMPUTED_SIZE
                                                        :text       "Right status"
                                                        :font       font
                                                        :text-fill  text-fill})
                                         (HBox/setHgrow Priority/NEVER)))
                         (VBox/setVgrow Priority/NEVER))
                       )]

    (def main-view view)
    view
    ))

(def create-main-view new-main-view)

(fx/sandbox #'create-main-view)
(update-main-file-menu main-view)