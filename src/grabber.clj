(ns grabber
  (:gen-class)
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [clojure.xml :as xml]
            [clojure.java.io :as io]
            
            [clj-http.client :as http]
            [hiccup.page :refer [html5]]
            [cheshire.core :as json])
  (:import java.time.LocalDate
           java.io.ByteArrayInputStream))

(def db-spec {:connection-uri "jdbc:sqlite:data.db"
              :classname      "org.sqlite.JDBC"})

(def currencies [:usd :eur :cny :jpy])

(defn init-tables []
  (let [create-table (fn [currency]
                       (let [currencies-fields (for [c currencies
                                                     :when (not= c currency)]
                                                 [c :real])
                             table-spec (concat [[:day :integer :primary :key]] currencies-fields)]
                         (jdbc/create-table-ddl currency table-spec)))]
    (dorun
     (jdbc/db-do-commands
      db-spec
      (for [cur currencies]
        (create-table cur))))))

(defn get-data [date]
  (let [[y m d] (re-seq #"\d+" (str date))]
    (-> (format "http://www.cbr.ru/scripts/XML_daily.asp?date_req=%s.%s.%s" d m y)
        (http/get {:as :stream})
        (:body)
        (slurp :encoding "windows-1251")
        (.getBytes)
        (java.io.ByteArrayInputStream.)
        (clojure.xml/parse))))

(defn extract-currencies [data]
  (-> (for [{item :content} (:content data)
            :let [dict (into {} (map (juxt :tag (comp first :content))) item)]]
        [(keyword (string/lower-case (:CharCode dict)))
         (Double/parseDouble (string/replace (:Value dict) "," "."))])
      (->> (into {}))
      (select-keys currencies)))

(defn store [day data]
  (doseq [[cur value] data
          :let [row (->> (for [c currencies
                               :when (not= cur c)
                               :let [converted (if (zero? value) 0 (/ (c data 0) value))]]
                           [c converted])
                         (into {:day day}))]]
    (jdbc/insert! db-spec cur row)))

(def errors (atom []))

(defn collect-data []
  (reset! errors [])
  (let [base-date (.minusMonths (LocalDate/now) 1)]
    (doseq [day (map inc (range (.lengthOfMonth base-date)))
            :let [current-date (.withDayOfMonth base-date day)]]
      (try
        (println "⦁ Colllect currencies for" (str current-date))
        (->> (get-data current-date)
             (extract-currencies)
             (store day))
        (Thread/sleep 500)
        (catch Exception e 
          (let [error-msg {:day   day
                           :error e}]
            (prn ::error day)
            (swap! errors conj error-msg)))))))

(defn write-plot [file-name content]
  (io/make-parents file-name)
  (spit file-name content))

(defn render-currency-plot [currency]
  (let [chart-data (jdbc/query db-spec (str "select * from " (name currency) " order by day"))
        days (map :day chart-data)
        graph-data (for [cur currencies
                         :let [items (map cur chart-data)]
                         :when (seq items)]
                     {:x    days
                      :y    items
                      :name (name cur)})]
    (->> (html5
          {:lang "ru"}
          [:head
           [:script {:src "https://cdn.plot.ly/plotly-1.2.0.min.js"}]]
          [:body {:style "background:grey"}
           [:div {:id    "plot"
                  :style "width:100%; height: 100%"}]
           [:script (format "var node = document.getElementById('plot'); Plotly.newPlot(node, %s, %s);" 
                            (json/generate-string graph-data)
                            (json/generate-string {:title  (name currency)
                                                   :font   {:size "20"}
                                                   :height "800"}))]])
         (write-plot (str "plots/" (name currency) ".html")))))

(defn rendering []
  (doseq [c currencies]
    (println "⦁ Render graph for" (name c))
    (render-currency-plot c)))

(defn -main [& _]
  (println "# Init tables")
  (init-tables)
  (println "# Collect currencies")
  (collect-data)
  (println "# Rendering")
  (rendering))

(comment

  (init-tables)

  (def cur-day (.withDayOfMonth (.minusMonths (java.time.LocalDate/now) 1) 1))

  (def cur-data (get-data cur-day))

  (def data
    (let [is (ByteArrayInputStream. (.getBytes cur-data))]
      (clojure.xml/parse is)))
  ;; => #'grabber/data

  (-> data
      :content
      first)
  ;; => {:tag :Valute,
  ;;     :attrs {:ID "R01010"},
  ;;     :content
  ;;     [{:tag :NumCode, :attrs nil, :content ["036"]}
  ;;      {:tag :CharCode, :attrs nil, :content ["AUD"]}
  ;;      {:tag :Nominal, :attrs nil, :content ["1"]}
  ;;      {:tag :Name, :attrs nil, :content ["РђРІСЃС‚СЂР°Р»РёР№СЃРєРёР№ РґРѕР»Р»Р°СЂ"]}
  ;;      {:tag :Value, :attrs nil, :content ["47,1448"]}]}

  (collect-data)

  (first @errors)


  (try
    (rendering)
    (catch Exception e (Throwable->map e)))
  
  )