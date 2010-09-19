(ns de.herrnorbert.CljSvgExample.core
  (:use [incanter core processing io]))

(def regions [{:id :Thüringen :gdp 48871}
              {:id :Schleswig-Holstein :gdp 73399}
              {:id :Sachsen-Anhalt :gdp 51480}
              {:id :Sachsen :gdp 92853}
              {:id :Saarland :gdp 28851}
              {:id :Rheinland-Pfalz :gdp 102526}
              {:id :Nordrhein-Westfalen :gdp 521746}
              {:id :Niedersachsen :gdp 205596}
              {:id :Mecklenburg-Vorpommern :gdp 35229}
              {:id :Hessen :gdp 216515}
              {:id :Hamburg :gdp 85757}
              {:id :Bremen :gdp 26753}
              {:id :Brandenburg :gdp 53891}
              {:id :Berlin :gdp 90134}
              {:id :Bayern :gdp 429862}
              {:id :Baden__x26__Württemberg :gdp 343736}])

(defn rnd-color []
  (color (rand-int 255) (rand-int 255) (rand-int 255)))

(defn map-rnd-color [data]
  (map #(assoc % :color (rnd-color)) data))

(defn map-color [val min max]
  (lerp-color (color 165 42 42) (color 0 200 0) (norm val min max)))

(defn map-gdp-color [regions]
  (let [min (apply min (map #(:gdp %) regions))
        max (apply max (map #(:gdp %) regions))]
    (map #(assoc % :color (map-color (:gdp %) min max)) regions)))

(defn sktch [image data [width height]]
  (let [shape (promise)]
    (sketch
     (setup []
            (deliver shape (load-shape this image))
            (doto this
              smooth
              no-stroke
              no-loop
              (size width height)))
     (draw []
           (.shape this @shape 0 0)
           (doseq [region data]
             (let [{:keys [id color]} region
                   child (.getChild @shape (name id))]
               (.disableStyle child)
               (.fill this color)
               (.noStroke this)
               (.shape this child 0 0)))))))

(defn view-germany [data]
  (let [image "Karte_Bundesrepublik_Deutschland.svg"
        size [600 830]
        sktch (sktch image data size)]
    (view sktch :size size)))

(defn view-rnd-color-germany []
  (view-germany (map-rnd-color regions)))

(defn view-gdp-germany []
  (view-germany (map-gdp-color regions)))

