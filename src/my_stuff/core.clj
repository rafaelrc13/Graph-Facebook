(ns my-stuff.core
  (:use [clj-facebook-graph.auth])
  (:require [clj-facebook-graph.client :as client])
  (:gen-class))

(def access "CAACEdEose0cBAJZC87ZCiUgexhxelL5KO8DBeCJkZAmdqCvr1HkkVPMXfSXwDiuqyZCFRXjyMjMMKl96SzsO0l2boyDj48ROCZB88b7itgqdzLhR0kSyMzcFV2ybQBZCmEpQhZCzdYDKmf3wzgMyhYFAxaEmgJaSapZB3BJe8r15ZBGGYVTyjYRfKfsEHU73LqBUblHHOw6meOwZDZD")

(defn read-edges [file]
  (into []
    (let [y (for [elem (with-open [rdr (clojure.java.io/reader file)]
      (reduce conj [] (line-seq rdr)))] 
       (clojure.string/split elem #"\s+"))]
      (for [[in out] y] [(keyword in) (keyword out)]))))
    
(defn full-map [maps]
  (into maps
    (map (fn [output] 
            (vec (reverse output)))
          maps)))

(defn merge-lists [& maps]
  (let [raw-map (reduce (fn [m1 m2]
    (reduce (fn [m [k v]]
      (update-in m [(keyword k)] (fnil conj []) (keyword v)))
        m1, m2))
    {}
    maps)]
    (into {}
    (map (fn [k] 
      {k (vec (distinct (raw-map k)))})
      (keys raw-map)))))

(defn into-map [dist coll]
  (into {}
    (map (fn [output]
            {output dist})
          coll)))

(defn modified-bfs [g s]
  (loop [distances {s 0}
         explored #{s}
         queue [s]]
    (if (empty? queue)
      distances
      (let [v (nth queue 0)
            neighbors (remove explored (g v))
            dist (distances v)]
        (recur 
          (into distances (into-map (inc dist) neighbors))
          (into explored neighbors)
          (subvec (into queue neighbors) 1))))))

(defn closeness-centrality [file]
  (let [edges (merge-lists (read-edges file))]
    (sort-by val > (into {}
      (for [[k v] edges] 
       {k (/ 1 (reduce + (vals (modified-bfs edges k))))})))))

(defn closeness-centrality-facebook [edges]
  (sort-by val > (into {}
    (for [[k v] edges] 
     {k (/ 1 (reduce + (vals (modified-bfs edges k))))}))))

(defn closeness-rank-vector [file]
  (let [result (closeness-centrality file)]
    (into []
      (map (fn [output]
            (first output))
        result))))

(defn closeness-rank-vector-facebook [edges]
  (let [result (closeness-centrality-facebook edges)]
    (into []
      (map (fn [output]
            (first output))
        result))))

(defn get-friends 
  ([access-token]
  (use 'clj-facebook-graph.auth)
  (require '(clj-facebook-graph [client :as client]))
  (let [facebook-auth {:access-token access-token}
        friends (((with-facebook-auth facebook-auth
                  (client/get "https://graph.facebook.com/me/friends")) :body) :data)]
        (into []
          (map (fn [output] 
                 (output :id))
            friends))))
  ([access-token ids]
    (use 'clj-facebook-graph.auth)
    (require '(clj-facebook-graph [client :as client]))
    (let [facebook-auth {:access-token access-token}
          friends (((with-facebook-auth facebook-auth
                    (client/get (str "https://graph.facebook.com/" ids "/friends"))) :body) :data)]
          (into []
            (map (fn [output] 
                   (output :id))
              friends)))))

(defn build-edges []
  (use 'clj-facebook-graph.auth)
  (require '(clj-facebook-graph [client :as client]))
  (let [facebook-auth {:access-token access}
        id (((with-facebook-auth facebook-auth
              (client/get "https://graph.facebook.com/me")) :body) :id)
        filter-friends (get-friends access)]
        (loop [edges [] 
               curr-id id
               friends filter-friends
               nodes filter-friends]
          (if (empty? nodes)
            edges
            (let [next-node (nth nodes 0)
                  connections 
                    (into []
                      (map (fn [elem] 
                          [curr-id elem])
                        friends))
                  new-friends
                    (try 
                      (get-friends access next-node)
                      (catch Exception e []))]
              (recur
                (into edges connections)
                next-node
                (filter (set filter-friends) new-friends)
                (subvec nodes 1)))))))

(defn -main
  [& args]
  (def access args)
  (closeness-centrality-facebook (merge-lists (full-map (build-edges)))))

