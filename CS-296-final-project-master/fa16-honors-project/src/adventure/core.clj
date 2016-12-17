(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created
for a game or something."
           :title "in the foyer"
           :dir {:south :grue-pen}
           :contents #{:raw-egg}}
   :grue-pen {:desc "It is very dark.  You are about to be eaten by a grue."
              :title "in the grue pen"
              :dir {:north :foyer :south :ece-building}
              :contents #{}}
   :ece-building {:desc "There are so many people in ECE-building. We have to go to another place. "
                  :title "in the ece-building"
                  :dir {:north :grue-pen :west :siebel-center :south :altgeld-hall :east :skytree}
                  :contents #{:bed}}
   :siebel-center {:desc "there are many people doing MP, you'd better leave. "
                   :title "in the siebel-center"
                   :dir {:east :ece-building}
                   :contents #{}}
   :altgeld-hall {:desc "you can take exam here, but if your hp is too low, you'd better go back to sleep. "
                  :title "in the altgeld-hall"
                  :dir {:north :ece-building}
                  :contents #{:skytree-card}}
   :skytree {:desc "you can find the stuff needed to pass the game on the top of the skytree by hitting get-stuff command, but you need to go to the destination first. "
             :title "in the skytree"
             :dir {:west :ece-building :east :sunshine-city}
             :contents #{}}
   :sunshine-city {:desc " you cannot go here unless you have raw-egg. "
             :title "in the sunshine-city"
             :dir {:west :skytree :east :panda-express}
             :contents #{}}
   :panda-express {:desc " you can eat food here but it costs you 20 dollars"
             :title "in the :panda-express"
             :dir {:west :skytree :south :union}
             :contents #{:orange-chicken}}
   :union {:desc " you can get the icard here for transportation"
             :title "in the :union"
             :dir {:north :panda-express :south :grainger}
             :contents #{:icard}}
   :grainger {:desc " you can borrow at most three books here. if you like a cup of coffee, go downstair"
             :title "in the :grainger"
             :dir {:north :union :downstair :royal-express :east :engineering-hall}
             :contents #{:book}}
   :royal-express {:desc " you can work here if you still have working time"
             :title "in the :royal-express"
             :dir {:upstair :grainger}
             :contents #{:coffee}}
    :engineering-hall {:desc " you can get your transcript here"
             :title "in the :engineering-hall"
             :dir {:west :grainger :east :quad}
             :contents #{:transcript}}
    :quad {:desc " you can pick leaf here"
             :title "in the :quad"
             :dir {:west :engineering-hall :east :arc}
             :contents #{:leaf}}
    :arc {:desc " you can work here and get some water"
             :title "in the :arc"
             :dir {:west :quad :east :iken}
             :contents #{:water}}
    :iken {:desc " you can get your meal here if you have your icard with you"
             :title "in the :iken"
             :dir {:west :arc}
             :contents #{:meal}}





   })

(def adventurer
  {:location :foyer
   :inventory #{}
   :money 100
   :hp 100
   :book 0
   :worktime 5
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))







(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))


(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)
        inventory (player :inventory)
        ]

    (cond
        (nil? dest)
            (do (println "you cannot go") player)
        (and (= dest :sunshine-city) (contains? inventory :raw-egg))
            (do (println "congratulations! ") (assoc-in player [:location] dest))
        (= dest :sunshine-city)
            (do (println" you cannot go")(assoc-in player [:location] :skytree))
        :else
            (assoc-in player [:location] dest))))





(defn study [player]
  (let [curr-room (player :location)]
    (let [curr-hp (player :hp)]
    (let [curr-seen (player :seen)]
    (if (= curr-room :siebel-center)

      (do (println "you finished a MP") (update-in player [:hp] - 20))

      (do (println "you cannot do MP here, go back to siebel")player))

    ))))



(defn pick [player]
  (let [curr-room (player :location)]
    ;into function will add all elements inside contents into inventory, and then we get curr-room, we look at the-map to check the contents
  (let [new-player (update-in player [:inventory] #(into % (-> the-map curr-room :contents)))]
     ; (update-in player [:inventory] #(conj % "raw-egg"))
      (do (println (str "now you have " (-> new-player :inventory)))new-player))))


(defn get-stuff [player]
  (let [curr-room (get-in player [:location])
        invent (player :inventory)]
    (if (and (= curr-room :skytree) (contains? invent :skytree-card))
      (do(println "you have the skytree-ticket and you can now get the ticket to pass the game")(let [new-player (update-in player [:inventory] #(disj % :skytree-card))
                    new-player2 (update-in new-player [:inventory] #(conj % :ticket))]
                           new-player2))

      (do (println"you do not have the car, sorry we cannot give the ticket")player))))


(defn eat [player]
  (let [location (get-in player [:location])
       curr-money (player :money)]
    (if (and (= location :panda-express) (> curr-money 20))
      (do (println "You cost 20 dollars to have 20 more hp") (let [new-player (update-in player [:hp] + 20)
                    new-player2 (update-in new-player [:money] - 20)]
                           new-player2))
      (do (println "You do not have enough money")player))
        ))

(defn relax[player]
  (let[curr-room (player :location)]
    (if (= curr-room :union)
      (do (println "Take a short nap in Union") (update-in player [:hp] + 5))
      (do (println "Why not find a place and take a short rest?")player)
    )))

; need to add book into inventory!!!!!!!!!!!!!!!!!!!!!!!!!
(defn borrow-book[player]
  (let[curr-room (player :location)]
    (let[curr-book (player :book)]
      (if (= curr-room :grainger)
        (if (< curr-book 3)
          (do (println "You have successfully borrowed a book!") (update-in player [:book] + 1))
          (do (println "You cannot borrow more than 3 books.") player))
        (do (println "You cannot borrow a book here.") player)))
    ))

(defn work[player]
  (let[curr-room (player :location)]
    (let[curr-worktime (player :worktime)]
     (if (or (= curr-room :royal-express) (= curr-room :arc))
       (if (> curr-worktime 0)
        (do (println "You can work here and get some payment.") (let [new-player (update-in player [:worktime] - 1)
          new-player2 (update-in new-player [:money] + 10)] new-player2))
        (do (println "You do not have enough working time.") player))
      (do (println "You cannot work here.") player)))))


(defn move[player]
  (let[curr-inv (player :inventory)]
    (let [curr-location (player :location)]
    (if (and (contains? curr-inv :icard) (= curr-location :quad))
      (do (println "You can get from here to siebel.") (assoc-in player [:location] :siebel-center))
      (do (println "You need to get the icard and in the quad for transportation.") player))
    )))

(defn pick [player]
  (let [curr-room (player :location)]
    ;into function will add all elements inside contents into inventory, and then we get curr-room, we look at the-map to check the contents
  (let [new-player (update-in player [:inventory] #(into % (-> the-map curr-room :contents)))]
     ; (update-in player [:inventory] #(conj % "raw-egg"))
      (do (println (str "now you have " (-> new-player :inventory)))new-player))))

(defn finish [player]
  (let [curr-location (player :location)
        curr-invent (player :inventory)]
    (if (and (= curr-location :iken) (contains? curr-invent :ticket) (contains? curr-invent :raw-egg) (contains? curr-invent :book) (contains? curr-invent :leaf))
      ((do (println "congratulations! you finish the game")player)
        (System/exit 0))
      (do (println "You dont have everything to finish!") player))))




(defn sleep [player]
 (let [curr-room (player :location)]
   (if (= curr-room :ece-building)
     (do (println "You are at full health")(assoc-in player [:hp] 100))
     (do (println "You can't sleep here. Go back to the ece-building")))))



(defn get-info [player]
  (do (println (str "hp is " (-> player :hp) ". " "location is " (-> player :location) ". " "you have visited: "
                    (-> player :seen) ". " "you now have "(-> player :inventory) ". " "you now have " (-> player :money) " dollars. "
                    "you now have "(-> player :book) " books. " "you now have "(-> player :worktime) " working hours. "))player))

(defn take-exam [player]
  (let [curr-hp (player :hp)]
    (if (< curr-hp 1)
      (do (println "you cannot take exam because your hp is so low")player)
      (do (println "you finished your exam and you lose all your hp")(assoc-in player [:hp] 0)))))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:west] (go :west player)
         [:east] (go :east player)
         [:upstair] (go :upstair player)
         [:downstair] (go :downstair player)
         [:sleep] (sleep player)
         [:study] (study player)
         [:get-info] (get-info player)
         [:take-exam] (take-exam player)
         [:pick] (pick player)
         [:get-stuff] (get-stuff player)
         [:eat] (eat player)
         [:relax] (relax player)
         [:borrow-book] (borrow-book player)
         [:work] (work player)
         [:move] (move player)
         [:finish] (finish player)
         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
