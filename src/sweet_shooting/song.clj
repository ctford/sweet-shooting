(ns sweet-shooting.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [overtone.inst.drum :as drums]
            [leipzig.temperament :as temperament]))

; Instruments
(definst bass [freq 110 dur 1.0 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.0001 0.8 0.5 0.1) (line:kr 1 0 dur) :action FREE))
      (rlpf (+ (* 440 (sin-osc 3)) 880) 0.3)
      (+ (square (* 1/2 freq)))
      (lpf 2000)
      (* (env-gen (adsr 0.0001 1.0 0.8 0.01) (line:kr 1 0 dur) :action FREE))
      (* volume)))

(definst sing [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (+ (sin-osc (* 3 freq)))
      (+ (sin-osc (* 4.95 freq)))
      (+ (sin-osc (* 6.95 freq)))
      (* (env-gen (adsr 0.01 0.5 0.1) (line:kr 1 0 dur) :action FREE))
      (pan2 0.4)
      (* 1/4 volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square (+ freq (* 40 (sin-osc 6))))
      (* (env-gen (adsr dur 0.8 0.5 0.2) (line:kr 1 0 dur) :action FREE))
      (lpf 2600)
      (hpf 440)
      (pan2 0.6)
      (* 1/4 volume)))

(def kit {:kick #(drums/kick2 :amp 5)
          :tick #(drums/closed-hat :amp 3 :t 0.03),
          :tock #(drums/open-hat :amp 4 :t 0.1)})

; Arrangement
(defmethod live/play-note :bass [{hertz :pitch seconds :duration}] (bass hertz seconds))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))
(defmethod live/play-note :harmony [{hertz :pitch seconds :duration}] (sing hertz seconds 0.5))
(defmethod live/play-note :melody [{hertz :pitch seconds :duration}] (sing hertz seconds 1.0))
(defmethod live/play-note :beat [{drum :drum}] ((drum kit)))

; Composition
(def beat
  (->> (rhythm (cycle [1/2 1/2 1 1/2 1/2 3/4 1/4]))
       (having :drum [:tick :tick :tock :tick :tick :kick :tock])
       (times 3)
       (then (->> (rhythm (concat (repeat 6 1/2) (repeat 4 1/4)))
                  (having :drum (cycle [:tick]))))
       (all :part :beat)))

(def bassline
  (->> (phrase [3 1/2 4.5 3 1/2 2.5 1/2 1/2 1/2 1/2]
               [0 -2 -4 0 -2 -4 -4 -3 -2 -4])
       (where :pitch (comp scale/lower scale/lower))
       (all :part :bass)))

(def accompaniment
  (let [tune
        (phrase [3.5 4.5 3.5 4.5]
                (map #(-> chord/triad (chord/root %)) [0 3 0 3]))]
    (->> tune
         (with (->> tune (where :pitch scale/lower)))
         (with (->> tune (where :pitch scale/raise)))
         (with (after 8 (phrase [8] [[11 18]])))
         (with (phrase [0] [16]))
         (all :part :accompaniment))))

(def alt
  (let [tune
        (phrase [4 4 4 4]
                [(-> chord/triad (chord/root -1) (update :i - 1/2))
                 (-> chord/triad (chord/root 0))
                 (-> chord/triad (chord/root -1) (update :i - 1/2))
                 (-> chord/triad (chord/root -4))])]
    (->> tune
         (with (->> tune (where :pitch scale/lower)))
         (with (->> tune (where :pitch scale/raise)))
         (all :part :accompaniment))))

(def alt-bassline
  (let [tune (->> (phrase (repeat 1/2) (cycle [-1.5 -2 -3 -4]))
                  (where :pitch scale/lower)
                  (take 32))]
    (->> tune
         (all :part :bass))))

(def alt-beat
  (->> (rhythm (repeat 1))
       (having :drum (repeat :kick))
       (take 16)
       (all :part :beat)))

(def melody
  (let [tune (phrase [1 1/2 1/2 1/2 2/2]
                     [2 1 0 1 0])
        tune2 (phrase [1/2 1/2 1/2 1/2 1/2 1/2 2/2]
                      [1 2 4 1 0 1 0])
        tune3 (phrase  [1/2 1/2 1/2 1/2 1/2 1/2 2/2]
                      [1 2 2 1 0 1 0])
        tune4 (phrase (repeat 8 1/2)
                      [0 -3 -2 0 -2 0])]
    (->> tune
         (then tune2)
         (then tune3)
         (then tune4)
         (all :part :melody))))

(def chorus
  (let [tune
        (->>
          (phrase (cycle [1 1/2 1/2 1/2 1/2 1/2 3/2])
                  (cycle [2 1 0 1 0 -2 -3]))
          (take 12)
          (all :part :melody))]
    (->> tune (then (drop-last 3 tune)))))

(def harmony
  (let [tune
        (->>
          (phrase (cycle [1 1/2 1/2 1/2 1/2 1/2 3/2])
                  (cycle [2 1 0 1 0 -2 -3]))
          (take 12)
          (where :pitch scale/raise)
          (all :part :harmony))]
    (with tune
      (after 2 tune)
      (where :pitch scale/raise (after 4 tune))
      (after 6 tune))))

; Track
(def track
  (->>
    bassline
    (then (with beat bassline))

    (then (with bassline chorus))
    (then (with bassline harmony chorus))

    (then (with beat melody accompaniment))
    (then (with beat bassline melody accompaniment))

    (then (with beat bassline harmony chorus accompaniment))
    (then (with beat bassline accompaniment))

    (then alt)
    (then (with alt-beat alt alt-bassline))

    (then (with beat bassline harmony chorus accompaniment))
    (then (with harmony melody))
    (then (with beat bassline harmony))
    (then (take 1 bassline))

    (where :pitch (comp temperament/equal scale/A scale/major))
    (tempo (bpm 90))))

(defn -main []
  (live/play track))

(comment
  ; Loop the track, allowing live editing.
  (live/jam (var track))
  (live/stop)
  (recording-start "sweet-shooting.wav")
  (live/play track)
  (recording-stop)
)
