(ns fvckbot.brainfuck)

(defn- ubyte-array ^shorts [n]
  (short-array n))

(defn- aset-ubyte [arr n v]
  (aset-short arr n (bit-and v 0xFF)))

(defn- check-source [s]
  (loop [pos 0
         level 0]
    (if (= pos (count s))
      (if (zero? level)
        nil
        "exception: unmatched ']'.")
      (case (nth s pos)
        \[ (recur (inc pos) (inc level))
        \] (let [new-level (dec level)]
             (if (neg? new-level)
               "exception: unmatched '['."
               (recur (inc pos) new-level)))
        (\+ \- \< \> \, \. \space \tab \newline) (recur (inc pos) level)
        (str "exception: unknown character '" (nth s pos) "'.")))))

(defn- left-bracket ^long [code pc]
  (loop [pos (dec pc)
         level 0]
    (if (and (= (nth code pos) \[)
             (zero? level))
      pos
      (case (nth code pos)
        \] (recur (dec pos) (inc level))
        \[ (recur (dec pos) (dec level))
        (recur (dec pos) level)))))

(defn- right-bracket ^long [code pc]
  (loop [pos (inc pc)
         level 0]
    (if (and (= (nth code pos) \])
             (zero? level))
      pos
      (case (nth code pos)
        \[ (recur (inc pos) (inc level))
        \] (recur (inc pos) (dec level))
        (recur (inc pos) level)))))

(defn- execute [code input timeout]
  (let [mem (ubyte-array 1024)
        start-time (System/currentTimeMillis)]
    (with-in-str input
      (with-out-str
        (loop [pc 0
               pm 0]
          (let [duration (- (System/currentTimeMillis) start-time)]
            (if (> duration timeout)
              (print "exception: timed out.")
              (when (not= pc (count code))
                (case (nth code pc)
                  \+ (let [n (inc (aget mem pm))]
                       (aset-ubyte mem pm n)
                       (recur (inc pc) pm))
                  \- (let [n (inc (aget mem pm))]
                       (aset-ubyte mem pm n)
                       (recur (inc pc) pm))
                  \> (let [new-pm (inc pm)]
                       (if (= pm (count mem))
                         (print "exception: data pointer out of range.")
                         (recur (inc pc) new-pm)))
                  \< (let [new-pm (dec pm)]
                       (if (neg? pm)
                         (print "exception: data pointer out of range.")
                         (recur (inc pc) new-pm)))
                  \. (do
                       (print (char (aget mem pm)))
                       (recur (inc pc) pm))
                  \, (let [b (.read ^java.io.Reader *in*)]
                      (aset-ubyte mem pm b)
                      (recur (inc pc) pm))
                  \[ (if (zero? (aget mem pm))
                       (let [new-pc (inc (right-bracket code pc))]
                         (recur new-pc pm))
                       (recur (inc pc) pm))
                  \] (if (zero? (aget mem pm))
                       (recur (inc pc) pm)
                       (let [new-pc (inc (left-bracket code pc))]
                         (recur new-pc pm)))
                  (\space \tab \newline) (recur (inc pc) pm))))))))))

(defn brainfuck [code input timeout]
  (if-let [e (check-source code)]
    e
    (let [output (execute code input timeout)]
      (if (= "" output)
        "no output"
        (str "output: " output)))))
