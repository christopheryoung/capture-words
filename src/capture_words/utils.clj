
(ns capture_words.utils)

(defn address-in-trie [elements]
  (interleave (repeat :children) elements))

(defn add-item-to-trie [trie elements]
  (let [element (last elements)
        location (address-in-trie elements)]
    (if (get-in trie location)
      (update-in trie location #(conj % {:terminus true}))
      (assoc-in trie location {:terminus true}))
    ))

(defn item-in-trie? [trie item]
  (let [address (address-in-trie item)
        entry (get-in trie address)]
    (if entry (:terminus entry))))

(defn- word-to-arr [word]
  (map str (.toCharArray word)))

(defn add-word [trie word]
  (add-item-to-trie trie (word-to-arr word)))

(defn word-in-trie? [trie word]
  (let [word-array (word-to-arr word)]
    (item-in-trie? trie word-array)))

(defn create-word-trie [words]
  (reduce add-word {} words))
