;(eval-when-compile
;  (require 'skk-macs)
;  (require 'skk-vars)
;  )
(require 'cl-lib)
(require 'wanakana-el)


(defvar skk-act09-always-use-gedan-input-list
  '(nil nil nil nil nil)
  "子音が無いときの左下段に `ann` などを割り当てる キー毎個別に設定する"
  )
(defvar skk-act09-left-vowel-after-y t
  "actでは p,y では次の母音を右手に鏡写しにして入力する この変数を
  non-nil にすると左手でも母音を入力出来るようにする"
  )

(defvar skk-act09-use-hjkl-arrow t
  "dvorak配列でも矢印の向きをhjklで指定する"
  )
;; 基本的にact09公式ドキュメントの順に実装していく

(defun skk-act09-make-cell (mozir)
  "アルファベットになっている mozir をカタカナとひらがなに変換した組を
返す"
  (let ((hira (wanakana-to-hiragana mozir) ))
    (cons (japanese-katakana hira) hira)
    )
  )

(let ((vowel-list '("a" "o" "e" "u" "i") )
      (right-vowel-assoc-list '( ("a" . "s")
				 ("o" . "n")
				 ("e" . "t")
				 ("u" . "h")
				 ("i" . "d")

				 ("'" . "l")
				 ("," . "r")
				 ("." . "c")
				 ("p" . "g")
				 ("y" . "f")
				 
				 (";" . "z")
				 ("q" . "v")
				 ("j" . "w")
				 ("k" . "m")
				 ("x" . "b")
				 ))
      )
  ;; 基本的な打ち方
  ;; ほぼ通常のローマ字を同じ
  ;; skk ではユーザー定義で上書きしていないシーケンスはそのまま使える
  ;; か行は "c" を用いる
  (loop for vo in vowel-list
	do (
	    (add-to-list 'skk-rom-kana-rule-list
			 (list (concat "c" vo)
			       nil
			       (skk-act09-make-cell (concat "k" vo))
			       ))
	    )
	)
  ;; や行、ぱ行は右手で母音を取る
  (loop for vo in vowel-list
	do (
	    (add-to-list 'skk-rom-kana-rule-list
			 (list (concat "y"
				       (cdr (assoc vo right-vowel-assoc-list)))
			       nil
			       (skk-act09-make-cell (concat "y" vo))
			       )
			 )
	    )
	)

  (loop for vo in vowel-list
	do (
	    (add-to-list 'skk-rom-kana-rule-list
			 (list (concat "p"
				       (cdr (assoc vo right-vowel-assoc-list)))
			       nil
			       (skk-act09-make-cell (concat "p" vo))
			       )
			 )
	    )
	)

  ;; "っ" は "'" で入力する
  (add-to-list 'skk-rom-kana-rule-list
	       ("'" nil ("ッ" . "っ")))
  ;; "ん" 単独は "nn" で入力する
  ;; これは元々対応している
  )

(provide 'skk-act09)
