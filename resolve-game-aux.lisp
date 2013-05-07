;(defun resolve-game (tabuleiro procura) 
;  (let ((tabuleiro-virado (vr-virar-tabuleiro-1 tabuleiro)))
;    (progn 
;      (setf tempo-inicio (get-universal-time))
;      (setf numero-linhas-maxima (list-length tabuleiro-virado))
;      (setf numero-colunas-maxima (list-length (car tabuleiro-virado)))
;      (cond ((string-equal procura "melhor.abordagem") ())
;            ((string-equal procura "a*.melhor.heuristica") ())
;            ((string-equal procura "a*.melhor.heuristica.alternativa") ())
;            ((string-equal procura "sondagem.iterativa") ())
;            ((string-equal procura "abordagem.alternativa") ())))))

;(defun resolve-game (tabuleiro) 
;  (let ((tabuleiro-virado (vr-virar-tabuleiro-1 tabuleiro)))
;    (progn 
;      (setf numero-linhas-maxima (list-length tabuleiro-virado))
;      (setf numero-colunas-maxima (list-length (car tabuleiro-virado)))
;      (setf tempo-inicio (get-internal-run-time))
;      (procura (cria-problema 
;                (vr-estado-inicial tabuleiro-virado) 
;                (list #'vr-operador) 
;                :objectivo? #'vr-objectivo? 
;                :estado= #'equal) 
;               "profundidade"
;               :espaco-em-arvore? T))))

(defun resolve-game (tabuleiro) 
  (let ((tabuleiro-virado (vr-virar-tabuleiro-1 tabuleiro)))
    (progn 
      (setf numero-linhas-maxima (list-length tabuleiro-virado))
      (setf numero-colunas-maxima (list-length (car tabuleiro-virado)))
      (setf tempo-inicio (get-internal-run-time))
      (sondagem-iterativa (cria-problema 
                           (vr-estado-inicial tabuleiro-virado) 
                           (list #'vr-operador) 
                           :objectivo? #'vr-objectivo? 
                           :estado= #'equal)))))


(defun resolve-game (tabuleiro) 
  (let ((tabuleiro-virado (vr-virar-tabuleiro-1 tabuleiro)))
    (progn 
      (setf numero-linhas-maxima (list-length tabuleiro-virado))
      (setf numero-colunas-maxima (list-length (car tabuleiro-virado)))
      (setf tempo-inicio (get-internal-run-time))
      (procura (cria-problema 
                (vr-estado-inicial tabuleiro-virado) 
                (list #'vr-operador) 
                :objectivo? #'vr-objectivo? 
                :estado= #'equal
                :heuristica #'vr-h-singletons
                ;:heuristica #'vr-h-pontuacao
                ;:heuristica #'vr-h-media-dos-tamanhos
                :custo #'(lambda (estado) (values 0))
                )
               "a*"
               :espaco-em-arvore? T))))
