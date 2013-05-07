(defstruct basic-stats
  profundidade
  pontuacao)

(defstruct compiled-stats
  profundidade-maxima
  profundidade-media
  profundidade-minima
  pontuacao-maxima
  pontuacao-media
  pontuacao-minima)


(defun compilar-stats (lista-basic-stats) 
  (let ((stats-compilados (make-compiled-stats :profundidade-maxima 0
                                               :profundidade-media 0
                                               :profundidade-minima 0
                                               :pontuacao-maxima 0
                                               :pontuacao-media 0
                                               :pontuacao-minima 0))
        (numero-de-samples (list-length lista-basic-stats))
        (total-profundidade 0)
        (total-pontuacao 0))
    (dolist (stats lista-basic-stats) 
      (progn
        (cond ((< (compiled-stats-profundidade-maxima stats-do-no) (basic-stats-profundidade istats)) 
               (set (compiled-stats-profundidade-maxima stats-do-no) (basic-stats-profundidade istats))))
        (cond ((> (compiled-stats-profundidade-minima stats-do-no) (basic-stats-profundidade istats)) 
               (set (compiled-stats-profundidade-minima stats-do-no) (basic-stats-profundidade istats))))
        (cond ((> (compiled-stats-pontuacao-maxima stats-do-no) (basic-stats-pontuacao istats)) 
               (set (compiled-stats-pontuacao-maxima stats-do-no) (basic-stats-pontuacao istats))))
        (cond ((> (compiled-stats-pontuacao-minima stats-do-no) (basic-stats-pontuacao istats)) 
               (set (compiled-stats-pontuacao-minima stats-do-no) (basic-stats-pontuacao istats))))
        (setf total-profundidade (+ (basic-stats-profundidade stats)))
        (setf total-pontuacao (+ (basic-stats-pontuacao stats)))))
    (setf (compiled-stats-pontuacao-media stats-compilados) (/ total-pontuacao numero-samples))
    (setf (compiled-stats-profundidade-media stats-compilados) (/ total-profundidade numero-samples)))
  (values stats-compilados))

    
    



(defun sondagem-iterativa (problema) 
  (let ((estado-inicial (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho))
    (while (null caminho)
      (setf caminho (isamp estado-inicial)))
    (values caminho)))


(defun isamp (problema estado stats)
  (cond ((funcall problema-objectivo? estado) (list estado))
        (t
         (let ((sucessores (problema-gera-sucessores problema estado)))
           (cond ((= (list-length sucessores) 0)
                  (setf (istats-stats pontuacao) (vr-estado-pontuacao estado))
                  (setf (istats-stats profundidade) (1+ (istats-stats profundidade)))
                  (values NIL))
                 (t 
                  (let* ((indice-escolhido (random (list-length sucessores)))
                         (sucessor-escolhido (nth indice-escolhido sucessores))
                         (solucao (isamp problema sucessor-escolhido stats)))
                    (progn
                      (setf sucessores nil)
                      (cond ((not (null solucao)) (values (cons estado solucao))))))))))))



(defun stats-rsamp (problema estado iteracoes-por-no) 
  (cond ((funcall problema-objectivo? estado) (list estado))
        (t 
         (let* ((sucessores (problema-gera-sucessores problema estado))
                (stats-compilados-dos-sucessores)
                (indice-sucessor-escolhido))
           (block iteracoes 
             (cond ((= (list-length sucessores) 0) (values NIL)))
             (dolist (sucessor sucessores)
               (let ((lista-stats (make-compiled-stats :profundidade-maxima 0
                                                       :profundidade-media 0
                                                       :profundidade-minima 0
                                                       :pontuacao-maxima 0
                                                       :pontuacao-media 0
                                                       :pontuacao-minima 0))
                     (total-profundidade 0)
                     (total-pontuacao 0))
                 (loop for i from 0 to (1- iteracoes-por-no)
                       (let* ((istats (make-basic-stats :profundidade 0
                                                        :pontuacao 0))
                              (caminho (isamp problema sucessor stats)))
                         (cond ((not (null caminho)) (return-from iteracoes (cons estado caminho)))
                               (t (setf (nth i lista-stats-no) istats)))))
                 (push (compilar-stats lista-stats) stats-compilados-dos-sucessores))))
           
           ;;; A partir daqui, pode-se usar os stats como fôr mais conveniente.
           (block escolha 
             (let ((melhor-media 0)
                   (melhor-indice)
                   (indice 0))
               (dolist (cstats (nreverse stats-compilados-dos-sucessores))
                 (cond ((> (compiled-stats-pontuacao-media cstats) melhor-media) 
                        (setf melhor-media (compiled-stats-pontuacao-media cstats))
                        (setf melhor-indice indice)))
                 (setf indice (1+ indice)))
               (setf indice-sucessor-escolhido melhor-indice)))
           (let ((melhor-sucessor (nth indice-sucessor-escolhido sucessores)))
             (setf sucessores nil)
             (setf stats-compilados-dos-sucessores nil)
             (values (stats-samp problema melhor-sucessor iteracoes-por-no)))))))