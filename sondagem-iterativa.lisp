(in-package :user)

(defun sondagem-iterativa (problema) 
  (let ((estado-inicial (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho))
    (labels ((isamp (estado)
                    (cond ((funcall objectivo? estado) (list estado))
                          (t
                           (let ((sucessores (problema-gera-sucessores problema)))
                             (cond ((= (list-length sucessores) 0) NIL)
                                   (t
                                    (let ((sucessor-escolhido (nth (random (list-length sucessores)))))
                                      (solucao (isamp sucessor-escolhido))))))))))
      (while (null caminho) 
        (setf caminho (isamp estado-inicial))))
    (values caminho)))