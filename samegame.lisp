;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; Copyright (C) Bruno Dias      <bruno.filipe.silva.dias@gmail.com>
;;; Copyright (C) Alexandre Bento <alexandre.bento@ist.utl.pt>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)


#|
(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (space 3)
                   (debug 3)
                   (safety 1)))
|#


(defun sondagem-iterativa (problema) 
  (let ((estado-inicial (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho)
        (*nos-gerados* 0)
	(*nos-expandidos* 0))
    (labels ((isamp (estado)
                    (cond ((funcall objectivo? estado) 
                           (progn 
                             ;(print "ESTADO OBJECTIVO")
                             (list estado)))
                          (t
                           (let ((sucessores (problema-gera-sucessores problema estado)))
                             (cond ((= (list-length sucessores) 0)
                                    (progn
                                      ;(print "SEM FILHOS")
                                      NIL))
                                   (t 
                                    (let ((indice-escolhido (random (list-length sucessores))))
                                      (let ((sucessor-escolhido (nth indice-escolhido sucessores)))
                                        (let ((solucao (isamp sucessor-escolhido)))
                                          (progn
                                            (setf sucessores nil)
                                            (cond ((not (null solucao)) (values (cons estado solucao)))))))))))))))
      (while (null caminho)
        (setf caminho (isamp estado-inicial))
        ;(mapcar #'(lambda (estado) (format t "~S" (vr-estado-lista-jogadas estado))) caminho)
        ;(format t "~%")
        (pprint caminho)
        ))
    (values caminho)))



(setq tempo-inicio nil)
(setq melhor-solucao nil)
(setq numero-linhas-maxima nil)
(setq numero-colunas-maxima nil)
(defconstant infinity most-positive-single-float)




;;;  pontuacao: pontos acumulados, até este ponto do jogo.
;;;  tabuleiro: lista de listas.
;;;  jogada: TODO
;;;  numero-pecas-eliminadas: TODO
;;;  pontos-ganhos-ultima-jogada

(defstruct vr-estado
  pontuacao
  tabuleiro 
  jogada 
  numero-pecas-eliminadas 
  pontuacao-anterior)



;;; Cada estado inicial será constituído por:
;;;  (1) um tabuleiro cheio
;;;  (2) sem qualquer pontuação
;;;  (3) sem qualquer jogada que crie este tabuleiro
;;;  (4) número de linhas, do tabuleiro
;;;  (5) número de colunas, do tabuleiro
;;; Note-se que não haverá distinção entre um estado 
;;; que contém um tabuleiro virado e um original. Isso 
;;; deve ao facto de que, durante a procura, não haverá 
;;; qualquer diferença entre um tabuleiro original e 
;;; um virado.

(defun vr-estado-inicial (tab) 
  (let ((nlinhas (list-length tab))
        (ncolunas (list-length (car tab))))
    (values (make-vr-estado :pontuacao 0
                            :tabuleiro tab
                            :jogada nil
                            :numero-pecas-eliminadas 0
                            :pontuacao-anterior 0))))



;;; Dado que este é um problema de optimização e que a 1ª solução poderá 
;;;  não ser a óptima, tentar-se-á percorrer, o que puder, do espaço de 
;;;  estados, tendo em conta a restrição de tempo imposta. Neste caso, ao 
;;;  fim de 5 minutos, deverá terminar a procura.

(defun vr-objectivo? (estado)
  (let ((tempo-actual (get-internal-run-time)))
    (let ((diferenca-tempo (- tempo-actual tempo-inicio)))
      (progn
        (format t "vr-objectivo? | pontuacao: ~D | tempo-inicio: ~D | tempo-actual: ~D | diferenca-tempo: ~D ~%" (vr-estado-pontuacao estado) tempo-inicio tempo-actual diferenca-tempo)
        (values (>= diferenca-tempo 300000))))))



(defun vr-operador (estado)
  (let ((grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado)))
        estados)
    (dolist (grupo grupos)
      (if (and (not (vr-singleton? grupo)) (> (list-length grupo) 2))
          (let ((copia-do-tabuleiro (vr-copiar-tabuleiro (vr-estado-tabuleiro estado)))
                (pontos-obtidos (vr-obter-pontos grupo)))
            (let ((novo-tabuleiro (vr-deslocar-tabuleiro (vr-remover-grupo grupo copia-do-tabuleiro))))
              (let ((novo-estado (make-vr-estado :pontuacao (+ (vr-estado-pontuacao estado) pontos-obtidos)
                                                 :tabuleiro novo-tabuleiro
                                                 :jogada (car grupo)
                                                 :numero-pecas-eliminadas 
                                                 :pontuacao-anterior pontos-obtidos)))
                (progn 
                  (push novo-estado estados)
                  (cond ((vr-melhor-solucao? novo-estado) 
                         (setf melhor-solucao novo-estado) 
                         (format t "MELHOR SOLUCAO TEM: ~D~%" (vr-estado-pontuacao novo-estado))
                         ))))))))
    (values estados)))

  
  
(defun vr-obter-pontos (grupo) 
  (let ((numero-pecas (list-length grupo)))
    (values (expt (- numero-pecas 2) 2))))



(defun vr-melhor-solucao? (estado) 
  (or (null melhor-solucao) (< (vr-estado-pontuacao melhor-solucao) (vr-estado-pontuacao estado))))
    
  



;;;--------------------------------------------------------------------
;;;---------------------Operações sobre tabuleiros---------------------
;;;--------------------------------------------------------------------

;;; O tabuleiro é "virado" para ficar uma lista de colunas, 
;;;  em vez de ser uma lista de linhas. Isto facilita a 
;;;  movimentação de peças, tanto para baixo como para a 
;;;  esquerda. Esta função permite obter uma representação 
;;;  do tabuleiro que facilita as movimentações, no tabuleiro.

(defun vr-virar-tabuleiro-1 (tabuleiro) 
  (let ((numero-colunas (list-length (car tabuleiro)))
        (novo-tabuleiro (list)))
    (loop 
      for i from 0 to (1- numero-colunas)
      do (setf novo-tabuleiro (append novo-tabuleiro (list (nreverse (mapcar #'(lambda (el) (append (nth i el))) tabuleiro))))))
    (values novo-tabuleiro)))



;;; Se fôr necessário converter um tabuleiro, na versão 
;;; "transposta", para a versão original. Note que é preciso, 
;;; primeiro, preencher o tabuleiro restante com NILs.

(defun vr-virar-tabuleiro-2 (tabuleiro) 
  (let ((numero-linhas  (list-length tabuleiro))
        (novo-tabuleiro (list)))
    (loop 
      for i from 0 to (1- numero-linhas)
      do (setf novo-tabuleiro (append novo-tabuleiro (list (mapcar #'(lambda (el) (append (nth i el))) tabuleiro)))))
    (values (nreverse novo-tabuleiro))))



;;; Função que permite deslocar as peças de um tabuleiro. Porém, 
;;;  esse tabuleiro necessita de estar virado, para que o resultado 
;;;  seja de acordo com o esperado.
;;;  (1) Remove os NIL das sublistas
;;;  (2) Remove as sublistas que se tornaram vazias
;;; Esta representação do tabuleiro permite reduzir a 
;;;  ocupação da memória, ao não considerar as posições 
;;;  com NIL.

(defun vr-deslocar-tabuleiro (tabuleiro) 
  (values (remove nil (mapcar #'(lambda (el) (remove nil el)) tabuleiro))))



(defun vr-criar-novo-tabuleiro(linhas colunas cor)
  (let ((tabuleiro (vr-cria-lista linhas)))
    (loop for i from 0 to  (1- linhas) do
          (setf (nth i tabuleiro) (vr-cria-lista colunas))
          (loop for j from 0 to (1- colunas)
              do (vr-add-cor-tabuleiro cor i j tabuleiro)))
    tabuleiro))



(defun vr-get-cor-tabuleiro (linha coluna tabuleiro)
  (nth coluna (vr-preenche-coluna (nth linha tabuleiro) (vr-length-maior-coluna tabuleiro))))



(defun vr-add-cor-tabuleiro (cor linha coluna tabuleiro)
  (setf (nth coluna (nth linha tabuleiro)) cor))



;;Devolve o tamanho da maior lista no tabuleiro
(defun vr-length-maior-coluna (tabuleiro-actual)
  (let ((tam 0))
    (loop for i from 0 to (length tabuleiro-actual)
        do (if (< tam (length (nth i tabuleiro-actual)))
               (setf tam (length (nth i tabuleiro-actual)))))
    tam))




(defun vr-procura-grupos (i j linhas colunas tabuleiro-actual novo-tab)
  (if (and (< i linhas)
           (< j colunas))
      (progn
        (if (equal (vr-get-cor-tabuleiro i j novo-tab) nil)
            (progn
              (let* ((cor (vr-get-cor-tabuleiro i j tabuleiro-actual))
                     (grupo (vr-get-grupo cor i j tabuleiro-actual novo-tab)))
                (if (not (null cor))
                    (cons grupo (vr-procura-grupos i (1+ j) linhas colunas tabuleiro-actual novo-tab))
                  (vr-procura-grupos i (1+ j) linhas colunas tabuleiro-actual novo-tab))
                )
              )
          (vr-procura-grupos i (1+ j) linhas colunas tabuleiro-actual novo-tab)))
    (if (and (< i linhas) (>= j colunas))
        (vr-procura-grupos (+ i 1) 0 linhas colunas tabuleiro-actual novo-tab)
      '())))





;;;--------------------------------------------------------------------
;;;----------------------Operações sobre grupos------------------------
;;;--------------------------------------------------------------------

;;Recebe uma lista de posicoes de um grupo e remove-os no tabuleiro pondo a NIL
(defun vr-remover-grupo (grupo tabuleiro-actual) 
  (if (null grupo) tabuleiro-actual
    (let ((linha (caar grupo)) (coluna (cdar grupo)) (tabuleiro (vr-copiar-tabuleiro tabuleiro-actual)))
      (vr-add-cor-tabuleiro nil linha coluna tabuleiro)
        (vr-remover-grupo (cdr grupo) tabuleiro))))



(defun vr-devolve-grupo(linha coluna tabuleiro-actual)
  (let ((novo-tab (vr-criar-novo-tabuleiro (length tabuleiro-actual)
                                        (vr-length-maior-coluna tabuleiro-actual)
                                        nil)) ;nil = cor em vazio
        (cor (vr-get-cor-tabuleiro linha coluna tabuleiro-actual)))
    (vr-get-grupo cor linha coluna tabuleiro-actual novo-tab)))



(defun vr-get-grupo(cor linha coluna tabuleiro-actual novo-tab)
  (if 
      (and (>= coluna 0) 
           (>= linha 0)
           (< linha (length tabuleiro-actual))
           (< coluna (vr-length-maior-coluna tabuleiro-actual))
           (not (null cor)))
      (progn
         (if (equal (vr-get-cor-tabuleiro linha coluna novo-tab) nil) ;nil = cor em vazio
             (if (equal (vr-get-cor-tabuleiro linha coluna tabuleiro-actual) cor)
                 (progn
                   (setf (nth coluna (nth linha novo-tab)) cor)
                   (append (list (cons linha coluna))
                           (vr-get-grupo cor linha (1+ coluna)  
                                      tabuleiro-actual novo-tab)
                           (vr-get-grupo cor linha (1- coluna)  
                                      tabuleiro-actual novo-tab)
                           (vr-get-grupo cor (1+ linha) coluna
                                      tabuleiro-actual novo-tab)
                           (vr-get-grupo cor (1- linha) coluna
                                      tabuleiro-actual novo-tab)))
               '()) '())) NIL))



(defun vr-get-todos-grupos(tabuleiro-actual)
    (let ((linhas (length tabuleiro-actual))
          (colunas (vr-length-maior-coluna tabuleiro-actual)))
      (vr-procura-grupos 0 0 linhas colunas tabuleiro-actual (vr-criar-novo-tabuleiro linhas colunas nil))))






;;;--------------------------------------------------------------------
;;;---------------------------Heurísticas------------------------------
;;;--------------------------------------------------------------------

;;; Heurística que conta o número de singletons presentes num dado estado. 
;;; Com esta heurística, dar-se-á prioridade a estados que tenham menos 
;;;  singletons.

(defun vr-h-singletons (estado) 
  (let ((grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado)))
        (numero-de-singletons 0))
    (dolist (grupo grupos)
      (let ((tamanho-do-grupo (list-length grupo)))
        (when (= tamanho-do-grupo 1) 
          (setf numero-de-singletons (1+ numero-de-singletons)))))
    (values numero-de-singletons)))



;;; Heurística que obtém o tamanho do maior grupo, num dado estado.
;;; Com esta heurística, dar-se-á prioridade a estados que tenham grupos 
;;; grandes.

(defun vr-h-tamanho-do-maior-grupo (estado) 
  (let ((grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado)))
        (tamanho-do-maior-grupo 1))
    (dolist (grupo grupos) 
      (let ((tamanho-do-grupo (list-length grupo)))
        (when (> tamanho-do-grupo tamanho-do-maior-grupo) 
          (setf tamanho-do-maior-grupo (1+ tamanho-do-maior-grupo)))))
    (values (/ 1 tamanho-do-maior-grupo))))



;;; Heurística que calcula o tamanho médio de um grupo, num dado estado.
;;; Esta heurística priveligia estados com médias maiores.

(defun vr-h-media-dos-tamanhos (estado) 
  (let ((grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado)))
        (soma-dos-tamanhos 0)
        (numero-de-grupos 0))
    (dolist (grupo grupos)
      (cond ((or (not (= (list-length grupo) 0)) (not (vr-singleton? grupo)))
              (let ((tamanho-do-grupo (list-length grupo)))
                (setf soma-dos-tamanhos (+ soma-dos-tamanhos tamanho-do-grupo))
                (setf numero-de-grupos (1+ numero-de-grupos))))
            (t (setf numero-grupos (1+ numero-grupos)))))
    (let ((resultado (values (/ 1 (/ soma-dos-tamanhos numero-de-grupos)))))
      (progn 
        ;(format t "HEURISTICA: ~D , ~D ~%" soma-dos-tamanhos numero-de-grupos)
        (values resultado)))))


(defun vr-h-pontuacao (estado)
  (if (= (vr-estado-pontuacao estado) 0)
      (values infinity)
    (values (/ 1 (vr-estado-pontuacao estado)))))


(defun vr-c1 (estado)
  (let ((nrGrupos 0)
        (grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado))))
    (dolist (grupo grupos)
      (if (not (vr-singleton? grupo))
          (setf nrGrupos (+ nrGrupos 1))))
    (values nrGrupos)))

(defun vr-h-tamanho-do-menor-grupo (estado) 
  (let ((grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado)))
        (tamanho-do-menor-grupo 0))
    (dolist (grupo grupos)
      (if (not (vr-singleton? grupo))
          (let ((tamanho-do-grupo (list-length grupo)))
            (when (< tamanho-do-grupo tamanho-do-menor-grupo) 
              (setf tamanho-do-menor-grupo tamanho-do-grupo))))
      (values tamanho-do-maior-grupo))))    

(defun vr-custo-1 (estado) 
  (if (= (vr-estado-numero-pecas-eliminadas estado) 0)
      (values 1)
    (values (/ 1 (vr-estado-numero-pecas-eliminadas estado)))))






;;;--------------------------------------------------------------------
;;;----------------------Funções utilitárias---------------------------
;;;--------------------------------------------------------------------

;;; Função que permite obter a lista de elementos presentes nas
;;; sublistas, de uma dada lista. 
;;; Exemplo: (vr-aplanar-lista ((1 2) (3 4))) => (1 2 3 4)

(defun vr-aplanar-lista (lista)
  (let ((result (list)))
    (dolist (el lista)
      (setf result (append result el)))
    (values result)))



;;; Função usada para saber se é possível realizar mais jogadas, 
;;; verificando se só existem singletons.

(defun vr-pode-reduzir-o-tabuleiro? (estado) 
  (let ((posicoes-do-tabuleiro (vr-aplanar-lista (vr-estado-tabuleiro estado)))
        (grupos (vr-get-todos-grupos (vr-estado-tabuleiro estado))))
    (values (not (= (list-length posicoes-do-tabuleiro) (list-length grupos))))))



;;; No final da procura, é necessário indicar a lista de jogadas feitas.
;;; Para tal, percorre-se todos os estados que pertencem à melhor 
;;; solução encontrada, extraindo-se o pár de coordenadas, indicado em
;;; (vr-estado-ultima-posicao-jogada estado). De notar que, dado que se 
;;; realizou uma transposição do tabuleiro, no início da procura, tem de 
;;; se trocar as posições do pár.

(defun vr-normaliza-jogadas (grupo numero-colunas)
  (let* ((grupoNormal (cons (vr-normaliza-posicao (car grupo) numero-colunas) nil)))
    (if (null (cdr grupo)) 
        (values grupoNormal)
      (cons (vr-normaliza-posicao (car grupo) numero-colunas) (vr-normaliza-jogadas (cdr grupo) numero-colunas)))))



(defun vr-copiar-tabuleiro (tabuleiro-actual)
  (if (null tabuleiro-actual) nil
    (progn
      (let ((novo-tab (vr-criar-novo-tabuleiro (length tabuleiro-actual) (vr-length-maior-coluna tabuleiro-actual) nil)))
        (loop for i from 0 to (1- (length tabuleiro-actual)) do
              (loop for j from 0 to (1- (vr-length-maior-coluna tabuleiro-actual))
                  do (vr-add-cor-tabuleiro (vr-get-cor-tabuleiro i j tabuleiro-actual) i j novo-tab)))
        novo-tab))))



(defun vr-cria-lista(n)
  (make-list n :initial-element nil))



(defun vr-preenche-coluna (lst tam)
  (if (= tam (length lst)) lst
    (vr-preenche-coluna (append lst '(nil)) tam)))



;;;Verifica se o grupo e' singleton
(defun vr-singleton? (grupo)
  (if (> (length grupo) 1) nil
    T))



;;Inverte uma dada posicao
(defun vr-normaliza-posicao (posicao numero-colunas)
  (if (null posicao) nil
    (let ((l (car posicao))
          (c (cdr posicao))
          (numColunas (- numero-colunas 1)))
      (cons (- numColunas c) l))))

                   

;;devolve a cor com menos pecas no tabuleiro e o respectivo numero
(defun vr-contar-pecas-por-cor (estado)
  (let ((tab (vr-aplanar-lista (vr-estado-tabuleiro estado)))
        (lstCores (remove-duplicates (vr-aplanar-lista (vr-estado-tabuleiro estado))))
        (menorCor nil))
    (let ((numPecas (list-length tab)))
      (dolist (cor lstCores)
        (let ((numCor (count cor tab)))
          (if (< numCor numPecas)
              (progn
                (setf numPecas numCor))
            (setf menorCor cor))))
      (cons menorCor numPecas))))
