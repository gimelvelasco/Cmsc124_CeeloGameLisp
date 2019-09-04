#|==========================================================================================================================|#
#|================================VELASCO, Gimel David F.========================2012-58922=================================|#
#|================================Cmsc 124==================================================================================|#
#|================================Long Exam 3====================================Nov. 26, 2016==============================|#
#|==========================================================================================================================|#
;Number 1

(defun ceelo-game (num_players) ;main ceelo game function
	(format t "~&Welcome ~S Players!" num_players)
	(let*
		(
			(players_bank nil)
			(players_bet nil)
			(players_bankTemp nil)
			(player_winner nil)
			(player_winners nil)
		)
		(do ;initializing bank of each player into 100
			((i 0))
			((>= i num_players) 'ok)
			(setf i (+ i 1))
			(setf players_bank (cons 100 players_bank))
		)
		(do  ;repeat round until one or more players are bankrupt
			()
			((<= (apply 'min players_bank) 0) 'Done)
			(setf players_bank (reverse-list players_bank))
			(format t "~&~%Players have ~S respectively" players_bank)
			(do
				((i 0))
				((>= i num_players) 'ok)
				(setf i (+ i 1))
				(format t "~&Input Player ~S bet: " i)
				(setf players_bet (cons (read) players_bet))
			)
			(setf min_bet (apply 'min players_bet))
			(setf pot (* min_bet num_players))
			(setf players_bet nil)
			(format t "~&Bets: ~S" min_bet)
			(format t "~&Pot: ~S" pot)
			(setf player_winners (car (game-round num_players))) ;here is where a round happens
			;(if (EQUAL (cdr player_winners) nil)
				(setf player_winner player_winners)
			;	(setf player_winner (car player_winners))
			;)
			(format t "~&Round Winner: Player ~S" player_winner)
			(do ;updating the bank of each player
				((j 1))
				((> j num_players) 'ok)
				(if (= player_winner j)
					(setf players_bankTemp (cons (+ (car players_bank) (- pot min_bet)) players_bankTemp))
					(setf players_bankTemp (cons (- (car players_bank) min_bet) players_bankTemp))
				)
				(setf players_bank (cdr players_bank))
				(setf j (+ j 1))
			)
			(setf players_bank players_bankTemp)
			(setf players_bankTemp nil)
		)
		(setf players_bank (reverse-list players_bank))
		(format t "~&~%Players have ~S respectively" players_bank)
		(setf biggest_bank (apply 'max players_bank))
		(format t "~&~%The Overall Winner has ~S in bank" biggest_bank)
	)
	'peace	
)

(defun game-round (num_players) ;function that simulates per round of a ceelo game
	(let* 
		(
			(max_score -1)
			(winner nil)
		)
		(do	;this do loop will simulate the number of players e.g. if there are 3 players, the do loop will iterate 3 times
			((turn_player 0))
			((>= turn_player num_players))
			(setf turn_player (+ turn_player 1))
			(format t "~&Player ~S:" turn_player)
			(do ;this do loop will continue to iterate until the current player gets a score other than reroll
				(
					(result_player (list nil 'REROLL))
				)
				((NOT (EQUAL (cadr result_player) 'REROLL)) )
				(setf result_player (round-roll))
				(setf score_player (cadr result_player))
				(format t " | ~S" result_player)
			)
			(format t " => Score is ~S" score_player)
			
			(if (EQUAL score_player 'leopards) ;these if block will assign integers to the scores so that it would be easier to check the highest score
				(setf score_player 668) ;mapping of scores are listed in the comments section at the bottom
				()
			)
			(if (EQUAL score_player 'assholes)
				(setf score_player 0)
				()
			)
			(if (EQUAL score_player 111)
				(setf score_player 667)
				()
			)
			
			(if (< max_score score_player) ;these if block will save which player has the highest score
				(do 
					((i 0))
					((= i 1) )
					(setf i 1)
					(setf winner nil)
					(setf max_score score_player)
					(setf winner (cons turn_player winner))
				)
				(if (= max_score score_player)
					(setf winner (cons turn_player winner))
					()
				)
			)
			
		)
		winner
	)

	
)

(defun round-roll () ;roll function will return a list of the combination and the score
	(let*
		(
			(d (+ (random 6) 1))
			(d2 (+ (random 6) 1))
			(d3 (+ (random 6) 1))
			(sum (+ d d2 d3))
			(comb (list d d2 d3))
		)
		(cond ;this cond block will determine the score according to the combination of the 3 die
			((= d d2 d3) (list comb (+ (* 100 d) (* 10 d) d) 'Trips))	;trips will yield a ranking value of its corresponding 3-digit number. this represents a score derived from triples. the value of 111 will be changed to 667 later since triple 1 is scores higher than other triples
			((= d d2) (list comb d3 'Point))	;points will yield the same assigned score
			((= d2 d3) (list comb d 'Point))
			((= d d3) (list comb d2 'Point))
			((= sum 15)(list comb 'Leopards))
			((= sum 6)(list comb 'Assholes))
			(T (list comb 'reroll))
		)
	)
)

(defun reverse-list (x)
	(do
		((ans nil))
		((= (length x) 0) ans)
		; body
		(setf ans (cons (car x) ans))
		(setf x (cdr x))
	)
)

#|
	Scoring Hierarchy:
		Leopards (668)
		Trips
			Triple 1 (667)
			Triple not 1 (222 to 666)
		Point (1 to 6)
		Assholes (0)

	Note:
		in some sources, a score of 6 yielded from pairs is considered as leopards
		also, a score of 1 yielded from pairs is considered as assholes
		these rules of scoring is not implemented in the game
|#

#|==========================================================================================================================|#
;Number 2

(defun eigen-fxn (input)
	(let*
		(
			(a (caar input))
			(b (cadar input))
			(c (caadr input))
			(d (cadadr input))
			(eigen_vec1 nil)
			(eigen_vec2 nil)
		)
		;(format t "~&a: ~S, b: ~S, c: ~S, d: ~S" a b c d)
		(setf eigen_val1 (* 0.5 (+ (+ a d) (sqrt (+ (* 4 b c) (square (- a d)))))))
		(setf eigen_val2 (* 0.5 (- (+ a d) (sqrt (+ (* 4 b c) (square (- a d)))))))
		
		(if (AND (= b 0) (= c 0))
			(do 
				((i 0))
				((= i 1) )
				(setf i 1)
				(format t "~&Eigenvalue1: ~S, " eigen_val1)
				(setf eigen_vec1 (list 1 0))
				(format t "~&Eigenvalue1: ~S, " eigen_val2)
				(setf eigen_vec2 (list 0 1))
			)
			(do 
				((i 0))
				((= i 1) )
				(setf i 1)
				(format t "~&Eigenvalue1: ~S, " eigen_val1)
				(setf eigen_vec1 (get-eigecvec a b c d eigen_val2))
				(format t "~&Eigenvalue1: ~S, " eigen_val2)
				(setf eigen_vec2 (get-eigecvec a b c d eigen_val1))
			)
		)

		;(format t "~&Eigenvalue1: ~S, Eigenvector1: ~S" eigen_val1 eigen_vec1)
		;(format t "~&Eigenvalue2: ~S, Eigenvector2: ~S" eigen_val2 eigen_vec2)
	)
)

(defun square (x)
	(* x x)
)

(defun get-eigecvec (a b c d eigenval)
	(let*
		(
			(a (- a eigenval))
			(b b)
			(c c)
			(d (- d eigenval))
			(eigenvec nil)
		)
		;(format t "~&~S ~S ~%~S ~S" a b c d)
		(if (< a b)
			(do 
				(
					(i 0)
					(eigenvec nil)
				)
				((= i 1) )
				(setf i 1)
				(setf eigenvec (cons b eigenvec))
				(setf eigenvec (cons a eigenvec))
				(format t "Eigenvector: ~S" eigenvec)
			)
			(do 
				(
					(i 0)
					(eigenvec nil)
				)
				((= i 1) )
				(setf i 1)
				(setf eigenvec (cons d eigenvec))
				(setf eigenvec (cons c eigenvec))
				(format t "Eigenvector: ~S" eigenvec)
			)
		)
		;eigenvec
	)
	;eigenvec
)

#|==========================================================================================================================|#
#|==========================================================================================================================|#
#|========================================================End===============================================================|#
#|==========================================================================================================================|#
#|==========================================================================================================================|#