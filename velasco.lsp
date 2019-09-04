#|
Cmsc 124 ME
November 16, 2016
|#
(defun ceelo ()
	(let
		(
			(p (+ (random 6) 1))
			(q (+ (random 6) 1))
			(r (+ (random 6) 1))
		)
		(cond
			;pqr has 456 - Auto_Win
			((OR 
					(AND (= p 4) (= q 5) (= r 6)) 
					(AND (= p 4) (= q 6) (= r 5)) 
					(AND (= p 5) (= q 4) (= r 6)) 
					(AND (= p 5) (= q 6) (= r 4)) 
					(AND (= p 6) (= q 5) (= r 4)) 
					(AND (= p 6) (= q 4) (= r 5))) 
			(list 'Leopards p q r))
			;pqr has 123 - Auto_Lose
			((OR 
				(AND (= p 1) (= q 2) (= r 3)) 
				(AND (= p 1) (= q 3) (= r 2)) 
				(AND (= p 2) (= q 1) (= r 3)) 
				(AND (= p 2) (= q 3) (= r 1)) 
				(AND (= p 3) (= q 2) (= r 1)) 
				(AND (= p 3) (= q 1) (= r 2))) 
			(list 'Assholes p q r))
			;pqr all 1s - Score_is_111
			((= 1 p q r) (list p 'is 'the 'score 'via 'Triple))
			;pqr all the same - Score_is_Triple_num
			((= p q r) (list p 'is 'the 'score 'via 'Triple))
			;pqr has pair - Score_is_otherpair
			((= p q) (list r 'is 'the 'score 'via 'PAIR))
			((= p r) (list q 'is 'the 'score 'via 'PAIR))
			((= q r) (list p 'is 'the 'score 'via 'PAIR))
			;pqr all different - No_Score
			(T (list 'Roll 'Again))
		)
	)
)
(ceelo)