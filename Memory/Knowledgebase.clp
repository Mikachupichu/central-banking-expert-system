(deftemplate indicator (slot name (type STRING)) (slot percentage (type FLOAT)) (slot fuzziness (type FLOAT)))
(deftemplate explanation (slot conclusion) (multislot observation) (slot reason))
 
;; IF there is a diagnosis and an explanation with the same conclusion THEN the conclusion, observation and reason are printed
(defrule report
    (explanation (conclusion ?d) (observation $?o) (reason ?r))
    =>
    (printout t "CONCLUSION: " ?d crlf)
    (printout t "OBSERVATION: " ?o crlf)
    (printout t "REASON: " ?r crlf)
)
 
;; IF there is an inputted economic indicator THEN retract it and assert an indicator with the same properties but with an embedded CF rather than a property
(defrule initialize-indicator
    (declare (salience 100))
    (economic-indicator (name ?name) (percentage ?percentage) (fuzziness ?fuzziness) (CF ?CF))
    ?old-indicator <- (economic-indicator (name ?name) (percentage ?percentage) (fuzziness ?fuzziness) (CF ?CF))
    =>
    (printout t "Enter the " ?name " in percent (%) or leave blank to keep database values. ")
    (bind ?input (read))
    (if (numberp ?input)
        then (bind ?value ?input)
        else (bind ?value ?percentage)
    )
    (retract ?old-indicator)
    (assert (indicator (name ?name) (percentage ?value) (fuzziness ?fuzziness)) CF ?CF)
)
 
;; IF there is the inflation indicator THEN assert the corresponding fact with the same percentage value and fuzziness
(defrule initialize-inflation
    (declare (salience 99))
    (indicator (name "Inflation") (percentage ?percentage) (fuzziness ?fuzziness))
    =>
    (bind ?low (- ?percentage ?fuzziness))
    (bind ?high (+ ?percentage ?fuzziness))
    (assert (inflation (?low 0.0) (?percentage 1.0) (?high 0.0)))
)
;; IF there is the expected inflation indicator THEN assert the corresponding fact with the same percentage value and fuzziness
(defrule initialize-expected-inflation
    (declare (salience 99))
    (indicator (name "Expected Inflation") (percentage ?percentage) (fuzziness ?fuzziness))
    =>
    (bind ?low (- ?percentage ?fuzziness))
    (bind ?high (+ ?percentage ?fuzziness))
    (assert (expected-inflation (?low 0.0) (?percentage 1.0) (?high 0.0)))
)
;; IF there is the unemployment indicator THEN assert the corresponding fact with the same percentage value and fuzziness
(defrule initialize-employment
    (declare (salience 99))
    (indicator (name "Unemployment Rate") (percentage ?percentage) (fuzziness ?fuzziness))
    =>
    (bind ?low (- ?percentage ?fuzziness))
    (bind ?high (+ ?percentage ?fuzziness))
    (assert (unemployment (?low 0.0) (?percentage 1.0) (?high 0.0)))
)
;; IF there is the potential GDP growth indicator THEN assert the corresponding fact with the same percentage value and fuzziness
(defrule initialize-potential-GDP-growth
    (declare (salience 99))
    (indicator (name "Potential GDP Growth Rate") (percentage ?percentage) (fuzziness ?fuzziness))
    =>
    (bind ?low (- ?percentage ?fuzziness))
    (bind ?high (+ ?percentage ?fuzziness))
    (assert (potential-GDP-growth (?low 0.0) (?percentage 1.0) (?high 0.0)))
)
;; IF there is the real GDP growth indicator THEN assert the corresponding fact with the same percentage value and fuzziness
(defrule initialize-real-GDP-growth
    (declare (salience 99))
    (indicator (name "Real GDP Growth Rate") (percentage ?percentage) (fuzziness ?fuzziness))
    =>
    (bind ?low (- ?percentage ?fuzziness))
    (bind ?high (+ ?percentage ?fuzziness))
    (assert (real-GDP-growth (?low 0.0) (?percentage 1.0) (?high 0.0)))
)

;; IF there is an inputted margin of equality THEN use the inputted margin of equality
(defrule margin-definition
    (declare (salience 98))
	(equality-margin ?equality)
	=>
	(assert (margin ?equality))
)
;; IF there is ¬ an inputted margin of equality THEN use 1% as the default margin of equality
(defrule margin-definition-default
    (declare (salience 98))
	(not (equality-margin ?))
	=>
	(assert (margin 1.0)) ;; Rounding to each percent unit (arbitrary default pick)
)

;; IF there is an inputted target inflation rate THEN use the inputted target inflation rate
(defrule target-inflation-definition
    (declare (salience 98))
	(target-inflation ?inflation)
	=>
	(assert (target ?inflation))
)
;; IF there is ¬ an inputted target inflation rate THEN use 2% [1] the default target inflation rate
(defrule target-inflation-definition-default
    (declare (salience 98))
	(not (target-inflation ?inflation))
	=>
	(assert (target 2.0))
)

;; IF there is an inputted ideal unemployment rate THEN use the inputted ideal unemployment rate
(defrule ideal-unemployment-definition
    (declare (salience 98))
	(ideal-unemployment-rate ?rate)
	=>
	(assert (ideal-unemployment ?rate))
)
;; IF there is ¬ an inputted ideal unemployment rate THEN use 5% [3] as the default ideal unemployment rate
(defrule ideal-unemployment-definition-default
    (declare (salience 98))
	(not (ideal-unemployment-rate ?rate))
	=>
	(assert (ideal-unemployment 5.0))
)

;; IF the output gap has not already been calculated ∧ the difference between the real and potential GDPs is significantly positive THEN the output gap is unideally positive [5]
(defrule output-gap-positive ;; A positive output gap increases long-term inflation [5]
    (not (output-gap-calculated))
    (or (and (real-GDP-growth high) (not (potential-GDP-growth high)))
        (and (real-GDP-growth medium) (potential-GDP-growth low))
    )
    =>
    (assert (output-gap positive)) ;; Output gap: Real GDP - Potential GDP [5]
    (assert (explanation
        (conclusion positive-output-gap)
        (observation real-GDP-growth-greater-than-potential)
        (reason output-gap-is-real-minus-potential))
    )
    (assert (output-gap-calculated))
)
;; IF the output gap has not already been calculated ∧ the difference between the real and potential GDPs is approximately zero THEN the output gap is considered to be zero and ideal [5]
(defrule output-gap-zero
    (not (output-gap-calculated))
    (or (and (real-GDP-growth medium) (potential-GDP-growth medium))
        (and (real-GDP-growth low) (potential-GDP-growth low))
        (and (real-GDP-growth high) (potential-GDP-growth high))
    )
    =>
    (assert (output-gap zero))
    (assert (explanation
        (conclusion zero-output-gap)
        (observation real-GDP-growth-same-as-potential)
        (reason output-gap-is-real-minus-potential))
    )
    (assert (output-gap-calculated))
)
;; IF the output gap has not already been calculated ∧ the difference between the real and potential GDPs is significantly negative THEN the output gap is unideally negative [5]
(defrule output-gap-negative ;; A negative output gap lowers long-term inflation [5]
    (not (output-gap-calculated))
    (or (and (potential-GDP-growth high) (not (real-GDP-growth high)))
        (and (potential-GDP-growth medium) (real-GDP-growth low))
    )
    =>
    (assert (output-gap negative))
    (assert (explanation
        (conclusion negative-output-gap)
        (observation real-GDP-growth-less-than-potential)
        (reason output-gap-is-real-minus-potential))
    )
    (assert (output-gap-calculated))
)

;; IF expected inflation is high THEN future inflation is high [2]
(defrule high-future-inflation-from-expectations
    (declare (CF 0.7))
    (expected-inflation high)
    =>
    (assert (future-inflation high))
)
;; IF unemployment is low THEN future inflation is high [3]
(defrule high-future-inflation-from-unemployment
    (declare (CF 0.7))
    (unemployment low)
    =>
    (assert (future-inflation high))
)
;; IF the output gap is positive THEN future inflation is high [5]
(defrule high-future-inflation-from-output-gap
    (declare (CF 0.7)) ;; All three factors are important
    (output-gap positive)
    =>
    (assert (future-inflation high))
)
;; IF expected inflation is low THEN future inflation is low [2]
(defrule low-future-inflation-from-expectations
    (declare (CF 0.7))
    (expected-inflation low)
    =>
    (assert (future-inflation low))
)
;; IF unemployment is high THEN future inflation is low [3]
(defrule low-future-inflation-from-unemployment
    (declare (CF 0.7))
    (unemployment high)
    =>
    (assert (future-inflation low))
)
;; IF the output gap is negative THEN future inflation is low [5]
(defrule low-future-inflation-from-output-gap
    (declare (CF 0.7))
    (output-gap negative)
    =>
    (assert (future-inflation low))
)
;; IF expected inflation is ideal THEN future inflation is ideal [2]
(defrule ideal-future-inflation-from-expectations
    (declare (CF 0.7))
    (expected-inflation ideal)
    =>
    (assert (future-inflation ideal))
)
;; IF unemployment is ideal THEN future inflation is ideal [3]
(defrule ideal-future-inflation-from-unemployment
    (declare (CF 0.7))
    (unemployment ideal)
    =>
    (assert (future-inflation ideal))
)
;; IF the output gap is zero THEN future inflation is ideal [5]
(defrule ideal-future-inflation-from-output-gap
    (declare (CF 0.7))
    (output-gap zero)
    =>
    (assert (future-inflation ideal))
)
 
;; IF inflation is high ∧ unemployment is high ∧ real GDP growth rate is low or negative THEN the economy suffers from stagflation [6]
(defrule stagflation
    (declare (CF 0.6))
    (inflation high)
    (unemployment high)
    (real-GDP-growth low)
    =>
    (assert (economy stagflating)) ;; The combination of high inflation, high unemployment, and low (or negative) real GDP growth indicates a risk of stagflation, a highly harmful economic phenomenon that indicates a major economic crisis [6]
    (assert (explanation ;; Central banks, although generally focused on maintaining price stability, can step in as lenders of last resort in cases of major economic crises such as stagflation [7]
        (conclusion stagflation)
        (observation high-inflation high-unemployment low-real-GDP-growth)
        (reason all-three-indicators-are-bad-thus-economic-crisis))
    )
)

;; IF provided THEN adding the real GDP growth rate to the target inflation rate forms an estimate of the neutral interest rate [8]
(defrule neutral-interest-rate-definition
    (declare (salience 98))
	(indicator (name "Real GDP Growth Rate") (percentage ?percentage))
	(target ?target)
	=>
	(bind ?rate (+ ?target ?percentage))
	(assert (neutral-interest-rate ?rate)) ;; Usually, approximately the interest rate during normal times (no inflationary or deflationary pressures)
)

;; IF the estimated long-term inflation rate is too high THEN the overnight interest rate must be increased [2] [9]
(defrule increase-interest-rate-from-future-inflation
    (declare (CF 0.6))
    (future-inflation high)
    =>
    (assert (interest-rate-decision increase))
)
;; IF the short-term inflation rate is too high THEN the overnight interest rate must be increased [2] [9]
(defrule increase-interest-rate-from-inflation
    (declare (CF 0.2)) ;; Current inflation matters much less because our ability to affect it is minimal
    (inflation high)
    =>
    (assert (interest-rate-decision increase))
)
;; IF the estimated long-term inflation rate is too low THEN the overnight interest rate must be decreased [2] [9]
(defrule decrease-interest-rate-from-future-inflation
    (declare (CF 0.6))
    (future-inflation low)
    =>
    (assert (interest-rate-decision decrease))
)
;; IF the short-term inflation rate is too low THEN the overnight interest rate must be decreased [2] [9]
(defrule decrease-interest-rate-from-inflation
    (declare (CF 0.2))
    (inflation low)
    =>
    (assert (interest-rate-decision decrease))
)
;; IF the estimated long-term inflation rate is ideal THEN the overnight interest rate must be maintained [2] [9]
(defrule maintain-interest-rate-from-future-inflation
    (declare (CF 0.6))
    (future-inflation ideal)
    =>
    (assert (interest-rate-decision maintain))
)
;; IF the short-term inflation rate is ideal THEN the overnight interest rate must be maintained [2] [9]
(defrule maintain-interest-rate-from-inflation
    (declare (CF 0.2))
    (inflation ideal)
    =>
    (assert (interest-rate-decision maintain))
)

;; IF there is an inputted basis points value THEN use the inputted basis points value
(defrule basis-percentage-points-definition
    (declare (salience 98))
	(basis-points ?points)
	=>
	(bind ?percentage (/ ?points 100.0))
	(assert (basis-percentage-points ?percentage))
)
;; IF there is ¬ an inputted basis points value THEN use 25 (0.25%) as the default basis points value [11]
(defrule basis-percentage-points-definition-default
    (declare (salience 98))
    (not (basis-points ?))
	=>
	(assert (basis-percentage-points 0.25)) ;; The standard increase or decrease in interest rates; they generally change gradually, by small increments or decrements of 0.25% [11]
)

;; IF there is an inputted current interest rate THEN use the inputted current interest rate
(defrule current-interest-definition
    (declare (salience 97))
    (current-interest-rate ?current)
    =>
    (assert (current-interest ?current))
)
;; IF there is ¬ an inputted current interest rate THEN use the neutral interest rate as the default current interest rate [11]
(defrule current-interest-definition-default
    (declare (salience 97))
    (not (current-interest-rate ?))
    (neutral-interest-rate ?neutral)
    =>
    (assert (current-interest ?neutral))
)

;; IF the interest rate decision is inputted ∧ the basis percentage points is inputted ∧ the current overnight interest rate is inputted THEN the recommended overnight interest rate is the current overnight interest rate plus the defuzzified interest rate decision (rounded to a basis point amount) [10] [11]
(defrule interest-rate-recommendation
    (declare (salience -100))
    ?decision <- (interest-rate-decision ?)
    (current-interest ?current)
    (basis-percentage-points ?points)
    =>
    (bind ?increment (moment-defuzzify ?decision))
    (bind ?recommended-rate (+ ?current ?increment))
    (bind ?adjusted-recommended-rate (* (round (/ ?recommended-rate ?points)) ?points)) ;; Central banks tend to adjust the interest rate by increments or decrements of the basis percentage points [11]
    (assert (interest-rate-recommendation ?adjusted-recommended-rate))
    (printout t "The recommended overnight interest rate based on current overnight interest rate, short-term inflation, and predicted long-term inflation is " ?adjusted-recommended-rate "%." crlf)
)

;; [1] “Why we target 2% inflation.” Accessed: Feb. 08, 2026. [Online]. Available: https://www.bankofcanada.ca/2025/09/why-we-target-2-inflation/
;; [2] B. Bundick and A. L. Smith, “Did the Federal Reserve Break the Phillips Curve? Theory and Evidence of Anchoring Inflation Expectations,” RWP, Sep. 2020, doi: 10.18651/RWP2020-11.
;; [3] D. Brauer, “The Natural Rate of Unemployment”.
;; [4] T. J. Fitzgerald and J. P. Nicolini, “Is there a Stable Relationship between Unemployment and Future Inflation? Evidence from U.S. Cities,” May 30, 2014, Working Paper. doi: 10.21034/wp.713.
;; [5] “What Is the Output Gap? - Back to Basics - Finance & Development, September 2013.” Accessed: Feb. 08, 2026. [Online]. Available: https://www.imf.org/external/pubs/ft/fandd/2013/09/basics.html
;; [6] World Economic Forum, "What is 'stagflation' and what does it mean for the future economy?" World Economic Forum, Oct 6, 2022. [Online]. Available: https://www.weforum.org/stories/2021/10/what-is-stagflation-and-what-does-it-mean-for-the-future-economy/. Accessed: Mar. 30, 2026.
;; [7] Bank of Canada, "Lender of last resort," Bank of Canada. [Online]. Available: https://www.bankofcanada.ca/core-functions/financial-system/lender-of-last-resort/. Accessed: Mar. 30, 2026.
;; [8] L. J. Mester, “Monetary Policy in Word and Deed,” no. 20231020, Oct. 2023, Accessed: Feb. 08, 2026. [Online]. Available: https://www.clevelandfed.org/collections/speeches/2023/sp-20231020-monetary-policy-in-word-and-deed
;; [9] "The Transmission of Monetary Policy", Sep. 1999, Accessed: Feb 08, 2026. [Online]. Available: https://publications.gc.ca/Collection/FB12-7-1999-17E.pdf
;; [10] “How higher interest rates affect inflation.” Accessed: Feb. 08, 2026. [Online]. Available: https://www.bankofcanada.ca/2023/12/how-higher-interest-rates-affect-inflation/
;; [11] D. Harari, “Interest rates and monetary policy: Economic indicators,” Feb. 2026, Accessed: Feb. 08, 2026. [Online]. Available: https://commonslibrary.parliament.uk/research-briefings/sn02802/
