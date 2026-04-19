(deftemplate economic-indicator (slot name (type STRING)) (slot percentage (type FLOAT)) (slot fuzziness (type FLOAT) (default 0.0)) (slot CF (type FLOAT) (default 1.0)))
(deftemplate inflation
    -8 12 percent
    (
        (low (-8 1) (1 0.5) (2 0))
        (ideal (0 0) (2 1) (4 0))
        (high (2 0) (3 0.5) (12 1))
    )
)
(deftemplate expected-inflation
    -8 12 percent
    (
        (low (-8 1) (1 0.5) (2 0))
        (ideal (0 0) (2 1) (4 0))
        (high (2 0) (3 0.5) (12 1))
    )
)
(deftemplate future-inflation
    -8 12 percent
    (
        (low (-8 1) (1 0.5) (2 0))
        (ideal (0 0) (2 1) (4 0))
        (high (2 0) (3 0.5) (12 1))
    )
)
(deftemplate unemployment
    2 15 percent
    (
        (low (2 1) (4 0))
        (ideal (3 0) (5 1) (7 0))
        (high (6 0) (7 0.5) (15 1))
    )
)
(deftemplate potential-GDP-growth
    -5 10 percent
    (
        (low (-5 1) (1 0))
        (medium (0 0) (3 1) (5 0))
        (high (4 0) (10 1))
    )
)
(deftemplate real-GDP-growth
    -5 10 percent
    (
        (low (-5 1) (1 0))
        (medium (0 0) (3 1) (5 0))
        (high (4 0) (10 1))
    )
)
(deftemplate output-gap
    -5 5 percent
    (
        (negative (-5 1) (0 0))
        (zero (-1 0) (0 1) (1 0))
        (positive (0 0) (5 1))
    )
)
(deftemplate interest-rate-decision
    -1 1 basis-points
    (
        (decrease (-1 1) (0 0))
        (maintain (-0.25 0) (0 1) (0.25 0))
        (increase (0 0) (1 1))
    )
)
(deffacts economic-data
    (economic-indicator (name "Inflation") (percentage 2.2) (fuzziness 0.1) (CF 0.8))
    (economic-indicator (name "Expected Inflation") (percentage 3.4) (fuzziness 0.3) (CF 0.6))
    (economic-indicator (name "Unemployment Rate") (percentage 5.5) (fuzziness 0.2) (CF 0.8))
    (economic-indicator (name "Potential GDP Growth Rate") (percentage 0.6) (fuzziness 0.3) (CF 0.6))
    (economic-indicator (name "Real GDP Growth Rate") (percentage 2.0) (fuzziness 0.1) (CF 0.8))
    (equality-margin 1.0)
    (target-inflation 2.0) ;; Usually 2 % [1]
    (ideal-unemployment-rate 5.0)
    (basis-points 25.0)
    (current-interest-rate 2.0)
)
