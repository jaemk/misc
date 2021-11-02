(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d12 :in all)

(defvar part1-01-input "
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
")

(test test-day12-sample-01-step-00
  (is (equal (str:concat "#<pos=<x=-1, y=0, z=2>, vel=<x=0, y=0, z=0>>"
                         #?"\n#<pos=<x=2, y=-10, z=-7>, vel=<x=0, y=0, z=0>>"
                         #?"\n#<pos=<x=4, y=-8, z=8>, vel=<x=0, y=0, z=0>>"
                         #?"\n#<pos=<x=3, y=5, z=-1>, vel=<x=0, y=0, z=0>>")
             (->
               (advent19.d12:parse part1-01-input)
               (advent19.d12:format-bodies)))))

(test test-day12-sample-01-step-01
  (is (equal (str:concat "#<pos=<x=2, y=-1, z=1>, vel=<x=3, y=-1, z=-1>>"
                         #?"\n#<pos=<x=3, y=-7, z=-4>, vel=<x=1, y=3, z=3>>"
                         #?"\n#<pos=<x=1, y=-7, z=5>, vel=<x=-3, y=1, z=-3>>"
                         #?"\n#<pos=<x=2, y=2, z=0>, vel=<x=-1, y=-3, z=1>>")
             (->
               (advent19.d12:parse part1-01-input)
               (advent19.d12:step-bodies)
               (advent19.d12:format-bodies)))))

(test test-day12-sample-01-step-02
  (is (equal (str:concat "#<pos=<x=5, y=-3, z=-1>, vel=<x=3, y=-2, z=-2>>"
                         #?"\n#<pos=<x=1, y=-2, z=2>, vel=<x=-2, y=5, z=6>>"
                         #?"\n#<pos=<x=1, y=-4, z=-1>, vel=<x=0, y=3, z=-6>>"
                         #?"\n#<pos=<x=1, y=-4, z=2>, vel=<x=-1, y=-6, z=2>>")
             (->
               (advent19.d12:parse part1-01-input)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:format-bodies)))))

(test test-day12-sample-01-step-03
  (is (equal (str:concat "#<pos=<x=5, y=-6, z=-1>, vel=<x=0, y=-3, z=0>>"
                         #?"\n#<pos=<x=0, y=0, z=6>, vel=<x=-1, y=2, z=4>>"
                         #?"\n#<pos=<x=2, y=1, z=-5>, vel=<x=1, y=5, z=-4>>"
                         #?"\n#<pos=<x=1, y=-8, z=2>, vel=<x=0, y=-4, z=0>>")
             (->
               (advent19.d12:parse part1-01-input)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:format-bodies)))))

(test test-day12-sample-01-step-10
  (is (equal (str:concat "#<pos=<x=2, y=1, z=-3>, vel=<x=-3, y=-2, z=1>>"
                          #?"\n#<pos=<x=1, y=-8, z=0>, vel=<x=-1, y=1, z=3>>"
                          #?"\n#<pos=<x=3, y=-6, z=1>, vel=<x=3, y=2, z=-3>>"
                          #?"\n#<pos=<x=2, y=0, z=4>, vel=<x=1, y=-1, z=-1>>")
             (->
               (advent19.d12:parse part1-01-input)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:step-bodies)
               (advent19.d12:format-bodies)))))

(test test-day12-part1-01
  (is (= 179
         (-> (advent19.d12:parse part1-01-input)
             (advent19.d12:part-1 10)))))

(test test-day12-part1-real
  (is (= 6423
         (-> (advent19.d12:input)
             (advent19.d12:part-1)))))

