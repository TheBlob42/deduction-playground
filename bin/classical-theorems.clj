;; AND
{:name "classical-1"
 :given [(and a false)]
 :conclusion [false]}
{:name "classical-2"
 :given [(and false b)]
 :conclusion [false]}
{:name "classical-3"
 :given [(and a true)]
 :conclusion [a]}
{:name "classical-4"
 :given [(and true b)]
 :conclusion [b]}
;; OR
{:name "classical-5"
 :given [(or a true)]
 :conclusion [true]}
{:name "classical-6"
 :given [(or true b)]
 :conclusion [true]}
{:name "classical-7"
 :given [(or a false)]
 :conclusion [a]}
{:name "classical-8"
 :given [(or false b)]
 :conclusion [b]}
;; IMPL
{:name "classical-9"
 :given [(impl false a)]
 :conclusion [true]}
{:name "classical-9-1"
 :given [(impl true true)]
 :conclusion [true]}
{:name "classical-9-2"
 :given [(impl a a)]
 :conclusion [true]}
;; NOT
{:name "classical-10"
 :given [(not (not a))]
 :conclusion [a]}
{:name "classical-11"
 :given [(not true)]
 :conclusion [false]}
{:name "classical-12"
 :given [(not false)]
 :conclusion [true]}

;; for temporal logic

{:name "classical-13"
 :given [(asap false)]
 :conclusion [false]}
