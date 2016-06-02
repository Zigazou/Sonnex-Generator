Sonnex-Generator
================

The Sonnex algorithm is an alternative to Soundex for french language.

Sonnex-Generator compiles a rules file into a function in a programming
language (only PHP for the moment).

Sonnex
------

Sonnex’s Characteristic:

- each letter produces a sound, there is no silent letter,
- each letter always produces the same sound,
- complex sounds are simplified.

The string must contain only one word.
The Sonnex code contains the following characters:

- 1 ← un, ein, in, ain
- 2 ← en, an
- 3 ← on
- a ← a, à, â
- b ← b, bb
- C ← ch
- d ← d, dd
- e ← e, eu
- E ← ê, é, è, ai, ei
- f ← f, ff, ph
- g ← gu
- i ← î, i, ille
- j ← j, ge
- k ← k, c, qu, ck
- l ← l, ll
- m ← m, mm
- n ← n, nn
- o ← o, ô
- p ← p, pp
- r ← r, rr
- s ← s, ss
- t ← t, tt
- u ← u, ù, û
- v ← v, w
- z ← z, s
- U ← ou

Examples
--------

Here are a few examples of sonnex results:

- balade | ballade → balad
- basilic | basilique → bazilik
- boulot | bouleau → bUlo
- cane | canne → kan
- censé | sensé → s2sE
- compte | comte | conte → k3t
- cygne | signe → sin
- date | datte → dat
- dessin | dessein → dEs1
- différend | différent → difEr2
- cric | crique → krik
- champ | chant → C2

Test
----

`make test` will generate a PHP version of Sonnex and run it on a list of
french words. The result file will be `test/out.txt`.