# EuterpeaFun

[Guide used to install Euterpea using Stack](https://gist.github.com/hs211216/f2b08c9470f4703660bcabbc5ec39ac1)

[Original Euterpea guide using cabal](https://www.euterpea.com/)

[Euterpea Book](https://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf)

Maybe in the future build a stack template that configures Euterpea automatically.

## Future plans

- Scale generator (based on steps vs. root)
- Chord generator (based on steps vs. root)
- I want "I" or "ii" in C Major to have meaning

-- AFTER those two are done ... ABSTRACT THEM!! 😂
I want these generators to create a list of values (without :+: or :=:). This way we can then fold them into a musical idea. This way we can create note-sets for specific areas of a measure/bar and use them for both creating chords and melodies.

-- AND AFTER THAT EVEN!! ... I want State ... what bar are we on, what measure, WHAT KEY?!? These values can help provide information on if we should resolve, what chords we could leverage at any given moment to modulate to another KEY, what notes are diatonic, etc.
