# EuterpeaFun

[Guide used to install Euterpea using Stack](https://gist.github.com/hs211216/f2b08c9470f4703660bcabbc5ec39ac1)

[Original Euterpea guide using cabal](https://www.euterpea.com/)

[Euterpea Book](https://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf)

[Note-Level/MIDI API](https://www.euterpea.com/api/euterpea-api/note-level-api/)

[Signal-Level/Audio API](https://www.euterpea.com/api/euterpea-api/signal-level-api/)

[Quick Types Reference](https://www.euterpea.com/wp-content/uploads/2016/12/Euterpea_Quick_Reference.pdf)

Maybe in the future build a stack template that configures Euterpea automatically.

## Future plans

~- Scale generator (based on steps vs. root)~ DONE
~- Chord generator (based on steps vs. root)~ DONE

- Chord Set Generator (based on key and diatonic chords in scale)
- I want "I" or "ii" in "x key" to have meaning
  - When done with this, begin researching more about "secondary functions" (ie. I/vi being relative and/or which keys I can "resolve" to either resolve or modulate

-- After all building blocks are done and seem okay, I want rules for generated progressions and melodies ... I want State ... what bar are we on, what measure, WHAT KEY?!? These values can help provide information on if we should resolve, what chords we could leverage at any given moment to modulate to another KEY, what notes are diatonic, etc.

## TODO Implementation Details

~- Move Scale Generator to own module~

- Move examples to their own modules
- Figure out best way for Haskell to handle CONSTANTS
- Convert to selective import/exports

## Documentation:

`playGenFromSet`

> - Takes a pattern set (list of interval patterns)
> - Creates a pitch space (a collection of MIDI note numbers)
> - Generates a pattern using your genFromPattern function
> - Plays the pattern using Euterpea's play function with the Vibraphone instrument

`playGenFromSetAndMode`

> - Takes a pattern set and a scale mode (from your ScaleMode type)
> - Creates more complex music with multiple voices (melody and bassline in two octaves)
> - Uses the specified scale mode to determine the pitch space
> - Uses different patterns and instruments to create a richer texture

`playSynthPattern`

> - Connects your pattern generator with your synthesizer
> - Generates a pattern based on a mode and pattern set
> - Converts the MIDI notes to frequencies
> - Plays each note in sequence using the synthesizer
> - Shows how the two systems (pattern generator and synthesizer) can work together
