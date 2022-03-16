# 31synthesizer

<h2>Resources</h2>

Graphics module https://ocaml.org/releases/4.07/htmlman/libref/Graphics.html

OCamlSDL module http://ocamlsdl.sourceforge.net/doc/html/index.html

Ocaml synth tutorial: https://smimram.github.io/monadic-synth/#our-first-sines

Msynth documentation: http://smimram.github.io/monadic-synth/odoc/msynth/Msynth/index.html

Stream module documentation: https://ocaml.org/api/Stream.html

Stream tutorial: https://ocaml.org/learn/tutorials/streams.html

Ocaml mm (multimedia) library documentation: https://v3.ocaml.org/p/mm/0.4.1/doc/Synth/Multitrack/class-create/index.html

Ocaml mm repo: https://github.com/savonet/ocaml-mm

Ocaml objects: https://ocaml.org/manual/objectexamples.html



<h2>Key Features</h2>
<ul>
<li>Oscillators</li>
<li>Filters (lowpass, bandpass, etc.)</li>
<li>Envelopes</li>
<li>EQ</li>
<li>Live Playback/Recording</li>
</ul>

<h2>Project Roadmap</h2>
<h3>MS1 (Alpha)</h3>

By MS1, we hope to fully understand the modules (Stream) and libraries (Msynth) that can be used to create a basic synthesizer, eventually producing an audible output. This also means understanding waves and oscillations in the context of sound. Ideally, at the end of MS1, the client will be able to create sound output of varying frequencies.
Satisfactory scope will consist of making sure we understand the basics of synthesizers, then implementing functions that produce a frequency output. We will also implement test cases to ensure there is a frequency output. 
Good scope adds the functionality of audible output, such that the functions we implement will produce audible sound. 
For excellent scope, we would implement a basic oscillator which is capable of producing outputs of different frequencies and different waveforms. 

<h3>MS2 (Beta)</h3>

With a basic oscillator having been implemented, we can begin to add additional features and functions. By an expected scope, we would want to implement the basic functionality of filters, including low-pass/high-pass filtering and bandpasses as well. 
Additional features we can also aim to implement is an equalizer, envelopes, or possibly additional sound instruments. 
At this point, we should begin to have a good understanding of the final features to be implemented by MS3 and a good idea of how those features will be implemented. 

<h3>MS3 (Release)</h3>

In the final sprint, we will implement a basic sequencer for live playback and recording. Given a satisfactory implementation of the sequencer, we also intend to create a basic graphical interface through which the client will be able to interact with it.
Any additional features that were not implemented in MS2 should be aimed to be completed by MS3. If all satisfactory features are implemented, adding extra instruments would be the final goal and being able to sync with a MIDI player to play various notes will be the excellent scope for this project. 
