type wave =
  | Sine
  | Saw
  | Triangle
  | Square

type synth = {
  waveform : wave;
  frequency : float;
  amplitutde : float;
}
