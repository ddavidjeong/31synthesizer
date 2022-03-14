type wave =
  | Saw
  | Triangle
  | Square

type synth = {
  waveform : wave;
  frequency : float;
}

let x = 0
