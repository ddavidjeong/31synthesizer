open Mm

type sound_state = {
  mutable ao : Mm_ao.writer;
  mutable buffer : Audio.t;
}

let new_ao_state channels sample_rate =
  new Mm_ao.writer channels sample_rate

let new_buf channels buf_len = Audio.create channels buf_len

let init_state channels sr buf_len =
  { ao = new_ao_state channels sr; buffer = new_buf channels buf_len }

let get_ao st = st.ao
let get_buf st = st.buffer
