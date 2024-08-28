export interface INeuron {
  name: string;
  id: number;
  layer: number;
  biased: boolean;
  input: number;
  output: number;
  err: number;
  incoming: number;
  outgoing: number;
}
