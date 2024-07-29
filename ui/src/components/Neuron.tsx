export interface Neuron {
  name: string;
  id: number;
  layer: number;
  biased: boolean;
  input: number;
  output: number;
  err: number;
}
