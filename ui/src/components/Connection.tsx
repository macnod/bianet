export interface Connection {
  id: number;
  source: string;
  target: string;
  weight: number;
  momentum: number;
  lr: number;
  delta: number;
}
