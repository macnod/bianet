export interface Connection {
  id: number;
  source: string;
  target: string;
  weight: number;
  learning_rate: number;
  momentum: number;
  delta: number;
  fire_count: number;
  update_count: number;
}
