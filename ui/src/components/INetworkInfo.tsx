export interface NetworkInfo {
  name: string,
  topology: Array<number>,
  thread_count: number,
  running: boolean,
  training: boolean,
  network_error: number,
  training_time: number,
  iterations: number,
  max_weight: number,
  min_weight: number
};
