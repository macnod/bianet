export interface ITSFrame {
  id: string,
  in: Array<number>,
  out: Array<number>
}

// Format for POSTing frames
export interface ITSFrames {
  frames: ITSFrame[]
}

export interface ITrainingSet {
  input_count: number,
  output_count: number,
  total_size: number,
  selection_size: number,
  frames: ITSFrame[]
}
