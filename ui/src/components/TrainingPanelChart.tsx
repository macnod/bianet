import React from 'react';
import { useState } from 'react';
import useSWR from 'swr';
import { LineChart } from '@mui/x-charts/LineChart';

const fetcher = (...args) => fetch(...args).then((res) => res.json());

interface TrainingError {
  error: number,
  time: number,
  iteration: number,
  iteration_time: number
}

interface TPCInterface {
  seed: number
}

function TrainingPanelChart(props:TPCInterface) {
  const [isTraining, setIsTraining] = useState(false);
  const [lastSeed, setLastSeed] = useState(0);
  const {
    data,
    error,
    isValidating,
    mutate
  } = useSWR(
    'http://localhost:3001/api/error',
    fetcher,
    {refreshInterval: (x) => (x && 'result' in x && x.result.training) ? 1000 : 0});

  if (isTraining != data.result.training) {
    setIsTraining(data.result.training);
    if (!isTraining)
      console.log("Training complete");
  }
  if (props.seed != lastSeed) {
    setLastSeed(props.seed);
    mutate({...data});
  }

  if (error) return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>

  return (
    <>
      <LineChart
        xAxis={[{ data: data.result.errors.map((e:TrainingError) => e.time) }]}
        series={[{ data: data.result.errors.map((e:TrainingError) => e.error) }]}
        width={800 + props.seed}
        height={600}
      />
    </>
  );
}

export default TrainingPanelChart;
