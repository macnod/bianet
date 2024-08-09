import React from 'react';
import { useState, useRef } from 'react';
import useSWR from 'swr';
import { LineChart } from '@mui/x-charts/LineChart';
import { TrainingError } from "./TrainingError.tsx";
import { makeUrl } from "./utilities.tsx";

const fetcher = (...args) => fetch(...args).then((res) => res.json());

function TrainingPanelChart() {
  const [isTraining, setIsTraining] = useState(true);
  const url = makeUrl('http', 'localhost', 3001, '/api/error', {});
  const interval = (data) => (
    data 
      && 'result' in data 
      && data.result.training
  ) ? 1000 : 0;
  const {
    data,
    error,
    isValidating
  } = useSWR(url, fetcher, {refreshInterval: interval});

  if (data) {
    if (isTraining != data.result.training) {
      setIsTraining(data.result.training);
      if (!isTraining)
        console.log("Training complete");
    }
  }

  if (error) return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>

  return (
    <>
      <div className="bianet-error-chart">
        <LineChart
          xAxis={[{ data: data.result.errors.map((e:TrainingError) => e.time) }]}
          series={[{ data: data.result.errors.map((e:TrainingError) => e.error) }]}
          height={600}
        />
      </div>
    </>
  );
}

export default TrainingPanelChart;
