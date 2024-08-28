import React from 'react';
import { useState } from 'react';
import { LineChart } from '@mui/x-charts/LineChart';
import { TrainingError } from "./TrainingError.tsx";
import Global from "../Global.tsx";
import { Result, getTrainingError } from './data-calls';

interface Props {
  global: Global
}

function TrainingChart(props:Props) {
  const [isTraining, setIsTraining] = useState(true);
  const pageSize = 500;
  const page = 1;
  const result:Result = getTrainingError(page, pageSize);
  if (!result.success) return result.error;
  if ('data' in result) {
    if (isTraining != result.data.training) {
      setIsTraining(result.data.training);
      if (!isTraining)
        console.log("Training complete");
    }
  }
  if (result.data.selection_size == 0) 
    return <div className="nothingHere"></div>;
  if ('data' in result) {
    if (isTraining != result.data.training) {
      setIsTraining(result.data.training);
      if (!isTraining)
        console.log("Training complete");
    }
  }
  return (
    <>
      <div className="bianet-error-chart">
        <LineChart
          xAxis={[{ data: result.data.training_log.map(
            (e:TrainingError) => e.elapsed_time) }]}
          series={[{ data: result.data.training_log.map(
            (e:TrainingError) => e.network_error) }]}
          height={600}
        />
      </div>
    </>
  );
}

export default TrainingChart;
