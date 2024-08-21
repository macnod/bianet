import React from 'react';
import { useState } from 'react';
import useSWR from 'swr';
import { LineChart } from '@mui/x-charts/LineChart';
import { TrainingError } from "./TrainingError.tsx";
import { makeUrl } from "./utilities.tsx";
import Global from "../Global.tsx";
import FailedStatus from "./FailedStatus.tsx";

const fetcher = (...args) => fetch(...args).then((res) => res.json());

interface Props {
  global: Global
}

function TrainingChart(props:Props) {
  const [isTraining, setIsTraining] = useState(true);
  const url = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_train);
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

  if (data && data.result) {
    if (isTraining != data.result.training) {
      setIsTraining(data.result.training);
      if (!isTraining)
        console.log("Training complete");
    }
  }

  if (error) return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>
  if (data.status === "fail") return <FailedStatus errors={data.errors} />

  return (
    <>
      <div className="bianet-error-chart">
        <LineChart
          xAxis={[{ data: data.result.training_log.map(
            (e:TrainingError) => e.elapsed_time) }]}
          series={[{ data: data.result.training_log.map(
            (e:TrainingError) => e.network_error) }]}
          height={600}
        />
      </div>
    </>
  );
}

export default TrainingChart;
