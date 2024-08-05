import React from 'react';
import useSWRMutation from 'swr/mutation';

async function postRequest(url:string, {arg}:{arg: string}) {
  return fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    },
    body: arg
  }).then(res => res.json());
}

interface TPTInterface {
  refreshChart: Function
}

function TrainingPanelTrain(props:TPTInterface) {
  const {trigger} = useSWRMutation(
    "http://localhost:3001/api/train", postRequest);
  return (
    <>
      <button onClick={async () => {
        try {
          const result = await trigger(JSON.stringify({
            target_error: 0.05,
            max_iterations: 5000,
            update_frequency: 0.1}));
          if (result.status === "ok") {
            props.refreshChart();
            console.log("Started training");
          }
        } catch(e) {
          console.log(e);
        }
      }}>
        Start Training
      </button>
    </>
  );
}

export default TrainingPanelTrain;
