import React from 'react';
import useSWRMutation from 'swr/mutation';

async function postRequest(url:string) {
  return fetch(url, {
    method: 'POST',
    body: ""
  }).then(res => res.json());
}

interface TPCWInterface {
  refreshChart: Function
}

function TrainingPanelClearWeights(props:TPCWInterface) {
  const {trigger} = useSWRMutation(
    "http://localhost:3001/api/clear-weights", postRequest);
  return (
    <>
      <button
        onClick={async () => {
          try {
            const data = await trigger();
            props.refreshChart();
            if (data.status === "ok")
              console.log("Weights cleared");
            else
              console.log(data.errors);
          } catch(e) {
            console.log(e);
          }
        }}
      >
        Clear Weights
      </button>
    </>
  );
}

export default TrainingPanelClearWeights;
