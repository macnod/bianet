import React from 'react';
import useSWRMutation from 'swr/mutation';
import { makeUrl } from './utilities.tsx';
import Global from "../Global.tsx";

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

interface Props {
  refresh: Function,
  global: Global
}

function ButtonTrain(props:Props) {
  const url = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_train);
  const {trigger} = useSWRMutation(url, postRequest);
  return (
    <>
      <button onClick={async () => {
        try {
          const result = await trigger(JSON.stringify({
            target_error: 0.05,
            max_iterations: 5000,
            update_frequency: 0.1}));
          if (result.status === "ok") {
            props.refresh();
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

export default ButtonTrain;
