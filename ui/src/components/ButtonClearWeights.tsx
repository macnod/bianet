import React from 'react';
import useSWRMutation from 'swr/mutation';
import { makeUrl } from "./utilities.tsx";
import Global from "../Global.tsx";

async function postRequest(url:string) {
  return fetch(url, {
    method: 'DELETE',
    body: ""
  }).then(res => res.json());
}

interface Props {
  refresh: Function,
  global: Global
}

function ButtonClearWeights(props:Props) {
  const url = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_train);
  const {trigger} = useSWRMutation(
    url, postRequest);
  return (
    <>
      <button
        onClick={async () => {
          try {
            const data = await trigger();
            props.refresh();
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

export default ButtonClearWeights;
