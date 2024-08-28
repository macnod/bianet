import { ReactElement } from "react";
import { NetworkInfo } from "./INetworkInfo";
import { INeuron } from './INeuron';
import { IConnection } from './IConnection';
import FailedStatus from "./FailedStatus.tsx";
import global from '../global.json';
import { makeUrl } from './utilities.tsx';
import useSWR from 'swr';
import { ITrainingSet } from './ITrainingSet';
import { ITrainingError } from './ITrainingError';

type ResultData = (
  NetworkInfo 
  | ITrainingSet 
  | INeuron[] 
  | IConnection[] 
  | ITrainingError[]
);
type PutData = ITSFrames;

interface Result {
  success: boolean,
  data?: ResultData,
  error?: ReactElement<FC>
}

function getFetcher(url:string): () => Result {
  return () => fetch(url).then((res) => res.json());
}

async function web_put(url:string, data:PutData): Promise<Result> {
  const response:Promise<Result> = await fetch(url, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    },
    body: JSON.stringify(data)
  });
  const result:Promise<Result> = await response.json();
  return result;
}

function web_delete(url:string): ResultData {
  fetch(url, {
    method: 'DELETE',
    headers: {'Accept': 'application/json'}
  }).then(res => res.json())
}

function errorResult(): Result {
  return {
    success: false,
    error: <div className="failed">Failed to load</div>
  };
}

function isValidatingResult(): Result {
  return {
    success: false,
    error: <div className="loading">Loading...</div>
  };
}

function failedResult(errors:Array<string>): Result {
  return {
    success: false,
    error: <FailedStatus errors={errors} />
  };
}

function assembleResult(data, isValidating, error): Result {
  if (error || data == null || typeof data === 'undefined') 
    return errorResult();
  if (isValidating) return isValidatingResult();
  if (data.status === "fail") return failedResult(data.errors);
  return {
    success: true,
    data: data.result
  }
}

function makeBianetUrl(endpoint:string, query:{[key:string]:string|number}) {
  return makeUrl(
    global.protocol,
    global.host,
    global.port,
    endpoint,
    query)
}

function getNetwork(): Result {
  const url:string = makeBianetUrl(global.api_net);
  const {data, error, isValidating} = useSWR(url, getFetcher(url));
  return assembleResult(data, error, isValidating);
}

function getTrainingSet(page:number, pageSize:number): Result {
  const url = makeBianetUrl(
    global.api_training_set,
    {
      page: page,
      "page-size": pageSize
    }
  );
  const {data, error, isValidating} = useSWR(url, getFetcher(url));
  return assembleResult(data, error, isValidating);
}

function putTrainingSet(frames:ITSFrames, result:Result) {
  const url:string = makeBianetUrl(global.api_training_set);
  const data:ResultData = web_put(url, frames).then(data => {
    assembleResult(data.result, null, false)});
}

function deleteTraining() {
  const url:string = makeBianetUrl(global.api_train);
  const data:ResultData = web_delete(url);
  return assembleResult(data, null, false);
}

function getNeuronData(query:{[key:string]:string|number|boolean}): Result {
  const url:string = makeBianetUrl(global.api_neurons, query);
  const {data, error, isValidating} = useSWR(url, getFetcher(url));
  return assembleResult(data, error, isValidating);
}

function getConnectionData(query:{[key:string]:string|number|boolean}): Result {
  const url:string = makeBianetUrl(global.api_connections, query);
  console.log("url: " + url);
  const {data, error, isValidating} = useSWR(url, getFetcher(url));
  console.log("data", data);
  return assembleResult(data, error, isValidating);
}

function getTrainingError(
  page:number, 
  pageSize:number, 
  refreshInterval:number
): Result {
  const url:string = makeBianetUrl(
    global.api_train,
    {
      page: page,
      "page-size": pageSize
    }
  );
  const {data, error, isValidating} = useSWR(
    url, 
    getFetcher(url),
    {
      refreshInterval: (x) => 
        x && 'result' in x && x.result.training ? 1000 : 0
    }
  );
  return assembleResult(data, error, isValidating);
}

export { 
  Result, 
  ResultData,
  getConnectionData,
  getNetwork,
  getNeuronData,
  getTrainingError,
  getTrainingSet, 
  putTrainingSet
};
