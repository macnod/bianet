import React from 'react';
import { useState } from 'react';
import NetworkSummary from "./NetworkSummary.tsx";
import SelectedNeuron from "./SelectedNeuron.tsx";
import SelectedConnection from "./SelectedConnection.tsx";
import NetworkGraph from "./NetworkGraph.tsx";
import ButtonClearWeights from "./ButtonClearWeights.tsx";
import Global from "../Global.tsx";
import { Result, getNetwork } from './data-calls.tsx';

interface Props {
  global: Global,
  network: NetworkInfo
}

function NetworkInfoPanel(props:Props) {
  const [selectedNeuron, setSelectedNeuron] = useState("");
  const [[selectedCxId, selectedCxColor], setSelectedCx] = useState(["", ""]);
  const [seed, setSeed] = useState(0);
  const result:Result = getNetwork();
  if (!result.success) return result.error;
  const refreshGraph = () => setSeed(seed + 1);
  return (
    <>
      <div className="panel-container">
        <div className="panel-sidebar">
          <ButtonClearWeights 
            global={props.global} 
            refresh={refreshGraph}/>
          <NetworkSummary 
            global={props.global} 
            network={result.data} />
          <SelectedNeuron 
            global={props.global} 
            selectedNeuron={selectedNeuron} />
          <SelectedConnection 
            id={selectedCxId}
            color={selectedCxColor}
            global={props.global}
          />
        </div>
        <div className="panel-main">
          <NetworkGraph
            key={seed}
            seed={seed}
            setSelectedNeuron={setSelectedNeuron}
            setSelectedConnection={setSelectedCx}
            global={props.global}
          />
        </div>
      </div>
    </>
  );
}

export default NetworkInfoPanel;
