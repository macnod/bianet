import React from 'react';
import { useState } from 'react';
import GraphPanelNetworkSummary from "./GraphPanelNetworkSummary.tsx";
import GraphPanelSelectedNeuron from "./GraphPanelSelectedNeuron.tsx";
import GraphPanelSelectedConnection from "./GraphPanelSelectedConnection.tsx";
import GraphPanelGraph from "./GraphPanelGraph.tsx";
import TrainingPanelClearWeights from "./TrainingPanelClearWeights.tsx";

function NetworkInfoPanel() {
  const [selectedNeuron, setSelectedNeuron] = useState("");
  const [[selectedCxId, selectedCxColor], setSelectedCx] = useState(["", ""]);
  const [seed, setSeed] = useState(0);
  const refreshGraph = () => setSeed(seed + 1);
  return (
    <>
      <div className="panel-container">
        <div className="panel-sidebar">
          <TrainingPanelClearWeights refresh={refreshGraph}/>
          <GraphPanelNetworkSummary />
          <GraphPanelSelectedNeuron selectedNeuron={selectedNeuron} />
          <GraphPanelSelectedConnection 
            id={selectedCxId}
            color={selectedCxColor}
          />
        </div>
        <div className="panel-main">
          <GraphPanelGraph
            key={seed}
            seed={seed}
            setSelectedNeuron={setSelectedNeuron}
            setSelectedConnection={setSelectedCx} 
          />
        </div>
      </div>
    </>
  );
}

export default NetworkInfoPanel;
