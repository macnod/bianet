import React from 'react';
import { useState } from 'react';
import GraphPanelNetworkSummary from "./GraphPanelNetworkSummary.tsx";
import GraphPanelSelectedNeuron from "./GraphPanelSelectedNeuron.tsx";
import GraphPanelSelectedConnection from "./GraphPanelSelectedConnection.tsx";
import GraphPanelGraph from "./GraphPanelGraph.tsx";

function GraphPanel() {
  const [selectedNeuron, setSelectedNeuron] = useState("");
  const [[selectedCxId, selectedCxColor], setSelectedCx] = useState(["", ""]);
  return (
    <>
      <div className="graphpanel-container">
        <div className="graphpanel-selection">
          <GraphPanelNetworkSummary />
          <GraphPanelSelectedNeuron selectedNeuron={selectedNeuron} />
          <GraphPanelSelectedConnection 
            id={selectedCxId}
            color={selectedCxColor}
          />
        </div>
        <div className="graphpanel-graph">
          <GraphPanelGraph 
            setSelectedNeuron={setSelectedNeuron}
            setSelectedConnection={setSelectedCx} 
          />
        </div>
      </div>
    </>
  );
}

export default GraphPanel;
