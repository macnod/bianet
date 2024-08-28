import React from 'react';
import { useState } from 'react';
import CustomTabPanel from "./CustomTabPanel.tsx";
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import TrainingChart from "./TrainingChart.tsx";
import ButtonClearWeights from "./ButtonClearWeights.tsx";
import ButtonTrain from "./ButtonTrain.tsx";
import TrainingLog from "./TrainingLog.tsx";
import TrainingSet from "./TrainingSet.tsx";
import Global from "../Global.tsx";
import { Result, getNetwork } from "./data-calls.tsx";

function a11yProps(index: number) {
  return {
    id: `train-tab-${index}`,
    'aria-controls': `train-tabpanel-${index}`,
  };
}

interface Props {
  global: Global
}

function TrainingPanel(props:Props) {
  const [selectedTab, setSelectedTab] = useState(0);
  const [seed, setSeed] = useState(Math.random());
  const result:Result = getNetwork();
  if (!result.success) return result.error;
  const handleChange = (_event: React.SyntheticEvent, newSelectedTab: number) => {
    setSelectedTab(newSelectedTab);
  };
  const refresh = () => {
    setSeed(Math.random());
  }
  
  return (
    <>
      <div className="panel-container">
        <div className="panel-sidebar">
          <ButtonClearWeights global={props.global} refresh={refresh}/>
          <br/>
          <ButtonTrain global={props.global} refresh={refresh}/>
        </div>
        <div className="panel-main">
          <Box sx={{ width: '100%' }}>
            <Box sx= {{ borderBottom: 1, borderColor: 'divider' }}>
              <Tabs value={selectedTab} onChange={handleChange} aria-label="Training tabs">
                <Tab label="Training Chart" {...a11yProps(0)} />
                <Tab label="Training Log" {...a11yProps(1)} />
                <Tab label="Training Set" {...a11yProps(2)} />
              </Tabs>
            </Box>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={0}>
              <TrainingChart key={seed} global={props.global} />
            </CustomTabPanel>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={1}>
              <TrainingLog key={seed} global={props.global} />
            </CustomTabPanel>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={2}>
              <TrainingSet 
                global={props.global} 
                inputCount={result.data.input_count}
                outputCount={result.data.output_count}
              />
            </CustomTabPanel>
          </Box>
        </div>
      </div>
    </>
  );
}

export default TrainingPanel;
