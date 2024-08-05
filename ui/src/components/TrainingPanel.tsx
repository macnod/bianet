import React from 'react';
import { useState } from 'react';
import CustomTabPanel from "./CustomTabPanel.tsx";
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import TrainingPanelChart from "./TrainingPanelChart.tsx";
import TrainingPanelClearWeights from "./TrainingPanelClearWeights.tsx";
import TrainingPanelTrain from "./TrainingPanelTrain.tsx";


function a11yProps(index: number) {
  return {
    id: `train-tab-${index}`,
    'aria-controls': `train-tabpanel-${index}`,
  };
}

function TrainingPanel() {
  const [selectedTab, setSelectedTab] = useState(0);
  const [seed, setSeed] = useState(0);
  const handleChange = (event: React.SyntheticEvent, newSelectedTab: number) => {
    setSelectedTab(newSelectedTab);
  };
  const refreshChart = () => {
    setSeed(Math.random());
    setSelectedTab(0);
  }
  return (
    <>
      <div className="panel-container">
        <div className="panel-sidebar">
          <TrainingPanelClearWeights refreshChart={refreshChart}/>
          <br/>
          <TrainingPanelTrain refreshChart={refreshChart}/>
        </div>
        <div className="panel-main">
          <Box sx={{ width: '100%' }}>
            <Box sx= {{ borderBottom: 1, borderColor: 'divider' }}>
              <Tabs value={selectedTab} onChange={handleChange} aria-label="Training tabs">
                <Tab label="Training Chart" {...a11yProps(0)} />
                <Tab label="Training Set" {...a11yProps(1)} />
              </Tabs>
            </Box>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={0}>
              <TrainingPanelChart seed={seed} />
            </CustomTabPanel>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={1}>
              <h3>Training Set</h3>
            </CustomTabPanel>
          </Box>
        </div>
      </div>
    </>
  );
}

export default TrainingPanel;
