import React from 'react';
import { useState } from 'react';
import CustomTabPanel from "./CustomTabPanel.tsx";
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import TrainingPanelChart from "./TrainingPanelChart.tsx";
import TrainingPanelClearWeights from "./TrainingPanelClearWeights.tsx";
import TrainingPanelTrain from "./TrainingPanelTrain.tsx";
import TrainingPanelLog from "./TrainingPanelLog.tsx";
import TrainingPanelTrainingSet from "./TrainingPanelTrainingSet.tsx";


function a11yProps(index: number) {
  return {
    id: `train-tab-${index}`,
    'aria-controls': `train-tabpanel-${index}`,
  };
}

function TrainingPanel() {
  const [selectedTab, setSelectedTab] = useState(0);
  const [seed, setSeed] = useState(Math.random());
  const handleChange = (event: React.SyntheticEvent, newSelectedTab: number) => {
    setSelectedTab(newSelectedTab);
  };
  const refresh = () => {
    setSeed(Math.random());
  }
  return (
    <>
      <div className="panel-container">
        <div className="panel-sidebar">
          <TrainingPanelClearWeights refresh={refresh}/>
          <br/>
          <TrainingPanelTrain refresh={refresh}/>
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
              <TrainingPanelChart key={seed} />
            </CustomTabPanel>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={1}>
              <TrainingPanelLog key={seed} />
            </CustomTabPanel>
            <CustomTabPanel tabgroup="train" value={selectedTab} index={2}>
              <TrainingPanelTrainingSet />
            </CustomTabPanel>
          </Box>
        </div>
      </div>
    </>
  );
}

export default TrainingPanel;
