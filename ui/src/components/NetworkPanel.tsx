import React from 'react';
import { useState } from 'react';
import NetworkInfoPanel from "./NetworkInfoPanel"
import CustomTabPanel from "./CustomTabPanel.tsx";
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';

function a11yProps(index: number) {
  return {
    id: `network-panel-tab-${index}`,
    'aria-controls': `network-panel-${index}`,
  };
}

function NetworkPanel() {
  const [value, setValue] = useState(0);
  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    setValue(newValue);
  };

  return (
    <>
      <Box sx={{ width: '80%' }}>
        <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
          <Tabs 
            value={value} 
            onChange={handleChange} 
            aria-label="Network Panel Tabs">
            <Tab label="Info" {...a11yProps(0)} />
            <Tab label="New" {...a11yProps(1)} />
          </Tabs>
        </Box>
        <CustomTabPanel tabgroup="network" value={value} index={0}>
          <NetworkInfoPanel />
        </CustomTabPanel>
        <CustomTabPanel tabgroup="network" value={value} index={1}>
          <h3>New Network Form</h3>
        </CustomTabPanel>
      </Box>
    </>
  );
}

export default NetworkPanel;
