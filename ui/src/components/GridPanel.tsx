import { React, useState } from 'react';
import CustomTabPanel from "./CustomTabPanel.tsx";
import NeuronPanel from "./NeuronPanel.tsx";
import ConnectionPanel from "./ConnectionPanel.tsx";
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import Global from "../Global.tsx";

function a11yProps(index: number) {
  return {
    id: `main-tab-${index}`,
    'aria-controls': `main-tabpanel-${index}`,
  };
}

interface Props {
  global: Global
}

function GridPanel(props:Props) {
  const [value, setValue] = useState(0);
  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    setValue(newValue);
  };
 
  return (
    <>
      <Box sx={{ width: '80%' }}>
        <Box sx= {{ borderBottom: 1, borderColor: 'divider' }}>
          <Tabs value={value} onChange={handleChange} aria-label="Grid tabs">
            <Tab label="Neurons" {...a11yProps(0)} />
            <Tab label="Connections" {...a11yProps(1)} />
          </Tabs>
        </Box>
        <CustomTabPanel tabgroup="grid" value={value} index={0}>
          <NeuronPanel global={props.global}/>
        </CustomTabPanel>
        <CustomTabPanel tabgroup="grid" value={value} index={1}>
          <ConnectionPanel global={props.global}/>
        </CustomTabPanel>
      </Box>
    </>
  );
}

export default GridPanel;
