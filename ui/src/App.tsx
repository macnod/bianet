import React from 'react';
import { SyntheticEvent, useState } from 'react';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import CustomTabPanel from "./components/CustomTabPanel.tsx";
import GridPanel from "./components/GridPanel.tsx";
import GraphPanel from "./components/GraphPanel.tsx";
import TrainingPanel from "./components/TrainingPanel.tsx";
import './App.css';
import styled from "styled-components";

function a11yProps(index: number) {
  return {
    id: `main-tab-${index}`,
    'aria-controls': `main-tabpanel-${index}`,
  };
}

const Page = styled.div`
  width: 100vw;
  height: 100%;
  min-height: 100%;
  box-sizing: border-box;
  overflow: hidden;
  display: flex;
  justify-content: left;
  align-items: left;
  text-align: left;
  padding: 2rem;
`;

function App() {
  const [value, setValue] = useState(0);
  const handleChange = (event: SyntheticEvent, newValue: number) => {
    setValue(newValue);
  };

  return (
    <>
      <h1>Bianet</h1>
      <Page>
        <Box sx={{ width: '100%' }}>
          <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
            <Tabs value={value} onChange={handleChange} aria-label="Bianet tabs">
              <Tab label="Grid" {...a11yProps(0)}/>
              <Tab label="Graph" {...a11yProps(1)}/>
              <Tab label="Training" {...a11yProps(2)}/>
            </Tabs>
          </Box>
          <CustomTabPanel tabgroup="main" value={value} index={0}>
            <GridPanel />
          </CustomTabPanel>
          <CustomTabPanel tabgroup="main" value={value} index={1}>
            <GraphPanel />
          </CustomTabPanel>
          <CustomTabPanel tabgroup="main" value={value} index={2}>
            <TrainingPanel />
          </CustomTabPanel>
        </Box>
      </Page>
    </>
  );
}

export default App
