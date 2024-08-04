import React from 'react';
import Box from '@mui/material/Box';

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
  tabgroup: string;
}

function CustomTabPanel(props: TabPanelProps) {
  const {children, value, index, tabgroup, ...other} = props;
  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`${tabgroup}-tab-${index}`}
      aria-labelledby={`simple-tab-${index}`}
    >
      {value === index && <Box sx={{ p: 3 }}>{children}</Box>}
    </div>
  );
}

export default CustomTabPanel;
