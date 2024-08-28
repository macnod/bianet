import React from 'react';
import { useState, ChangeEvent } from 'react';
import { ReactGrid, Column, Row, DefaultCellTypes } from '@silevis/reactgrid';
import "@silevis/reactgrid/styles.css";
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { ITrainingError } from "./ITrainingError";
import Global from "../Global.tsx";
import { Result, getTrainingError } from './data-calls';

function getColumns(entry:ITrainingError): Column[] {
  return Object.keys(entry).map(name => ({
    columnId: name,
    width: 120
  }));
}

function getRows(entries:ITrainingError[]): Row[] {
  return [
    {
      rowId: "header",
      cells: Object.keys(entries[0]).map(name => ({
        type: "header",
        text: name}))
    },
    ...entries.map<Row>((entry, idx) => ({
      rowId: idx,
      cells: Object.keys(entry).map<DefaultCellTypes>(name => ({
        type: "number",
        value: entry[name]
      }))
    }))
  ];
}

interface Props {
  global: Global
}

function TrainingLog(props:Props) {
  const [isTraining, setIsTraining] = useState(false);
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const result:Result = getTrainingError(page, pageSize);
  if (!result.success) return result.error;
  if ('data' in result) {
    if (isTraining != result.data.training) {
      setIsTraining(result.data.training);
      if (!isTraining)
        console.log("Training complete");
    }
  }
  if (result.data.selection_size == 0) 
    return <div className="nothingHere"></div>;
  const rows = getRows(result.data.training_log);
  const columns = getColumns(result.data.training_log[0]);
  const pageCount = Math.ceil(result.data.total_size / pageSize);
  return (
    <>
      <Stack>
          <ReactGrid rows={rows} columns={columns} />
          <Pagination
            count={pageCount}
            showFirstButton={true}
            showLastButton={true}
            onChange={onPageChange}
            page={page}
            boundaryCount={3}
            siblingCount={3}
          />
      </Stack>
    </>
  );
}

export default TrainingLog;
