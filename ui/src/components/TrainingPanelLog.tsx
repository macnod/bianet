import React from 'react';
import { useState, ChangeEvent } from 'react';
import useSWR from 'swr';
import { ReactGrid, Column, Row, DefaultCellTypes } from '@silevis/reactgrid';
import "@silevis/reactgrid/styles.css";
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { TrainingError } from "./TrainingError.tsx";
import { makeUrl, isEmpty } from "./utilities.tsx";

const fetcher = (...args) => fetch(...args).then((res) => res.json());

function getColumns(entry:TrainingError): Column[] {
  return Object.keys(entry).map(name => ({
    columnId: name,
    width: 120
  }));
}

function getRows(entries:TrainingError[]): Row[] {
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

function TrainingPanelLog() {
  const [isTraining, setIsTraining] = useState(false);
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const url = makeUrl(
    'http', 'localhost', 3001, '/api/error', {
      page: page,
      "page-size": pageSize
    });
  const {data, error, isValidating} = useSWR(url, fetcher, {
    refreshInterval: (x) => 
      (x && 'result' in x && x.result.training) ? 1000 : 0});
  
  if (data && !isEmpty(data)) {
    if (isTraining != data.result.training) {
      setIsTraining(data.result.training);
      if (!isTraining)
        console.log("Training complete");
    }
  }

  if (error) return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>

  const rows = data.result.selection_size == 0 ? [] : getRows(data.result.errors);
  const columns = data.result.selection_size == 0 ? [] : getColumns(data.result.errors[0]);
  const pageCount = Math.ceil(data.result.total_size / pageSize);
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

export default TrainingPanelLog;
