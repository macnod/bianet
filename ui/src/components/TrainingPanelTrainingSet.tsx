import React from 'react';
import { useState, ChangeEvent } from 'react';
import useSWR from 'swr';
import { ReactGrid, Column, Row, DefaultCellTypes } from '@silevis/reactgrid';
import "@silevis/reactgrid/styles.css";
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { makeUrl } from "./utilities.tsx";

const fetcher = (...args) => fetch(...args).then((res) => res.json());

function getColumns(inputCount:number, entry:Array<number>): Column[] {
  return [
    {
      columnId: 'frame-index',
      width: 75
    },
    ...Array.from(
      {length: entry.length},
      (value, index) => ({
        columnId: '' + index,
        width: 100
      }))
  ];
}

function getRows(inputCount:number, entries:Array<Array<number>>): Row[] {
  return [
    {
      rowId: "header",
      cells: [
        {
          type: "header",
          text: "frame"
        },
        ...Array.from(
          {length: entries[0].length},
          (value, index) => {
            let io = index < inputCount ? "i" : "o";
            let n = (index < inputCount ? index : index - inputCount) + 1;
            return {
              type: "header",
              text: `${io}-${n}`}})
      ]
    },
    ...entries.map<Row>((entry, idx) => ({
      rowId: idx,
      cells: [
        {
          type: "number",
          value: idx
        },
        ...entry.map<DefaultCellTypes>(value => ({
          type: "number",
          value: value
        }))
      ]
    }))
  ];
}

function TrainingPanelTrainingSet() {
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const url = makeUrl(
    'http', 'localhost', 3001, '/api/training-set', {
      page: page,
      "page-size": pageSize
    });
  const {data, error, isValidating} = useSWR(url, fetcher);
  
  if (error) return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>

  const rows = data.result.selection_size == 0 
    ? [] 
    : getRows(data.result.input_count, data.result.frames);
  const columns = data.result.selection_size == 0 
    ? [] 
    : getColumns(data.result.input_count, data.result.frames[0]);
  const pageCount = Math.ceil(data.result.total_size / pageSize);
  return (
    <>
      <Stack>
        <div className="bianet-grid">
          <ReactGrid rows={rows} columns={columns} stickyLeftColumns={1}/>
        </div>
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

export default TrainingPanelTrainingSet;
