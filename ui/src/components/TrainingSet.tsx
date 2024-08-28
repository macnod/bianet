import React from 'react';
import { useState, ChangeEvent } from 'react';
import useSWR from 'swr';
import { ReactGrid, Column, Row, CellChange, NumberCell } from '@silevis/reactgrid';
import "@silevis/reactgrid/styles.css";
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { makeUrl } from "./utilities.tsx";
import Global from "../Global.tsx";
import FailedStatus from "./FailedStatus.tsx";
import { Result, getTrainingSet, putTrainingSet } from "./data-calls.tsx";
import { ITrainingSet, ITSFrame, ITSFrames } from './ITrainingSet';

const fetcher = (...args) => fetch(...args).then((res) => res.json());

function applyChanges(
  changes: CellChange<TextCell|NumberCell>[],
  data: ITrainingSet
): ITSFrames {
  let newFrames = {};
  changes.forEach((change) => {
    var target:string = change.columnId < data.input_count ? "in" : "out";
    if (target !== "") {
      data.frames[change.rowId][target][change.columnId] = change.newCell.value;
      newFrames[change.rowId] = data.frames[change.rowId];
    }
  });
  return {frames: Object.values(newFrames)};
}

async function postRequest(url:string, frames:ITSFrames) {
  return fetch(url, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    },
    body: JSON.stringify(frames)
  }).then(res => res.json());
}

function getColumns(inputCount:number, outputCount:number): Column[] {
  return [
    {
      columnId: 'frame-index',
      width: 75,
      resizable: true
    },
    ...Array.from(
      {length: inputCount + outputCount},
      (_value, index) => ({
        columnId: '' + index,
        width: 100,
        resizable: true
      }))
  ];
}

function getRows(frames:ITSFrame[]): Row[] {
  if (typeof frames !== 'undefined' && frames.length > 0) {
    return [
      {
        rowId: "header",
        cells: [
          {
            type: "header",
            text: "frame id"
          },
          ...Array.from(
            {length: frames[0].in.length},
            (_value, index) => ({
              type: "header",
              text: `in-${index + 1}`})),
          ...Array.from(
            {length: frames[0].out.length},
            (_value, index) => ({
              type: "header",
              text: `out-${index + 1}`}))
        ],
      },
      ...frames.map<Row>((frame, idx) => ({
        rowId: idx,
        cells: [
          {
            type: "text",
            text: frame.id.valueOf()
          },
          ...frame.in.map<NumberCell>((value:Number) => ({
            type: "number",
            value: value.valueOf()
          })),
          ...frame.out.map<NumberCell>((value:Number) => ({
            type: "number",
            value: value.valueOf()
          }))
        ]
      }))
    ];
  } else {
    return [];
  }
}

interface Props {
  global: Global,
  inputCount: number,
  outputCount: number
}

function TrainingSet(props:Props) {
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const [key, setKey] = useState(0);
  const [columns, setColumns] = useState(
    getColumns(props.inputCount, props.outputCount));
  const onPageChange = (_event:ChangeEvent<unknown>, page:number) => 
    setPage(page);
  const result:Result = getTrainingSet(page, pageSize);
  if (!result.success) return result.error;

  const handleChanges = (changes:CellChange<TextCell>[]) => {
    putTrainingSet(applyChanges(changes, result.data), result);
    console.log('result: ', result);
    setKey(Math.random());
  }
  const handleColumnResize = (ci: Id, width: number) => {
    setColumns((prevColumns) => {
      const columnIndex = prevColumns.findIndex(el => el.columnId === ci);
      const resizedColumn = prevColumns[columnIndex];
      const updatedColumn = {...resizedColumn, width };
      prevColumns[columnIndex] = updatedColumn;
      return [...prevColumns];
    });
  }

  const rows = getRows(result.data.frames);
  const pageCount = Math.ceil(result.data.total_size / pageSize);
  return (
    <>
      <Stack>
        <div className="bianet-grid">
          <ReactGrid 
            key={key}
            rows={rows} 
            columns={columns} 
            onCellsChanged={handleChanges}
            onColumnResized={handleColumnResize}
            stickyLeftColumns={1}
          />
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

export default TrainingSet;
