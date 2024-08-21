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

const fetcher = (...args) => fetch(...args).then((res) => res.json());

interface TSFrame {
  id: string,
  in: Array<Number>,
  out: Array<Number>
}

function getColumns(frame:TSFrame): Column[] {
  return [
    {
      columnId: 'frame-index',
      width: 75
    },
    ...Array.from(
      {length: frame.in.length + frame.out.length},
      (_value, index) => ({
        columnId: '' + index,
        width: 100
      }))
  ];
}

function getRows(frames:TSFrame[]): Row[] {
  console.log(frames);
  console.log(frames[0]);
  if (frames.length > 0) {
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
  global: Global
}

function TrainingSet(props:Props) {
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const url = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_training_set, 
    {
      page: page,
      "page-size": pageSize
    });
  console.log("url: ", url)
  const {data, error, isValidating} = useSWR(url, fetcher);
  
  if (error) return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>
  if (data.status === "fail") return <FailedStatus errors={data.errors} />

  const rows = data.result.selection_size == 0 
    ? [] 
    : getRows(data.result.frames);
  const columns = data.result.selection_size == 0 
    ? [] 
    : getColumns(data.result.frames[0]);
  const pageCount = Math.ceil(data.result.total_size / pageSize);
  return (
    <>
      <Stack>
        <div className="bianet-grid">
          <ReactGrid 
            rows={rows} 
            columns={columns} 
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
