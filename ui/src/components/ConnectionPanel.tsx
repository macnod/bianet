import React from 'react';
import { useState, ChangeEvent } from 'react';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { Connection } from "./Connection.tsx";
import "@silevis/reactgrid/styles.css";
import useSWR from 'swr';
import { makeUrl } from "./utilities.tsx";
import Global from "../Global.tsx";
import FailedStatus from "./FailedStatus.tsx";

const fetcher = (...args) => fetch(...args).then((res) => res.json());

function connectionFieldType(name: string): {[key:string]:string} {
  if (name === "source" || name === "target")
    return {type: "text", valueField: "text"};
  return {type: "number", valueField: "value"};
}

function getConnectionColumns(connection: Connection): Column[] {
  return Object.keys(connection).map(name => {
    return {
      columnId: name,
      width: 120
    }});
}

function headerRow(connection: Connection): Row {
  return {
    rowId: 'header',
    cells: Object.keys(connection).map(name => ({
      type: "header",
      text: name
    }))
  };
}

function getConnections(connections: Neuron[]): Row[] {
  return [
    headerRow(connections[0]),
    ...connections.map<Row>((connection, idx) => {
      return {
        rowId: idx,
        cells: Object.keys(connection).map((name) => {
          let field = {type: connectionFieldType(name)["type"]};
          let key = connectionFieldType(name)["valueField"];
          field[key] = connection[name];
          return field;
        })
      };
    })
  ];
}

interface Props {
  global: Global
}

function ConnectionPanel(props:Props) {
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const url = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_connections,
    {
      page: page,
      "page-size": pageSize
    });
  const {data, error, isValidating} = useSWR(url, fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>;
  if (data.status === "fail") return <FailedStatus errors={data.errors} />
  const connectionRows = getConnections(data.result.connections);
  const connectionColumns = getConnectionColumns(data.result.connections[0]);
  const pageCount = Math.ceil(data.result.total_size / pageSize);
  return (
    <> 
      <Stack alignItems="center">
        <ReactGrid rows={connectionRows} columns={connectionColumns} />
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

export default ConnectionPanel;
