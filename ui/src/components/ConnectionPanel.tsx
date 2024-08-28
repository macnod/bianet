import React from 'react';
import { useState, ChangeEvent } from 'react';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { IConnection } from "./IConnection";
import "@silevis/reactgrid/styles.css";
import Global from "../Global.tsx";
import { Result, getConnectionData } from './data-calls.tsx';

const fetcher = (...args) => fetch(...args).then((res) => res.json());

function connectionFieldType(name: string): {[key:string]:string} {
  if (name === "source" || name === "target")
    return {type: "text", valueField: "text"};
  return {type: "number", valueField: "value"};
}

function getConnectionColumns(connection: IConnection): Column[] {
  return Object.keys(connection).map(name => {
    return {
      columnId: name,
      width: 120
    }});
}

function headerRow(connection: IConnection): Row {
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
  const onPageChange = (_event:ChangeEvent, page:number) => setPage(page);
  const result:Result = getConnectionData({page: page, "page-size": pageSize});
  if (!result.success) return result.error;
  const connectionRows = getConnections(result.data.connections);
  const connectionColumns = getConnectionColumns(result.data.connections[0]);
  const pageCount = Math.ceil(result.data.total_size / pageSize);
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
