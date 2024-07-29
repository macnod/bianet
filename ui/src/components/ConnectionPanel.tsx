import { React, useState } from 'react';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import { Connection } from "./Connection.tsx";
import "@silevis/reactgrid/styles.css";

const getConnections = (): Connection[] => [
  { id:  1, source: "0-1", target: "1-3", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  2, source: "0-1", target: "1-4", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  3, source: "0-1", target: "1-5", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  4, source: "0-1", target: "1-6", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  5, source: "0-1", target: "1-7", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  6, source: "0-1", target: "1-8", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },

  { id:  7, source: "0-2", target: "1-3", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  8, source: "0-2", target: "1-4", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id:  9, source: "0-2", target: "1-5", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 10, source: "0-2", target: "1-6", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 11, source: "0-2", target: "1-7", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 12, source: "0-2", target: "1-8", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },

  { id: 13, source: "1-3", target: "2-9", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 14, source: "1-4", target: "2-9", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 15, source: "1-5", target: "2-9", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 16, source: "1-6", target: "2-9", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 17, source: "1-7", target: "2-9", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 18, source: "1-8", target: "2-9", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },

  { id: 19, source: "1-3", target: "2-10", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 20, source: "1-4", target: "2-10", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 21, source: "1-5", target: "2-10", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 22, source: "1-6", target: "2-10", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 23, source: "1-7", target: "2-10", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 24, source: "1-8", target: "2-10", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },

  { id: 25, source: "1-3", target: "2-11", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 26, source: "1-4", target: "2-11", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 27, source: "1-5", target: "2-11", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 28, source: "1-6", target: "2-11", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 29, source: "1-7", target: "2-11", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 30, source: "1-8", target: "2-11", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },

  { id: 31, source: "1-3", target: "2-12", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 32, source: "1-4", target: "2-12", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 33, source: "1-5", target: "2-12", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 34, source: "1-6", target: "2-12", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 35, source: "1-7", target: "2-12", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 36, source: "1-8", target: "2-12", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },

  { id:  9, source: "2-9",  target: "3-13", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 10, source: "2-11", target: "3-13", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 11, source: "2-12", target: "3-13", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
  { id: 12, source: "2-13", target: "3-13", weight: 0.0, momentum: 0.0, lr: 0.0, delta: 0.0 },
];

const getConnectionColumns = (): Column[] => [
  { columnId: "id", width: 100 },
  { columnId: "source", width: 100 },
  { columnId: "target", width: 100 },
  { columnId: "weight", width: 100 },
  { columnId: "momentum", width: 100 },
  { columnId: "lr", width: 100 },
  { columnId: "delta", width: 100 },
];

const headerRow: Row = {
  rowId: "header",
  cells: [
    { type: "header", text: "id" },
    { type: "header", text: "source" },
    { type: "header", text: "target" },
    { type: "header", text: "weight" },
    { type: "header", text: "momentum" },
    { type: "header", text: "lr" },
    { type: "header", text: "delta" },
  ]
};

const getConnectionRows = (connections: Connection[]): Row[] => [
  headerRow,
  ...connections.map<Row>((connection, idx) => ({
    rowId: idx,
    cells: [
      { type: "number", value: connection.id },
      { type: "text", text: connection.source },
      { type: "text", text: connection.target },
      { type: "number", value: connection.weight },
      { type: "number", value: connection.momentum },
      { type: "number", value: connection.lr },
      { type: "number", value: connection.delta },
    ]
  }))
];

function ConnectionPanel() {
  const [connections] = useState<Connection[]>(getConnections());
  const connectionRows = getConnectionRows(connections);
  const connectionColumns = getConnectionColumns();
 
  return (
    <>
      <ReactGrid rows={connectionRows} columns={connectionColumns} />
    </>
  );
}

export default ConnectionPanel;
