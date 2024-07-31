import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import { Connection } from "./Connection.tsx";
import "@silevis/reactgrid/styles.css";
import useSWR from 'swr';

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

function ConnectionPanel() {
  const {
    data,
    error,
    isValidating
  } = useSWR('http://localhost:3001/api/connections?page-size=1000', fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="Loading">Loading...</div>;
  const connectionRows = getConnections(data.result.connections);
  const connectionColumns = getConnectionColumns(data.result.connections[0]);
  return <ReactGrid rows={connectionRows} columns={connectionColumns} />;
}

export default ConnectionPanel;
