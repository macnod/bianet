import React from 'react';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import Global from "../Global.tsx";
import { Result, ResultData, getConnectionData } from './data-calls.tsx';

function assembleColumns(): Column[] {
  return [
    {
      columnId: "property",
      width: 120
    },
    {
      columnId: "value",
      width: 200
    }
  ]
}

function assembleRows(selectedEdge:string, color:string, data:ResultData): Row[] {
  let cx = data.connections[0];
  return [
    {
      rowId: "header",
      cells: [
        {type: "header", text: "property"},
        {type: "header", text: "value"}
      ]
    },
    {
      rowId: 1,
      cells: [
        {type: "text", text: "source"},
        {type: "text", text: cx.source}]
    },
    {
      rowId: 2,
      cells: [
        {type: "text", text: "target"},
        {type: "text", text: cx.target}]
    },
    {
      rowId: 3,
      cells: [
        {type: "text", text: "weight"},
        {type: "text", text: '' + cx.weight}]
    },
    {
      rowId: 4,
      cells: [
        {type: "text", text: "learning rate"},
        {type: "text", text: '' + cx.learning_rate}]
    },
    {
      rowId: 5,
      cells: [
        {type: "text", text: "momentum"},
        {type: "text", text: '' + cx.momentum}]
    },
    {
      rowId: 6,
      cells: [
        {type: "text", text: "delta"},
        {type: "text", text: '' + cx.delta}]
    },
    {
      rowId: 7,
      cells: [
        {type: "text", text: "fired"},
        {type: "text", text: '' + cx.fire_count}]
    },
    {
      rowId: 8,
      cells: [
        {type: "text", text: "updated"},
        {type: "text", text: '' + cx.update_count}]
    },
    {
      rowId: 9,
      cells: [
        {type: "text", text: "rgb"},
        {type: "text", text: color}]
    }        
  ];
}

interface Props {
  id: string,
  color: string,
  global: Global
}

function SelectedConnection(props:Props) {
  const id = props.id === "" ? "-1" : props.id;
  console.log("id=" + id);
  const result:Result = getConnectionData({id: id});
  if (!result.success) return result.error;
  console.log("selected connection: " + props.id);
  console.log(result);
  if (result.data.selection_size == 0)
    return <div className="nothingHere"></div>;
  const rows = assembleRows(props.id, props.color, result.data);
  const columns = assembleColumns();
  return (
    <>
      <h3>Selected Connection</h3>
      <ReactGrid rows={rows} columns={columns} />
    </>
  );
}

export default SelectedConnection;
