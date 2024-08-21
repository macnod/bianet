import React from 'react';
import useSWR from 'swr';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import { makeUrl } from './utilities.tsx';
import Global from "../Global.tsx";

const fetcher = (...args) => fetch(...args).then((res) => res.json());

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

function assembleRows(selectedEdge:string, color:string, data:Object): Row[] {
  if (selectedEdge != null
    && 'status' in data && data.status === "ok"
    && 'total_size' in data.result && data.result.total_size == 1) {
      let cx = data.result.connections[0];
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
  return [];
}

interface Props {
  id: string,
  color: string,
  global: Global
}

function SelectedConnection(props:Props) {
  const url = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_connections,
    { id: props.id });
  const {
    data,
    error,
    isValidating
  } = useSWR(url, fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating)
    return <div className="loading">Loading...</div>;
  if (props.id == "" || data.status === "fail")
    return ""
  const rows = assembleRows(props.id, props.color, data);
  const columns = assembleColumns();
  return (
    <>
      <h3>Selected Connection</h3>
      <ReactGrid rows={rows} columns={columns} />
    </>
  );
}

export default SelectedConnection;
