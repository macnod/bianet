import React from 'react';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import Global from "../Global.tsx";
import { NetworkInfo } from "./INetworkInfo";

function assembleRows(data: NetworkInfo): Row[] {
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
        {type: "text", text: "name"},
        {type: "text", text: data.name}]
    },
    {
      rowId: 2,
      cells: [
        {type: "text", text: "topology"},
        {type: "text", text: data.topology.join(", ")}]
    },
    {
      rowId: 3,
      cells: [
        {type: "text", text: "thread count"},
        {type: "text", text: '' + data.thread_count}]
    },
    {
      rowId: 4,
      cells: [
        {type: "text", text: "running"},
        {type: "text", text: data.running ? 'true' : 'false'}]
    },
    {
      rowId: 5,
      cells: [
        {type: "text", text: "training"},
        {type: "text", text: data.training ? 'true' : 'false'}]
    },
    {
      rowId: 6,
      cells: [
        {type: "text", text: "network error"},
        {type: "text", text: '' + data.network_error}]
    },
    {
      rowId: 7,
      cells: [
        {type: "text", text: "training time"},
        {type: "text", text: data.training_time + ' seconds'}]
    },    
    {
      rowId: 8,
      cells: [
        {type: "text", text: "iterations"},
        {type: "text", text: '' + data.iterations}]
    },
    {
      rowId: 9,
      cells: [
        {type: "text", text: "min weight"},
        {type: "text", text: '' + data.min_weight}]
    },
    {
      rowId: 10,
      cells: [
        {type: "text", text: "max weight"},
        {type: "text", text: '' + data.max_weight}]
    }
  ];
}

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

interface Props {
  global: Global,
  network: NetworkInfo
}

function NetworkSummary(props:Props) {
  const rows:Row[] = assembleRows(props.network);
  const columns:Column[] = assembleColumns();
  return (
    <>
      <h3>Network Overview</h3>
      <ReactGrid rows={rows} columns={columns} />
    </>
  );
}

export default NetworkSummary;
