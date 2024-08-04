import React from 'react';
import useSWR from 'swr';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';

const fetcher = (...args) => fetch(...args).then((res) => res.json());

interface NetworkInfo {
  status: string,
  result: {
    name: string,
    topology: Array<number>,
    thread_count: number,
    running: boolean,
    training: boolean,
    network_error: number,
    training_time: number,
    iterations: number,
    max_weight: number,
    min_weight: number
  }
}

function assembleRows(data: NetworkInfo): Row[] {
  let result = data.result;
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
        {type: "text", text: result.name}]
    },
    {
      rowId: 2,
      cells: [
        {type: "text", text: "topology"},
        {type: "text", text: result.topology.join(", ")}]
    },
    {
      rowId: 3,
      cells: [
        {type: "text", text: "thread count"},
        {type: "text", text: '' + result.thread_count}]
    },
    {
      rowId: 4,
      cells: [
        {type: "text", text: "running"},
        {type: "text", text: result.running ? 'true' : 'false'}]
    },
    {
      rowId: 5,
      cells: [
        {type: "text", text: "training"},
        {type: "text", text: result.training ? 'true' : 'false'}]
    },
    {
      rowId: 6,
      cells: [
        {type: "text", text: "network error"},
        {type: "text", text: '' + result.network_error}]
    },
    {
      rowId: 7,
      cells: [
        {type: "text", text: "training time"},
        {type: "text", text: result.training_time + ' seconds'}]
    },    
    {
      rowId: 8,
      cells: [
        {type: "text", text: "iterations"},
        {type: "text", text: '' + result.iterations}]
    },
    {
      rowId: 9,
      cells: [
        {type: "text", text: "min weight"},
        {type: "text", text: '' + result.min_weight}]
    },
    {
      rowId: 10,
      cells: [
        {type: "text", text: "max weight"},
        {type: "text", text: '' + result.max_weight}]
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

function GraphPanelNetworkSummary() {
  const {
    data,
    error,
    isValidating
  } = useSWR('http://localhost:3001/api/net', fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating)
    return <div className="Loading">Loading...</div>;
  const rows = assembleRows(data);
  const columns = assembleColumns();
  return (
    <>
      <h3>Network Overview</h3>
      <ReactGrid rows={rows} columns={columns} />
    </>
  );
}

export default GraphPanelNetworkSummary;
