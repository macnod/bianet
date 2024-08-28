import React from 'react';
import { useState, ChangeEvent } from 'react';
import { ReactGrid, Column, Row, DefaultCellTypes } from '@silevis/reactgrid';
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { INeuron } from "./INeuron";
import "@silevis/reactgrid/styles.css";
import Global from "../Global.tsx";
import { Result, getNeuronData } from './data-calls'

const fetcher = (...args) => fetch(...args).then((res) => res.json());

const neuronFieldTypes = {
  "name": {type: "text", valueField: "text"},
  "biased": {type: "checkbox", valueField: "checked"}
};

function neuronFieldType(name: string): {[key:string]:string} {
  if (name in neuronFieldTypes) {
    return neuronFieldTypes[name]
  } else {
    return {type: "number", valueField: "value"};
  }
}

function getNeuronColumns(neuron: INeuron): Column[] {
  return Object.keys(neuron).map(name => ({
      columnId: name,
      width: 100
    }));
}

function headerRow(neuron: INeuron): Row {
  return {
    rowId: "header",
    cells: Object.keys(neuron).map(name => ({
      type: "header", 
      text: name
    }))
  };
}

function getNeurons(neurons: INeuron[]): Row[] {
  return [
    headerRow(neurons[0]),
    ...neurons.map<Row>((neuron, idx) => ({
      rowId: idx,
      cells: Object.keys(neuron).map<DefaultCellTypes>((name) => {
        let field = {type: neuronFieldType(name)["type"]};
        let key:string = neuronFieldType(name)["valueField"];
        field[key] = neuron[name];
        return field;
      })
    }))
  ];
}

interface Props {
  global: Global
}

function NeuronPanel(props:Props) {
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const result:Result = getNeuronData({page: page, pageSize: pageSize});
  if (!result.success) return result.error;
  const neuronRows = getNeurons(result.data.neurons);
  const neuronColumns = getNeuronColumns(result.data.neurons[0]);
  const pageCount = Math.ceil(result.data.total_size / pageSize);
  return (
    <>
      <Stack alignItems="center">
        <ReactGrid rows={neuronRows} columns={neuronColumns} />
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

export default NeuronPanel;
