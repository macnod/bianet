import React from 'react';
import { useState, ChangeEvent } from 'react';
import { ReactGrid, Column, Row, DefaultCellTypes } from '@silevis/reactgrid';
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';
import { Neuron } from "./Neuron.tsx";
import "@silevis/reactgrid/styles.css";
import useSWR from 'swr';
import { makeUrl } from "./utilities.tsx";

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

function getNeuronColumns(neuron: Neuron): Column[] {
  return Object.keys(neuron).map(name => ({
      columnId: name,
      width: 100
    }));
}

function headerRow(neuron: Neuron): Row {
  return {
    rowId: "header",
    cells: Object.keys(neuron).map(name => ({
      type: "header", 
      text: name
    }))
  };
}

function getNeurons(neurons: Neuron[]): Row[] {
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

function NeuronPanel() {
  const pageSize = 25;
  const [page, setPage] = useState(1);
  const onPageChange = (event:ChangeEvent, page:number) => setPage(page);
  const url = makeUrl(
    'http', 'localhost', 3001, '/api/neurons', {
      page: page,
      "page-size": pageSize
    });
  const {data, error, isValidating} = useSWR(url, fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="loading">Loading...</div>;
  const neuronRows = getNeurons(data.result.neurons);
  const neuronColumns = getNeuronColumns(data.result.neurons[0]);
  const pageCount = Math.ceil(data.result.total_size / pageSize);
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
