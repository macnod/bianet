import { React, useState } from 'react';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import { Neuron } from "./Neuron.tsx";
import "@silevis/reactgrid/styles.css";

const getNeurons = (): Neuron[] => [
  { name: "neuron-0-1", id:  1, layer: 0, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-0-2", id:  2, layer: 0, biased: false, input: 0.0, output: 0.0, err: 0.0 },

  { name: "neuron-1-3", id:  3, layer: 1, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-1-4", id:  4, layer: 1, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-1-5", id:  5, layer: 1, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-1-6", id:  6, layer: 1, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-1-7", id:  7, layer: 1, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-1-8", id:  8, layer: 1, biased:  true, input: 0.0, output: 0.0, err: 0.0 },

  { name: "neuron-2-9", id:  9, layer: 2, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-2-10", id: 10, layer: 2, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-2-11", id: 11, layer: 2, biased: false, input: 0.0, output: 0.0, err: 0.0 },
  { name: "neuron-2-12", id: 12, layer: 2, biased:  true, input: 0.0, output: 0.0, err: 0.0 },

  { name: "neuron-3-13", id: 13, layer: 0, biased: false, input: 0.0, output: 0.0, err: 0.0 },
];

const getNeuronColumns = (): Column[] => [
  { columnId: "name", width: 100 },
  { columnId: "id", width: 100 },
  { columnId: "layer", width: 100 },
  { columnId: "biased", width: 100 },
  { columnId: "input", width: 100 },
  { columnId: "output", width: 100 },
  { columnId: "err", width: 100 },
];

const headerRow: Row = {
  rowId: "header",
  cells: [
    { type: "header", text: "name" },
    { type: "header", text: "id" },
    { type: "header", text: "layer" },
    { type: "header", text: "biased" },
    { type: "header", text: "input" },
    { type: "header", text: "output" },
    { type: "header", text: "err" },
  ]
};

const getNeuronRows = (neurons: Neuron[]): Row[] => [
  headerRow,
  ...neurons.map<Row>((neuron, idx) => ({
    rowId: idx,
    cells: [
      { type: "text", text: neuron.name },
      { type: "number", value: neuron.id },
      { type: "number", value: neuron.layer },
      { type: "checkbox", checked: neuron.biased },
      { type: "number", value: neuron.input },
      { type: "number", value: neuron.output },
      { type: "number", value: neuron.err },
    ]
  }))
];

function NeuronPanel() {
  const [neurons] = useState<Neuron[]>(getNeurons());
  const neuronRows = getNeuronRows(neurons);
  const neuronColumns = getNeuronColumns();
 
  return (
    <>
      <ReactGrid rows={neuronRows} columns={neuronColumns} />
    </>
  );
}

export default NeuronPanel;
