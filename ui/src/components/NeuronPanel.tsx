import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import { Neuron } from "./Neuron.tsx";
import "@silevis/reactgrid/styles.css";
import useSWR from 'swr';

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
    ...neurons.map<Row>((neuron, idx) => {
    return {
      rowId: idx,
      cells: Object.keys(neuron).map((name) => {
        let field = {type: neuronFieldType(name)["type"]};
        let key = neuronFieldType(name)["valueField"];
        field[key] = neuron[name];
        return field;
      })
    };
    })
  ];
}

function NeuronPanel() {
  const {
    data,
    error,
    isValidating
  } = useSWR('http://localhost:3001/api/neurons?page-size=1000', fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating) return <div className="Loading">Loading...</div>;
  const neuronRows = getNeurons(data.result.neurons);
  const neuronColumns = getNeuronColumns(data.result.neurons[0]);
  return <ReactGrid rows={neuronRows} columns={neuronColumns} />;
}

export default NeuronPanel;
