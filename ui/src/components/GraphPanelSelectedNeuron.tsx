import useSWR from 'swr';
import { ReactGrid, Column, Row } from '@silevis/reactgrid';

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

function assembleRows(selectedNeuron, data): Row[] {
  if (selectedNeuron !== ""
    && 'status' in data && data.status === "ok"
    && 'total_size' in data.result && data.result.total_size == 1) {
      let neuron = data.result.neurons[0];
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
            {type: "text", text: neuron.name}]
        },
        {
          rowId: 2,
          cells: [
            {type: "text", text: "layer"},
            {type: "text", text: '' + neuron.layer}]
        },
        {
          rowId: 3,
          cells: [
            {type: "text", text: "biased"},
            {type: "text", text: neuron.biased ? "true" : "false"}]
        },
        {
          rowId: 4,
          cells: [
            {type: "text", text: "input"},
            {type: "text", text: '' + neuron.input}]
        },
        {
          rowId: 5,
          cells: [
            {type: "text", text: "output"},
            {type: "text", text: '' + neuron.output}]
        },
        {
          rowId: 6,
          cells: [
            {type: "text", text: "error"},
            {type: "text", text: '' + neuron.err}]
        },
        {
          rowId: 7,
          cells: [
            {type: "text", text: "incoming"},
            {type: "text", text: '' + neuron.incoming}]
        },        
        {
          rowId: 8,
          cells: [
            {type: "text", text: "outgoing"},
            {type: "text", text: '' + neuron.outgoing}]
        }
      ];
  }
  return [];
}

interface GPSNProps {
  selectedNeuron: string;
}

function GraphPanelSelectedNeuron(props: GPSNProps) {
  const endpoint = 'http://localhost:3001/api/neurons?name=' + props.selectedNeuron;
  const {
    data,
    error,
    isValidating
  } = useSWR(endpoint, fetcher);
  if (error)
    return <div className="failed">Failed to load</div>;
  if (isValidating)
    return <div className="loading">Loading...</div>;
  if (props.selectedNeuron === "")
    return ""
  const rows = assembleRows(props.selectedNeuron, data);
  const columns = assembleColumns();
  return (
    <>
      <h3>Selected Neuron</h3>
      <ReactGrid rows={rows} columns={columns} />
    </>
  );
}

export default GraphPanelSelectedNeuron;
