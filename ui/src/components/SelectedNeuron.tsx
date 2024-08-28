import { ReactGrid, Column, Row } from '@silevis/reactgrid';
import Global from "../Global.tsx";
import { Result, ResultData, getNeuronData } from './data-calls.tsx';

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

function assembleRows(selectedNeuron:string, data:ResultData): Row[] {
  let neuron = data.neurons[0];
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

interface Props {
  selectedNeuron: string,
  global: Global
}

function SelectedNeuron(props: Props) {
  const name = props.selectedNeuron === "" ? "none" : props.selectedNeuron;
  const result:Result = getNeuronData({name: name});
  if (!result.success) return result.error;
  if (result.data.selection_size == 0)
    return <div className="nothingHere"></div>;
  const rows = assembleRows(name, result.data);
  const columns = assembleColumns();
  return (
    <>
      <h3>Selected Neuron</h3>
      <ReactGrid rows={rows} columns={columns} />
    </>
  );
}

export default SelectedNeuron;
