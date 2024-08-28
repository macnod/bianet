import React from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import useSWR from 'swr';
import { INeuron } from "./INeuron";
import { IConnection } from "./IConnection";
import { makeUrl } from "./utilities.tsx";
import Global from '../Global.tsx';

const fetcher = (...args) => fetch(...args).then((res) => res.json());

interface MinMax {
  min: number,
  max: number
}

function weightExtremes(connections: IConnection[]):MinMax {
  let max = connections[0].weight;
  let min = connections[0].weight;
  for (var i = 1; i < connections.length; i++) {
    let cx = connections[i];
    if (max < cx.weight) max = cx.weight;
    if (min > cx.weight) min = cx.weight;
  }
  return {min: min, max: max};
}

function valueToRGBColor(value:number):string {
  function componentToHex(c:number):string {
    const hex = c.toString(16);
    return hex.length === 1 ? '0' + hex : hex;
  }

  function rgbToHex(components:Array<number>):string {
    let [r, g, b] = components;
    return `#${componentToHex(r)}${componentToHex(g)}${componentToHex(b)}`;
  }

  // If the number is positive, then color goes from white (0) to green (1.0)
  // If the number is negative, then color goes from white (0) to red (1.0)
  const red = 0;
  const green = 1;
  const blue = 2;
  let components:Array<number> = [1.0, 1.0, 1.0];
  if (value < 0) {
    components[red] = 1.0;
    components[green] = 1.0 + value;
    components[blue] = 1.0 + value;
    return rgbToHex(components.map((c) => Math.floor(c * 255)));
  }
  components[red] = 1.0 - value;
  components[green] = 1.0;
  components[blue] = 1.0 - value;
  return rgbToHex(components.map((c) => Math.floor(c * 255)));
}

function assembleElements(
  neurons: INeuron[], 
  connections: IConnection[]
) {
  let we = weightExtremes(connections);
  const normalize = (w:number) => w >= 0 ? w / we.max : -w / we.min;
  return [
    ...neurons.map(neuron => (
      {
        data: {
          id: neuron.name,
          label: neuron.name
        }
      })),
    ...connections.map(cx => (
      {
        data: {
          id: cx.id,
          source: cx.source,
          target: cx.target,
          label: cx.target
        },
        style: {
          'line-color': valueToRGBColor(normalize(cx.weight))
        }
      }))
  ];
}

interface Props {
  setSelectedNeuron: Function,
  setSelectedConnection: Function,
  global: Global,
  seed: number
}

function NetworkGraph(props:Props) {
  const pageSize = 100000;
  const neuronUrl = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_neurons,
    { "page-size": pageSize });
  const cxUrl = makeUrl(
    props.global.protocol,
    props.global.host,
    props.global.port,
    props.global.api_connections,
    { "page-size": pageSize });
  const {
    data: neuronData,
    error: neuronError,
    isValidating: neuronIsValidating 
  } = useSWR(neuronUrl, fetcher);
  const {
    data: cxData,
    error: cxError,
    isValidating: cxIsValidating
  } = useSWR(cxUrl, fetcher);
  if (neuronError || cxError)
    return <div className="failed">Failed to load</div>;
  if (neuronIsValidating || cxIsValidating)
    return <div className="loading">Loading...</div>;
  if (cxData.status === "fail" || neuronData.status === "fail") {
    return 
      <div className="errors">
        <ul>
            {cxData.errors.map((e:string) => "<li>" + e + "</li>")}
        </ul>
      </div>;
  }
  const elements = assembleElements(
    neuronData.result.neurons, 
    cxData.result.connections);
  const roots = neuronData.result.neurons
    .filter((neuron:INeuron) => neuron.layer === 0)
    .map((neuron:INeuron) => neuron.name);
  return (
    <>
      <CytoscapeComponent
        key={props.seed}
        cy={
          (cy) => {
            cy.on('select', 'node', (evt) => 
              props.setSelectedNeuron(evt.target.id()));
            cy.on('select', 'edge', (evt) => {
              props.setSelectedConnection(
                [evt.target.id(),
                  evt.target.style('line-color')]);
            });
          }
        }
        elements={elements}
        layout={{
          name: 'breadthfirst',
          directed: true,
          roots: roots
        }}
        style={{width: '60vw', height:'70vh'}}
        stylesheet={[
          {
            selector: 'node',
            style: {
              width: '2px',
              height: '50px'
            }
          },
          {
            selector: 'edge',
            style: {
              width: '1px'
            }
          }
        ]} 
      />
    </>
  );
}

export default NetworkGraph;
