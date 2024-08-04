import React from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import useSWR from 'swr';
import { Neuron } from "./Neuron.tsx"
import { Connection } from "./Connection.tsx"

const fetcher = (...args) => fetch(...args).then((res) => res.json());

interface MinMax {
  min: number,
  max: number
}

function weightExtremes(connections: Connection[]):MinMax {
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

  // If the number is positive, then it goes from white (0) to green (1.0)
  // If the number is negative, then it goes from white (0) to red (1.0)
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
  neurons: Neuron[], 
  connections: Connection[]
) {
  let we = weightExtremes(connections);
  console.log(we);
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

interface GPGProps {
  setSelectedNeuron: Function,
  setSelectedConnection: Function
}

function GraphPanelGraph(props:GPGProps) {
  const {
    data: neuronData,
    error: neuronError,
    isValidating: neuronIsValidating 
  } = useSWR('http://localhost:3001/api/neurons?page-size=1000', fetcher);
  const {
    data: cxData,
    error: cxError,
    isValidating: cxIsValidating
  } = useSWR('http://localhost:3001/api/connections?page-size=1000', fetcher);
  if (neuronError || cxError)
    return <div className="failed">Failed to load</div>;
  if (neuronIsValidating || cxIsValidating)
    return <div className="Loading">Loading...</div>;
  const elements = assembleElements(
    neuronData.result.neurons, 
    cxData.result.connections);
  const roots = neuronData.result.neurons
    .filter((neuron:Neuron) => neuron.layer === 0)
    .map((neuron:Neuron) => neuron.name);
  return (
    <CytoscapeComponent
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
        roots: {roots}
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
  );
}

export default GraphPanelGraph;
