import React, { useState } from 'react';
import { makeUrl } from './utilities.tsx';
import Stack from '@mui/material/Stack';

const url = makeUrl('http', 'localhost', 3001, '/api/train', {});

async function postRequest(url:string, {arg}:{arg: string}) {
  return fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    },
    body: arg
  }).then(res => res.json());
}

interface CNInterface {
  refresh: Function
}

function CreateNetwork(props:CNInterface) {
  const [name, setName] = useState("");
  const [topology, setTopology] = useState("");
  const [threadCount, setThreadCount] = useState(1);
  const handleSubmit = (event) => {
    event.preventDefault();
    const networkInfo = {
      name: name,
      topology: topology.split("[^0-9]+").map(Number),
      thread_count: threadCount
    };
    fetch(url, {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(networkInfo)
    })
    .then((response) => response.json())
    .then((newNetworkInfo) => {
      setName("");
      setTopology("");
      setThreadCount(1);
      props.refresh();
    })
    .catch((error) => {
      console.log(error);
    });
  };

  return (
    <form onSubmit={handleSubmit}>
      <Stack>
        <label>
          Neural Network Name: 
          <input 
            name="name" 
            value={name} 
            onChange={(event) => setName(event.target.value)} 
          />
        </label>
        <label>
          Topology: 
          <input 
            name="topology"
            value={topology}
            onChange={(event) => setTopology(event.target.value)}
          />
        </label>
        <label>
          Thread Count:
          <input 
            name="threadCount" 
            value={threadCount}
            onChange={(event) => setThreadCount(Number(event.target.value))}
          />
        </label>
        <button type="submit">Create</button>
      </Stack>
    </form>
  );
}

export default CreateNetwork;
