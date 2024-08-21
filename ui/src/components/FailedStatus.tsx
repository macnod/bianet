import React from 'react';

interface Props {
  errors: Array<string>
}

function FailedStatus(props:Props) {
  return (
      <div className="errors">
        <ul>
          {props.errors.map((e:string) => <li>{e}</li>)}
        </ul>
      </div>
    ) 
}

export default FailedStatus;
