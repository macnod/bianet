function isEmpty(obj) {
  for (const prop in obj) {
    if (Object.hasOwn(obj, prop)) {
      return false;
    }
  }
  return true;
}

function makeUrl(
  protocol:string, 
  host:string, 
  port:number = 3000,
  path:string, 
  parameters:Object): string {
    let endpoint = `${protocol}://${host}:${port}${path}`;
    if (isEmpty(parameters))
      return endpoint;
    let paramString = Object.keys(parameters).map((k) => {
      const value = '' + parameters[k];
      return `${k}=${value}`;
    }).join('&');
    return endpoint + '?' + paramString;
}

export {makeUrl, isEmpty};
