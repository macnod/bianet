interface GlobalInterface {
  [key:string]: string|number
}

export const global:GlobalInterface = {
  "port": 3001,
  "api_neurons": "/api/neurons",
  "api_connections": "/api/connections",
  "api_net": "/api/net",
  "api_train": "/api/train",
  "api_training_set": "/api/training-set"
}
