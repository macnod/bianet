interface Global {
  api_neurons: string,
  api_connections: string,
  api_net: string,
  api_train: string,
  api_training_set: string
}

const global = {
  api_neurons: "/api/neurons",
  api_connections: "/api/connections",
  api_net: "/api/net",
  api_train: "/api/train",
  api_training_set: "/api/training-set"
}

export {global};
