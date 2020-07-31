import axios from 'axios';

const host = process.env.VUE_APP_API_HOST || '';
const api = axios.create({
  baseURL: `${host}/api/v1`,
  headers: {
    common: {
      'Content-Type': 'application/json',
    },
  },
});

export default api;
