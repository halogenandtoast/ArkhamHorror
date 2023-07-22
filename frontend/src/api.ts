import axios from 'axios';

const host = import.meta.env.VITE_API_HOST || '';
const api = axios.create({
  baseURL: `${host}/api/v1`,
  headers: {
    'Content-Type': 'application/json',
  },
});

export default api;
