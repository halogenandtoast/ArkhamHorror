import axios from 'axios';

const token = localStorage.getItem('token');

if (token !== null) {
  axios.defaults.headers.common.Authorization = `Bearer ${token}`;
}

export default axios.create({
  baseURL: 'http://localhost:3000/api/v1',
});
