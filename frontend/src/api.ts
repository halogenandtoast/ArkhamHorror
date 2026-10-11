import axios from 'axios';
import { setMaintenance, maintenanceFromResponse } from '@/maintenance';

const host = import.meta.env.VITE_API_HOST || '';
const api = axios.create({
  baseURL: `${host}/api/v1`,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Only cleared by a success AFTER a 503, so ordinary traffic does not write to
// the ref on every single response.
let maintenanceActiveSeen = false;

/* Notice a maintenance window from any call, not just the one that checks for
 * it. The server refuses everything with a 503 while the database is being
 * worked on, and without this each view would surface that as its own unhelpful
 * "could not load" error.
 *
 * The rejection is still passed along: callers that already handle a failure
 * keep handling it, and the banner is shown over the top. */
api.interceptors.response.use(
  (response) => {
    // A successful call means the window is over -- the surest signal we get,
    // since nothing pushes to a client that has no socket open.
    if (maintenanceActiveSeen) {
      maintenanceActiveSeen = false;
      setMaintenance(false);
    }
    return response;
  },
  (error) => {
    const message = maintenanceFromResponse(error?.response?.data);
    if (message !== undefined) {
      maintenanceActiveSeen = true;
      setMaintenance(true, message);
    }
    return Promise.reject(error);
  },
);

export default api;
