import { GetterTree, ActionTree, MutationTree } from 'vuex';
import {
  RootState,
  GameState,
} from '../types';
import api from '../api';

const mutations: MutationTree<GameState> = {
  setCycles(state, cycles) {
    state.cycles = cycles;
  },
  setScenarios(state, scenarios) {
    state.scenarios = scenarios;
  },
};

const actions: ActionTree<GameState, RootState> = {
  fetchCycles({ state, commit }): Promise<void> {
    if (state.cycles.length === 0) {
      return api.get<string[]>('arkham/cycles').then((cycles) => {
        commit('setCycles', cycles.data);
      });
    }

    return Promise.resolve();
  },
  fetchScenarios({ state, commit }): Promise<void> {
    if (Object.keys(state.scenarios).length === 0) {
      return api.get<string[]>('arkham/scenarios').then((scenarios) => {
        commit('setScenarios', scenarios.data);
      });
    }

    return Promise.resolve();
  },
};

const getters: GetterTree<GameState, RootState> = {
  cycles: (state) => state.cycles,
  cycleScenarios: (state) => (cycle: string) => state.scenarios[cycle],
};

const state: GameState = {
  cycles: [],
  scenarios: {},
};

const store = {
  state,
  getters,
  actions,
  mutations,
};

export default store;
