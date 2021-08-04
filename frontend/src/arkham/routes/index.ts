import Game from '@/arkham/views/Game.vue';
import Decks from '@/arkham/views/Decks.vue';
import JoinGame from '@/arkham/views/JoinGame.vue';
import ReplayGame from '@/arkham/views/ReplayGame.vue';
import NewCampaign from '@/arkham/views/NewCampaign.vue';

export default [
  {
    path: '/decks',
    name: 'Decks',
    component: Decks,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/campaigns/new',
    name: 'NewCampaign',
    component: NewCampaign,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/games/:gameId',
    name: 'Game',
    component: Game,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/games/:gameId/spectate',
    name: 'Spectate',
    component: Game,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/games/:gameId/join',
    name: 'JoinGame',
    component: JoinGame,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/games/:gameId/replay',
    name: 'ReplayGame',
    component: ReplayGame,
    meta: { requiresAuth: true },
    props: true,
  },
];
