import NewGame from '@/arkham/views/NewGame.vue';
import Game from '@/arkham/views/Game.vue';

export default [
  {
    path: '/games/:gameId',
    name: 'Game',
    component: Game,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/new-game',
    name: 'NewGame',
    component: NewGame,
    meta: { requiresAuth: true },
  },
];
