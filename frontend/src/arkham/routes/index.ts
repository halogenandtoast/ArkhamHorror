import Game from '@/arkham/views/Game.vue';
import EditGame from '@/arkham/views/EditGame.vue';

export default [
  {
    path: '/games/:gameId',
    name: 'Game',
    component: Game,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/games/:gameId/edit',
    name: 'EditGame',
    component: EditGame,
    meta: { requiresAuth: true },
    props: true,
  },
];
