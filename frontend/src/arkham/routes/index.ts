import Game from '@/arkham/views/Game.vue';

export default [
  {
    path: '/games/:gameId',
    name: 'Game',
    component: Game,
    meta: { requiresAuth: true },
    props: true,
  },
];
