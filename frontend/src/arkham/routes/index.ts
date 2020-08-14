import Game from '@/arkham/views/Game.vue';
import EditGame from '@/arkham/views/EditGame.vue';
import NewCampaign from '@/arkham/views/NewCampaign.vue';

export default [
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
    path: '/games/:gameId/edit',
    name: 'EditGame',
    component: EditGame,
    meta: { requiresAuth: true },
    props: true,
  },
];
