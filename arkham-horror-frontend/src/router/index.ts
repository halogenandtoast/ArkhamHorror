import Vue from 'vue';
import VueRouter from 'vue-router';
import Home from '@/views/Home.vue';
import SignIn from '@/views/SignIn.vue';
import SignUp from '@/views/SignUp.vue';
import NewGame from '@/views/NewGame.vue';
import NewCampaign from '@/views/NewCampaign.vue';
import Campaign from '@/views/Campaign.vue';

Vue.use(VueRouter);

const routes = [
  {
    path: '/',
    name: 'Home',
    component: Home,
  },
  {
    path: '/sign-in',
    name: 'SignIn',
    component: SignIn,
    meta: { guest: true },
  },
  {
    path: '/sign-up',
    name: 'SignUp',
    component: SignUp,
    meta: { guest: true },
  },
  {
    path: '/new-campaign',
    name: 'NewCampaign',
    component: NewCampaign,
    meta: { requiresAuth: true },
  },
  {
    path: '/campaign/:id',
    name: 'Campaign',
    component: Campaign,
    meta: { requiresAuth: true },
  },
  {
    path: '/new-game',
    name: 'NewGame',
    component: NewGame,
    meta: { requiresAuth: true },
  },
  {
    path: '/about',
    name: 'About',
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component: () => import(/* webpackChunkName: "about" */ '../views/About.vue'),
  },
];

const router = new VueRouter({
  mode: 'history',
  base: process.env.BASE_URL,
  routes,
});

router.beforeEach((to, from, next) => {
  if (to.matched.some((record) => record.meta && record.meta.requiresAuth)) {
    if (localStorage.getItem('token') === null) {
      next({ path: '/sign-in', query: { nextUrl: to.fullPath } });
    } else {
      next();
    }
  } else if (to.matched.some((record) => record.meta && record.meta.guest)) {
    if (localStorage.getItem('token') === null) {
      next();
    } else {
      next({ path: '/' });
    }
  } else {
    next();
  }
});

export default router;
