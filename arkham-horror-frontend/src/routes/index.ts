import Home from '@/views/Home.vue';
import SignIn from '@/views/SignIn.vue';
import SignUp from '@/views/SignUp.vue';

export default [
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
    path: '/about',
    name: 'About',
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component: () => import(/* webpackChunkName: "about" */ '../views/About.vue'),
  },
];
