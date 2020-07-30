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
];
