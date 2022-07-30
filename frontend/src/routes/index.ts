import Home from '@/views/Home.vue';
import SignIn from '@/views/SignIn.vue';
import SignUp from '@/views/SignUp.vue';
import Settings from '@/views/Settings.vue';

export default [
  {
    path: '/',
    name: 'Home',
    component: Home,
    meta: { requiresAuth: true, title: "Arkham Horror" },
  },
  {
    path: '/settings',
    name: 'Settings',
    component: Settings,
    meta: { requiresAuth: true, title: "Arkham Horror: Settings" },
  },
  {
    path: '/sign-in',
    name: 'SignIn',
    component: SignIn,
    meta: { guest: true, title: "ArkhamHorror: Sign in" },
  },
  {
    path: '/sign-up',
    name: 'SignUp',
    component: SignUp,
    meta: { guest: true, title: "ArkhamHorror: Sign up"},
  },
];
