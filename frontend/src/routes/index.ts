import Home from '@/views/Home.vue';
import SignIn from '@/views/SignIn.vue';
import SignUp from '@/views/SignUp.vue';
import Settings from '@/views/Settings.vue';
import About from '@/views/About.vue';
import Admin from '@/views/Admin.vue';
import PasswordReset from '@/views/PasswordReset.vue';
import UpdatePassword from '@/views/UpdatePassword.vue';

export default [
  {
    path: '/',
    name: 'Home',
    component: Home,
    meta: { requiresAuth: true, title: "Arkham Horror" },
  },
  {
    path: '/new-game',
    name: 'NewGame',
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
    path: '/about',
    name: 'About',
    component: About,
    meta: { requiresAuth: false, title: "Arkham Horror: About" },
  },
  {
    path: '/admin',
    name: 'Admin',
    component: Admin,
    meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: About" },
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
  {
    path: '/password-reset',
    name: 'PasswordReset',
    component: PasswordReset,
    meta: { guest: true, title: "ArkhamHorror: Reset Password"},
  },
  {
    path: '/password-reset/:resetId',
    name: 'UpdatePassword',
    component: UpdatePassword,
    meta: { guest: true, title: "ArkhamHorror: Set New Password"},
    props: true,
  },
];
