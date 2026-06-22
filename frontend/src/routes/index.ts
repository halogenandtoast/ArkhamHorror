export default [
  {
    path: '/',
    name: 'Home',
    component: () => import('@/views/Home.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror" },
  },
  {
    path: '/new-game',
    name: 'NewGame',
    component: () => import('@/views/Home.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror" },
  },
  {
    path: '/settings',
    name: 'Settings',
    component: () => import('@/views/Settings.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Settings" },
  },
  {
    path: '/about',
    name: 'About',
    component: () => import('@/views/About.vue'),
    meta: { requiresAuth: false, title: "Arkham Horror: About" },
  },
  {
    path: '/admin',
    component: () => import('@/arkham/components/Admin/UI.vue'),
    meta: { requiresAuth: true, requiresAdmin: true },
    children: [
      {
        path: '',
        name: 'Admin',
        component: () => import('@/views/Admin.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Admin" },
      },
      {
        path: 'rooms',
        name: 'Rooms',
        component: () => import('@/views/Rooms.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Rooms" },
      },
    ],
  },
  {
    path: '/sign-in',
    name: 'SignIn',
    component: () => import('@/views/SignIn.vue'),
    meta: { guest: true, title: "ArkhamHorror: Sign in" },
  },
  {
    path: '/sign-up',
    name: 'SignUp',
    component: () => import('@/views/SignUp.vue'),
    meta: { guest: true, title: "ArkhamHorror: Sign up"},
  },
  {
    path: '/password-reset',
    name: 'PasswordReset',
    component: () => import('@/views/PasswordReset.vue'),
    meta: { guest: true, title: "ArkhamHorror: Reset Password"},
  },
  {
    path: '/password-reset/:resetId',
    name: 'UpdatePassword',
    component: () => import('@/views/UpdatePassword.vue'),
    meta: { guest: true, title: "ArkhamHorror: Set New Password"},
    props: true,
  },
];
