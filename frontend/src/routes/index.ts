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
    component: () => import('@/views/NewGame.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: New Game" },
  },
  {
    path: '/watch',
    name: 'Watch',
    component: () => import('@/views/Watch.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Watch" },
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
      {
        // What the nightly archive sweep has reclaimed, and the controls for
        // archiving or restoring a single game by hand.
        path: 'archives',
        name: 'AdminArchives',
        component: () => import('@/views/AdminArchives.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Archives" },
      },
      {
        // Play statistics, read off a server-side materialized view.
        path: 'stats',
        name: 'AdminGameStats',
        component: () => import('@/views/AdminGameStats.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Stats" },
      },
      {
        // The home-page banner. Writing one here publishes it: the public read
        // has no filter, so there is no draft state to save into.
        path: 'notifications',
        name: 'AdminNotifications',
        component: () => import('@/views/AdminNotifications.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Notifications" },
      },
      {
        // Who has been let into which feature, for rolling one out a few
        // accounts at a time rather than to everybody at once.
        path: 'users',
        name: 'AdminUsers',
        component: () => import('@/views/AdminUsers.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Users" },
      },
      {
        // The card marketplace review queue: nothing people submit is listed
        // until it is approved here.
        path: 'submissions',
        name: 'AdminSubmissions',
        component: () => import('@/views/AdminSubmissions.vue'),
        meta: { requiresAuth: true, requiresAdmin: true, title: "Arkham Horror: Submissions" },
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
