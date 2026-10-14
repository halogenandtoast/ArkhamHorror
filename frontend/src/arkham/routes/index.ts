import { RouteLocationNormalized } from 'vue-router';

export default [
  {
    path: '/card-builder',
    name: 'CardBuilder',
    component: () => import('@/arkham/views/CardBuilder.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Custom Card Sets" },
    props: true,
  },
  {
    /* One of your own sets, open in the builder.
     *
     * Its own url rather than a remembered selection, so a set can be linked to,
     * bookmarked and reopened by a reload -- and so the back button means what
     * it looks like it means after clicking into one. */
    path: '/card-builder/sets/:setId',
    name: 'CardBuilderSet',
    component: () => import('@/arkham/views/CardBuilder.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Custom Card Sets" },
    props: true,
  },
  {
    // Nested under the card builder's path, so the marketplace reads as part of
    // the same section rather than a separate corner of the app.
    path: '/card-builder/marketplace',
    name: 'CardMarketplace',
    component: () => import('@/arkham/views/CardMarketplace.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Card Marketplace" },
    props: true,
  },
  {
    /* Your own listings. Its own page rather than a filter on the marketplace,
     * because a listing outlives the set it came from and carries history --
     * versions, decisions, reasons -- that nobody else is shown. */
    path: '/card-builder/listings',
    name: 'MyListings',
    component: () => import('@/arkham/views/MyListings.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: My Listings" },
    props: true,
  },
  {
    // One published set, in full. The listing only carries a row's worth of
    // cards, so seeing the rest is its own page rather than a taller row.
    path: '/card-builder/marketplace/:publishedId',
    name: 'CardMarketplaceSet',
    component: () => import('@/arkham/views/MarketplaceSet.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Card Marketplace" },
    props: true,
  },
  {
    /* A set somebody shared the link to. Its own page, nested with the rest of
     * the section, rather than a mode of the marketplace: this set has not been
     * published, has no version and no listing, and the only thing to do with
     * it is take it.
     *
     * Behind auth like everything else here. Opening the link is what records
     * that you have access, so there has to be a "you" -- and taking the cards
     * means having a library to put them in. The router sends a signed-out
     * visitor to sign in and brings them back to the link. */
    path: '/card-builder/shared/:token',
    name: 'SharedCardSet',
    component: () => import('@/arkham/views/SharedSet.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Shared Card Set" },
    props: true,
  },
  {
    path: '/cards',
    name: 'Cards',
    component: () => import('@/arkham/views/Cards.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Cards" },
    props: true,
  },
  {
    path: '/deck/:deckId',
    name: 'Deck',
    component: () => import('@/arkham/views/Deck.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Deck" },
    props: true,
  },
  {
    path: '/decks',
    name: 'Decks',
    component: () => import('@/arkham/views/Decks.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Decks" },
    props: true,
  },
  {
    path: '/achievements',
    name: 'Achievements',
    component: () => import('@/arkham/views/Achievements.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Achievements" },
    props: true,
  },
  {
    path: '/campaigns/new',
    name: 'NewCampaign',
    component: () => import('@/arkham/views/NewCampaign.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: New Game" },
    props: true,
  },
  {
    path: '/events/:id',
    name: 'OrganizerDashboard',
    component: () => import('@/arkham/views/OrganizerDashboard.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Organizer Dashboard" },
    props: true,
  },
  {
    path: '/games/:gameId',
    name: 'Game',
    component: () => import('@/arkham/views/Game.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror" },
    props: true,
  },
  {
    path: '/admin/games/:gameId',
    name: 'AdminGame',
    component: () => import('@/arkham/views/Game.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror" },
    props: (route: RouteLocationNormalized) => ({ ...route.params, spectate: true }),
  },
  {
    path: '/games/:gameId/spectate',
    name: 'Spectate',
    component: () => import('@/arkham/views/Game.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Spectate" },
    props: (route: RouteLocationNormalized) => ({ ...route.params, spectate: true }),
  },
  {
    path: '/games/:gameId/log',
    name: 'CampaignLog',
    component: () => import('@/arkham/views/CampaignLog.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Campaign Log" },
    props: true,
  },
  {
    path: '/games/:gameId/join',
    name: 'JoinGame',
    component: () => import('@/arkham/views/JoinGame.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Join Game" },
    props: true,
  },
  {
    path: '/games/:gameId/claim-seat',
    name: 'ClaimSeat',
    component: () => import('@/arkham/views/ClaimSeat.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Claim Seat" },
    props: true,
  },
  {
    path: '/games/:gameId/replay',
    name: 'ReplayGame',
    component: () => import('@/arkham/views/ReplayGame.vue'),
    meta: { requiresAuth: true, title: "Arkham Horror: Replay" },
    props: true,
  },
];
