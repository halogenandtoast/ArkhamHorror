--
-- Arkham Horror LCG schema.
--
-- GENERATED FILE -- do not edit by hand.
-- Produced by scripts/gen-setup-sql.sh, which replays migrations/deploy/*.sql
-- in sqitch.plan order into a throwaway database and dumps the result. To
-- change the schema, add a migration and run `make setup-sql`.
--
--
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;

--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';

--
-- Name: check_no_higher_step_exists(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_no_higher_step_exists() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN
        RETURN OLD;
    END IF;

    -- Check if the game still exists
    IF EXISTS (
        SELECT 1
        FROM arkham_games
        WHERE id = OLD.arkham_game_id
    ) THEN
        -- If the game exists, check if there is any step with a higher step number for the same game
        IF EXISTS (
            SELECT 1
            FROM arkham_steps
            WHERE arkham_game_id = OLD.arkham_game_id
              AND step > OLD.step
        ) THEN
            RAISE EXCEPTION 'Cannot delete step % because a higher step exists for the same game.', OLD.step;
        END IF;
    END IF;

    RETURN OLD;
END;
$$;

--
-- Name: check_step_before_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_step_before_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN
        RETURN OLD;
    END IF;

    -- Raise an exception if trying to delete a step that is equal to or less than the current step in the game
    IF OLD.step <= (
        SELECT step
        FROM arkham_games
        WHERE id = OLD.arkham_game_id
    ) THEN
        RAISE EXCEPTION 'Cannot delete step % because it is equal to or less than the current step in the corresponding game.', OLD.step;
    END IF;

    RETURN OLD;
END;
$$;

--
-- Name: enforce_step_order_per_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.enforce_step_order_per_game() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF coalesce(current_setting('arkham.archiving', true), '') = 'on' THEN
        RETURN NEW;
    END IF;

    -- Step 0 always starts a chain.
    IF NEW.step = 0 THEN
        RETURN NEW;
    END IF;

    -- Normal case: the immediately preceding step exists.
    IF EXISTS (
        SELECT 1 FROM arkham_steps
        WHERE arkham_game_id = NEW.arkham_game_id
          AND step = NEW.step - 1
    ) THEN
        RETURN NEW;
    END IF;

    -- Recovery case: the game has no recorded steps at all (its undo history
    -- was pruned or never persisted). Allow this insert to start a fresh
    -- contiguous chain from the game's current step instead of wedging the
    -- game so no action can ever be taken again.
    IF NOT EXISTS (
        SELECT 1 FROM arkham_steps
        WHERE arkham_game_id = NEW.arkham_game_id
    ) THEN
        RETURN NEW;
    END IF;

    RAISE EXCEPTION 'Cannot insert step % for game % without step %', NEW.step, NEW.arkham_game_id, NEW.step - 1;
END;
$$;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: arkham_achievements; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_achievements (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    achievement character varying NOT NULL,
    earned_at timestamp with time zone,
    arkham_game_id uuid,
    progress jsonb NOT NULL
);

--
-- Name: arkham_api_keys; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_api_keys (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    name character varying NOT NULL,
    prefix character varying NOT NULL,
    digest character varying NOT NULL,
    scopes character varying DEFAULT ''::character varying NOT NULL,
    last_used_at timestamp with time zone,
    expires_at timestamp with time zone,
    revoked_at timestamp with time zone,
    usage_window_start timestamp with time zone,
    usage_count integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_build_auth_requests; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_build_auth_requests (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    redirect_uri character varying NOT NULL,
    scopes character varying NOT NULL,
    return_to character varying NOT NULL,
    created_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_build_connections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_build_connections (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    account_id character varying NOT NULL,
    username character varying NOT NULL,
    scopes character varying DEFAULT ''::character varying NOT NULL,
    access_token character varying NOT NULL,
    access_token_expires_at timestamp with time zone NOT NULL,
    refresh_token character varying NOT NULL,
    refresh_token_expires_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_card_set_share_accesses; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_card_set_share_accesses (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    card_set_share_id uuid NOT NULL,
    user_id bigint NOT NULL,
    custom_card_set_id uuid,
    subscribed boolean DEFAULT true NOT NULL,
    taken_at timestamp with time zone,
    revoked_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_card_set_shares; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_card_set_shares (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    custom_card_set_id uuid NOT NULL,
    token character varying NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_card_set_submissions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_card_set_submissions (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    published_card_set_id uuid NOT NULL,
    version_id uuid NOT NULL,
    user_id bigint NOT NULL,
    version integer NOT NULL,
    status character varying DEFAULT 'pending'::character varying NOT NULL,
    notify boolean DEFAULT true NOT NULL,
    reason character varying,
    reviewed_by_user_id bigint,
    reviewed_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_card_set_subscriptions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_card_set_subscriptions (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    custom_card_set_id uuid NOT NULL,
    published_card_set_id uuid NOT NULL,
    version integer NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    subscribed boolean DEFAULT true NOT NULL
);

--
-- Name: arkham_custom_card_sets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_custom_card_sets (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    name character varying NOT NULL,
    source_code character varying,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    description text,
    url text,
    investigator_expansion boolean DEFAULT false NOT NULL,
    icon text,
    tokens jsonb DEFAULT '[]'::jsonb NOT NULL,
    keywords jsonb DEFAULT '[]'::jsonb NOT NULL
);

--
-- Name: arkham_custom_cards; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_custom_cards (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    card_code character varying NOT NULL,
    def jsonb NOT NULL,
    art text,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    custom_card_set_id uuid NOT NULL
);

--
-- Name: arkham_decks; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_decks (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    name text NOT NULL,
    investigator_name text NOT NULL,
    list jsonb NOT NULL,
    url text,
    "overlay" jsonb,
    last_used_at timestamp with time zone
);

--
-- Name: arkham_decks_user_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.arkham_decks_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: arkham_decks_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.arkham_decks_user_id_seq OWNED BY public.arkham_decks.user_id;

--
-- Name: arkham_epic_events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_epic_events (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    name text NOT NULL,
    organizer_user_id bigint NOT NULL,
    scenario_id text,
    campaign_id text,
    difficulty text NOT NULL,
    shared_state jsonb NOT NULL,
    total_investigators integer NOT NULL,
    step integer NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_epic_groups; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_epic_groups (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_epic_event_id uuid NOT NULL,
    ordinal integer NOT NULL,
    arkham_game_id uuid,
    name text NOT NULL,
    seat_count integer NOT NULL
);

--
-- Name: arkham_epic_members; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_epic_members (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_epic_event_id uuid NOT NULL,
    user_id bigint NOT NULL,
    role text NOT NULL,
    group_ordinal integer
);

--
-- Name: arkham_epic_steps; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_epic_steps (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_epic_event_id uuid NOT NULL,
    step integer NOT NULL,
    arkham_game_id uuid,
    game_step integer,
    delta jsonb NOT NULL,
    created_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_game_archives; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_game_archives (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_game_id uuid NOT NULL,
    state text NOT NULL,
    bucket text NOT NULL,
    object_key text NOT NULL,
    steps integer NOT NULL,
    top_step integer NOT NULL,
    bytes bigint NOT NULL,
    archived_at timestamp with time zone NOT NULL,
    game_updated_at timestamp with time zone NOT NULL,
    requested_at timestamp with time zone,
    requested_by bigint,
    restored_at timestamp with time zone,
    error_message text
);

--
-- Name: arkham_game_outcomes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_game_outcomes (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_game_id uuid NOT NULL,
    result text NOT NULL,
    certainty text DEFAULT 'Certain'::text NOT NULL,
    note text,
    source text NOT NULL,
    scenario_id text,
    resolution_tag text,
    resolution integer,
    computed_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT valid_game_outcome_certainty CHECK ((certainty = ANY (ARRAY['Certain'::text, 'Uncertain'::text]))),
    CONSTRAINT valid_game_outcome_result CHECK ((result = ANY (ARRAY['Won'::text, 'Lost'::text])))
);

--
-- Name: arkham_games; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_games (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    name text NOT NULL,
    current_data jsonb NOT NULL,
    multiplayer_variant text NOT NULL,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now(),
    step integer,
    allow_spectators boolean DEFAULT false NOT NULL
);

--
-- Name: arkham_game_stats; Type: MATERIALIZED VIEW; Schema: public; Owner: -
--

CREATE MATERIALIZED VIEW public.arkham_game_stats AS
 SELECT g.id AS game_id,
    g.created_at,
    g.updated_at,
    g.multiplayer_variant,
    ((g.current_data -> 'gameMode'::text) ? 'This'::text) AS is_campaign,
    (g.current_data #>> '{gameMode,This,id}'::text[]) AS campaign_id,
        CASE
            WHEN ((g.current_data #>> '{gameMode,That,id}'::text[]) ~~ 'c%'::text) THEN substr((g.current_data #>> '{gameMode,That,id}'::text[]), 2)
            ELSE (g.current_data #>> '{gameMode,That,id}'::text[])
        END AS scenario_id,
    COALESCE((g.current_data #>> '{gameMode,This,difficulty}'::text[]), (g.current_data #>> '{gameMode,That,difficulty}'::text[])) AS difficulty,
    (g.current_data #>> '{gameGameState,tag}'::text[]) AS state,
        CASE
            WHEN (jsonb_typeof((g.current_data -> 'gamePlayerCount'::text)) = 'number'::text) THEN ((g.current_data ->> 'gamePlayerCount'::text))::integer
            ELSE NULL::integer
        END AS player_count,
    COALESCE((g.current_data #>> '{gameOutcome,result}'::text[]), o.result) AS outcome_result,
    COALESCE((g.current_data #>> '{gameOutcome,certainty}'::text[]), o.certainty, 'Certain'::text) AS outcome_certainty,
    COALESCE((g.current_data #>> '{gameOutcome,note}'::text[]), o.note) AS outcome_note,
    COALESCE((g.current_data #>> '{gameOutcome,source}'::text[]), o.source) AS outcome_source,
        CASE
            WHEN (COALESCE((g.current_data #>> '{gameOutcome,ending,scenario}'::text[]), o.scenario_id) ~~ 'c%'::text) THEN substr(COALESCE((g.current_data #>> '{gameOutcome,ending,scenario}'::text[]), o.scenario_id), 2)
            ELSE COALESCE((g.current_data #>> '{gameOutcome,ending,scenario}'::text[]), o.scenario_id)
        END AS outcome_scenario,
    COALESCE((g.current_data #>> '{gameOutcome,ending,resolution,tag}'::text[]), o.resolution_tag) AS outcome_resolution_tag,
    COALESCE(
        CASE
            WHEN (jsonb_typeof((g.current_data #> '{gameOutcome,ending,resolution,contents}'::text[])) = 'number'::text) THEN ((g.current_data #>> '{gameOutcome,ending,resolution,contents}'::text[]))::integer
            ELSE NULL::integer
        END, o.resolution) AS outcome_resolution,
    COALESCE(r.completed, '{}'::text[]) AS completed_scenarios,
    COALESCE(r.resolved, '{}'::text[]) AS resolved_scenarios
   FROM ((public.arkham_games g
     LEFT JOIN LATERAL ( SELECT array_agg(
                CASE
                    WHEN (e.key ~~ 'c%'::text) THEN substr(e.key, 2)
                    ELSE e.key
                END ORDER BY e.key) AS completed,
            array_agg(
                CASE
                    WHEN (e.key ~~ 'c%'::text) THEN substr(e.key, 2)
                    ELSE e.key
                END ORDER BY e.key) FILTER (WHERE ((e.value ->> 'tag'::text) = 'Resolution'::text)) AS resolved
           FROM jsonb_each(
                CASE
                    WHEN (jsonb_typeof((g.current_data #> '{gameMode,This,resolutions}'::text[])) = 'object'::text) THEN (g.current_data #> '{gameMode,This,resolutions}'::text[])
                    ELSE '{}'::jsonb
                END) e(key, value)) r ON (true))
     LEFT JOIN public.arkham_game_outcomes o ON ((o.arkham_game_id = g.id)))
  WITH NO DATA;

--
-- Name: arkham_game_undo_floors; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_game_undo_floors (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_game_id uuid NOT NULL,
    floor_step bigint NOT NULL
);

--
-- Name: arkham_log_entries; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_log_entries (
    id bigint NOT NULL,
    body text NOT NULL,
    arkham_game_id uuid NOT NULL,
    step integer NOT NULL,
    created_at timestamp without time zone DEFAULT now(),
    payload jsonb,
    seq integer,
    group_id text
);

--
-- Name: arkham_log_entries_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.arkham_log_entries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: arkham_log_entries_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.arkham_log_entries_id_seq OWNED BY public.arkham_log_entries.id;

--
-- Name: arkham_players; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_players (
    arkham_game_id uuid NOT NULL,
    user_id bigint NOT NULL,
    investigator_id text NOT NULL,
    id uuid DEFAULT public.uuid_generate_v4()
);

--
-- Name: arkham_players_user_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.arkham_players_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: arkham_players_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.arkham_players_user_id_seq OWNED BY public.arkham_players.user_id;

--
-- Name: arkham_published_card_set_likes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_published_card_set_likes (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    published_card_set_id uuid NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_published_card_set_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_published_card_set_versions (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    published_card_set_id uuid NOT NULL,
    version integer NOT NULL,
    note character varying,
    name character varying NOT NULL,
    cards jsonb NOT NULL,
    created_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_published_card_sets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_published_card_sets (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    custom_card_set_id uuid,
    name character varying NOT NULL,
    latest_version integer NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    approved_version integer,
    description text,
    url text,
    investigator_expansion boolean DEFAULT false NOT NULL,
    icon text
);

--
-- Name: arkham_stats_refreshes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_stats_refreshes (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    name character varying NOT NULL,
    refreshed_at timestamp with time zone NOT NULL,
    duration_ms integer NOT NULL
);

--
-- Name: arkham_steps; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_steps (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    arkham_game_id uuid NOT NULL,
    choice jsonb NOT NULL,
    step integer NOT NULL,
    action_diff jsonb NOT NULL
);

--
-- Name: arkham_third_edition_history; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_third_edition_history (
    id bigint NOT NULL,
    table_id uuid NOT NULL,
    game jsonb NOT NULL,
    created_at timestamp with time zone NOT NULL
);

--
-- Name: arkham_third_edition_history_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.arkham_third_edition_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: arkham_third_edition_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.arkham_third_edition_history_id_seq OWNED BY public.arkham_third_edition_history.id;

--
-- Name: arkham_third_edition_tables; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_third_edition_tables (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    host_id bigint NOT NULL,
    started boolean NOT NULL,
    version integer NOT NULL,
    payload jsonb NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

--
-- Name: notifications; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.notifications (
    id integer NOT NULL,
    body text,
    created_at timestamp without time zone
);

--
-- Name: notifications_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.notifications_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: notifications_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.notifications_id_seq OWNED BY public.notifications.id;

--
-- Name: password_resets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.password_resets (
    id uuid DEFAULT public.uuid_generate_v1mc() NOT NULL,
    user_id bigint NOT NULL,
    expires_at timestamp with time zone NOT NULL
);

--
-- Name: password_resets_user_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.password_resets_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: password_resets_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.password_resets_user_id_seq OWNED BY public.password_resets.user_id;

--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id bigint NOT NULL,
    username character varying NOT NULL,
    email character varying NOT NULL,
    password_digest character varying NOT NULL,
    beta boolean DEFAULT false NOT NULL,
    phase_transition_notifications boolean DEFAULT false NOT NULL,
    features text DEFAULT ''::text NOT NULL,
    allow_spectators_by_default boolean DEFAULT false NOT NULL,
    admin boolean DEFAULT false
);

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;

--
-- Name: arkham_decks user_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_decks ALTER COLUMN user_id SET DEFAULT nextval('public.arkham_decks_user_id_seq'::regclass);

--
-- Name: arkham_log_entries id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_log_entries ALTER COLUMN id SET DEFAULT nextval('public.arkham_log_entries_id_seq'::regclass);

--
-- Name: arkham_players user_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_players ALTER COLUMN user_id SET DEFAULT nextval('public.arkham_players_user_id_seq'::regclass);

--
-- Name: arkham_third_edition_history id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_third_edition_history ALTER COLUMN id SET DEFAULT nextval('public.arkham_third_edition_history_id_seq'::regclass);

--
-- Name: notifications id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications ALTER COLUMN id SET DEFAULT nextval('public.notifications_id_seq'::regclass);

--
-- Name: password_resets user_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets ALTER COLUMN user_id SET DEFAULT nextval('public.password_resets_user_id_seq'::regclass);

--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);

--
-- Name: arkham_achievements arkham_achievements_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_achievements
    ADD CONSTRAINT arkham_achievements_pkey PRIMARY KEY (id);

--
-- Name: arkham_api_keys arkham_api_keys_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_api_keys
    ADD CONSTRAINT arkham_api_keys_pkey PRIMARY KEY (id);

--
-- Name: arkham_build_auth_requests arkham_build_auth_requests_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_build_auth_requests
    ADD CONSTRAINT arkham_build_auth_requests_pkey PRIMARY KEY (id);

--
-- Name: arkham_build_connections arkham_build_connections_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_build_connections
    ADD CONSTRAINT arkham_build_connections_pkey PRIMARY KEY (id);

--
-- Name: arkham_card_set_share_accesses arkham_card_set_share_accesses_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_share_accesses
    ADD CONSTRAINT arkham_card_set_share_accesses_pkey PRIMARY KEY (id);

--
-- Name: arkham_card_set_shares arkham_card_set_shares_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_shares
    ADD CONSTRAINT arkham_card_set_shares_pkey PRIMARY KEY (id);

--
-- Name: arkham_card_set_submissions arkham_card_set_submissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_submissions
    ADD CONSTRAINT arkham_card_set_submissions_pkey PRIMARY KEY (id);

--
-- Name: arkham_card_set_subscriptions arkham_card_set_subscriptions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_subscriptions
    ADD CONSTRAINT arkham_card_set_subscriptions_pkey PRIMARY KEY (id);

--
-- Name: arkham_custom_card_sets arkham_custom_card_sets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_card_sets
    ADD CONSTRAINT arkham_custom_card_sets_pkey PRIMARY KEY (id);

--
-- Name: arkham_custom_cards arkham_custom_cards_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_cards
    ADD CONSTRAINT arkham_custom_cards_pkey PRIMARY KEY (id);

--
-- Name: arkham_decks arkham_decks_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_decks
    ADD CONSTRAINT arkham_decks_pkey PRIMARY KEY (id);

--
-- Name: arkham_epic_events arkham_epic_events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_events
    ADD CONSTRAINT arkham_epic_events_pkey PRIMARY KEY (id);

--
-- Name: arkham_epic_groups arkham_epic_groups_arkham_epic_event_id_ordinal_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_groups
    ADD CONSTRAINT arkham_epic_groups_arkham_epic_event_id_ordinal_key UNIQUE (arkham_epic_event_id, ordinal);

--
-- Name: arkham_epic_groups arkham_epic_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_groups
    ADD CONSTRAINT arkham_epic_groups_pkey PRIMARY KEY (id);

--
-- Name: arkham_epic_members arkham_epic_members_arkham_epic_event_id_user_id_role_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_members
    ADD CONSTRAINT arkham_epic_members_arkham_epic_event_id_user_id_role_key UNIQUE (arkham_epic_event_id, user_id, role);

--
-- Name: arkham_epic_members arkham_epic_members_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_members
    ADD CONSTRAINT arkham_epic_members_pkey PRIMARY KEY (id);

--
-- Name: arkham_epic_steps arkham_epic_steps_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_steps
    ADD CONSTRAINT arkham_epic_steps_pkey PRIMARY KEY (id);

--
-- Name: arkham_game_archives arkham_game_archives_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_archives
    ADD CONSTRAINT arkham_game_archives_pkey PRIMARY KEY (id);

--
-- Name: arkham_game_outcomes arkham_game_outcomes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_outcomes
    ADD CONSTRAINT arkham_game_outcomes_pkey PRIMARY KEY (id);

--
-- Name: arkham_game_undo_floors arkham_game_undo_floors_arkham_game_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_undo_floors
    ADD CONSTRAINT arkham_game_undo_floors_arkham_game_id_key UNIQUE (arkham_game_id);

--
-- Name: arkham_game_undo_floors arkham_game_undo_floors_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_undo_floors
    ADD CONSTRAINT arkham_game_undo_floors_pkey PRIMARY KEY (id);

--
-- Name: arkham_games arkham_games_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_games
    ADD CONSTRAINT arkham_games_pkey PRIMARY KEY (id);

--
-- Name: arkham_log_entries arkham_log_entries_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_log_entries
    ADD CONSTRAINT arkham_log_entries_pkey PRIMARY KEY (id);

--
-- Name: arkham_published_card_set_likes arkham_published_card_set_likes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_likes
    ADD CONSTRAINT arkham_published_card_set_likes_pkey PRIMARY KEY (id);

--
-- Name: arkham_published_card_set_versions arkham_published_card_set_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_versions
    ADD CONSTRAINT arkham_published_card_set_versions_pkey PRIMARY KEY (id);

--
-- Name: arkham_published_card_sets arkham_published_card_sets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_sets
    ADD CONSTRAINT arkham_published_card_sets_pkey PRIMARY KEY (id);

--
-- Name: arkham_stats_refreshes arkham_stats_refreshes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_stats_refreshes
    ADD CONSTRAINT arkham_stats_refreshes_pkey PRIMARY KEY (id);

--
-- Name: arkham_steps arkham_steps_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_steps
    ADD CONSTRAINT arkham_steps_pkey PRIMARY KEY (id);

--
-- Name: arkham_third_edition_history arkham_third_edition_history_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_third_edition_history
    ADD CONSTRAINT arkham_third_edition_history_pkey PRIMARY KEY (id);

--
-- Name: arkham_third_edition_tables arkham_third_edition_tables_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_third_edition_tables
    ADD CONSTRAINT arkham_third_edition_tables_pkey PRIMARY KEY (id);

--
-- Name: notifications notifications_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_pkey PRIMARY KEY (id);

--
-- Name: password_resets password_resets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_pkey PRIMARY KEY (id);

--
-- Name: arkham_api_keys unique_api_key_digest; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_api_keys
    ADD CONSTRAINT unique_api_key_digest UNIQUE (digest);

--
-- Name: arkham_build_connections unique_arkham_build_connection_user; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_build_connections
    ADD CONSTRAINT unique_arkham_build_connection_user UNIQUE (user_id);

--
-- Name: arkham_card_set_shares unique_card_set_share; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_shares
    ADD CONSTRAINT unique_card_set_share UNIQUE (custom_card_set_id);

--
-- Name: arkham_card_set_share_accesses unique_card_set_share_access; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_share_accesses
    ADD CONSTRAINT unique_card_set_share_access UNIQUE (card_set_share_id, user_id);

--
-- Name: arkham_card_set_shares unique_card_set_share_token; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_shares
    ADD CONSTRAINT unique_card_set_share_token UNIQUE (token);

--
-- Name: arkham_card_set_submissions unique_card_set_submission_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_submissions
    ADD CONSTRAINT unique_card_set_submission_version UNIQUE (version_id);

--
-- Name: arkham_card_set_subscriptions unique_card_set_subscription; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_subscriptions
    ADD CONSTRAINT unique_card_set_subscription UNIQUE (custom_card_set_id);

--
-- Name: arkham_game_archives unique_game_archive; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_archives
    ADD CONSTRAINT unique_game_archive UNIQUE (arkham_game_id);

--
-- Name: arkham_game_outcomes unique_game_outcome; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_outcomes
    ADD CONSTRAINT unique_game_outcome UNIQUE (arkham_game_id);

--
-- Name: arkham_published_card_set_likes unique_published_card_set_like; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_likes
    ADD CONSTRAINT unique_published_card_set_like UNIQUE (published_card_set_id, user_id);

--
-- Name: arkham_published_card_set_versions unique_published_card_set_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_versions
    ADD CONSTRAINT unique_published_card_set_version UNIQUE (published_card_set_id, version);

--
-- Name: arkham_stats_refreshes unique_stats_refresh_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_stats_refreshes
    ADD CONSTRAINT unique_stats_refresh_name UNIQUE (name);

--
-- Name: arkham_achievements unique_user_achievement; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_achievements
    ADD CONSTRAINT unique_user_achievement UNIQUE (user_id, achievement);

--
-- Name: arkham_custom_cards unique_user_custom_card; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_cards
    ADD CONSTRAINT unique_user_custom_card UNIQUE (user_id, card_code);

--
-- Name: arkham_custom_card_sets unique_user_custom_card_set_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_card_sets
    ADD CONSTRAINT unique_user_custom_card_set_name UNIQUE (user_id, name);

--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);

--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);

--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);

--
-- Name: arkham_decks_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_decks_user_id_idx ON public.arkham_decks USING btree (user_id);

--
-- Name: arkham_epic_groups_event_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_epic_groups_event_idx ON public.arkham_epic_groups USING btree (arkham_epic_event_id);

--
-- Name: arkham_epic_groups_game_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_epic_groups_game_idx ON public.arkham_epic_groups USING btree (arkham_game_id);

--
-- Name: arkham_epic_members_event_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_epic_members_event_idx ON public.arkham_epic_members USING btree (arkham_epic_event_id);

--
-- Name: arkham_epic_steps_event_step_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX arkham_epic_steps_event_step_idx ON public.arkham_epic_steps USING btree (arkham_epic_event_id, step);

--
-- Name: arkham_games_allow_spectators_updated_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_games_allow_spectators_updated_at_idx ON public.arkham_games USING btree (updated_at DESC) WHERE allow_spectators;

--
-- Name: arkham_third_edition_history_table_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_third_edition_history_table_idx ON public.arkham_third_edition_history USING btree (table_id, id DESC);

--
-- Name: arkham_third_edition_tables_open_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_third_edition_tables_open_idx ON public.arkham_third_edition_tables USING btree (created_at DESC) WHERE (NOT started);

--
-- Name: arkham_third_edition_tables_seats_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX arkham_third_edition_tables_seats_idx ON public.arkham_third_edition_tables USING gin (((payload -> 'seats'::text)) jsonb_path_ops);

--
-- Name: idx_arkham_achievements_earned; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_achievements_earned ON public.arkham_achievements USING btree (achievement) WHERE (earned_at IS NOT NULL);

--
-- Name: idx_arkham_achievements_game; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_achievements_game ON public.arkham_achievements USING btree (arkham_game_id);

--
-- Name: idx_arkham_api_keys_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_api_keys_user ON public.arkham_api_keys USING btree (user_id);

--
-- Name: idx_arkham_build_auth_requests_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_build_auth_requests_created ON public.arkham_build_auth_requests USING btree (created_at);

--
-- Name: idx_arkham_card_set_share_accesses_set; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_card_set_share_accesses_set ON public.arkham_card_set_share_accesses USING btree (custom_card_set_id);

--
-- Name: idx_arkham_card_set_share_accesses_share; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_card_set_share_accesses_share ON public.arkham_card_set_share_accesses USING btree (card_set_share_id, created_at);

--
-- Name: idx_arkham_card_set_share_accesses_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_card_set_share_accesses_user ON public.arkham_card_set_share_accesses USING btree (user_id);

--
-- Name: idx_arkham_card_set_submissions_set; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_card_set_submissions_set ON public.arkham_card_set_submissions USING btree (published_card_set_id);

--
-- Name: idx_arkham_card_set_submissions_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_card_set_submissions_status ON public.arkham_card_set_submissions USING btree (status, created_at);

--
-- Name: idx_arkham_card_set_subscriptions_published; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_card_set_subscriptions_published ON public.arkham_card_set_subscriptions USING btree (published_card_set_id);

--
-- Name: idx_arkham_custom_cards_set; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_custom_cards_set ON public.arkham_custom_cards USING btree (custom_card_set_id);

--
-- Name: idx_arkham_decks_user_last_used; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_decks_user_last_used ON public.arkham_decks USING btree (user_id, last_used_at DESC);

--
-- Name: idx_arkham_game_archives_archived_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_game_archives_archived_at ON public.arkham_game_archives USING btree (archived_at DESC);

--
-- Name: idx_arkham_game_archives_state; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_game_archives_state ON public.arkham_game_archives USING btree (state);

--
-- Name: idx_arkham_game_stats_campaign; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_game_stats_campaign ON public.arkham_game_stats USING btree (campaign_id) WHERE is_campaign;

--
-- Name: idx_arkham_game_stats_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_game_stats_created ON public.arkham_game_stats USING btree (created_at);

--
-- Name: idx_arkham_game_stats_game; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_arkham_game_stats_game ON public.arkham_game_stats USING btree (game_id);

--
-- Name: idx_arkham_game_stats_outcome; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_game_stats_outcome ON public.arkham_game_stats USING btree (outcome_result) WHERE (state = 'IsOver'::text);

--
-- Name: idx_arkham_game_stats_scenario; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_game_stats_scenario ON public.arkham_game_stats USING btree (scenario_id) WHERE (NOT is_campaign);

--
-- Name: idx_arkham_log_entry_gameid_seq; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_log_entry_gameid_seq ON public.arkham_log_entries USING btree (arkham_game_id, seq DESC);

--
-- Name: idx_arkham_published_card_set_likes_set; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_published_card_set_likes_set ON public.arkham_published_card_set_likes USING btree (published_card_set_id);

--
-- Name: idx_arkham_published_card_sets_updated; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_arkham_published_card_sets_updated ON public.arkham_published_card_sets USING btree (updated_at DESC);

--
-- Name: log_entries_game_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX log_entries_game_id ON public.arkham_log_entries USING btree (arkham_game_id);

--
-- Name: steps_game_step_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX steps_game_step_idx ON public.arkham_steps USING btree (arkham_game_id, step);

--
-- Name: arkham_steps prevent_invalid_step_deletion; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER prevent_invalid_step_deletion BEFORE DELETE ON public.arkham_steps FOR EACH ROW EXECUTE FUNCTION public.check_step_before_delete();

--
-- Name: arkham_achievements arkham_achievements_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_achievements
    ADD CONSTRAINT arkham_achievements_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE SET NULL;

--
-- Name: arkham_achievements arkham_achievements_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_achievements
    ADD CONSTRAINT arkham_achievements_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_api_keys arkham_api_keys_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_api_keys
    ADD CONSTRAINT arkham_api_keys_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_build_auth_requests arkham_build_auth_requests_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_build_auth_requests
    ADD CONSTRAINT arkham_build_auth_requests_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_build_connections arkham_build_connections_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_build_connections
    ADD CONSTRAINT arkham_build_connections_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_share_accesses arkham_card_set_share_accesses_card_set_share_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_share_accesses
    ADD CONSTRAINT arkham_card_set_share_accesses_card_set_share_id_fkey FOREIGN KEY (card_set_share_id) REFERENCES public.arkham_card_set_shares(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_share_accesses arkham_card_set_share_accesses_custom_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_share_accesses
    ADD CONSTRAINT arkham_card_set_share_accesses_custom_card_set_id_fkey FOREIGN KEY (custom_card_set_id) REFERENCES public.arkham_custom_card_sets(id) ON DELETE SET NULL;

--
-- Name: arkham_card_set_share_accesses arkham_card_set_share_accesses_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_share_accesses
    ADD CONSTRAINT arkham_card_set_share_accesses_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_shares arkham_card_set_shares_custom_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_shares
    ADD CONSTRAINT arkham_card_set_shares_custom_card_set_id_fkey FOREIGN KEY (custom_card_set_id) REFERENCES public.arkham_custom_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_submissions arkham_card_set_submissions_published_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_submissions
    ADD CONSTRAINT arkham_card_set_submissions_published_card_set_id_fkey FOREIGN KEY (published_card_set_id) REFERENCES public.arkham_published_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_submissions arkham_card_set_submissions_reviewed_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_submissions
    ADD CONSTRAINT arkham_card_set_submissions_reviewed_by_user_id_fkey FOREIGN KEY (reviewed_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;

--
-- Name: arkham_card_set_submissions arkham_card_set_submissions_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_submissions
    ADD CONSTRAINT arkham_card_set_submissions_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_submissions arkham_card_set_submissions_version_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_submissions
    ADD CONSTRAINT arkham_card_set_submissions_version_id_fkey FOREIGN KEY (version_id) REFERENCES public.arkham_published_card_set_versions(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_subscriptions arkham_card_set_subscriptions_custom_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_subscriptions
    ADD CONSTRAINT arkham_card_set_subscriptions_custom_card_set_id_fkey FOREIGN KEY (custom_card_set_id) REFERENCES public.arkham_custom_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_card_set_subscriptions arkham_card_set_subscriptions_published_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_card_set_subscriptions
    ADD CONSTRAINT arkham_card_set_subscriptions_published_card_set_id_fkey FOREIGN KEY (published_card_set_id) REFERENCES public.arkham_published_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_custom_card_sets arkham_custom_card_sets_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_card_sets
    ADD CONSTRAINT arkham_custom_card_sets_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_custom_cards arkham_custom_cards_custom_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_cards
    ADD CONSTRAINT arkham_custom_cards_custom_card_set_id_fkey FOREIGN KEY (custom_card_set_id) REFERENCES public.arkham_custom_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_custom_cards arkham_custom_cards_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_custom_cards
    ADD CONSTRAINT arkham_custom_cards_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_decks arkham_decks_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_decks
    ADD CONSTRAINT arkham_decks_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_epic_events arkham_epic_events_organizer_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_events
    ADD CONSTRAINT arkham_epic_events_organizer_user_id_fkey FOREIGN KEY (organizer_user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_epic_groups arkham_epic_groups_arkham_epic_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_groups
    ADD CONSTRAINT arkham_epic_groups_arkham_epic_event_id_fkey FOREIGN KEY (arkham_epic_event_id) REFERENCES public.arkham_epic_events(id) ON DELETE CASCADE;

--
-- Name: arkham_epic_groups arkham_epic_groups_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_groups
    ADD CONSTRAINT arkham_epic_groups_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_epic_members arkham_epic_members_arkham_epic_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_members
    ADD CONSTRAINT arkham_epic_members_arkham_epic_event_id_fkey FOREIGN KEY (arkham_epic_event_id) REFERENCES public.arkham_epic_events(id) ON DELETE CASCADE;

--
-- Name: arkham_epic_members arkham_epic_members_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_members
    ADD CONSTRAINT arkham_epic_members_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_epic_steps arkham_epic_steps_arkham_epic_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_epic_steps
    ADD CONSTRAINT arkham_epic_steps_arkham_epic_event_id_fkey FOREIGN KEY (arkham_epic_event_id) REFERENCES public.arkham_epic_events(id) ON DELETE CASCADE;

--
-- Name: arkham_game_archives arkham_game_archives_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_archives
    ADD CONSTRAINT arkham_game_archives_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_game_archives arkham_game_archives_requested_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_archives
    ADD CONSTRAINT arkham_game_archives_requested_by_fkey FOREIGN KEY (requested_by) REFERENCES public.users(id) ON DELETE SET NULL;

--
-- Name: arkham_game_outcomes arkham_game_outcomes_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_outcomes
    ADD CONSTRAINT arkham_game_outcomes_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_game_undo_floors arkham_game_undo_floors_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_game_undo_floors
    ADD CONSTRAINT arkham_game_undo_floors_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_log_entries arkham_log_entries_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_log_entries
    ADD CONSTRAINT arkham_log_entries_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_players arkham_players_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_players
    ADD CONSTRAINT arkham_players_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_players arkham_players_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_players
    ADD CONSTRAINT arkham_players_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_published_card_set_likes arkham_published_card_set_likes_published_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_likes
    ADD CONSTRAINT arkham_published_card_set_likes_published_card_set_id_fkey FOREIGN KEY (published_card_set_id) REFERENCES public.arkham_published_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_published_card_set_likes arkham_published_card_set_likes_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_likes
    ADD CONSTRAINT arkham_published_card_set_likes_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_published_card_set_versions arkham_published_card_set_versions_published_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_set_versions
    ADD CONSTRAINT arkham_published_card_set_versions_published_card_set_id_fkey FOREIGN KEY (published_card_set_id) REFERENCES public.arkham_published_card_sets(id) ON DELETE CASCADE;

--
-- Name: arkham_published_card_sets arkham_published_card_sets_custom_card_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_sets
    ADD CONSTRAINT arkham_published_card_sets_custom_card_set_id_fkey FOREIGN KEY (custom_card_set_id) REFERENCES public.arkham_custom_card_sets(id) ON DELETE SET NULL;

--
-- Name: arkham_published_card_sets arkham_published_card_sets_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_published_card_sets
    ADD CONSTRAINT arkham_published_card_sets_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: arkham_steps arkham_steps_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_steps
    ADD CONSTRAINT arkham_steps_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;

--
-- Name: arkham_third_edition_history arkham_third_edition_history_table_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_third_edition_history
    ADD CONSTRAINT arkham_third_edition_history_table_id_fkey FOREIGN KEY (table_id) REFERENCES public.arkham_third_edition_tables(id) ON DELETE CASCADE;

--
-- Name: arkham_third_edition_tables arkham_third_edition_tables_host_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_third_edition_tables
    ADD CONSTRAINT arkham_third_edition_tables_host_id_fkey FOREIGN KEY (host_id) REFERENCES public.users(id) ON DELETE CASCADE;

--
-- Name: password_resets password_resets_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);

--
--


--
-- Schema baseline for upgrade.sh (generated by scripts/gen-schema-baseline.sh)
-- Records which migrations this dump already contains so upgrade.sh applies
-- only newer ones. Regenerate this block whenever setup.sql is regenerated.
--
CREATE TABLE IF NOT EXISTS public.arkham_schema_migrations (
    name text PRIMARY KEY,
    applied_at timestamptz NOT NULL DEFAULT now()
);
INSERT INTO public.arkham_schema_migrations (name) VALUES ('users') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_games') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_players') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_decks') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_created_at_to_arkham_games') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_url_to_decks') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_beta_to_users') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_steps') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('create_log_entries') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('create_password_resets') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_action_diff_to_arkham_steps') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_cascades') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('change_player_id_to_uuid') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('change_deck_url_to_nullable') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_step_constraint') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_epic') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_game_undo_floors') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_achievements') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_custom_cards') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_deck_overlay') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_custom_card_sets') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_published_card_sets') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_published_card_set_likes') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_last_used_at_to_decks') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_phase_transition_notifications_to_users') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_api_keys') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_card_set_submissions') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_game_stats') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_descriptions_to_card_sets') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_urls_to_card_sets') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_payload_to_log_entries') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_key_to_log_entries') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('log_entry_group') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_build_connections') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_game_archives') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_features_to_users') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('investigator_expansion_card_sets') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_third_edition_tables') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_card_set_shares') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('arkham_game_outcomes') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('allow_spectators') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('notifications') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_admin_to_users') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('align_model_column_types') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('dump_only_keys_and_indexes') ON CONFLICT DO NOTHING;
INSERT INTO public.arkham_schema_migrations (name) VALUES ('add_primary_key_to_arkham_steps') ON CONFLICT DO NOTHING;
