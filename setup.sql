--
-- PostgreSQL database dump
--

-- Dumped from database version 14.13 (Homebrew)
-- Dumped by pg_dump version 14.13 (Homebrew)

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
    -- Your logic here
    RETURN OLD;
END;
$$;


--
-- Name: enforce_chronological_order_per_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.enforce_chronological_order_per_game() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    violating_step RECORD;
BEGIN
    -- Only proceed if this is an AFTER DELETE statement-level trigger
    IF (TG_OP = 'DELETE') AND (TG_LEVEL = 'STATEMENT') THEN
        -- Check for any steps that now violate the chronological order
        WITH affected_games AS (
            SELECT DISTINCT arkham_game_id FROM oldtab
        ), violating_steps AS (
            SELECT s.arkham_game_id, s.step
            FROM arkham_steps s
            LEFT JOIN arkham_steps prev_s ON
                prev_s.arkham_game_id = s.arkham_game_id AND
                prev_s.step = s.step - 1
            WHERE s.step > 0 AND prev_s.step IS NULL AND
                s.arkham_game_id IN (SELECT arkham_game_id FROM affected_games)
        )
        SELECT * INTO violating_step FROM violating_steps LIMIT 1;

        IF FOUND THEN
            RAISE EXCEPTION 'Step % for game % exists without previous step', violating_step.step, violating_step.arkham_game_id;
        END IF;
    END IF;

    RETURN NULL;
END;
$$;


--
-- Name: enforce_step_order_per_game(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.enforce_step_order_per_game() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Enforce that the step being inserted follows the chronological order
    IF NEW.step = 0 THEN
        RETURN NEW;
    END IF;

    -- Ensure the previous step exists for the same game
    IF NOT EXISTS (
        SELECT 1 FROM arkham_steps
        WHERE arkham_game_id = NEW.arkham_game_id
          AND step = NEW.step - 1
    ) THEN
        RAISE EXCEPTION 'Cannot insert step % for game % without step %', NEW.step, NEW.arkham_game_id, NEW.step - 1;
    END IF;

    RETURN NEW;  -- Allow the insertion to proceed if the previous step exists
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: arkham_decks; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_decks (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    user_id bigint NOT NULL,
    name text NOT NULL,
    investigator_name text NOT NULL,
    list jsonb NOT NULL,
    url text
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
-- Name: arkham_games; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_games (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    name text NOT NULL,
    current_data jsonb NOT NULL,
    multiplayer_variant text NOT NULL,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now(),
    step integer
);


--
-- Name: arkham_log_entries; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_log_entries (
    id bigint NOT NULL,
    body text NOT NULL,
    arkham_game_id uuid NOT NULL,
    step integer NOT NULL,
    created_at timestamp without time zone DEFAULT now()
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
-- Name: arkham_steps; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.arkham_steps (
    id uuid DEFAULT public.uuid_generate_v4(),
    arkham_game_id uuid NOT NULL,
    choice jsonb NOT NULL,
    step integer NOT NULL,
    action_diff jsonb NOT NULL
);


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
    beta boolean DEFAULT false NOT NULL
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
-- Name: password_resets user_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets ALTER COLUMN user_id SET DEFAULT nextval('public.password_resets_user_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: arkham_decks arkham_decks_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_decks
    ADD CONSTRAINT arkham_decks_pkey PRIMARY KEY (id);


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
-- Name: password_resets password_resets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_pkey PRIMARY KEY (id);


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
-- Name: log_entries_game_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX log_entries_game_id ON public.arkham_log_entries USING btree (arkham_game_id);


--
-- Name: steps_game_step_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX steps_game_step_idx ON public.arkham_steps USING btree (arkham_game_id, step);


--
-- Name: arkham_steps enforce_chronological_order_per_game; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER enforce_chronological_order_per_game AFTER DELETE ON public.arkham_steps REFERENCING OLD TABLE AS oldtab FOR EACH STATEMENT EXECUTE FUNCTION public.enforce_chronological_order_per_game();


--
-- Name: arkham_steps enforce_step_order_per_game; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER enforce_step_order_per_game BEFORE INSERT ON public.arkham_steps FOR EACH ROW EXECUTE FUNCTION public.enforce_step_order_per_game();


--
-- Name: arkham_steps prevent_invalid_step_deletion; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER prevent_invalid_step_deletion BEFORE DELETE ON public.arkham_steps FOR EACH ROW EXECUTE FUNCTION public.check_step_before_delete();


--
-- Name: arkham_decks arkham_decks_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_decks
    ADD CONSTRAINT arkham_decks_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


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
-- Name: arkham_steps arkham_steps_arkham_game_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.arkham_steps
    ADD CONSTRAINT arkham_steps_arkham_game_id_fkey FOREIGN KEY (arkham_game_id) REFERENCES public.arkham_games(id) ON DELETE CASCADE;


--
-- Name: password_resets password_resets_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- PostgreSQL database dump complete
--

