module Arkham.Treachery.Cards.TheKingsEdict (theKingsEdict, theKingsEdictEffect, TheKingsEdict (..)) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types (Field (EnemyClues, EnemyDoom))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheKingsEdict = TheKingsEdict TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdict :: TreacheryCard TheKingsEdict
theKingsEdict = treachery TheKingsEdict Cards.theKingsEdict

instance RunMessage TheKingsEdict where
  runMessage msg t@(TheKingsEdict attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      cultists <- select $ EnemyWithTrait Cultist
      cultistsWithClues <-
        select $ EnemyWithTrait Cultist <> EnemyAt LocationWithAnyClues
      msgs <- case cultistsWithClues of
        [] -> pure [gainSurge attrs]
        xs -> concatForM xs $ \cultist -> do
          mlid <- selectOne $ locationWithEnemy cultist
          pure $ do
            lid <- maybeToList mlid
            [RemoveClues (toSource attrs) (toTarget lid) 1, PlaceClues (toSource attrs) (toTarget cultist) 1]
      pushAll $ msgs <> map (createCardEffect Cards.theKingsEdict Nothing source) cultists
      pure t
    _ -> TheKingsEdict <$> runMessage msg attrs

newtype TheKingsEdictEffect = TheKingsEdictEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdictEffect :: EffectArgs -> TheKingsEdictEffect
theKingsEdictEffect = cardEffect TheKingsEdictEffect Cards.theKingsEdict

instance HasModifiersFor TheKingsEdictEffect where
  getModifiersFor target@(EnemyTarget eid) (TheKingsEdictEffect a) | target == a.target = do
    clueCount <- field EnemyClues eid
    doomCount <- field EnemyDoom eid
    pure $ toModifiers a [EnemyFight (clueCount + doomCount) | clueCount + doomCount > 0]
  getModifiersFor _ _ = pure []

instance RunMessage TheKingsEdictEffect where
  runMessage msg e@(TheKingsEdictEffect attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ toId e)
    _ -> TheKingsEdictEffect <$> runMessage msg attrs
