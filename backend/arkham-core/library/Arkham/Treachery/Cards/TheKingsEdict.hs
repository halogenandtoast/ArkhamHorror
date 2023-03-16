module Arkham.Treachery.Cards.TheKingsEdict
  ( theKingsEdict
  , TheKingsEdict(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
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
      cultists <- selectList $ EnemyWithTrait Cultist
      cultistsWithClues <-
        selectList $ EnemyWithTrait Cultist <> EnemyAt LocationWithAnyClues
      msgs <- case cultistsWithClues of
        [] -> pure [gainSurge attrs]
        xs -> concatForM xs $ \cultist -> do
          mlid <- selectOne $ locationWithEnemy cultist
          pure $ do
            lid <- maybeToList mlid
            [RemoveClues (toTarget lid) 1, PlaceClues (toTarget cultist) 1]
      pushAll
        $ msgs
        <> map
             (CreateEffect (toCardCode attrs) Nothing source . toTarget)
             cultists
      pure t
    _ -> TheKingsEdict <$> runMessage msg attrs
