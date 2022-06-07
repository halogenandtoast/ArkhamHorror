module Arkham.Treachery.Cards.TheKingsEdict
  ( theKingsEdict
  , TheKingsEdict(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Runner

newtype TheKingsEdict = TheKingsEdict TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdict :: TreacheryCard TheKingsEdict
theKingsEdict = treachery TheKingsEdict Cards.theKingsEdict

instance RunMessage TheKingsEdict where
  runMessage msg t@(TheKingsEdict attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <-
        selectList $ EnemyWithTrait Cultist <> EnemyAt LocationWithAnyClues
      enemiesInPlay <- selectList $ EnemyWithTrait Cultist
      t <$ case targets of
        [] -> push (Surge iid source)
        xs -> do
          msgs <- concat <$> for
            xs
            \enemy -> do
              mlid <- selectOne $ locationWithEnemy enemy
              pure $ case mlid of
                Nothing -> []
                Just lid ->
                  [ RemoveClues (LocationTarget lid) 1
                  , PlaceClues (EnemyTarget enemy) 1
                  ]
          pushAll
            $ msgs
            <> map
                 (CreateEffect
                   (toCardCode attrs)
                   Nothing
                   source . EnemyTarget
                 )
                 enemiesInPlay

    _ -> TheKingsEdict <$> runMessage msg attrs
