module Arkham.Types.Treachery.Cards.TheKingsEdict
  ( theKingsEdict
  , TheKingsEdict(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheKingsEdict = TheKingsEdict TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdict :: TreacheryCard TheKingsEdict
theKingsEdict = treachery TheKingsEdict Cards.theKingsEdict

instance TreacheryRunner env => RunMessage env TheKingsEdict where
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
              lid <- getId enemy
              pure
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
