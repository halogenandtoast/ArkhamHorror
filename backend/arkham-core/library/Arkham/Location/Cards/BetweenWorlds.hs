module Arkham.Location.Cards.BetweenWorlds
  ( betweenWorlds
  , BetweenWorlds(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype BetweenWorlds = BetweenWorlds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

betweenWorlds :: LocationCard BetweenWorlds
betweenWorlds = location BetweenWorlds Cards.betweenWorlds 3 (Static 1)

instance HasAbilities BetweenWorlds where
  getAbilities (BetweenWorlds a) =
    withBaseAbilities a [mkAbility a 1 $ ForcedAbility $ RoundEnds Timing.When]

instance RunMessage BetweenWorlds where
  runMessage msg l@(BetweenWorlds attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <- selectList $ investigatorAt (toId attrs)
      if null iids
        then do
          let
            asTreachery =
              lookupEncounterCard Treacheries.betweenWorlds (toCardId attrs)
          enemies <- selectList $ enemyAt (toId attrs)
          pushAll
            $ [ EnemyMove enemy (toId attrs) | enemy <- enemies ]
            <> [RemoveLocation (toId attrs), AddToEncounterDiscard asTreachery]
        else pushAll
          [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
          | iid <- iids
          ]
      pure l
    _ -> BetweenWorlds <$> runMessage msg attrs
