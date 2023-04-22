module Arkham.Location.Cards.LandlordsQuarters (
  landlordsQuarters,
  LandlordsQuarters (..),
) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing

newtype LandlordsQuarters = LandlordsQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

landlordsQuarters :: LocationCard LandlordsQuarters
landlordsQuarters =
  location LandlordsQuarters Cards.landlordsQuarters 2 (PerPlayer 1)

instance HasAbilities LandlordsQuarters where
  getAbilities (LandlordsQuarters a) =
    withBaseAbilities a $
      guard (locationRevealed a)
        *> [ restrictedAbility a 1 Here $
              ForcedAbility $
                RevealLocation Timing.After You $
                  LocationWithId $
                    toId a
           , haunted hauntedText a 2
           ]
   where
    hauntedText =
      "Search the encounter deck and discard pile for a Swarm of Rats and spawn it in Moldy Halls. Shuffle the encounter deck."

instance RunMessage LandlordsQuarters where
  runMessage msg l@(LandlordsQuarters attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $
        FindEncounterCard
          iid
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard]
          (cardIs Enemies.swarmOfRats)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $
        FindEncounterCard
          iid
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard]
          (cardIs Enemies.swarmOfRats)
      pure l
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      spawnSwarmOfRats <-
        createEnemyAtLocationMatching_ card (locationIs Locations.moldyHalls)
      push spawnSwarmOfRats
      pure l
    _ -> LandlordsQuarters <$> runMessage msg attrs
