module Arkham.Location.Cards.TrapRoom (
  trapRoom,
  TrapRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TrapRoom = TrapRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trapRoom :: LocationCard TrapRoom
trapRoom = location TrapRoom Cards.trapRoom 3 (PerPlayer 1)

instance HasAbilities TrapRoom where
  getAbilities (TrapRoom attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $
        ForcedAbility $
          RevealLocation Timing.After You $
            LocationWithId $
              toId attrs
      | locationRevealed attrs
      ]

instance RunMessage TrapRoom where
  runMessage msg l@(TrapRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        getSomeRats =
          FindEncounterCard
            iid
            (toTarget attrs)
            [FromEncounterDeck, FromEncounterDiscard]
            $ cardIs Cards.swarmOfRats
      playerCount <- getPlayerCount
      l <$ pushAll (getSomeRats : [getSomeRats | playerCount >= 3])
    FoundEncounterCard iid target card
      | isTarget attrs target ->
          l <$ push (SpawnEnemyAtEngagedWith (EncounterCard card) (toId attrs) iid)
    _ -> TrapRoom <$> runMessage msg attrs
