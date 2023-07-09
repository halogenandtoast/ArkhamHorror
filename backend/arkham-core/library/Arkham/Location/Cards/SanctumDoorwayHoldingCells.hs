module Arkham.Location.Cards.SanctumDoorwayHoldingCells (
  sanctumDoorwayHoldingCells,
  SanctumDoorwayHoldingCells (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Cultist))

newtype SanctumDoorwayHoldingCells = SanctumDoorwayHoldingCells LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanctumDoorwayHoldingCells :: LocationCard SanctumDoorwayHoldingCells
sanctumDoorwayHoldingCells = location SanctumDoorwayHoldingCells Cards.sanctumDoorwayHoldingCells 1 (PerPlayer 1)

instance HasAbilities SanctumDoorwayHoldingCells where
  getAbilities (SanctumDoorwayHoldingCells attrs) =
    withRevealedAbilities
      attrs
      [ mkAbility attrs 1 $
          ForcedAbility $
            RevealLocation Timing.After Anyone $
              LocationWithId $
                toId attrs
      ]

instance RunMessage SanctumDoorwayHoldingCells where
  runMessage msg l@(SanctumDoorwayHoldingCells attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ FindEncounterCard
          iid
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard]
        $ CardWithType EnemyType
          <> CardWithTrait Cultist
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      pushM $ createEnemyAt_ card (toId attrs) Nothing
      pure l
    _ -> SanctumDoorwayHoldingCells <$> runMessage msg attrs
