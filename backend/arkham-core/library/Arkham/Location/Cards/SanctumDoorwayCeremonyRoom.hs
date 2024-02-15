module Arkham.Location.Cards.SanctumDoorwayCeremonyRoom (
  sanctumDoorwayCeremonyRoom,
  SanctumDoorwayCeremonyRoom (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype SanctumDoorwayCeremonyRoom = SanctumDoorwayCeremonyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanctumDoorwayCeremonyRoom :: LocationCard SanctumDoorwayCeremonyRoom
sanctumDoorwayCeremonyRoom = location SanctumDoorwayCeremonyRoom Cards.sanctumDoorwayCeremonyRoom 3 (PerPlayer 2)

instance HasAbilities SanctumDoorwayCeremonyRoom where
  getAbilities (SanctumDoorwayCeremonyRoom attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          ( AnyCriterion [AssetExists $ assetAt $ toId attrs, InvestigatorExists $ investigatorAt $ toId attrs]
          )
          $ ForcedAbility
          $ RoundEnds Timing.When
      ]

instance RunMessage SanctumDoorwayCeremonyRoom where
  runMessage msg l@(SanctumDoorwayCeremonyRoom attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      cancelEffect <-
        selectAny $ investigatorAt (toId attrs) <> InvestigatorWithKey SkullKey
      unless cancelEffect $ do
        investigators <- select $ investigatorAt $ toId attrs
        assets <- select $ assetAt $ toId attrs
        pushAll
          $ [InvestigatorDirectDamage iid (toAbilitySource attrs 1) 0 1 | iid <- investigators]
          <> [AssetDamage aid (toAbilitySource attrs 1) 0 1 | aid <- assets]
      pure l
    _ -> SanctumDoorwayCeremonyRoom <$> runMessage msg attrs
