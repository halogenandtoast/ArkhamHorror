module Arkham.Location.Cards.FarAboveYourHouse where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (farAboveYourHouse)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype FarAboveYourHouse = FarAboveYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

farAboveYourHouse :: LocationCard FarAboveYourHouse
farAboveYourHouse =
  location FarAboveYourHouse Cards.farAboveYourHouse 2 (PerPlayer 1)

instance HasAbilities FarAboveYourHouse where
  getAbilities (FarAboveYourHouse attrs) =
    withBaseAbilities attrs
      $ [ skillTestAbility
          $ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage FarAboveYourHouse where
  runMessage msg l@(FarAboveYourHouse attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid SkillWillpower (Fixed 4)
      pure l
    FailedSkillTest _ _ source SkillTestInitiatorTarget {} _ n
      | isAbilitySource attrs 1 source -> do
          investigatorIds <- getInvestigatorIds
          pushAll
            $ concat
            $ replicate @[[Message]]
              n
              [ toMessage $ randomDiscard iid' (toAbilitySource attrs 1)
              | iid' <- investigatorIds
              ]
          pure l
    _ -> FarAboveYourHouse <$> runMessage msg attrs
