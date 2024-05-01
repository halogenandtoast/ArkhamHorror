module Arkham.Asset.Cards.HolyRosary2 (holyRosary2, HolyRosary2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Prelude

newtype HolyRosary2 = HolyRosary2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holyRosary2 :: AssetCard HolyRosary2
holyRosary2 = assetWith HolyRosary2 Cards.holyRosary2 (sanityL ?~ 2)

instance HasModifiersFor HolyRosary2 where
  getModifiersFor (InvestigatorTarget iid) (HolyRosary2 a) =
    pure $ toModifiers a [SkillModifier #willpower 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities HolyRosary2 where
  getAbilities (HolyRosary2 x) =
    [ controlledAbility x 1 HasRemainingBlessTokens
        $ ReactionAbility
          ( SkillTestResult
              #after
              You
              (SkillTestOnTreachery AnyTreachery <> SkillTestWithSkillType #willpower)
              (SuccessResult AnyValue)
          )
          (exhaust x)
    ]

instance RunMessage HolyRosary2 where
  runMessage msg a@(HolyRosary2 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- min 2 <$> getRemainingBlessTokens
      pushAll $ replicate n (AddChaosToken BlessToken)
      pure a
    _ -> HolyRosary2 <$> runMessage msg attrs
