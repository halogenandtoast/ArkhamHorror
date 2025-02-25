module Arkham.Asset.Assets.AntikytheraPropheticTimepiece5 (antikytheraPropheticTimepiece5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Window
import Arkham.Matcher

newtype AntikytheraPropheticTimepiece5 = AntikytheraPropheticTimepiece5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antikytheraPropheticTimepiece5 :: AssetCard AntikytheraPropheticTimepiece5
antikytheraPropheticTimepiece5 = asset AntikytheraPropheticTimepiece5 Cards.antikytheraPropheticTimepiece5

instance HasAbilities AntikytheraPropheticTimepiece5 where
  getAbilities (AntikytheraPropheticTimepiece5 a) =
    [ controlled
        a
        1
        ( exists (YourLocation <> LocationWithAnyClues)
            <> youExist (InvestigatorCanDiscoverCluesAt YourLocation)
        )
        $ triggered
          (SkillTestResult #after You AnySkillTest $ SuccessResult $ GameValueOneOf [static 1, static 3])
          (exhaust a)
    , controlled
        a
        1
        (youExist $ oneOf [can.draw.cards, can.gain.resources])
        $ triggered
          (SkillTestResult #after You AnySkillTest $ SuccessResult $ GameValueOneOf [static 2, static 4])
          (exhaust a)
    , restricted a 1 ControlsThis
        $ triggered
          (SkillTestResult #after You AnySkillTest $ SuccessResult $ static 5)
          (exhaust a)
    ]

instance RunMessage AntikytheraPropheticTimepiece5 where
  runMessage msg a@(AntikytheraPropheticTimepiece5 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getPassedBy -> n) _ -> do
      if
        | n `elem` [1, 3] -> discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
        | n `elem` [2, 4] -> do
            drawCards iid (attrs.ability 1) 1
            gainResources iid (attrs.ability 1) 1
        | n == 5 -> onNextTurnEffect (attrs.ability 1) iid do
            push $ GainActions iid (attrs.ability 1) 1
        | otherwise -> pure ()

      pure a
    _ -> AntikytheraPropheticTimepiece5 <$> liftRunMessage msg attrs
