module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.WalkersTrumpet (walkersTrumpet) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Modifier
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype WalkersTrumpet = WalkersTrumpet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walkersTrumpet :: AssetCard WalkersTrumpet
walkersTrumpet = asset WalkersTrumpet Cards.walkersTrumpet

instance HasAbilities WalkersTrumpet where
  -- Exhaust as a test begins for +1 skill value per chaos token it reveals.
  getAbilities (WalkersTrumpet a) =
    [ controlled a 1 NoRestriction
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any) (exhaust a)
    ]

instance RunMessage WalkersTrumpet where
  runMessage msg a@(WalkersTrumpet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTestId >>= traverse_ \sid ->
        skillTestModifier
          sid
          (attrs.ability 1)
          iid
          (AnySkillValueCalculated $ CountChaosTokens $ RevealedChaosTokens AnyChaosToken)
      pure a
    _ -> WalkersTrumpet <$> liftRunMessage msg attrs
