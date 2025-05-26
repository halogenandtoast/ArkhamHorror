module Arkham.Location.Cards.Waterfall (waterfall) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Waterfall = Waterfall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterfall :: LocationCard Waterfall
waterfall = symbolLabel $ location Waterfall Cards.waterfall 4 (PerPlayer 1)

instance HasAbilities Waterfall where
  getAbilities (Waterfall a) =
    extendRevealed
      a
      [ restricted a 1 (HasSupply Rope)
          $ freeReaction (InitiatedSkillTest #when You #any #any $ WhileInvestigating (be a))
      , restricted a 2 (youExist $ HandWith AnyCards)
          $ forced
          $ SkillTestResult #after You (WhileInvestigating (be a)) #failure
      ]

instance RunMessage Waterfall where
  runMessage msg l@(Waterfall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier (-3))
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      randomDiscard iid (attrs.ability 2)
      pure l
    _ -> Waterfall <$> liftRunMessage msg attrs
