module Arkham.Asset.Assets.SecondSight5 (secondSight5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Discover
import Arkham.ForMovement
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier

newtype SecondSight5 = SecondSight5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondSight5 :: AssetCard SecondSight5
secondSight5 = asset SecondSight5 Cards.secondSight5

instance HasAbilities SecondSight5 where
  getAbilities (SecondSight5 a) =
    [skillTestAbility $ controlled_ a 1 $ investigateActionWith_ #willpower]

instance RunMessage SecondSight5 where
  runMessage msg a@(SecondSight5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #cultist attrs attrs $ doStep 1 msg
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
      investigateWith_ #willpower sid iid (attrs.ability 1)
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      if attrs.use #charge == 0
        then do
          assignHorror iid (attrs.ability 1) 1
          toDiscardBy iid (attrs.ability 1) attrs
        else removeTokens (attrs.ability 1) attrs Charge 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (attrs.use #charge > 0) do
        chooseOne iid
          $ Label "Spend 1 charge to discover 1 additional clue" [DoStep 2 msg]
          : [Label "Do not spend charge" []]
      pure a
    DoStep 2 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      currentLocation <- getJustLocation iid
      connectedLocations <- select $ ConnectedTo NotForMovement (LocationWithInvestigator $ InvestigatorWithId iid)
      currentLocationDiscovery <- discover currentLocation (attrs.ability 1) 1
      connectedLocationDiscoveries <- for connectedLocations \loc -> do
        d <- discover loc (attrs.ability 1) 1
        pure (loc, d)
      chooseOne iid
        $ targetLabel currentLocation [RemoveTokens (toSource attrs) (toTarget attrs) Charge 1, Msg.DiscoverClues iid currentLocationDiscovery]
        : map (\(loc, d) -> targetLabel loc [RemoveTokens (toSource attrs) (toTarget attrs) Charge 1, Msg.DiscoverClues iid d]) connectedLocationDiscoveries
      pure a
    _ -> SecondSight5 <$> liftRunMessage msg attrs
