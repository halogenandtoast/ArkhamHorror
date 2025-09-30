module Arkham.Location.Cards.UnvisitedIsleHauntedSpring (unvisitedIsleHauntedSpring) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleHauntedSpring = UnvisitedIsleHauntedSpring LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleHauntedSpring :: LocationCard UnvisitedIsleHauntedSpring
unvisitedIsleHauntedSpring = location UnvisitedIsleHauntedSpring Cards.unvisitedIsleHauntedSpring 2 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleHauntedSpring where
  getModifiersFor (UnvisitedIsleHauntedSpring a) = whenUnrevealed a $ maybeModifySelf a do
    sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleHauntedSpring where
  getAbilities (UnvisitedIsleHauntedSpring a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here $ ActionAbility [Action.Circle] $ ActionCost 1
      , scenarioI18n $ hauntedI "unvisitedIsleHauntedSpring.haunted" a 2
      ]

instance RunMessage UnvisitedIsleHauntedSpring where
  runMessage msg l@(UnvisitedIsleHauntedSpring attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#intellect, #agility] (Fixed 9)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      chooseOrRunOneM iid $ withI18n do
        when hasAssets do
          countVar 1 $ labeled' "discardAssets" $ chooseAndDiscardAsset iid (attrs.ability 2)
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 2) 1
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleHauntedSpring <$> liftRunMessage msg attrs
