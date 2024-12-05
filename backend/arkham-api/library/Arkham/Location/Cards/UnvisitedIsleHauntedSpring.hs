module Arkham.Location.Cards.UnvisitedIsleHauntedSpring (
  unvisitedIsleHauntedSpring,
  UnvisitedIsleHauntedSpring (..),
)
where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
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
  getAbilities (UnvisitedIsleHauntedSpring attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "You must either discard an asset you control, or take 1 damage" attrs 2
      ]

instance RunMessage UnvisitedIsleHauntedSpring where
  runMessage msg l@(UnvisitedIsleHauntedSpring attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#intellect, #agility] (Fixed 9)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      chooseOrRunOneM iid do
        when hasAssets do
          labeled "Discard an asset you control" $ push $ ChooseAndDiscardAsset iid (toSource attrs) AnyAsset
        labeled "Take 1 damage" $ assignDamage iid (attrs.ability 2) 1
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleHauntedSpring <$> liftRunMessage msg attrs
