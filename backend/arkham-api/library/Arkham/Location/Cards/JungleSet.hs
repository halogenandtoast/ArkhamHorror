module Arkham.Location.Cards.JungleSet (jungleSet) where

import Arkham.Ability
import Arkham.Capability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Search
import Arkham.Strategy

newtype JungleSet = JungleSet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jungleSet :: LocationCard JungleSet
jungleSet = location JungleSet Cards.jungleSet 3 (PerPlayer 1)

instance HasAbilities JungleSet where
  getAbilities (JungleSet a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)
      , playerLimit PerGame $ restricted a 2 (Here <> can.reveal.cards You) actionAbility
      ]

instance RunMessage JungleSet where
  runMessage msg l@(JungleSet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeledValidate' (notNull assets) "discardAssets" do
          chooseTargetM iid assets $ toDiscardBy iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      revealingEdit iid (attrs.ability 2) iid (FromTopOfDeck 6) \s ->
        s
          { searchFoundStrategy = DrawFound iid 1
          , searchMatcher = basic (#item <> #asset)
          , searchZones = [(FromTopOfDeck 6, ShuffleBackIn)]
          }

      pure l
    _ -> JungleSet <$> liftRunMessage msg attrs
