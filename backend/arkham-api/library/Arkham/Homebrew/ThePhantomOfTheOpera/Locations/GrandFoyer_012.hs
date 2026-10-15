module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.GrandFoyer_012 (grandFoyer_012) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandFoyer_012 = GrandFoyer_012 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandFoyer_012 :: LocationCard GrandFoyer_012
grandFoyer_012 = symbolLabel $ location GrandFoyer_012 Cards.grandFoyer_012 3 (Static 0)

instance HasAbilities GrandFoyer_012 where
  getAbilities (GrandFoyer_012 a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "grandFoyer.resign" $ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "grandFoyer_012.parley"
          $ playerLimit PerRound
          $ restricted a 1 (Here <> exists (ConcealedCardAt (be a))) parleyAction_
      ]

instance RunMessage GrandFoyer_012 where
  runMessage msg l@(GrandFoyer_012 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- getConcealedAt NotForExpose attrs.id
      chooseHandleTargetM iid (attrs.ability 1) concealed
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ConcealedCardTarget cid) -> do
      revealConcealed iid (attrs.ability 1) cid
      whenM (selectAny $ ConcealedCardWithId cid <> IsDecoy) $ doStep 1 msg
      pure l
    DoStep 1 (HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ConcealedCardTarget _)) -> do
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        unscoped $ countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        scope "grandFoyer" $ labeled "addDecoyAndShuffle" do
          makeDecoyAt iid attrs.id
          shuffleConcealedAt attrs.id
      pure l
    _ -> GrandFoyer_012 <$> liftRunMessage msg attrs
