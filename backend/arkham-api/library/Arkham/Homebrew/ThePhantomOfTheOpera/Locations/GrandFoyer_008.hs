module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.GrandFoyer_008 (grandFoyer_008) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandFoyer_008 = GrandFoyer_008 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandFoyer_008 :: LocationCard GrandFoyer_008
grandFoyer_008 = symbolLabel $ location GrandFoyer_008 Cards.grandFoyer_008 3 (Static 0)

instance HasAbilities GrandFoyer_008 where
  getAbilities (GrandFoyer_008 a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "grandFoyer.resign" $ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "grandFoyer_008.parley"
          $ restricted a 1 (Here <> exists (ConcealedCardAt (be a)))
          $ parleyAction (HandDiscardCost 1 #any)
      ]

instance RunMessage GrandFoyer_008 where
  runMessage msg l@(GrandFoyer_008 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- getConcealedAt NotForExpose attrs.id
      chooseHandleTargetM iid (attrs.ability 1) concealed
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ConcealedCardTarget cid) -> do
      revealConcealed iid (attrs.ability 1) cid
      whenM (selectAny $ inHandOf NotForPlay iid) do
        chooseOneM iid $ scenarioI18n do
          scope "grandFoyer" $ labeled "discardToExpose" $ doStep 1 msg
          unscoped $ labeled "doNotExposeConcealed" nothing
      pure l
    DoStep 1 (HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ConcealedCardTarget cid)) -> do
      cards <- select $ inHandOf NotForPlay iid
      focusCards cards $ chooseTargetM iid cards \card -> do
        discardCard iid (attrs.ability 1) card
        exposeConcealed iid (attrs.ability 1) cid
      pure l
    _ -> GrandFoyer_008 <$> liftRunMessage msg attrs
