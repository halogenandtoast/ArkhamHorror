module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.GrandFoyer_009 (grandFoyer_009) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandFoyer_009 = GrandFoyer_009 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandFoyer_009 :: LocationCard GrandFoyer_009
grandFoyer_009 = symbolLabel $ location GrandFoyer_009 Cards.grandFoyer_009 3 (Static 0)

instance HasAbilities GrandFoyer_009 where
  getAbilities (GrandFoyer_009 a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "grandFoyer.resign" $ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "grandFoyer_009.parley"
          $ restricted a 1 (Here <> exists (ConcealedCardAt (be a)))
          $ parleyAction (ResourceCost 1)
      ]

instance RunMessage GrandFoyer_009 where
  runMessage msg l@(GrandFoyer_009 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- getConcealedAt NotForExpose attrs.id
      chooseHandleTargetM iid (attrs.ability 1) concealed
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (ConcealedCardTarget cid) -> do
      revealConcealed iid (attrs.ability 1) cid
      whenM (selectAny $ InvestigatorWithId iid <> InvestigatorWithResources (atLeast 1)) do
        chooseOneM iid $ scenarioI18n do
          scope "grandFoyer" $ labeled "spendToExpose" do
            spendResources iid 1
            exposeConcealed iid (attrs.ability 1) cid
          unscoped $ labeled "doNotExposeConcealed" nothing
      pure l
    _ -> GrandFoyer_009 <$> liftRunMessage msg attrs
