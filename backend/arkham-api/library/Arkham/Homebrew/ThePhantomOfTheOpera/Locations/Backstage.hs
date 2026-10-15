module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.Backstage (backstage) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Backstage = Backstage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstage :: LocationCard Backstage
backstage = symbolLabel $ location Backstage Cards.backstage 3 (PerPlayer 1)

instance HasAbilities Backstage where
  getAbilities (Backstage a) =
    extendRevealed
      a
      [ restricted
          a
          1
          (ActExists (ActWithClues (AtLeast $ PerPlayer 1)) <> EachUndefeatedInvestigator (at_ (be a)))
          $ Objective
          $ FastAbility Free
      , scenarioI18n
          $ withI18nTooltip "backstage.scour"
          $ restricted a 2 (Here <> exists (ConcealedCardAt (be a)))
          $ FastAbility (DiscardAssetCost (AssetControlledBy You <> DiscardableAsset))
      ]

instance RunMessage Backstage where
  runMessage msg l@(Backstage attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceCurrentAct (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      concealed <- getConcealedAtAll NotForExpose attrs.id
      chooseNM iid (min 2 (length concealed))
        $ targets concealed (revealConcealed iid (attrs.ability 2) . (.id))
      pure l
    _ -> Backstage <$> liftRunMessage msg attrs
