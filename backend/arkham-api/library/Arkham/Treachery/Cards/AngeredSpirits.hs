module Arkham.Treachery.Cards.AngeredSpirits (angeredSpirits) where

import Control.Lens (cosmos, _head)
import Arkham.Ability hiding (cosmos)
import Arkham.Asset.Uses
import Arkham.Helpers.Ref
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AngeredSpirits = AngeredSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

angeredSpirits :: TreacheryCard AngeredSpirits
angeredSpirits = treachery AngeredSpirits Cards.angeredSpirits

instance HasAbilities AngeredSpirits where
  getAbilities (AngeredSpirits a) =
    restricted
      a
      1
      OnSameLocation
      (FastAbility $ ExhaustAssetCost $ #spell <> AssetControlledBy You <> AssetWithUses Charge)
      : [ restricted a 2 (TokensOnThis #charge $ lessThan 4) (forcedOnElimination iid)
        | iid <- maybeToList a.owner
        ]

exhaustPayment :: Payment -> Maybe Target
exhaustPayment = preview (cosmos . _ExhaustPayment . _head)

instance RunMessage AngeredSpirits where
  runMessage msg t@(AngeredSpirits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ (exhaustPayment -> Just target) -> do
      moveTokens (attrs.ability 1) (targetToSource target) attrs Charge 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withTreacheryInvestigator attrs (`sufferPhysicalTrauma` 1)
      pure t
    _ -> AngeredSpirits <$> liftRunMessage msg attrs
