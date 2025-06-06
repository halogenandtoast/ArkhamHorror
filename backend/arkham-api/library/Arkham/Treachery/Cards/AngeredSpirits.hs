module Arkham.Treachery.Cards.AngeredSpirits (angeredSpirits) where

import Arkham.Ability
import Arkham.Asset.Uses
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
    restricted a 1 OnSameLocation (FastAbility $ ExhaustAssetCost $ #spell <> AssetControlledBy You)
      : [ restricted a 2 (TokensOnThis #charge $ lessThan 4) (forcedOnElimination iid)
        | iid <- maybeToList a.owner
        ]

instance RunMessage AngeredSpirits where
  runMessage msg t@(AngeredSpirits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ (ExhaustPayment [target]) -> do
      spendUses (attrs.ability 1) target Charge 1
      placeTokens (attrs.ability 1) attrs Charge 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withTreacheryInvestigator attrs (`sufferPhysicalTrauma` 1)
      pure t
    _ -> AngeredSpirits <$> liftRunMessage msg attrs
