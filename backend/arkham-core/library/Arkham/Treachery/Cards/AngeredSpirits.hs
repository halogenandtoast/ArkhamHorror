module Arkham.Treachery.Cards.AngeredSpirits (angeredSpirits, AngeredSpirits (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AngeredSpirits = AngeredSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

angeredSpirits :: TreacheryCard AngeredSpirits
angeredSpirits = treachery AngeredSpirits Cards.angeredSpirits

instance HasAbilities AngeredSpirits where
  getAbilities (AngeredSpirits a) =
    restrictedAbility
      a
      1
      OnSameLocation
      (FastAbility $ ExhaustAssetCost $ AssetWithTrait Spell <> AssetControlledBy You)
      : [ restrictedAbility a 2 (ChargesOnThis $ lessThan 4) (forcedOnElimination iid)
        | iid <- maybeToList a.owner
        ]

instance RunMessage AngeredSpirits where
  runMessage msg t@(AngeredSpirits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ (ExhaustPayment [target]) -> do
      pushAll [SpendUses target Charge 1, PlaceResources (attrs.ability 1) (toTarget attrs) 1]
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withTreacheryInvestigator attrs \tormented -> push (SufferTrauma tormented 1 0)
      pure t
    _ -> AngeredSpirits <$> liftRunMessage msg attrs
