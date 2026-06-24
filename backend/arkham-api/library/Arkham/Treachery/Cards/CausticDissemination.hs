module Arkham.Treachery.Cards.CausticDissemination (causticDissemination) where

import Arkham.Ability
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Matcher hiding (AssetCard)
import Arkham.Placement
import Arkham.Trait (Trait (Oozified))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (AssetDefeated)

newtype CausticDissemination = CausticDissemination TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

causticDissemination :: TreacheryCard CausticDissemination
causticDissemination = treachery CausticDissemination Cards.causticDissemination

instance HasAbilities CausticDissemination where
  getAbilities (CausticDissemination a) =
    [ limited (MaxPer Cards.causticDissemination PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when
    , mkAbility a 2 $ SilentForcedAbility $ AssetDefeated #when (BySource $ SourceIs $ toSource a) AnyAsset
    ]

instance RunMessage CausticDissemination where
  runMessage msg t@(CausticDissemination attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery (toId attrs) NextToAgenda
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt (LocationWithTrait Oozified)
      for_ investigators \iid -> assignDamageAndHorror iid attrs 1 1
      toDiscard (attrs.ability 1) attrs
      pure t
    UseCardAbility _ (isSource attrs -> True) 2 (getDefeatedAsset -> aid) _ -> do
      scenarioSpecific "devour" (toTarget aid)
      pure t
    _ -> CausticDissemination <$> liftRunMessage msg attrs
