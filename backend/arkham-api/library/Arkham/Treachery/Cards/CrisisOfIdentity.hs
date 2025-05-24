module Arkham.Treachery.Cards.CrisisOfIdentity (crisisOfIdentity) where

import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Taboo
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrisisOfIdentity = CrisisOfIdentity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crisisOfIdentity :: TreacheryCard CrisisOfIdentity
crisisOfIdentity = treachery CrisisOfIdentity Cards.crisisOfIdentity

instance RunMessage CrisisOfIdentity where
  runMessage msg t@(CrisisOfIdentity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      role <- field InvestigatorClass iid
      assets <- selectTargets $ assetControlledBy iid <> AssetWithClass role <> DiscardableAsset
      events <- selectTargets $ eventControlledBy iid <> EventWithClass role
      skills <- selectTargets $ skillControlledBy iid <> SkillWithClass role

      if tabooed TabooList20 attrs
        then do
          cards <- selectTargets $ inHandOf NotForPlay iid <> basic (CardWithClass role)
          chooseTargetM iid (assets <> events <> skills <> cards) $ toDiscardBy iid attrs
          discardTopOfDeckAndHandle iid attrs 1 attrs
        else do
          for_ (assets <> events <> skills) (toDiscardBy iid attrs)
          discardTopOfDeckAndHandle iid attrs 1 attrs
      pure t
    DiscardedTopOfDeck iid [card] _ (isTarget attrs -> True) -> do
      push $ SetRole iid $ fromMaybe Neutral . headMay . toList $ cdClassSymbols $ toCardDef card
      pure t
    _ -> CrisisOfIdentity <$> liftRunMessage msg attrs
