module Arkham.Treachery.Cards.WrackedByTime (wrackedByTime) where

import Arkham.Asset.Types (Field (AssetOwner))
import Arkham.Deck
import Arkham.Id
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Message qualified as Msg
import Arkham.Trait (Trait (Shattered))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {damagedAssets :: Set AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WrackedByTime = WrackedByTime (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByTime :: TreacheryCard WrackedByTime
wrackedByTime =
  treachery (WrackedByTime . (`with` Metadata mempty)) Cards.wrackedByTime

instance RunMessage WrackedByTime where
  runMessage msg t@(WrackedByTime (attrs `With` meta)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid1 <- getRandom
      revelationSkillTest sid1 iid attrs #willpower (Fixed 3)
      others <- select $ at_ (LocationWithTrait Shattered) <> not_ (InvestigatorWithId iid)
      for_ others \iid' -> do
        sid <- getRandom
        revelationSkillTest sid iid' attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    Msg.AssignAssetDamageWithCheck aid (isSource attrs -> True) _ _ _ -> do
      pure . WrackedByTime $ attrs `with` Metadata (insertSet aid $ damagedAssets meta)
    After (Revelation _ (isSource attrs -> True)) -> do
      assets <- selectWithField AssetOwner $ mapOneOf AssetWithId $ setToList (damagedAssets meta)
      for_ assets \(aid, mowner) -> do
        let deck = maybe EncounterDeck InvestigatorDeck mowner
        shuffleIntoDeck deck aid
      pure t
    _ -> WrackedByTime . (`with` meta) <$> liftRunMessage msg attrs
