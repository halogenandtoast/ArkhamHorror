module Arkham.Treachery.Cards.CenturiesOfSecrets (centuriesOfSecrets, CenturiesOfSecrets (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Trait (Trait (Curse))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CenturiesOfSecrets = CenturiesOfSecrets TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centuriesOfSecrets :: TreacheryCard CenturiesOfSecrets
centuriesOfSecrets = treachery CenturiesOfSecrets Cards.centuriesOfSecrets

instance RunMessage CenturiesOfSecrets where
  runMessage msg t@(CenturiesOfSecrets attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      push $ DiscardTopOfEncounterDeck iid n (toSource attrs) (Just $ toTarget attrs)
      pure t
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      when (any (`cardMatch` CardWithTrait Curse) cards) $ do
        assetIds <- select $ assetControlledBy iid <> AllyAsset
        pushAll
          $ InvestigatorDirectDamage iid (toSource attrs) 1 0
          : [Msg.AssetDamage aid (toSource attrs) 1 0 | aid <- assetIds]
      pure t
    _ -> CenturiesOfSecrets <$> runMessage msg attrs
