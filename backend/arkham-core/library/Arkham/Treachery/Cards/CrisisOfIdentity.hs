module Arkham.Treachery.Cards.CrisisOfIdentity (
  crisisOfIdentity,
  CrisisOfIdentity (..),
) where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CrisisOfIdentity = CrisisOfIdentity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crisisOfIdentity :: TreacheryCard CrisisOfIdentity
crisisOfIdentity = treachery CrisisOfIdentity Cards.crisisOfIdentity

instance RunMessage CrisisOfIdentity where
  runMessage msg t@(CrisisOfIdentity attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      role <- field InvestigatorClass iid
      assets <-
        selectList
          $ assetControlledBy iid
          <> AssetWithClass role
          <> DiscardableAsset
      events <-
        selectList
          $ EventControlledBy (InvestigatorWithId iid)
          <> EventWithClass role
      skills <-
        selectList
          $ SkillControlledBy (InvestigatorWithId iid)
          <> SkillWithClass role
      pushAll
        $ map (toDiscardBy iid attrs) assets
        <> map (toDiscardBy iid attrs) events
        <> map (toDiscardBy iid attrs) skills
        <> [DiscardTopOfDeck iid 1 (toSource attrs) (Just $ toTarget attrs)]
      pure t
    DiscardedTopOfDeck iid [card] _ target | isTarget attrs target -> do
      push $ SetRole iid $ fromMaybe Neutral . headMay . toList $ cdClassSymbols $ toCardDef card
      pure t
    _ -> CrisisOfIdentity <$> runMessage msg attrs
