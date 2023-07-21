module Arkham.Treachery.Cards.ArrowsFromTheTrees (
  arrowsFromTheTrees,
  ArrowsFromTheTrees (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ArrowsFromTheTrees = ArrowsFromTheTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrowsFromTheTrees :: TreacheryCard ArrowsFromTheTrees
arrowsFromTheTrees = treachery ArrowsFromTheTrees Cards.arrowsFromTheTrees

instance RunMessage ArrowsFromTheTrees where
  runMessage msg t@(ArrowsFromTheTrees attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      let countAllies = \i -> selectCount $ assetControlledBy i <> AllyAsset
      allyCount <- countAllies iid
      investigatorAssetPairs <- do
        others <-
          selectList $
            NotInvestigator (InvestigatorWithId iid)
              <> InvestigatorAt (LocationWithTrait Ancient)
        forToSnd others countAllies
      pushAll $
        InvestigatorAssignDamage iid source DamageAny (allyCount + 1) 0
          : [ InvestigatorAssignDamage iid' source DamageAny (allyCount' + 1) 0
            | (iid', allyCount') <- investigatorAssetPairs
            ]
      pure t
    _ -> ArrowsFromTheTrees <$> runMessage msg attrs
