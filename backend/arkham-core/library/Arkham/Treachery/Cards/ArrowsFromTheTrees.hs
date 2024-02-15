module Arkham.Treachery.Cards.ArrowsFromTheTrees (
  arrowsFromTheTrees,
  ArrowsFromTheTrees (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
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
      let countAllies = \i -> selectCount $ assetControlledBy i <> #ally
      allyCount <- countAllies iid
      investigatorAssetPairs <- do
        others <-
          select
            $ NotInvestigator (InvestigatorWithId iid)
            <> InvestigatorAt (LocationWithTrait Ancient)
        forToSnd others countAllies
      pushAll
        $ assignDamage iid source (allyCount + 1)
        : [assignDamage iid' source (allyCount' + 1) | (iid', allyCount') <- investigatorAssetPairs]
      pure t
    _ -> ArrowsFromTheTrees <$> runMessage msg attrs
