module Arkham.Treachery.Cards.ChaosInTheWater (chaosInTheWater, ChaosInTheWater (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChaosInTheWater = ChaosInTheWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheWater :: TreacheryCard ChaosInTheWater
chaosInTheWater = treachery ChaosInTheWater Cards.chaosInTheWater

instance RunMessage ChaosInTheWater where
  runMessage msg t@(ChaosInTheWater attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      innocentRevelerIds <- select $ assetControlledBy iid <> assetIs Assets.innocentReveler
      investigatorsWithRevelers <-
        catMaybes <$> traverse (selectOne . HasMatchingAsset . AssetWithId) innocentRevelerIds
      sid <- getRandom
      pushAll
        [ revelationSkillTest sid iid' source #agility (Fixed 4)
        | iid' <- nub (iid : investigatorsWithRevelers)
        ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) (DamageFirst Assets.innocentReveler) 1 0
      pure t
    _ -> ChaosInTheWater <$> runMessage msg attrs
