module Arkham.Homebrew.CircusExMortis.Locations.ExoticAnimalCar (exoticAnimalCar) where

import Arkham.Helpers.Modifiers (modifyEachMap)
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Monster))

newtype ExoticAnimalCar = ExoticAnimalCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exoticAnimalCar :: LocationCard ExoticAnimalCar
exoticAnimalCar = location ExoticAnimalCar Cards.exoticAnimalCar 3 (Static 1)

instance HasModifiersFor ExoticAnimalCar where
  getModifiersFor (ExoticAnimalCar a) =
    fromMaybe mempty <$> runMaybeT do
      st <- MaybeT getSkillTest
      liftGuardM
        $ skillTestMatches st.investigator (toSource a) st
        $ SkillTestOneOf [WhileAttackingAnEnemy monsterHere, WhileEvadingAnEnemy monsterHere]
      let tokens = filter ((== #skull) . (.face)) st.revealedChaosTokens
      lift $ modifyEachMap a tokens \t -> [ForcedChaosTokenChange t.face [#autofail]]
   where
    monsterHere = EnemyWithTrait Monster <> EnemyAt (be a)

instance HasAbilities ExoticAnimalCar where
  getAbilities (ExoticAnimalCar a) = extendRevealed a []

instance RunMessage ExoticAnimalCar where
  runMessage msg (ExoticAnimalCar attrs) = runQueueT $ ExoticAnimalCar <$> liftRunMessage msg attrs
