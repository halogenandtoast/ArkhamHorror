module Arkham.Location.Cards.BoneRiddenPit (boneRiddenPit, BoneRiddenPit (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ThePitOfDespair.Helpers

newtype BoneRiddenPit = BoneRiddenPit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boneRiddenPit :: LocationCard BoneRiddenPit
boneRiddenPit = locationWith BoneRiddenPit Cards.boneRiddenPit 6 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor BoneRiddenPit where
  getModifiersFor (BoneRiddenPit x) = do
    n <- length <$> selectAgg id InvestigatorKeys (investigatorAt x.id)
    modifySelf x [ShroudModifier (-n) | n > 0]

instance HasAbilities BoneRiddenPit where
  getAbilities (BoneRiddenPit x) =
    extendRevealed
      x
      [ groupLimit PerGame
          $ restricted x 1 (Here <> thisIs x LocationWithoutClues <> youExist (InvestigatorWithKey YellowKey))
          $ FastAbility Free
      ]

instance RunMessage BoneRiddenPit where
  runMessage msg l@(BoneRiddenPit attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      flashback Flashback2
      recoverMemory ABattleWithAHorrifyingDevil
      removeChaosToken #cultist
      pure l
    _ -> BoneRiddenPit <$> liftRunMessage msg attrs
