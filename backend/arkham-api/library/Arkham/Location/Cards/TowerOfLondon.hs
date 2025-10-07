module Arkham.Location.Cards.TowerOfLondon (towerOfLondon) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.RiddlesAndRain.Helpers

newtype TowerOfLondon = TowerOfLondon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towerOfLondon :: LocationCard TowerOfLondon
towerOfLondon =
  symbolLabel
    $ locationWith TowerOfLondon Cards.towerOfLondon 3 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 2) "The Tower Bridge"

instance HasAbilities TowerOfLondon where
  getAbilities (TowerOfLondon a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> oneOf [youExist InvestigatorWithAnyResources, exists $ EnemyWithPlacement InTheShadows])
      $ forced
      $ TurnEnds #after You

instance RunMessage TowerOfLondon where
  runMessage msg l@(TowerOfLondon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOrRunOneM iid do
        withI18n $ countVar 2 $ labeled' "loseResources" $ loseResources iid (attrs.ability 1) 2
        scenarioI18n $ labeled' "towerOfLondon.attack" $ doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      inShadows <- select $ EnemyWithPlacement InTheShadows
      chooseOneAtATimeM iid do
        targets inShadows \enemy -> initiateEnemyAttack enemy (attrs.ability 1) iid
      pure l
    _ -> TowerOfLondon <$> liftRunMessage msg attrs
