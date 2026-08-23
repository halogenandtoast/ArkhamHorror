module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.FoyerBloodyNight (foyerBloodyNight) where

import Arkham.Ability hiding (resignAction)
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Civilian))

newtype FoyerBloodyNight = FoyerBloodyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerBloodyNight :: LocationCard FoyerBloodyNight
foyerBloodyNight = symbolLabel $ location FoyerBloodyNight Cards.foyerBloodyNight 2 (Static 0)

instance HasAbilities FoyerBloodyNight where
  getAbilities (FoyerBloodyNight a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists (EnemyWithTrait Civilian <> enemyAt a)) actionAbility
      , resignAction a
      ]

instance RunMessage FoyerBloodyNight where
  runMessage msg l@(FoyerBloodyNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      civilians <- select $ EnemyWithTrait Civilian <> enemyAt attrs
      chooseTargetM iid civilians \eid -> do
        card <- fetchCard eid
        placeUnderneath ScenarioTarget [card]
      pure l
    _ -> FoyerBloodyNight <$> liftRunMessage msg attrs
