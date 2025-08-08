module Arkham.Act.Cards.Timelock (timelock) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard, PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Shattered))

newtype Timelock = Timelock ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelock :: ActCard Timelock
timelock = act (4, A) Timelock Cards.timelock Nothing

instance HasAbilities Timelock where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted
          a
          1
          (youExist $ at_ (LocationWithoutClues <> LocationWithTrait Shattered))
          actionAbility
    , restricted
        a
        2
        (exists $ "Relic of Ages" <> AssetWithCardsUnderneath (HasCard $ cardIs Locations.pnakotus))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage Timelock where
  runMessage msg a@(Timelock attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
      selectEach (colocatedWith iid) \iid' -> moveTo attrs iid' aPocketInTime
      withLocationOf iid \lid -> do
        card <- field LocationCard lid
        relic <- selectJust $ AssetWithTitle "Relic of Ages"
        selectEach (UnengagedEnemy <> enemyAt lid) \eid -> enemyMoveTo attrs eid aPocketInTime
        removeLocation lid
        placeUnderneath relic [card]
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    _ -> Timelock <$> liftRunMessage msg attrs
